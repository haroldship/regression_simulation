
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyServer(function(input, output) {
  
  
  sampleUnif <- function(n.Y) {
    runif(n.Y, min.x, max.x)
  }
  
  sampleNorm <- function(n.Y) {
    rnorm(n.Y, mean=mean.g, sd=sd.g)
  }
  
  sampleNarrow <- function(n.Y) {
    X <- rnorm(n.Y, mean=mean.g, sd=sd.g)
    X + rnorm(n.Y, sd=2)
  }
  
  k <- function(x) {
    ifelse(abs(x)<1,15/16*(1-x^2)^2,0)
  }
  
  
  onesim <- function(h, n.Y, grid.n, sampleY) {
    # get a sample for Y
    Y <- sampleY(n.Y)
    # matrix with n.Y rows, 1 column per grid point, each cell is Y[i]-grid[j]
    dist.mat <- outer(Y, values$grid.x, FUN=function(Y, x) Y-x)
    # divide the difference by the bandwidth
    X.h <- dist.mat / h
    # apply the kernel
    kX <- k(X.h)
    # sum the columns (the sample points) and divide by n*h
    f_hat <- colSums(kX) / (h*n.Y)
    # compute the regression
    f_hat_bar <- mean(f_hat)
    beta1_hat <- sum( (f_hat-f_hat_bar) * (values$g-values$g_bar) ) / values$sse_g
    beta0_hat <- f_hat_bar - beta1_hat * values$g_bar
    # compute errors of 3 types: RSS and R^2 on regression, MSE on KDE estimate of f
    rss <- sum( (f_hat - f_hat_bar - beta1_hat*(values$g-values$g_bar))^2 )
    sst <- sum( (f_hat - f_hat_bar)^2 )
    rsquared <- 1 - rss/sst
    mse <- mean( (values$f - f_hat)^2 )
    
    list(f_hat=f_hat, beta1_hat=beta1_hat, beta0_hat=beta0_hat, rss=rss, sst=sst, rsquared=rsquared, mse=mse)
  }
  
  # support for this problem
  min.x <- 0
  max.x <- 10
  mean.g <- (max.x - min.x) / 2
  sd.g <- 1
  
    # number of monte carlos to run
  MC <- 500
  
  # valued to be computed based on the inputs
  values <- reactiveValues()
  
  MC.frame <- reactive({
    # get values from UI
    values$h <- input$h

    # number of samples of Y to compute f_hat
    values$n.Y <- input$n.Y

    # of grid points
    values$grid.n <- input$grid.n
    
    # compute f, g, etc. on this grid
    values$grid.x <- seq(min.x, max.x, length.out=values$grid.n)
    
    if (input$func == "unif") {
      sampleY <- sampleUnif
      values$f <- dunif(values$grid.x, min=min.x, max=max.x)
    } else if (input$func == "norm") {
      sampleY <- sampleNorm
      values$f <- dnorm(values$grid.x, mean=mean.g, sd=sd.g)
    } else {
      sampleY <- sampleNarrow
      X <- dnorm(values$grid.x, mean=mean.g, sd=sd.g)
      values$f <- X^2
    }

    # The true function g, evaluated on the grid (X values for regression)
    values$g <- dnorm(values$grid.x, mean=mean.g, sd=sd.g)
    
    # compute regression f = beta*g + epsilon
    values$f_bar <- mean(values$f)
    values$g_bar <- mean(values$g)
    values$sse_g <- sum( (values$g-values$g_bar)^2 )
    values$beta1 <- sum((values$f - values$f_bar)*(values$g-values$g_bar)) / values$sse_g
    values$beta0 <- values$f_bar - values$beta1 * values$g_bar

    set.seed(1)
    
    MC.out <- t(replicate(MC, onesim(values$h, values$n.Y, values$grid.n, sampleY), simplify=TRUE))
    MC.frame <- data.frame(f_hat=I(t(simplify2array(MC.out[,1]))),
                           apply(MC.out[,-c(1)], 2, unlist))    
  })
  
  output$distPlot <- renderPlot({
    # histogram of beta1_hat
    beta1_hat <- MC.frame()$beta1_hat
    min.hist <- min(-0.5, round(min(beta1_hat)-0.05, 1))
    max.hist <- max(0.5, round(max(beta1_hat)+0.05, 1))
    hist(beta1_hat, main=substitute(paste("Density of ", hat(beta)[1])),
         xlab=expression(hat(beta)[1]), ylim=c(0,6),
         breaks=seq(min.hist,max.hist,by=0.05), freq=FALSE)
    abline(v=values$beta1, col="blue")
    abline(v=mean(beta1_hat), col="red", lty=5)
    
  })
  
  output$xyPlot <- renderPlot({
    data <- MC.frame()
    plot(values$g, colMeans(data$f_hat), col="darkseagreen",
         xlab=expression(g), ylab=expression(hat(f)))
    abline(a=values$beta0, b=values$beta1, col="blue")
    abline(a=mean(data$beta0_hat), b=mean(data$beta1_hat), col="red", lty=5)
  })
  
  output$mcPlot <- renderPlot({
    # plot mean and 95% confidence intervals of f_hat(x) along the grid
    data <- MC.frame()
    f_hat_bar <- colMeans(data$f_hat)
    
    X <- c(values$grid.x, rev(values$grid.x))
    lower <- apply(data$f_hat, 2, quantile, 0.841)
    upper <- rev(apply(data$f_hat, 2, quantile, 0.159))
    Y <- c(lower, upper)
    
    plot(values$grid.x, f_hat_bar, cex=0.01, ylab=expression(hat(f)), xlab="g", ylim=c(0,0.7),
         main=expression(paste(bar(hat(f)), " with ", hat(sigma)[hat(f)])))
    polygon(X, Y, col="lightgray")
    lines(values$grid.x, f_hat_bar)
  })
  
  output$pvaluePlot1 <- renderPlot({
    grid.n <- values$grid.n
    
    degfree = grid.n - 2
    
    data <- MC.frame()
    stderr.computed <- sqrt(data$rss / (values$sse_g * degfree) )
    stderr.empirical <- sd(data$beta1_hat)
    tstat <- abs(data$beta1_hat / stderr.computed)
    pval <- 2*pt(tstat, df=degfree, lower.tail=FALSE)
    main <- substitute(paste("p-value using computed std error of ", hat(beta)[1], " with ", degfree, " degrees of freedom"),
                       list(degfree=degfree))
    hist(pval, breaks=seq(0,1,by=0.05), xlab="p-value",
         main=main, freq=FALSE)
    mtext(paste(100*mean(pval < 0.05), "% of p-values less than 0.05", sep=""), side=3, col="red")
  })
  
  output$pvaluePlot2 <- renderPlot({
    grid.n <- values$grid.n

    degfree = grid.n - 2
    
    data <- MC.frame()
    stderr.computed <- sqrt(data$rss / (values$sse_g * degfree) )
    stderr.empirical <- sd(data$beta1_hat)
    tstat <- abs(data$beta1_hat / stderr.empirical)
    pval <- 2*pt(tstat, df=degfree, lower.tail=FALSE)
    main <- substitute(paste("p-value using empirical std error of ", hat(beta)[1], " with ", degfree, " degrees of freedom"),
                       list(degfree=degfree))
    hist(pval, breaks=seq(0,1,by=0.05), xlab="p-value",
         main=main, freq=FALSE)
    mtext(paste(100*mean(pval < 0.05), "% of p-values less than 0.05", sep=""), side=3, col="red")
  })
  
})
