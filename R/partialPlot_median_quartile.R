#create function for partial plot in RF
#but instead of averaging across all levels of the other variables, use median, and quartiles

require(randomForest)
require(ggplot2)

#source code for partial plot
#modified for continuous x variables (will not work for categorical)

partialPlot_range_withpoints<-function (x, pred.data, x.var1,  y, which.class, w, plot = TRUE, xlab=as.character(x.name1),
                                        add = FALSE, palette=PuBl,backtransform=FALSE,
          n.pt1 = min(length(unique(pred.data[, xname1])), 500), rug = TRUE,  ...) 
{
  classRF <- x$type != "regression"
  #is FALSE for regressions
  if (is.null(x$forest)) 
    stop("The randomForest object must contain the forest.\n")
  
  xname1 <- x.var1
  
  
  
  xv1 <- pred.data[, xname1]
 
  n <- 1
  if (missing(w)) 
    w <- rep(1, n)
  #only valid for classification - find out which class to focus on for prediction
  if (classRF) {
    if (missing(which.class)) {
      focus <- 1
    }
    else {
      focus <- charmatch(which.class, colnames(x$votes))
      if (is.na(focus)) 
        stop(which.class, "is not one of the class labels.")
    }
  }
  ####
  #stop  if xv1  or xv2 is a factor
  if (is.factor(xv1))
    stop("Cannot predict factors.\n")
  #if (is.factor(xv2))
    #stop("Cannot predict factors.\n")
  
   
  #predict for all values of x1
    if (is.ordered(xv1)) 
      xv1 <- as.numeric(xv1)
  
  
    x.pt1 <- seq(min(xv1), max(xv1), length = n.pt1)
    y.pt <- data.frame("x"=x.pt1, "median"=rep(0,n.pt1), "fifth"=rep(0,n.pt1),"ninetyfifth"=rep(0,n.pt1))
    
  for (i in seq(along = x.pt1)) {
   
      x.data <- pred.data
      #replace inputs for xdata1 with current level of x1[i]
      x.data[, xname1] <- x.pt1[i]
      
      
      if (classRF) {
        pr <- predict(x, x.data, type = "prob")
        y.pt$median[i] <- median(pr[, focus], na.rm = TRUE)
        y.pt$twentyfifth[i] <- quantile(pr[, focus],.25, na.rm = TRUE)
        y.pt$seventyfifth[i] <- quantile(pr[, focus],.75, na.rm = TRUE)
      }
      else {
        y.pt$median[i] <- median(predict(x, x.data), na.rm = TRUE)
        y.pt$twentyfifth[i] <- quantile(predict(x, x.data),.25, na.rm = TRUE)
        y.pt$seventyfifth[i] <- quantile(predict(x, x.data),.75, na.rm = TRUE)
      }
   
        
  }
  
    
      
 
  
  #color.palette = colorRampPalette(PuBl, space="Lab"),
  
  z<-ggplot(data=y.pt)
  z=z+geom_ribbon(aes(x, ymin=twentyfifth, ymax=seventyfifth),  fill="grey", aes=.2)
  z=z+geom_path(aes(x, median), lwd=2)
  z=z+theme_bw()+xlab(xlab)+ylab(y)#+scale_x_log10()
  z=z+theme(
    axis.text=element_text(size=12),
    axis.title=element_text(size=12, face="bold"),
    legend.title=element_text(size=12, face="bold"),
    legend.text=element_text(size=12),
    panel.grid.major = element_line(colour=NA),
    panel.grid.minor = element_line(colour = NA),
    panel.background = element_rect(colour = NA)
  )
  print(z)
  
  
 
  
  

  invisible(list(x1 = x.pt1, y = y.pt))
}