#create function for two variable partial plot in RF
#convert logit to actual probability
require(randomForest)
require(ggplot2)

#source code for partial plot
#modified for 2 continuous x variables (will not work for categorical)

partialPlot2<-function (x, pred.data, x.var1, x.var2, y, which.class, w, plot = TRUE, add = FALSE, palette=PuBl,
          n.pt1 = min(length(unique(pred.data[, xname1])), 51),n.pt2 = min(length(unique(pred.data[, xname2])), 51), rug = FALSE, 
          xlab = deparse(substitute(x.var1)), ylab =deparse(substitute(x.var2)), log.transform.x1=F,log.transform.x2=F,
                main = deparse(substitute(y)), plotseq=seq(min(y.pt), max(y.pt), length=6) , ...) 
{
  classRF <- x$type != "regression"
  #is FALSE for regressions
  if (is.null(x$forest)) 
    stop("The randomForest object must contain the forest.\n")
  
  xname1 <- x.var1
  
  xname2 <- x.var2
    
  
  
  xv1 <- pred.data[, xname1]
  xv2 <- pred.data[, xname2]
  n <- nrow(pred.data)
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
  if (is.factor(xv2))
    stop("Cannot predict factors.\n")
  
   
  #predict for all combinations of xv1 and xv2
    if (is.ordered(xv1)) 
      xv1 <- as.numeric(xv1)
  if (is.ordered(xv2)) 
    xv2 <- as.numeric(xv2)

  
    x.pt1 <- seq(min(xv1), max(xv1), length = n.pt1)
    x.pt2 <- seq(min(xv2), max(xv2), length = n.pt2)
  if(log.transform.x1==T)     x.pt1 <- 10^seq(log10(min(xv1)), log10(max(xv1)), length = n.pt1)
    
  
  if(log.transform.x2==T)  x.pt2 <- 10^seq(log10(min(xv2)), log10(max(xv2)), length = n.pt2) 
  

    y.pt <- matrix(0,ncol=n.pt2, nrow=n.pt1)
    
  for (i in seq(along = x.pt1)) {
    for(j in seq(along=x.pt2)){
      x.data <- pred.data
      #replace inputs for xdata1 with current level of x1[i]
      x.data[, xname1] <- rep(x.pt1[i], n)
      #replace inputs for xdata2 with current level of x2[j]
      x.data[, xname2] <- rep(x.pt2[j], n)
      if (classRF) {
        pr <- predict(x, x.data, type = "prob")
        y.pt[i,j] <- weighted.mean(pr[, focus], w, na.rm = TRUE)
      }
      else {
        y.pt[i,j] <- weighted.mean(predict(x, x.data), 
                                   w, na.rm = TRUE)
      }
   
        
         }
  }
  
    
      if (plot) 
        PuBl<-c( "#F7FCFD", "#E0ECF4", "#BFD3E6", "#9EBCDA", "#8C96C6", "#88419D", "#810F7C")
 

  
  #color.palette = colorRampPalette(PuBl, space="Lab"),
  
  filled.contour(x=x.pt1, y=x.pt2, y.pt,levels=plotseq,ylim=range(x.pt2),color.palette=colorRampPalette(PuBl, space="Lab"),
                                  lwd=2, xlab=xname1, ylab=xname2)
  
 

  invisible(list(x1 = x.pt1,x2=x.pt2, y = y.pt))
  mean.probs=expand.grid(x=x.pt1, y=x.pt2)
  mean.probs$z=as.vector((y.pt))
  colnames(mean.probs)=paste(xname1, xname2, "mean_probability", sep=",")
  tablename=paste(xname1,xname2, "meanprobs.csv")
  write.csv(mean.probs, tablename, row.names=F )
}