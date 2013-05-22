#' Heatmap
#' 
#' @param pv        The data matrix
#' @param groups    Groups of arrays
image2 <- function(pv, groups=NULL, bkgval=6.8, as=c(21, 20), col=c("orange","turquoise3","brown","seagreen"), low="blue1", mid="white", high="red1", ncolors=123, bar=T, main="", xlab1="log2 signal intensity", scale=T, cex.y.axis=0.4, labticks=TRUE){
  library(marray)
  
  # Layout
  if (!is.null(groups)) {
    layout(matrix(1:2, 1, 2), width = c(3, 8))
    if (bar==T) {
      layout(matrix(1:3, 1, 3), width = c(2, 8, 1))
    }
  } else {
     if (bar==T) {
      layout(matrix(1:2, 1, 2), width = c(8, 1))
     }
  }
  
  # Plot box with mean curves
  if (!is.null(groups)) {
   a <- aggregate(pv, list(as.character(groups)), mean, na.rm=T)
   ra <- range(a[, 2:ncol(a)], na.rm=T)
   n <- nlevels(factor(groups))
   l <- levels(factor(groups))
   w <- which(groups==l[1])
   if(length(w) == 1) {
     cm <- pv[w, ncol(pv):1]
   } else {
     cm <- colMeans(pv[w, ncol(pv):1], na.rm=T)
   }
   wcm <- which(cm < bkgval)
   par(mar = c(4, 1, 7, 5))
   plot(cm, 1:length(cm), 
        type="l", lwd=1.5, col=col[1], 
        yaxt="n", 
        xlab=xlab1, ylab="", cex.axis=0.75,
        ylim=c(as[1], length(cm) - as[2]), xlim=ra)
   axis(4, 1:length(cm), rep("", length(cm)), labels=labticks, tick=labticks)
   lines(rep(bkgval,length(cm)), 1:length(cm), lty=2, col="purple", lwd=1)
   for (i in 2:n) {
     w <- which(groups==l[i])
     if (length(w)==1) {
       cm <- pv[w,ncol(pv):1]
     } else {
       cm <- colMeans(pv[w,ncol(pv):1], na.rm=T)
     }
     lines(cm, 1:length(cm), col=col[i])
     wcm <- intersect(wcm, which(cm<bkgval))
   }
   if (length(wcm) > 0) {
    axis(4,(1:length(cm))[wcm], rep("", length(wcm)), las=1, cex.axis=cex.y.axis, col.ticks="purple", tick=F)
   }
  }
  
  m <- maPalette(low=low, high=high, mid=mid, k=ncolors)
  matrix <- pv[, ncol(pv):1]
  
  # Scale data
  if (scale==T) {
   me <- colMeans(matrix, na.rm=T)
   s <- apply(matrix, 2, sd, na.rm=T)
   matrix <- t((t(matrix) - me) / s)
  }
  
  zlim <- range(matrix, na.rm=T)
  a <- max(c(-floor(zlim[1]), ceiling(zlim[2])))
  zlim <- c(-a, a)
  vlim <- min(zlim):max(zlim)
  par(mar = c(4, 2.5, 7, 2))
  image(matrix, col=m, zlim=zlim, xaxt="n", yaxt="n", main=main)
  box()
  axis(2, seq(0, 1, length=ncol(matrix)), colnames(matrix), las=1, cex.axis=cex.y.axis, labels=labticks, tick=labticks)
  axis(3, seq(0,1,length=nrow(matrix)), rownames(matrix), las=3, tick=F, labels=FALSE)
  
  if (!is.null(groups)) {
    if (length(wcm) > 0){
     axis(2, seq(0, 1, length=ncol(matrix))[wcm], colnames(matrix)[wcm], las=1, cex.axis=cex.y.axis, col.axis="purple", col.ticks="purple", tick=F)
    }
    for (i in 1:n) {
      w <- which(groups==l[i])
      axis(3, seq(0, 1, length=nrow(matrix))[w], rownames(matrix)[w], las=3, tick=F, col.axis=col[i])
    }
  }
  
  if (bar==T) {
    par(mar = c(4, 0, 6, 3))
    q <- as.character(vlim)
    image(1, vlim, matrix(vlim, 1, length(vlim)), axes=F, xlab="", ylab="", col=m)
    axis(4, at=vlim, las=1)
    box()
  }
}
