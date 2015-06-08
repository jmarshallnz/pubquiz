score_plot <- function(w, round) {

  colorFunc <- function(x, alpha=1) {
    cf <- colorRamp(c("red", "blue"), space="Lab")
    y <- 1/(1 + exp(-250*(x-0.5)))
    rgb(cf(y), alpha=alpha*255, maxColorValue=255)
  }

  colorFunc2 <- function(x, alpha=1) {
    cf <- colorRamp(c("red", "grey80", "blue"), space="Lab")
    y <- 1/(1 + exp(-250*(x-0.5)))
    rgb(cf(y), alpha=alpha*255, maxColorValue=255)
  }

  par(mai=c(0.1,0.5,0.1,0.5))
  plot(NULL, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1), main="", xaxt="n", yaxt="n", xaxs="i", yaxs="i")
  mtext(c(-10,0,10), side=2, line=0.5, at=c(0.02, 0.5, 0.98), las=1, cex=1.5)

  # background colouring
  x <- seq(0,1,by=0.01)
  y1 <- 1-x^2
  y2 <- 1-(1-x)^2

  if (round == 1) {
    # background
    polygon(c(0,0,0.5,1,1),c(0.5,1,0.5,1,0.5), col=colorFunc(1,0.2), border=NA)
    polygon(c(0,0,0.5,1,1),c(0.5,0,0.5,0,0.5), col=colorFunc(0,0.2), border=NA)

    lines(c(0,0.5,1),c(0,0.5,0),lwd=2, col=colorFunc(0,1))
    lines(c(1,0.5,0),c(1,0.5,1),lwd=2, col=colorFunc(1,1))
    lines(c(0,1),c(0.5,0.5),lwd=1)

    points(w,1-w,bg=colorFunc2(1-w,1),pch=21,cex=4)
    points(w,w,bg=colorFunc2(w,1),pch=21,cex=4)
  } else {
    # background
    y3 <- (y1+y2)/2
    polygon(c(0,x[1:51],x[51:1],0),c(0.5,y1[1:51],y3[51:1],0.5), col=colorFunc(1,0.2), border=NA)
    polygon(c(0,x[1:51],x[51:1],0),c(0.5,y2[1:51],y3[51:1],0.5), col=colorFunc(0,0.2), border=NA)
    polygon(c(1,x[101:51],x[51:101],1),c(0.5,y1[101:51],y3[51:101],0.5), col=colorFunc(0,0.2), border=NA)
    polygon(c(1,x[101:51],x[51:101],1),c(0.5,y2[101:51],y3[51:101],0.5), col=colorFunc(1,0.2), border=NA)

    lines(x, pmax(y1,y2), lwd=2, col=colorFunc(1,1))
    lines(x, pmin(y1,y2), lwd=2, col=colorFunc(0,1))
    lines(x, y3, lwd=1)

    points(w,1-w^2,bg=colorFunc2(1-w,1),pch=21,cex=4)
    points(w,1-(1-w)^2,bg=colorFunc2(w,1),pch=21,cex=4)
  }

  legend("bottom", legend=c("Score if correct", "Score if incorrect"), fill=c("blue", "red"), bty="n", cex=1.3)
}
