
gs <- function(method="loess", se=FALSE, lty=2, lwd=1.35, col = .ggblue,...) {
  args <- list(...)
  c(args,list(method=method,se=se,lty=lty,lwd=lwd,col=col))
}
ga <- function(intercept=0, slope=1, lwd=1.35, col="darkgrey",...) {
  args <- list(...)
  c(args,list(intercept=intercept, slope=slope,col=col,lwd=lwd))
}
gh <- function(yintercept=0, lwd=1.35, col="darkgrey",...) {
  args <- list(...)
  c(args,list(yintercept=yintercept,lwd=lwd,col=col))
}
scatt_as <- function(..., identity = ga(), smooth = gs()) {
  scatt(..., identity = identity, smooth = smooth)
}
scatt_h <- function(..., hline = gh()) {
  scatt(..., hline = hline)
}
scatt_hs <- function(...,smooth = gs()) {
  scatt(..., smooth = smooth())
}

