hlines<-c(c(1.0, 0.5, 0.0, -0.5))
chai.vol.panel <- function(x, ...) {
  lines(x, ...)
  panel.number <- parent.frame()$panel.number
  #abline(h = c(1.0, 0.5, 0.0, -0.25), col = "red", lty = "solid", lwd = 1.5)
  abline(v = c("2018-02-09"), col = "red", lty = "solid", lwd = 1.5)
}
