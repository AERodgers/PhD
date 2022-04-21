drawResiduals <- function(myModel){
    myResiduals <- residuals(myModel)
    par(mfrow = c(1, 3))
    hist(myResiduals,
         xlab = "Residuals",
         main = "(a) Histogram of residuals")
    qqnorm(myResiduals,
           main = "(b) Q-Q Plot of residuals")
    qqline(myResiduals,
           xlab = "Fitted values",
           ylab = "Residuals")
    plot(fitted(myModel),
         myResiduals,
         main = "(c) Residual plot")
}
