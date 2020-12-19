#' @title Two_Fold Cross_Validation of Linear....
#' @description Two_Fold Cross_Validation of Linear
#' @param a Forecast variables
#' @param b Response variables
#' @return residual matrices
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' re <- Two_Fold_Cross_Validation(a,b)
#' print(re)
#' }
#' @export
Two_Fold_Cross_Validation <- function(a,b){
  n <- length(a)
  e1 <- e2 <- e3 <- e4 <- matrix(0,n,n)
  re <- numeric(4)
  for (i in 1:n) {
    
    for (j in (i+1):n-1){
      y <- a[-c(i,j)]
      x <- b[-c(i,j)]
      
      J1 <- lm(y ~ x)
      yhat1 <- J1$coef[1] + J1$coef[2] * b[c(i,j)]
      e1[i,j] <- mean((a[c(i,j)] - yhat1)^2)
      
      J2 <- lm(y ~ x + I(x^2))
      yhat2 <- J2$coef[1] + J2$coef[2] * b[c(i,j)] + J2$coef[3] *
        b[c(i,j)]^2
      e2[i,j] <- mean((a[c(i,j)] - yhat2)^2)
      
      J3 <- lm(log(y) ~ x)
      logyhat3 <- J3$coef[1] + J3$coef[2] * b[c(i,j)]
      yhat3 <- exp(logyhat3)
      e3[i,j] <-mean(( a[c(i,j)] - yhat3)^2)
      
      J4 <- lm(log(y) ~ log(x))
      logyhat4 <- J4$coef[1] + J4$coef[2] * log(b[c(i,j)])
      yhat4 <- exp(logyhat4)
      e4[i,j] <- mean((a[c(i,j)] - yhat4)^2)
      
    }
    
  }
  re <- c(2*sum(e1)/(n*(n-1)),2*sum(e2)/(n*(n-1)),2*sum(e3)/(n*(n-1)),2*sum(e4)/(n*(n-1)))
  re
}
NULL