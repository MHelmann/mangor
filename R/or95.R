#' @title inverse logistic link function
#' @description A function that returns the odds ratios and 95% confidence intervals from a vector of coeﬀicient estimates and a vector of standard error
#' @param coef coefficient from the fitted model
#' @param se standard error of the coefficient
#' @param siglevel significance level
#' @param roundto number of decimals places to round to
#' @return "(OR, ORlcl, ORucl)"
#' @importFrom stats qnorm
#' @author Maksim Helmann
#' @export

OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}
