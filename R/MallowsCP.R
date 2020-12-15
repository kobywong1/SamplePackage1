#' Mallow's CP
#'
#' @param x Data in Regression Form
#' @param d Source of data in regression
#'
#' @return Best Regression Functional Form According to Mallow's CP
#' @export
#'
#' @examples mcp(df$wage~df$gender+df$age,df)
mcp<-function(x,d){
  ss=regsubsets(x,data=d,nbest=1)
  s=summary(ss)
  return(coef(ss, which.min(s$cp)))
}
