#' Detailed Summary Statistics
#'
#' @param x Variable to be Analyzed
#'
#' @return Table of more summary statistics, including mean, standard deviation, minimum value, 25th percentile, median, 75th percentile, maximum value, interquartile range, upper limit and lower limit.
#' @export
#'
#' @examples detailed_summary(data$V1)
detailed_summary<-function(x){
  summary<-c(mean(x),sd(x),min(x),
             quantile(x,.25),median(x),
             quantile(x,.75),max(x),
             quantile(x,.75)-quantile(x,.25),
             quantile(x,.75)+1.5*(quantile(x,.75)-quantile(x,.25)),
             quantile(x,.25)-1.5*(quantile(x,.75)-quantile(x,.25)) )
  tablea<-matrix(summary,ncol=10)
  colnames(tablea)<-c("Mean","Standard Deviation","Minimum", "25th Percentile",
                      "Median","75th Percentile","Maximum","IQR", "Upper Limit","Lower Limit")
  return(tablea)
}
