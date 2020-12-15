#' Bootstrapping
#'
#' @param X Data
#' @param n Number of Trials
#'
#' @return Table of the mean, standard error and 95% confidence intervals of the bootstrapped Alpha and Beta values.
#' @export
#'
#' @examples boostrap(data$V1,10^3)
bootstrap<-function(X,n){
  tboot<-function(X){
    data_sample<-sample(X, size=length(X), replace=TRUE)
    alpha<-((mean(data_sample))^2)/(var(data_sample))
    beta<-(mean(data_sample))/(var(data_sample))
    return(c(alpha,beta))
  }
  bvalues<-replicate(n,tboot(X))
  mean_a<-round(mean(bvalues[1,]),5)
  se_a<-round(sd(bvalues[1,]),5)
  ci_a<-paste(round(quantile(bvalues[1,], probs = 0.025),5), "to",
              round(quantile(bvalues[1,], probs = 0.975),5))
  mean_b<-round(mean(bvalues[2,]),5)
  se_b<-round(sd(bvalues[2,]),5)
  ci_b<-paste(round(quantile(bvalues[2,], probs = 0.025),5), "to",
              round(quantile(bvalues[2,], probs = 0.975),5))

  summary<-c(mean_a,se_a,ci_a,mean_b,se_b,ci_b)
  table<-matrix(summary,nrow=3)
  rownames(table)<-c("Mean","Standard Error","95% Confidence Intervals")
  colnames(table)<-c("Bootstrapped Alpha","Bootstrapped Beta")
  return(table)

}
