#' Dummy Variable
#'
#' @param x Specified Variable
#' @param i Inequality (Describes relationship between the variable and the condition) (i.e. "=","<",">=")
#' @param c Condition
#'
#' @return Generates dummy variables for a variable under a specified relationship
#' @export
#'
#' @examples dummy(data$V1,"<",5) -> generates dummy variables equal to 1 if data$V1<5, 0 otherwise.
#' dummy(data$V1,"=","MALE") -> generates dummy variables that are equal to 1 if data$V1 is "MALE", 0 otherwise.
dummy<-function(x,i,c){
  if(i=="="){
    data<-ifelse(x==c,1,0)
    return(data)
  }
  if(i=="<"){
    data<-ifelse(x<c,1,0)
    return(data)
  }
  if(i==">"){
    data<-ifelse(x>c,1,0)
    return(data)
  }
  if(i=="<="){
    data<-ifelse(x<=c,1,0)
    return(data)
  }
  if(i==">="){
    data<-ifelse(x>=c,1,0)
    return(data)
  }
}
