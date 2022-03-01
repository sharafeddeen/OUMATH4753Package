#' @title Binomial Barplot
#'
#' @param iter number of times the barplot is created or made
#' @param n  number of bars produced
#' @param p value of probability
#'
#' @return a binomial barplot
#' @export
#'
#' @examples
#' mybin(iter=100,n=10, p=0.7)
mybin=function(iter=10000,n=10, p=0.7){  #creates a binomial function
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE) # make a matrix to hold the samples. It is initially filled with NA's
  succ=c() #make a vector to hold the number of successes in each trial
  for( i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p)) #Fill each column with a new sample
    succ[i]=sum(sam.mat[,i]) #calculate a statistic from the sample (this case it is the sum)
  }
  succ.tab=table(factor(succ,levels=0:n)) #creates a success table
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes") #Make a barplot of the proportions
  succ.tab/iter #prints the table
}
