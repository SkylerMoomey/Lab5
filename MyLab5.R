#task 2

#Dr. Stewart's sampling function
## sampling function
# iter = iterations, n=sample size
# set default values
mybin=function(iter=100,n=10, p=0.5){ 
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}

mybin(n=10, p=0.7)
mybin(iter=200, n=10, p=0.7)
mybin(iter=500, p=0.7)
mybin(iter=1000, p=0.7)
mybin(iter=10000, p=0.7)


#verify values for table with 10000 iterations

dbinom(x=0, size=10, prob=0.7)
dbinom(x=1, size=10, prob=0.7)
dbinom(x=2, size=10, prob=0.7)
dbinom(x=3, size=10, prob=0.7)
dbinom(x=4, size=10, prob=0.7)
dbinom(x=5, size=10, prob=0.7)
dbinom(x=6, size=10, prob=0.7)
dbinom(x=7, size=10, prob=0.7)
dbinom(x=8, size=10, prob=0.7)
dbinom(x=9, size=10, prob=0.7)
dbinom(x=10, size=10, prob=0.7)

#They are all approximately equal relative frequencies to mybin simulation

# Task 3

#sample of binomial w/o replacement, 12 white marbles, 8 black marbles
whites <- c()
for(i in 1:12) {
  whites <- c(whites, "White")
}
whites

blacks <- c()
for(i in 1:12) {
  blacks <- c(blacks, "Black")
}
blacks
population <- c(whites, blacks)

## sample
sample(population, size=5, replace=FALSE)

##hypergeometric distribution simulation function from Dr. Stewart
myhyper=function(iter=100,N=20,r=12,n=5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes over the trials
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="HYPERGEOMETRIC simulation", xlab="Number of successes")
  succ.tab/iter
}

myhyper()
myhyper(iter=200)
myhyper(iter=500)
myhyper(iter=1000)
myhyper(iter=10000)

##verify values

?dhyper
dhyper(0, 12, 8, 5)
dhyper(1, 12, 8, 5)
dhyper(2, 12, 8, 5)
dhyper(3, 12, 8, 5)
dhyper(4, 12, 8, 5)
dhyper(5, 12, 8, 5)