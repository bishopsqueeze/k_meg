##------------------------------------------------------------------
## The purpose of this script is to:
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(caret)
library(foreach)
library(doMC)
library(plyr)

##------------------------------------------------------------------
## register cores
##------------------------------------------------------------------
registerDoMC(4)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/meg/data")

##------------------------------------------------------------------
## Define a directory to hold the benchmark datasets for each subject
##------------------------------------------------------------------
bench.path  <- "/Users/alexstephens/Development/kaggle/meg/data/benchmark"

##------------------------------------------------------------------
## Raw data structure
##------------------------------------------------------------------
## str(rawdata)
##  List of 5
##
##  $ tmin : num [1, 1] -0.5
##  $ tmax : num [1, 1] 1
##  $ sfreq: num [1, 1] 250
##  $ y    : int [1:594, 1] 0 0 1 1 0 0 0 1 0 1 ...
##  $ X    : num [1:594, 1:306, 1:375] 4.71e-12 -5.79e-14 -1.23e-12 5.62e-13 -2.48e-12 ...
##  ..- attr(*, "Csingle")= logi TRUE
##  - attr(*, "header")=List of 3
##  ..$ description: chr "MATLAB 5.0 MAT-file Platform: posix, Created on: Sat Apr  5 19:16:41 2014"
##  ..$ version    : chr "5"
##  ..$ endian     : chr "little"
##------------------------------------------------------------------

## grab the subject raw data files
data.files   <- dir(,pattern="*.Rdata")


## loop over each subject and summarize the files
for (j in 1:length(data.files)) {

    ## load the data file (loaded object == "rawdata")
    load( data.files[j] )

    ## summarize the data cube
    if (j == 1) {
        data.summary <- data.frame( file=data.files[j],
        tmin=rawdata$tmin,
        tmax=rawdata$tmax,
        sfreq=rawdata$sfreq,
        ntrial=dim(rawdata$X)[1],
        nchannels=dim(rawdata$X)[2],
        nobs=dim(rawdata$X)[3],
        nhit=sum(rawdata$y))
    } else {
        data.summary <- rbind(data.summary,
        data.frame( file=data.files[j],
        tmin=rawdata$tmin,
        tmax=rawdata$tmax,
        sfreq=rawdata$sfreq,
        ntrial=dim(rawdata$X)[1],
        nchannels=dim(rawdata$X)[2],
        nobs=dim(rawdata$X)[3],
        nhit=sum(rawdata$y)))
    }
}




## grab the subject raw data files
data.files   <- dir(,pattern="train_")


## loop over each subject and create a benchmark dataset
#for (j in 1:length(data.files)) {
for (j in 1:1) {
    
    ## load the data file (loaded object == "rawdata")
    load( data.files[j] )
    
    ## grab dimensions
    test.num    <- dim(rawdata$X)[1]
    channel.num <- dim(rawdata$X)[2]
    obs.num     <- dim(rawdata$X)[3]
   
    ## create a time sequence [-0.5, +1.0]
    t.vec       <- seq(from=rawdata$tmin, to=rawdata$tmax, length.out=obs.num)
    
    ## identify the start/stop of the post-stimulus data
    stim.lo     <- which(t.vec >= 0)[1]
    stim.hi     <- obs.num
    
    ## loop over each test case, and extract the post-stimulus data;
    ## create a single vector for each case that contains the all channel
    ## observations.  Format should be {Channel1(start,stop), Channel2(start,stop), ... }
    
    ## pre-allocate a matrix
    subject.y   <- as.vector(rawdata$y)
    subject.mat <- matrix(NA, nrow=test.num, ncol=(((stim.hi - stim.lo + 1)*channel.num)))
    
    ## load the data into a matrix
    for (i in 1:test.num) {
        subject.mat[i,]  <- as.vector( t(rawdata$X[i,,stim.lo:stim.hi]) )
    }
    colnames(subject.mat) <- paste("x",1:ncol(subject.mat),sep="")
    
    ## find highly-correlated predictors
    #high.rho <- findCorrelation(subject.mat, cutoff = 0.75)
    
    ## pre-process the data
    preproc     <- preProcess(subject.mat, method=c("center", "scale"))
    subject.pp  <- predict(preproc, newdata=subject.mat)
    
    ## set-up the fit parameters
    ctrl    <- trainControl(method="cv", number=6)

    ## repeat the single-subject experimenal results
    tmp.fit <- train(   x=subject.pp,
                        y=as.factor(subject.y),
                        method="plr",
                        trControl=ctrl)
                        
    #training <- predict(preProc, bbbDescr[1:100,-3])
    #test     <- predict(preProc, bbbDescr[101:208,-3])

}









