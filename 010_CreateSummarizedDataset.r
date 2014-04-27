##------------------------------------------------------------------
## The purpose of this script is to:
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(caret)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/meg/data")

##------------------------------------------------------------------
## define columns headers for later use
##------------------------------------------------------------------

## channel/timeste ranges
ch.range        <- 1:306
ts.range        <- 126:375

ch.mean.cols    <- paste("cme", ch.range, sep="")
ch.sd.cols      <- paste("csd", ch.range, sep="")
ch.skew.cols    <- paste("csk", ch.range, sep="")
ch.kurt.cols    <- paste("cku", ch.range, sep="")

ts.mean.cols    <- paste("tme", ts.range, sep="")
ts.sd.cols      <- paste("tsd", ts.range, sep="")
ts.skew.cols    <- paste("tsk", ts.range, sep="")
ts.kurt.cols    <- paste("tku", ts.range, sep="")

##------------------------------------------------------------------
## loop over all *train* files and generate a summarized dataset ...
## using only the post-stimulus data
##------------------------------------------------------------------

## raw training data files
data.files      <- dir(,pattern="train")

## pre-allocate a matrix to hold the data
trainSummary <- matrix(NA, nrow=9414, ncol=4*(length(ch.range)+length(ts.range)))
trainId      <- c()
trainTrial   <- 1:9414
trainCounter <- 1
trainClass   <- c()

## loop over the training data files
for (j in 1:length(data.files)) {

    ## load the data file (loaded object == "rawdata")
    cat("loading file ... ", data.files[j], "\n")
    load( data.files[j] )
    
    ## get the number of trials for this subject
    num.trials <- dim(rawdata$X)[1]
    
    ## get the class data
    trainClass <- c(trainClass, rawdata$y)
    
    ## loop over all trials/slices and generate summary data
    for (i in 1:num.trials) {
        
        ## slice dimensions = 306 rows [channels] x 375 cols [time]
        tmp.trial <- rawdata$X[i,ch.range,ts.range]
        
        ## compute mean/sd for each
        channel.mean    <- as.vector(apply(tmp.trial, 1, mean))
        channel.sd      <- as.vector(apply(tmp.trial, 1, sd))
        channel.skew    <- as.vector(apply(tmp.trial, 1, skewness))
        channel.kurt    <- as.vector(apply(tmp.trial, 1, kurtosis))
        
        timestep.mean   <- as.vector(apply(tmp.trial, 2, mean))
        timestep.sd     <- as.vector(apply(tmp.trial, 2, sd))
        timestep.skew   <- as.vector(apply(tmp.trial, 2, skewness))
        timestep.kurt   <- as.vector(apply(tmp.trial, 2, kurtosis))
        
        ## load the matrix
        trainSummary[trainCounter,] <- c(channel.mean, channel.sd, channel.skew, channel.kurt,
                                         timestep.mean, timestep.sd, timestep.skew, timestep.kurt)
                                         
        trainCounter <- trainCounter + 1
    }
    
    ## load additional info
    trainId     <- c(trainId, rep(j, num.trials))
}
colnames(trainSummary) <- c(ch.mean.cols, ch.sd.cols, ch.skew.cols, ch.kurt.cols,
                            ts.mean.cols, ts.sd.cols, ts.skew.cols, ts.kurt.cols)

##------------------------------------------------------------------
## loop over all *test* files and generate a summarized dataset
##------------------------------------------------------------------

## raw testing data files
data.files      <- dir(,pattern="test")

## pre-allocate a matrix to hold the data
testSummary <- matrix(NA, nrow=4058, ncol=4*(length(ch.range)+length(ts.range)))
testId      <- c()
testTrial   <- 1:4058
testCounter <- 1

## loop over the testing data files
for (j in 1:length(data.files)) {
    
    ## load the data file (loaded object == "rawdata")
    cat("loading file ... ", data.files[j], "\n")
    load( data.files[j] )
    
    ## get the number of trials for this subject
    num.trials <- dim(rawdata$X)[1]
    
    ## loop over all trials/slices and generate summary data
    for (i in 1:num.trials) {
        
        ## slice dimensions = 306 rows [channels] x 375 cols [time]
        tmp.trial <- rawdata$X[i,ch.range,ts.range]
        
        ## compute mean/sd for each
        channel.mean    <- as.vector(apply(tmp.trial, 1, mean))
        channel.sd      <- as.vector(apply(tmp.trial, 1, sd))
        channel.skew    <- as.vector(apply(tmp.trial, 1, skewness))
        channel.kurt    <- as.vector(apply(tmp.trial, 1, kurtosis))

        timestep.mean   <- as.vector(apply(tmp.trial, 2, mean))
        timestep.sd     <- as.vector(apply(tmp.trial, 2, sd))
        timestep.skew   <- as.vector(apply(tmp.trial, 2, skewness))
        timestep.kurt   <- as.vector(apply(tmp.trial, 2, kurtosis))
        
        ## load the matrix
        testSummary[testCounter,] <- c(channel.mean, channel.sd, channel.skew, channel.kurt,
                                       timestep.mean, timestep.sd, timestep.skew, timestep.kurt)
        testCounter <- testCounter + 1
    }
    
    ## load additional info
    testId     <- c(testId, rep(j+16, num.trials))
}
colnames(testSummary) <- c(ch.mean.cols, ch.sd.cols, ch.skew.cols, ch.kurt.cols,
                           ts.mean.cols, ts.sd.cols, ts.skew.cols, ts.kurt.cols)

##------------------------------------------------------------------
## pre-process the data jointly
##------------------------------------------------------------------

## combine the two matrices
allSummary <- rbind(trainSummary, testSummary)

## identify near-zero variance columns
nzv <- nearZeroVar(allSummary)
if (length(nzv) > 0) {
    allSummary.nz <- allSummary[,-nzv]
} else {
    allSummary.nz <- allSummary
}

## isolate highly correlated values
allCorr          <- cor(allSummary.nz)
allHighlyCorr    <- findCorrelation(allCorr, cutoff=0.90, verbose=FALSE)

if (length(allHighlyCorr) > 0) {
    allSummary.nz.co <- allSummary.nz[,-allHighlyCorr]
} else {
    allSummary.nz.co <- allSummary.nz
}

## find linear combinations
allLinearCombos  <- findLinearCombos(allSummary.nz.co)

if (!is.null(allLinearCombos$remove)) {
    allSummary.nz.co.lc <- allSummary.nz.co[,-allLinearCombos$remove]
} else {
    allSummary.nz.co.lc <- allSummary.nz.co
}


##------------------------------------------------------------------
## split the data and center/scale the two chunks
##------------------------------------------------------------------
## Note:  We don't center/scale the full dataset b/c there are some
##        systematic differences in the data. Do train/test specific
##        center/scaling
##------------------------------------------------------------------

## combine the id and trail information
allSummary.out              <- cbind(as.vector(c(trainId, testId)), allSummary.nz.co.lc)
colnames(allSummary.out)[1] <- c("id")

## split into training/testing dataset
trainDescr        <- allSummary.out[1:9414, ]
trainClass        <- trainClass
testDescr         <- allSummary.out[(9414+1):nrow(allSummary.out), ]

## preprocess the training/test dataset
train.pp          <- preProcess(trainDescr, method=c("center", "scale"))
test.pp           <- preProcess(testDescr, method=c("center","scale"))

## apply the preprocessing to the train/test datasets
trainDescr.pp     <- predict(train.pp, trainDescr)
testDescr.pp      <- predict(test.pp, testDescr)

##------------------------------------------------------------------
## save the results
##------------------------------------------------------------------
save(   allSummary,
        allSummary.out,
        trainDescr,
        trainDescr.pp,
        trainClass,
        testDescr,
        testDescr.pp,
        file="003_megData_exStimulusSummarizedData.Rdata")







