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
figs.path   <- "/Users/alexstephens/Development/kaggle/meg/figs/"

##------------------------------------------------------------------
## Raw data structure
##------------------------------------------------------------------

## grab the subject raw data files
data.files   <- dir(,pattern="train_")

## loop over each subject and create a benchmark dataset
#for (j in 1:length(data.files)) {
for (i in 1:1) {
    
    ## load the data file (loaded object == "rawdata")
    load( data.files[i] )
   
    ## basic info
    num.trials <- dim(rawdata$X)[1]
    
    ## create a time sequence [-0.5, +1.0]
    t.vec       <- seq(from=rawdata$tmin, to=rawdata$tmax, length.out=obs.num)
    
    ## plot summaries of each of the trials
    for (j in 1:num.trials) {
        
        tmp.col <- ifelse(rawdata$y[j] == 1, "red", "black")
        tmp.lab <- ifelse(rawdata$y[j] == 1, "face", "scramble")
        
        ## average over all channels
        tmp.filename <- gsub(".Rdata",paste("_trial_",j,"_channelAvg.pdf",sep=""), data.files[i])
        pdf(paste(figs.path,tmp.filename,sep=""))
            plot(t.vec, apply(rawdata$X[j,,], 2, mean), type="l", xlab="time", ylab="fT", main=tmp.lab, col=tmp.col)
        dev.off()
        
        ## average over all timesteps
        tmp.filename <- gsub(".Rdata",paste("_trial_",j,"_timestepAvg.pdf",sep=""), data.files[i])
        pdf(paste(figs.path,tmp.filename,sep=""))
            plot(apply(rawdata$X[j,,], 1, mean), type="l", xlab="channel", ylab="fT", main=tmp.lab, col=tmp.col)
        dev.off()
        
    }
}

