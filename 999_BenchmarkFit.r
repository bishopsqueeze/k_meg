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
registerDoMC(3)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/meg/data")

##------------------------------------------------------------------
## Load the summarized dataset (drop superfluous files)
##------------------------------------------------------------------
load("003_megData_exStimulusSummarizedData.Rdata")
rm(allSummary, allSummary.out, trainDescr, testDescr)

##------------------------------------------------------------------
## Define the holdout sample
##------------------------------------------------------------------

## define the true samples
trainDescr  <- trainDescr.pp[,-1]   ## drop the id
testDescr   <- testDescr.pp[,-1]    ## drop the id

## define the holdout sample
set.seed(88888888)
holdSmp     <- sample.int(nrow(trainDescr), round(0.20*nrow(trainDescr)))

## split data into holdout and training sample
holdClass   <- trainClass[holdSmp]
holdDescr   <- trainDescr[holdSmp, ]
trainClass  <- trainClass[-holdSmp]
trainDescr  <- trainDescr[-holdSmp, ]

## set-up the fit parameters
ctrl    <- trainControl(method="cv", number=5)

## repeat the single-subject experimenal results
tmp.svm <- train(   x=trainDescr,
                    y=as.factor(trainClass),
                    method="svmRadial",
                    trControl=ctrl,
                    tuneLength=8)

tmp.gbm <- train(   x=trainDescr,
                    y=as.factor(trainClass),
                    method="gbm",
                    trControl=ctrl,
                    tuneLength=10)








