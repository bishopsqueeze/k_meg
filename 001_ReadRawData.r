##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Simply read the raw data files
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(R.matlab)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/meg/data")

##------------------------------------------------------------------
## Read the raw data by converting from .mat to .Rdata
##------------------------------------------------------------------
matlab.path  <- "/Users/alexstephens/Development/kaggle/meg/data"
matlab.files <- dir(, pattern=".mat")

for (i in 13:length(matlab.files)) {
    
    tmp.file <- matlab.files[i]
    tmp.out  <- gsub(".mat", ".Rdata", tmp.file)
    pathname <- file.path(matlab.path, tmp.file)
    rawdata  <- readMat(pathname)
    
    save(rawdata, file=tmp.out)
    
}

