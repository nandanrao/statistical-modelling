## Preparatory script for the "Regression modelling with massive data
## sets" workshop

##############################
## Prepare R
##############################
## You will need the latest version of R (3.3.1). If you do not have it
## then download it from https://cran.r-project.org
## Update the installed packages
update.packages(ask = FALSE, repos = 'http://cran.rstudio.com/')

## Install some packages that we will use in this workshop
PFpackages <- c('biglm', 'ffbase', 'ggplot2', 'sgd', 'bigmemory',
                'glmnet', 'doMC', 'doParallel')
install.packages(PFpackages, repos = 'http://cran.rstudio.com/')
## Load the packages for creating appropriate objects for the "big"
## data sets
# ffbase Provides support for data.frame like objects that connect to
# files stored out of memory
library(ffbase)
# bigmemory provides the big.matrix object which links to matrices stored
# out of memory
library(bigmemory)


##############################
## Directories. 
##############################
mydir <- "/bigreg"
HiggsDir <- paste0(mydir, "/", "HIGGS")
AirlinesDir <- paste0(mydir, "/", "airlines"); 
setwd(mydir)


varnames <- c("signal", paste0("feature", 1:21), paste0("HLfeature",1:7))

Higgs_ffdf <- load.ffdf(paste0(HiggsDir, "/", "HIGGSffdf"))
Higgs_ffdf$test <- c(ff(0, 10500000), ff(1, 500000))

## Prepare a big.matrix object
Higgs_bigmatrix <- read.big.matrix(filename = paste0(HiggsDir, "/", "HIGGSdata.csv"),
                                   header = FALSE,
                                   col.names = varnames,
                                   type= "double",
                                   extraCols = "test",
                                   backingfile="HIGGSdata.bin",
                                   descriptorfile="HIGGSdata.desc")
Higgs_bigmatrix[, "test"] <- c(rep(0, 10500000), rep(1, 500000))

Airlines <- load.ffdf(paste0(AirlinesDir, "/", "airlinesffdf"))
