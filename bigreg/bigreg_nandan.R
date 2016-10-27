## Preparatory script for the "Regression modelling with massive data
## sets" workshop

## Ioannis Kosmidis, 27-28 October 2016

## This script will prepare your R installation and also download some
## of the data sets we will use for illustration during the
## workshop. I'd recommend to set mydir to the path of a directory in
## an external drive that has **at least 50GB of free space**.

## You need to be patient as it takes a while to download the files
## and prepare the data. In my machine (late 2012, MacBook Pro,
## Retina, 13-inch, 2.9GHz Intel Core i7, 8GB RAM) it took 42 minutes
## to download, decompress and prepare all objects on an external USB3
## solid state hard drive.

## A fast internet connection does help!

## If you are on Linux/Mac OS X the script should work out of the box
## and the only input that is required from you in the variable
## mydir. Otherwise please read comments for recommended software on
## how to decompress the download files.



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
AirlinesDir <- paste0(mydir, "/", "airlines"); cdir <- getwd()
setwd(mydir)

##############################
## Higgs data
##############################
cdir <- getwd()
setwd(HiggsDir)
# Read the HIGGS data in a ffdf object: Had to wait ~ 15 minutes
# on my laptop for the following to complete
varnames <- c("signal", paste0("feature", 1:21), paste0("HLfeature",1:7))
Higgs_ffdf <- read.csv.ffdf(file = "HIGGSdata.csv",
                            header = FALSE,
                            VERBOSE = TRUE,
                            next.rows = 5e+05,
                            col.names = varnames,
                            colClasses = rep("double", length(varnames)))
# set test variable (0 if observation is for training, 1 if for test)
Higgs_ffdf$test <- c(ff(0, 10500000), ff(1, 500000))
# Write ffdf object to the disk and go back to the working directory
save.ffdf(Higgs_ffdf, dir = "./HIGGSffdf", overwrite = TRUE)

## Prepare a big.matrix object
Higgs_bigmatrix <- read.big.matrix(filename = "HIGGSdata.csv",
                                   header = FALSE,
                                   col.names = varnames,
                                   type= "double",
                                   extraCols = "test",
                                   backingfile="HIGGSdata.bin",
                                   descriptorfile="HIGGSdata.desc")
Higgs_bigmatrix[, "test"] <- c(rep(0, 10500000), rep(1, 500000))
setwd(cdir)


##############################
## Airlines data
##############################
years <- 1987:2008 
vars <- c("Year", "Month", "DayofMonth", "DayOfWeek", "DepTime",
          "CRSDepTime", "ArrTime", "CRSArrTime", "UniqueCarrier",
          "FlightNum", "TailNum", "ActualElapsedTime",
          "CRSElapsedTime", "AirTime", "ArrDelay", "DepDelay",
          "Origin", "Dest", "Distance", "TaxiIn", "TaxiOut", "Cancelled",
          "CancellationCode", "Diverted", "CarrierDelay", "WeatherDelay",
          "NASDelay", "SecurityDelay", "LateAircraftDelay")
classes <- c(rep("double", 8), "factor", "double", "factor",
                rep("double", 5), rep("factor", 2), rep("double", 11))
Airlines <- read.csv.ffdf(file = paste0(AirlinesDir, "/", years[1], ".csv"),
                          header = TRUE, next.rows = 5e+05,
                          col.names = vars, colClasses = classes,
                          VERBOSE = TRUE)
for (year in years[-1]) {
    filepath <- paste0(AirlinesDir, "/", year, ".csv")
    yeardata <- read.csv.ffdf(file = filepath,
                          header = TRUE, next.rows = 5e+05,
                          col.names = vars, VERBOSE = TRUE,
                          colClasses = if (year < 2003) classes else NA)
    Airlines <- ffdfappend(Airlines, yeardata); delete(yeardata)
}
save.ffdf(Airlines, dir = paste0(AirlinesDir, "/", "airlinesffdf"),
          overwrite = TRUE)
setwd(cdir)
