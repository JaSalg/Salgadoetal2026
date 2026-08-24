## First set the working directory (change directory to where you have the needed files)

setwd("C:/Users/jaume/Desktop/PhD/Projects/STINg-NGA interplay in aboriginals/00_MANUSCRIPT")

## Load all needed packages and the installers if needed

library(tidyverse)

## Load the data of Tajima D without the null values for TE and SE as a dataframe

TajimaSE<-read_table("Tajima_SE_no_null.txt")
TajimaTE<-read_table("Tajima_TE_no_null.txt")


## Run the statistics for our data

TajSE <- quantile(TajimaSE$TajimaD,c(0.99, 0.01))
TajTE <- quantile(TajimaTE$TajimaD,c(0.99, 0.01))
