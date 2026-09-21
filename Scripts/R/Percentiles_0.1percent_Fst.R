## First set the working directory (change directory to where you have the needed files)

setwd("C:/Users/jaume/Desktop/PhD/Projects/STINg-NGA interplay in aboriginals/00_MANUSCRIPT")

## Load all needed packages and the installers if needed

library(tidyverse)

## Load the data of the Fst scans marking "-nan" as the null value

Fst_top_vs_south<-read_table("TopvsSouth.txt", na ="-nan")
Fst_top_vs_central<-read_table("TopvsCentral.txt", na ="-nan")
Fst_central_vs_south<-read_table("CentralvsSouth.txt", na ="-nan")

## Run the statistics for our data, ignoring the null values

Fst_TvS <- quantile(Fst_top_vs_south$WEIR_AND_COCKERHAM_FST,c(0.999, 0.001), na.rm = TRUE)
Fst_TvC <- quantile(Fst_top_vs_central$WEIR_AND_COCKERHAM_FST,c(0.999, 0.001), na.rm = TRUE)
Fst_CvS <- quantile(Fst_central_vs_south$WEIR_AND_COCKERHAM_FST,c(0.999, 0.001), na.rm = TRUE)
