## First set the working directory (change directory to where you have the needed files)

## Load all needed packages and the installers if needed

library(readxl)
library(corrplot)

## Import the table with the SNPs

SNP_data <- read_excel("All_table.xlsx", sheet = "NADase SNP >99 Matrix")

## Remove ID column as it is not needed

SNP_data <- SNP_data[ ,-1]

## Generate a correlation matrix using the data

cor_SNPs <- cor(SNP_data)

## Plot the correlation matrix, while only displaying the lower half as it is symmetric 

corrplot(cor_SNPs, method = 'color', type = 'lower', tl.pos = 'l')


