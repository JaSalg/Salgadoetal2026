## First set the working directory (change directory to where you have the needed files)

## Load all needed packages and the installers if needed

library(readxl)
library(pegas)
library(ape)
library(adegenet)
library(ggplot2)
library(dplyr)

## Load the metadata table

Metadata_HapNet <- read_excel("C:/Users/jaume/Desktop/PhD/Projects/STINg-NGA interplay in aboriginals/00_MANUSCRIPT/All_table.xlsx", 
                              sheet = "Metadata_R", col_types = c("text", "text", "skip", "skip", "skip"))



## Change the Region field to Factors

Metadata_HapNet$Region<-as.factor(Metadata_HapNet$Region)

## Lastly change the "ID" column name to Name

colnames(Metadata_HapNet)[1] <- "Name"

## Import the fasta file with the sequences I want, turning into a DNAbin, from there into an haplotype, and then to an haplotype network

PromAli_DNAbin<-fasta2DNAbin("promoter_alignement.fasta")
PromAli_Hap<-haplotype(PromAli_DNAbin)
PromAli_HapNet<-haploNet(PromAli_Hap)

## Make a function that allows to say which haplotype does each sequence have

countHap <- function(hap = h, dna = x){
  with(
    stack(setNames(attr(hap, "index"), rownames(hap))),
    table(hap = ind, pop = attr(dna, "dimnames")[[1]][values])
  )
}

## Use the function with the DNAbin and the Haplotype data

count.hap <- countHap(PromAli_Hap,PromAli_DNAbin)

## Turn it into a dataframe, for each sequence it displays each haplotype 
## if the sequence corresponds to that haplotype Freq = 1, otherwise Freq = 0

count.hap.df <- as.data.frame(count.hap)

## Select only for Freq = 1 so all negative rows (rows that display combinations
## of strains that do NOT have a specific haplotype) are discarded

count.hap.Freq <- count.hap.df[count.hap.df$Freq== 1,]

## Rename the column "pop" to "Name" so it matches the metadata

colnames(count.hap.Freq)[2] <- "Name"

## Merge the haplotypes dataframe and the Metadata

count.hap.Freq <- merge(count.hap.Freq, Metadata_HapNet, by="Name")

## Make a new table with the region and the haplotypes

count.hap.Freq.pop <- table(count.hap.Freq$hap, count.hap.Freq$Region)

## Plot the haplotype network. The last table will be used to make the pie charts on each node. 
## Node size is determined by the frequency on the Haplotype network.
## Other adjustments are aesthetic 

plot(PromAli_HapNet, size=attr(PromAli_HapNet, "freq"), scale.ratio=100, pie=count.hap.Freq.pop, show.mutation=1, labels = FALSE)
legend("bottomleft", colnames(count.hap.Freq.pop), col=rainbow(ncol(count.hap.Freq.pop)), pch=19, ncol=2)

## To present a bar graph displaying the % of each haplotype per region make a new data table by 
## selecting by region and haplotype, and mutating the number of each haplotype / total strains

count.hap.Freq.percent <- count.hap.Freq%>%
  dplyr::group_by(Region, hap)%>%
  dplyr::tally()%>%
  dplyr::mutate(Freq=n/sum(n))


## Plot the data where the columns (x) represent the different regions,
## with the freq as the values (y), and the haplotypes being stacked (fill).
## Add the labels by referencing the % in the table.

ggplot(count.hap.Freq.percent, aes(fill=hap, y=Freq, x=Region)) + 
  geom_bar(stat="identity", position = "fill") +
  geom_text(aes(label=paste0(sprintf("%1.1f", Freq*100),"%")), position=position_stack(vjust=0.5), colour="black",check_overlap = TRUE, size = 3.5)

