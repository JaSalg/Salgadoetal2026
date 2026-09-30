## First set the working directory (change directory to where you have the needed files)


## Load all needed packages

library(readxl)
library(ggtree)
library(itol.toolkit)
library(dplyr)


## Import the data table

Labels_data <- read_excel("All_table.xlsx", sheet = "Metadata_R", 
                          col_types = c("text", "text", "skip", "text", "skip"))

## Change the Region and Activity fields from characters (text) to Factors

Labels_data$Region<-as.factor(Labels_data$Region)
Labels_data$Activity<-as.factor(Labels_data$Activity)

## Change the "ID" column's name to Name

colnames(Labels_data)[1] <- "Name"


## Load the phylogenetic tree 

Tree_ngaPlus <- read.tree("pro_nga_ifs_tree.newick")

## Use itol.tooltips to add the metadata to the tree

Unit_region<- create_unit(data = Labels_data %>%
                          select(Name, Region),
                        key = 'pro_nga_ifs_color_Region',
                        type = "DATASET_COLORSTRIP",
                        tree = Tree_ngaPlus)

Unit_activity<- create_unit(data = Labels_data %>%
                            select(Name, Activity),
                          key = 'pro_nga_ifs_color_Activity',
                          type = "DATASET_COLORSTRIP", 
                          tree = Tree_ngaPlus)

## Print the Units as data sets to use them in the iToL browser software

write_unit(Unit_region)
write_unit(Unit_activity)

## The toolkit will adjudicate random colors to our data
## which you can change by editing the .txt file directly
## Substitute so the end results are: 

## Region

#  "Southeast" -> "#3d58a6"
#  "Central Australia" -> "#f6921e"
#  "Top End" -> "#ed3224"

## Activity

#  "High_Active" -> "#3d58a6"
#  "Low_Active" -> "#26a9e0"
#  "High_Inactive" -> "#f6921e"
#  "Low_Inactive" -> "#ed3224"
