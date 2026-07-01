## First set the working directory (change directory to where you have the needed files)

## Load all needed packages

library(readxl)
library(ggtree)
library(itol.toolkit)
library(dplyr)

## Upload the data file

Ska_Table <- read_excel("All_table.xlsx", sheet = "Metadata_R",
                       col_types = c("text","text", "skip", "text", "text"))


## Convert all fields to factors 

Ska_Table$Region<-as.factor(Ska_Table$Region)
Ska_Table$Activity<-as.factor(Ska_Table$Activity)
Ska_Table$emm_pattern<-as.factor(Ska_Table$emm_pattern)


## Change the "ID" column name to Name

colnames(Ska_Table)[1] <- "Name"


## Load the tree 

TreeSka <- read.tree("Ska_tree.newick")


## Use itol.tooltips to add the metadata to the tree

Unit_Region <- create_unit(data = Ska_Table %>%
                          select(Name, Region),
                        key = 'Ska_color_Region',
                        type = "DATASET_SYMBOL", 
                        color = "wesanderson", 
                        tree = TreeSka)

Unit_Activity <- create_unit(data = Ska_Table %>%
                             select(Name, Activity),
                           key = 'Ska_color_Activity',
                           type = "DATASET_SYMBOL", 
                           color = "wesanderson", 
                           tree = TreeSka)

Unit_Pattern <- create_unit(data = Ska_Table %>%
                             select(Name, emm_pattern),
                           key = 'Ska_color_Pattern',
                           type = "DATASET_SYMBOL", 
                           color = "wesanderson", 
                           tree = TreeSka)


## Print the Units as data sets to use them in the iToL browser software

write_unit(Unit_Region)
write_unit(Unit_Activity)
write_unit(Unit_Pattern)

## The toolkit will give random colors and shapes to every Region/Activity/emm-pattern
## so edit the resulting .txt before using it in iTOL
## make sure that all use shape "2" (circle) the position is "-1" (end of the branch)
## and that the colors are as follows: 

## Region

#  "Southeast" -> "#3d58a6"
#  "Central Australia" -> "#f6921e"
#  "Top End" -> "#ed3224"

## Activity

#  "High_Active" -> "#3d58a6"
#  "Low_Active" -> "#26a9e0"
#  "High_Inactive" -> "#f6921e"
#  "Low_Inactive" -> "#ed3224"

## Pattern

#  "A-C" -> "#9A6324"
#  "D" -> "#469990"
#  "E" -> "#bfef45"
#  "Y" -> "#800000"
#  "Outlier" -> "#dcbeff"
