## First set the working directory (change directory to where you have the needed files)

## Load all needed packages and the installers if needed

#if (!requireNamespace("BiocManager", quietly=TRUE))
#  install.packages("BiocManager")
#BiocManager::install("ggtree")
#BiocManager::install("phytools")
#BiocManager::install("ggnewscale")

library(devtools)
#install_github("acarafat/tangler")
#install.packages("ggforce")

library(readxl)
library(pegas)
library(tangler)
library(ggnewscale)
library(ggplot2)
library(dplyr)
library(ggtree)
library(phangorn)
library(randomcoloR)
library(phytools)
library(ggforce)

## Then load the metadata

Metadata_phylo <- read_excel("All_table.xlsx", sheet = "Metadata_R", 
                             col_types = c("text","text", "text", "text", "skip"))

## Then change the all fields except the ID from characters (text) to Factors

Metadata_phylo$Region<-as.factor(Metadata_phylo$Region)
Metadata_phylo$emm_type<-as.factor(Metadata_phylo$emm_type)
Metadata_phylo$Activity<-as.factor(Metadata_phylo$Activity)

## Lastly change the "ID" column name to Name

colnames(Metadata_phylo)[1] <- "Name"

## Now load the .newick files and turn them into a class "tree" objects

Tree_Genome <- read.tree('fullgenome757.newick')
Tree_nga <- read.tree('nga_only_tree.newick')

## As the trees are unrooted, put their root on the midpoint of the tree

Tree_Genome <- midpoint(Tree_Genome)
Tree_nga <- midpoint(Tree_nga)

## Match taxa between trees

shared <- intersect(Tree_Genome$tip.label, Tree_nga$tip.label)

Tree_Genome <- drop.tip(Tree_Genome, setdiff(Tree_Genome$tip.label, shared))
Tree_nga <- drop.tip(Tree_nga, setdiff(Tree_nga$tip.label, shared))

Metadata_phylo <- Metadata_phylo %>% filter(Name %in% shared)


## Rotate the trees according to heuristic for maximum congruency

cop <- cophylo(Tree_nga, Tree_Genome, rotate=TRUE)

## Convert the trees to a ggtree compatible format

nga_tree  <- cop$trees[[1]]
genome_tree <- cop$trees[[2]]

## Annotate again the trees

p_right <- ggtree(nga_tree, branch.length="none") %<+% Metadata_phylo +
  geom_tippoint(aes(color=Activity)) +
  scale_color_manual(values=c("#3d58a6", "#26a9e0", "#f6921e", "#ed3224"), breaks = c("High_Active", "Low_Active", "High_Inactive", "Low_Inactive"))

p_left <- ggtree(genome_tree, branch.length="none") %<+% Metadata_phylo +
  geom_tippoint(aes(color=Region)) +
  scale_color_manual(values=c("#3d58a6", "#f6921e", "#ed3224"), breaks = c("Southeast", "Central Australia", "Top End"))

## Flip the right tree so they are facing

dl <- p_left$data
dr <- p_right$data

dr$x <- max(dr$x) - dr$x + max(dl$x) + max(dl$x)*0.55  # shift + flip horizontal
dl$tree <- "left"
dr$tree <- "right"

p_facing <- ggplot() +
  geom_tree(data = dl) +
  geom_tree(data = dr)

## Make a dataframe with connections

tips_left  <- dl %>% filter(isTip)
tips_right <- dr %>% filter(isTip)

## Match by tip label as these are the same for both trees

pairs <- tips_left %>%
  inner_join(tips_right, by="label", suffix=c(".left",".right")) %>%
  left_join(Metadata_phylo, by = c("label"="Name"))

## Prepare the proper palette for the emm-types, assuring that there will 
## be a unique colour for each emm-type, and that the emm-types of interest 
## have recognisable colours

palette_emmtype<-setNames(distinctColorPalette( length(unique(Metadata_phylo$emm_type))),
                          unique(Metadata_phylo$emm_type))

## This makes a palette -vector containing colours- with the name of the emmtype 
## associated to each of the colours.
## We can assocaite preferred colours to the emm-types of interest, but the palette
## will be redone every time, so overlapping colours may occur in new iterations.


palette_emmtype['emm121'] <- "#e12d31" 
palette_emmtype['emm113'] <- "#eeb747"
palette_emmtype['emm81'] <- "#df6d3a"
palette_emmtype['emm95'] <- "#e4d346"
palette_emmtype['emm197'] <- "#ecf038"
palette_emmtype['emm88'] <- "#ebebb3"
palette_emmtype['emm89'] <- "#8944dd"
palette_emmtype['emm28'] <- "#b0429c"
palette_emmtype['emm1'] <- "#57a8ec"
palette_emmtype['emm12'] <- "#55d4ed"
palette_emmtype['emm4'] <- "#55439f"
palette_emmtype['emm101'] <- "#710c04"

## Then change colours that are too close to the emm-types of interest 

palette_emmtype['emm22'] <- "#e7cfa8"
palette_emmtype['emm114'] <- "#53efa9"
palette_emmtype['emm218'] <- "#54bcbb"
palette_emmtype['emm76'] <- "#aa6b6d"
palette_emmtype['emm157'] <- "#586880"

# Then make a palette where everything except the colouts of interest are in light grey

palette_emmtype2 <- palette_emmtype
palette_emmtype2[1:80] <- "#ececec"
palette_emmtype2['emm121'] <- "#e12d31" 
palette_emmtype2['emm113'] <- "#eeb747"
palette_emmtype2['emm81'] <- "#df6d3a"
palette_emmtype2['emm95'] <- "#e4d346"
palette_emmtype2['emm197'] <- "#ecf038"
palette_emmtype2['emm88'] <- "#ebebb3"
palette_emmtype2['emm89'] <- "#8944dd"
palette_emmtype2['emm28'] <- "#b0429c"
palette_emmtype2['emm1'] <- "#57a8ec"
palette_emmtype2['emm12'] <- "#55d4ed"
palette_emmtype2['emm4'] <- "#55439f"
palette_emmtype2['emm101'] <- "#710c04"

## In a similar way make 3 palettes where only the emm-types of interest 
## for each region are coloured

## NT

palette_emmtypeNT <- palette_emmtype
palette_emmtypeNT[1:80] <- "#ececec"
palette_emmtypeNT['emm121'] <- "#e12d31" 
palette_emmtypeNT['emm113'] <- "#eeb747"
palette_emmtypeNT['emm81'] <- "#df6d3a"
palette_emmtypeNT['emm89'] <- "#8944dd"
palette_emmtypeNT['emm28'] <- "#b0429c"
palette_emmtypeNT['emm101'] <- "#710c04"

## CA

palette_emmtypeCA <- palette_emmtype
palette_emmtypeCA[1:80] <- "#ececec"
palette_emmtypeCA['emm113'] <- "#eeb747"
palette_emmtypeCA['emm81'] <- "#df6d3a"
palette_emmtypeCA['emm95'] <- "#e4d346"
palette_emmtypeCA['emm197'] <- "#ecf038"
palette_emmtypeCA['emm88'] <- "#ebebb3"

## SE

palette_emmtypeSE <- palette_emmtype
palette_emmtypeSE[1:80] <- "#ececec"
palette_emmtypeSE['emm89'] <- "#8944dd"
palette_emmtypeSE['emm28'] <- "#b0429c"
palette_emmtypeSE['emm1'] <- "#57a8ec"
palette_emmtypeSE['emm12'] <- "#55d4ed"
palette_emmtypeSE['emm4'] <- "#55439f"

## curved connecting lines colored by emm_type, first on the regular palette
## afterwards on the greyed out ones

## General tree

tangle_final <- p_facing +
  geom_curve(
    data = pairs,
    aes(
      x = x.left,
      y = y.left,
      xend = x.right,
      yend = y.right,
      color = emm_type
    ),
    curvature = 0.1,
    linewidth = 0.15) +
  scale_color_manual(values = palette_emmtype) +
  theme_tree()

## Grey general

tangle_finalGrey <- p_facing +
  geom_curve(
    data = pairs,
    aes(
      x = x.left,
      y = y.left,
      xend = x.right,
      yend = y.right,
      color = emm_type
    ),
    curvature = 0.1,
    linewidth = 0.15) +
  scale_color_manual(values = palette_emmtype2) +
  theme_tree()

## NT

tangle_finalNT <- p_facing +
  geom_curve(
    data = pairs,
    aes(
      x = x.left,
      y = y.left,
      xend = x.right,
      yend = y.right,
      color = emm_type
    ),
    curvature = 0.1,
    linewidth = 0.15) +
  scale_color_manual(values = palette_emmtypeNT) +
  theme_tree()

## CA

tangle_finalCA <- p_facing +
  geom_curve(
    data = pairs,
    aes(
      x = x.left,
      y = y.left,
      xend = x.right,
      yend = y.right,
      color = emm_type
    ),
    curvature = 0.1,
    linewidth = 0.15) +
  scale_color_manual(values = palette_emmtypeCA) +
  theme_tree()


## SE

tangle_finalSE <- p_facing +
  geom_curve(
    data = pairs,
    aes(
      x = x.left,
      y = y.left,
      xend = x.right,
      yend = y.right,
      color = emm_type
    ),
    curvature = 0.1,
    linewidth = 0.15) +
  scale_color_manual(values = palette_emmtypeSE) +
  theme_tree()


#Now we plot three separate trees, the tanglegram itself; 
#then the genome tree with the data from location;
#and the nga tree with the score data

tangle_final
tangle_finalGrey
tangle_finalNT
tangle_finalCA
tangle_finalSE
p_right
p_left


