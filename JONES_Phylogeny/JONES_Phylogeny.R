#Install Packages####
library(tidyr)
library(tidyverse)
library(phangorn)
library(BiocManager)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("msa")
BiocManager::install("ggtree")
BiocManager::install("ShortRead")
BiocManager::install("ggmsa")
library(ggtree)
library(msa)
library(purrr)
library(ShortRead)
library(knitr)
library(ggmsa)
#Read In Data####
BIG <- readDNAStringSet("Big.fasta", format = "fasta")
GRASS <- read.csv("Grass.csv")
BIG2 <- readDNAStringSet("Big2.fasta", format = "fasta")
#Alignment####

alignment <- msa(BIG2)


#Change Tip Labels####
alignment@unmasked@ranges@NAMES <- 
  paste(alignment@unmasked@ranges@NAMES %>% str_split(pattern = " ") %>% map_chr(2),
        alignment@unmasked@ranges@NAMES %>% str_split(pattern = " ") %>% map_chr(3),
        sep = "_")
#Convert Format####
PHY <- as.phyDat(alignment)

DIST <- dist.ml(PHY)

tree <- NJ(DIST)
plot(tree)

tree2 <- pratchet(PHY)
tree2 <- nnls.phylo(tree2, DIST)


#ggtree####

ggtree(tree2, layout = "rectangular", branch.length = "none") +
  geom_tiplab(size = 3) +xlim(0,27)


#msa plot####
ggmsa(BIG, start = 280, end = 360, color = "Shapely_NT")
