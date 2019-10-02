### August 28, 2019
## LML
## Savanna Seeding
## For some reason, the species lists that Chad was using for phylogeny and Laura and Quinn were using for traits were different. I don't know why and I can't figure it out, but I'm going back to the original data to remake the list. Here goes.

library(tidyverse)
library(reshape2)
library(dplyr)
library(Hmisc)

## Import
## ALL old bray data. Will need to pull out just the sites that were resurveyed in 2014
SuT <- read.csv("data/commSurvey/ORIGINAL_Savanna_1950_herb2.csv")
m1950 <-melt (SuT)
colnames(m1950) <- c("site", "sppcode", "freq")

## Most of the resurvey data in 2014. Two sites are missing which will be added in a later step
wide2014 <- read.csv("data/commSurvey/ORIGINAL_savanna_ladwig.csv")
m2014 <-melt (wide2014)
colnames(m2014) <- c("site", "sppcode", "freq")
# For this data, the frequency is not always integers because sometimes a different number of plots were sampled at each site. For this new data, I didn't pick 20 random quadrats to use for the analysis because I didn't know how many plots were orgininally sampled in the 1950s. Just looking at the 1950s frequencies, it's clear that >20 plots were sampled at some sites. 
m2014$freq <-round(m2014$freq, digits=0)

# putting in data from the two missing sites (1146 and 1188)
savnow <- read.csv("data/commSurvey/ORIGINAL_savannaNow_1146_1188.csv")
savnow$freq <-round(savnow$freq, digits=0)

# joining 2014 savanna data, and then to the old data
sav2014 <-rbind(savnow, m2014)
savanna <-rbind(sav2014, m1950)

# splitting site and year
savanna <- separate(savanna, site, into = c("site", "year"), sep = "_")
savanna$site <- as.numeric(savanna$site)

# Filter out sites that were not resampled in 2014. Here is list of the resurveyed sites
savanna_sites <- c(1145, 1146, 1147, 1148, 1157, 1160, 1162, 1163, 1165, 1166, 1187, 1188, 1189, 1190, 1194, 1206)
savanna <- filter(savanna, site %in% savanna_sites) #site has to be in that list to be included

# Filter out frequencies that are zero (so species that weren't present in the plot)
savanna <- filter(savanna, freq >0)

save(savanna, file = "output/commCleaned_ORIGINALdata_20190826.RData")

