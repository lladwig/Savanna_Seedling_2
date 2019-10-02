## This merges the community species list and seed mix species list to create one master species list. This list also indicates if the species were present in past or current surveys or in the seed mixes. it is created from the species list from the original, raw community survey data on Aug 26 & 27th
# LML
# 8/27/2019

library(tidyverse)

## Load species list from community data
load("output/commCleaned_2.RData") # created in Sav_Comm_traits_DataCleaning, contains dataset called comm_sav_names2

## loads cleaned species list created in Sav_Seeds_traits_DataCleaning.R and the dataset is called seedup
load("output/seedListCleaned.RData")

## Load species list from seed mixes
s <- seedup %>% 
  distinct(accSppName, .keep_all = TRUE) %>% # unique spp in seed lists
  mutate(seed_mix = "yes") %>% #make a seed mix column
  select(accSppName, seed_mix) #select only the columns we wnat

#merge species lists from community data and seed mixes
spplist_complete <- full_join(comm_sav_names2, s) 
spplist_complete [is.na(spplist_complete)] <- "no" # replace NA with "no"

save(spplist_complete, file = "output/spplist_complete.RData")
