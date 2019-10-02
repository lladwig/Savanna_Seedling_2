## I think I was trying to figure out what species are in Try.
# Quinn Sorenson
# June 2019


library(tidyverse)

trySppList <- read.table("Data/Traits/TryAccSpecies.txt", header = TRUE, sep ="\t") #read in TRY species list
load(file = "output/traitSppList.RData")



namesTryList <- traitSppList %>% 
  left_join(trySppList %>% select(AccSpeciesID, AccSpeciesName), by = c("accSppName" = "AccSpeciesName")) #join TRY species list onto  specis list so we can find out what the species ID number is.

##### creates a list of Spp ID numbers separated by commas to input into TRY text box... why does TRY do this to us. ----------
write.csv(paste(unique(namesTryList[!is.na(namesTryList$AccSpeciesID),]$AccSpeciesID), collapse = ", "), file = "output/TryAccSppID.csv")

##### Traits collected from TRY

# 11 Leaf area per leaf dry mass (SLA or 1/LMA): not yet refined if petiole is included
# 3116 Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis excluded
# 3106 Plant height vegetative
# 26 Dry seed mass
# 28 Dispersal syndrome