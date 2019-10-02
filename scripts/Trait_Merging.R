# April 16, 2019
# Laura Ladwig
# UW Postdoc

# We do not have complete trait coverage with the Damschen traits database, so we are gathering traits from other datasets. This is the code that will merge all trait datasets into one
# order of data use. (1) Damschen WI, (2) Cedar creek, (3) Michigan field collected (jae), (4) Michigan greenhouse, (5) Damschen Ozark, (6) TRY

## Current traits of focus: SLA, Seed Mass, Plant Height

library(tidyverse)
library(taxize)

####################
## Importing data ##
####################

## Damschen lab trait data - just some select traits
## This has both WI and MO (Ozark) species so probably want to separate those out. Ideally we'll just use WI data because MO is outside our area of interest, but some trait is better than nothing, so for species not in the WI dataset we can pull from Ozarks data. So, it's probably best to have a "state" column which says what state the data are from
d_trait_path <- file.path(".", "data", "traits", "cc_trait_20190117.csv")
d_trait <- read_csv(d_trait_path)

d_trait <- d_trait %>% 
  mutate(ourName = paste(Genus, Species)) #Create Species ID column with Genus and specific epithet
d_traitCheck <- tnrs(query = d_trait$ourName, source = "iPlant_TNRS")

d_traitCheck$accSppName <- ifelse(d_traitCheck$acceptedname =="", d_traitCheck$matchedname, d_traitCheck$acceptedname)
d_traitCheck$accSppName <- ifelse(d_traitCheck$accSppName == "Koeleria cristata", "Koeleria macrantha", d_traitCheck$accSppName)
d_traitCheck$accSppName <- ifelse(d_traitCheck$accSppName == "Populus virginiana", "Populus deltoides", d_traitCheck$accSppName)
d_traitCheck$accSppName <- ifelse(d_traitCheck$accSppName == "Acacia angustissim", "Acaciella angustissima", d_traitCheck$accSppName)
d_trait <- d_trait %>% 
  left_join(d_traitCheck %>% select(accSppName, submittedname), by = c("ourName" = "submittedname"))


## Damschen traits have WI (W) and Ozark (Z) measurments in the same file. Need to separate because we only want to use the Ozark traits if we have no better options. I feel like this should have been done further up in the code, but I don't know how to move it there without messing up the code, so we'll just leave it here
# WI only traits
d_wi_trait <- d_trait %>% 
  subset(select =c(accSppName, Genus, Species, WorZ_SppCode, seedmass_mg, leaf_height_cm, sla)) %>%# just pull out traits of interest. 
  separate(WorZ_SppCode, into = c("state", "sppcode"), sep = 1) %>%# Separate the W or Z off the front of the species code, make it it's own column 
  filter(state == "W") #this should keep just the WI species

# Ozark (Z) only traits. This looks funky. Ozark database only has 292 species while the WI database has 695 spp? I thought the Ozark database was more diverse? Are we missing something? 
d_oz_trait <- d_trait %>% 
  subset(select =c(accSppName,Genus, Species, WorZ_SppCode, seedmass_mg, leaf_height_cm, sla)) %>%# just pull out traits of interest. 
  separate(WorZ_SppCode, into = c("state", "sppcode"), sep = 1) %>%# Separate the W or Z off the front of the species code, make it it's own column 
  filter(state == "Z") #this should keep just the Ozark species


## Damschen lab trait data - species name updates
# This file helps correct name errors and updates in the dataset and helps it work nicely with other datasets. I *think* the trait data and savanna community surveys should merge nicely once this code is in place, but I don't know for sure
load("output/WIsppnames_mergingList_Clean.RData") #names

## Cedar creek traits
# Chad sent these over email from Cedar Creek
trait_MN_path <- file.path(".", "data", "traits", "e133_Plant traits.csv")
trait_MN <- read_csv(trait_MN_path)

trait_MN <- trait_MN %>% 
  mutate(ourName = Species) #Create Species ID column with Genus and specific epithet
trait_MNCheck <- tnrs(query = trait_MN$ourName, source = "iPlant_TNRS")

trait_MNCheck$accSppName <- ifelse(trait_MNCheck$acceptedname =="", trait_MNCheck$matchedname, trait_MNCheck$acceptedname)
trait_MNCheck$accSppName <- ifelse(trait_MNCheck$accSppName == "Koeleria cristata", "Koeleria macrantha", trait_MNCheck$accSppName)
trait_MNCheck$accSppName <- ifelse(trait_MNCheck$accSppName == "Populus virginiana", "Populus deltoides", trait_MNCheck$accSppName)
trait_MNCheck$accSppName <- ifelse(trait_MNCheck$accSppName == "Acacia angustissim", "Acaciella angustissima", trait_MNCheck$accSppName)
trait_MNCheck$accSppName <- ifelse(trait_MNCheck$accSppName == "Erechtites hieracifolia", "Erechtites hieraciifolius", trait_MNCheck$accSppName)
trait_MNCheck$accSppName <- ifelse(trait_MNCheck$accSppName == "Petalostemum", "Dalea purpurea", trait_MNCheck$accSppName)
trait_MNCheck$accSppName <- ifelse(trait_MNCheck$accSppName == "Petalostemum candidum", "Dalea candida", trait_MNCheck$accSppName)
trait_MNCheck$accSppName <- ifelse(trait_MNCheck$accSppName == "Amphicarpa bracteata", "Amphicarpaea bracteata", trait_MNCheck$accSppName)
trait_MNCheck$accSppName <- ifelse(trait_MNCheck$accSppName == "Leptoloma cognatum", "Digitaria arenicola", trait_MNCheck$accSppName)


trait_MN <- trait_MN %>% 
  left_join(trait_MNCheck %>% select(accSppName, submittedname), by = c("ourName" = "submittedname"))


## Traits from Zirbel et al. 2017 JAE
# Chad et al collected these from plants in MI savannas/prairies or seed data were gathered from KEW
trait_JAE_path <- file.path(".", "data", "traits", "trait_data_field_Zirbel_2017_dryad.csv")
trait_jae <- read_csv(trait_JAE_path)

trait_jae <- trait_jae %>% 
  mutate(ourName = species)
trait_jae$ourName <-  str_replace(trait_jae$ourName, "[.]", " ")#Create Species ID column with Genus and specific epithet
trait_jaeCheck <- tnrs(query = trait_jae$ourName, source = "iPlant_TNRS")

trait_jaeCheck$accSppName <- ifelse(trait_jaeCheck$acceptedname =="", trait_jaeCheck$matchedname, trait_jaeCheck$acceptedname)

trait_jae <- trait_jae %>% 
  left_join(unique(trait_jaeCheck %>% select(accSppName, submittedname)), by = c("ourName" = "submittedname"))
trait_jae <- trait_jae %>%
  group_by(accSppName) %>%
  summarize(meanSeedMass= mean(seed.mass),
            meanSla = mean(SLA),
            meanHt = mean(plant.height)) %>%
  ungroup()


## Traits from Chad's greenhouse project
trait_gh_path <- file.path(".", "data", "traits", "cle_prairie_trait_data_2016.csv")
trait_gh <- read_csv(trait_gh_path)

trait_gh <- trait_gh %>% 
  mutate(ourName = species) #Create Species ID column with Genus and specific epithet
trait_gh$ourName <-  str_replace(trait_gh$ourName, "[.]", " ")#Create Species ID column with Genus and specific epithet

trait_ghCheck <- tnrs(query = trait_gh$ourName, source = "iPlant_TNRS")

trait_ghCheck$accSppName <- ifelse(trait_ghCheck$acceptedname =="", trait_ghCheck$matchedname, trait_ghCheck$acceptedname)
trait_ghCheck$accSppName <- ifelse(trait_ghCheck$accSppName == "Potentilla", "Potentilla arguta", trait_ghCheck$accSppName)
trait_ghCheck$accSppName <- ifelse(trait_ghCheck$accSppName == "Solidago", "Oligoneuron rigidum", trait_ghCheck$accSppName)
trait_gh <- trait_gh %>% 
  left_join(trait_ghCheck %>% select(accSppName, submittedname), by = c("ourName" = "submittedname"))

### TRY TRAITS!! 
trait_tr_path <- file.path(".", "data", "traits", "tryTraits.txt")
trait_tr <- read.delim(trait_tr_path,  header = TRUE, sep ="\t")
trait_tr <- trait_tr %>%
  select(AccSpeciesName, TraitName, StdValue) %>%
  filter(TraitName %in% c("Seed dry mass", "Plant height vegetative", "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included")) %>%
  group_by(AccSpeciesName, TraitName) %>%
  summarize(meanStVal = mean(StdValue))%>%
  ungroup()



## Save all traits objects together.
#save(trait_gh, trait_jae, trait_MN, d_trait, d_oz_trait, d_wi_trait, trait_tr, file = "output/traits_all.RData") 



# Note: could get seed data from KEW dataset. Some KEW data is in Zirbel_jae, but haven't looked specifically for this project yet.

#####   Make species list for needed traits from comm and seedList ------------------------------

# Merging order for trait data: (1) Damschen WI, (2) Cedar creek (MN), (3) Michigan field collected (jae), (4) Michigan greenhouse (gh), (5) Damschen Ozark, (6) TRY

load("output/seedListCleaned.RData")#seedup
load("output/commCleaned.RData") #comm_sav_names
load("output/traits_all.RData")


traitSppList <- unique(bind_rows(seedup %>% select(accSppName), 
                                 comm_sav_names %>% select(accSppName))) #this created a combined species list for the seed and community data.

#save(traitSppList, file = "output/traitSppList.RData")


#################################
###### Merging trait datasets #####
#################################

###### Seed mass #########
#~*~*~*~*~*~*~*~*~*~*~*~*
SeedMassSppList <- traitSppList %>% 
  left_join(d_wi_trait %>% select(accSppName, seedmass_mg), by = "accSppName") %>%
  #left_join(trait_MN %>% select(accSppName, seedmass_mg), by = "accSppName") %>% trait_MN doesn't have seed data, so hashed out this line
  left_join(trait_jae %>% select(accSppName, meanSeedMass), by = "accSppName") %>%
  #left_join(trait_gh %>% select(accSppName, seed.mass)) by = "accSppName") %>% #also doesn't have seed data
  left_join(d_oz_trait %>% select(accSppName, seedmass_mg), by = "accSppName") %>%
  left_join(trait_tr %>% filter(TraitName == "Seed dry mass") %>% select(AccSpeciesName, meanStVal), by = c("accSppName" = "AccSpeciesName"))
  ### ADD TRY OR KEW DATA HERE FOR SEED TRAITS

## Get out the dublicate species rows that came in with the last merge
SeedMassSppList <- SeedMassSppList %>%
  group_by(accSppName)%>%
  summarize(seed_wi = mean(seedmass_mg.x),
            seed_jae = mean(meanSeedMass),
            seed_oz = mean(seedmass_mg.y),
            seed_tr = mean(meanStVal)) %>%
  ungroup()

SeedMassSppList$seed <- SeedMassSppList$seed_wi #make one seed column; 164 species with seed masses

SeedMassSppList$seed <- ifelse(is.na(SeedMassSppList$seed), #if seed data are missing; increase to 193
                               SeedMassSppList$seed_jae, #true, use MI
                               SeedMassSppList$seed) #false, leave the NA

SeedMassSppList$seed <- ifelse(is.na(SeedMassSppList$seed), #if seed data are missing; increase to 207
                               SeedMassSppList$seed_oz, #true, use OZ
                               SeedMassSppList$seed) #false, leave the NA

SeedMassSppList$seed <- ifelse(is.na(SeedMassSppList$seed), #if seed data are missing; increase to 349
                               SeedMassSppList$seed_tr, #true, use OZ
                               SeedMassSppList$seed) #false, leave the NA

#### LOTS OF MISSING DATA. WE NEED TRY OR KEW DATA



###### SLA #########
#~*~*~*~*~*~*~*~*~*~

#rename SLA things becuase it just gets confusing
d_wi_trait <- d_wi_trait %>% rename_at("sla",~"sla_wi")
trait_MN <- trait_MN %>% rename_at("Ave SLA (cm^2/g)", ~"sla_mn")
trait_jae <- trait_jae %>% rename_at("meanSla", ~"sla_mi_jae")
trait_gh <- trait_gh %>% rename_at("sla",~"sla_mi_gh")
d_oz_trait <- d_oz_trait %>% rename_at("sla",~"sla_oz")

slaSppList <- traitSppList %>% 
  left_join(d_wi_trait %>% select(accSppName, sla_wi), by = "accSppName") %>%
  left_join(trait_MN %>% select(accSppName, sla_mn), by = "accSppName") %>%
#  left_join(trait_jae %>% select(accSppName, sla_mi_jae), by = "accSppName") %>% # sla data in trait_jae seem a bit wonky. e.g., for ACHMIL it ranged from 740 to 12. I just can't handle that kind of variation, so I'm not using it here
  left_join(trait_gh %>% select(accSppName, sla_mi_gh), by = "accSppName") %>% 
  left_join(d_oz_trait %>% select(accSppName, sla_oz), by = "accSppName") %>%
  left_join(trait_tr %>% filter(TraitName == 
                                  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included") %>%
              select(AccSpeciesName, meanStVal), by = c("accSppName" = "AccSpeciesName")) %>%
              rename_at("meanStVal",~"sla_tr")

## Get out the dublicate species rows that came in with the last merge
slaSppList <- slaSppList %>%
  group_by(accSppName)%>%
  summarize(sla_wi = mean(sla_wi), 
            sla_mn = mean(sla_mn),
            #sla_mi_jae = mean(sla_mi_jae), 
            sla_mi_gh = mean(sla_mi_gh),
            sla_oz = mean(sla_oz),
            sla_tr = mean(sla_tr))

## Merging all the sla columns into one
slaSppList$sla <- slaSppList$sla_wi #make one seed column; 183 species with SLA

slaSppList$sla <- ifelse(is.na(slaSppList$sla), #if sla data are missing; increase to 201
                               slaSppList$sla_mn, #true, use MN
                               slaSppList$sla) #false, leave the NA

#slaSppList$sla <- ifelse(is.na(slaSppList$sla), #if sla data are missing
#                         slaSppList$sla_mi_jae, #true, use MI jae
#                         slaSppList$sla) #false, leave the NA

slaSppList$sla <- ifelse(is.na(slaSppList$sla), #if sla data are missing; increase to 219
                         slaSppList$sla_mi_gh, #true, use MI gh
                         slaSppList$sla) #false, leave the NA

slaSppList$sla <- ifelse(is.na(slaSppList$sla), #if sla data are missing; increase to 232
                         slaSppList$sla_oz, #true, use OZ
                         slaSppList$sla) #false, leave the NA

slaSppList$sla <- ifelse(is.na(slaSppList$sla), #if sla data are missing; increase to 269
                         slaSppList$sla_tr, #true, use TRY
                         slaSppList$sla) #false, leave the NA





###### Plant Height #####
#~*~*~*~*~*~*~*~*~*~*~*~*

## JAE height is in meters and the others are in cm. TRY is also in meters!! but I'll have to fix it later inthe code
trait_jae$meanHtcm <- trait_jae$meanHt*100


#rename plant height becuase it just gets confusing
d_wi_trait <- d_wi_trait %>% rename_at("leaf_height_cm",~"height_wi")
trait_jae <- trait_jae %>% rename_at("meanHtcm", ~"height_mi_jae")
d_oz_trait <- d_oz_trait %>% rename_at("leaf_height_cm",~"height_oz")

heightSppList <- traitSppList %>% 
  left_join(d_wi_trait %>% select(accSppName, height_wi), by = "accSppName") %>%
  #left_join(trait_MN %>% select(accSppName, sla_mn), by = "accSppName") %>% #no height
  left_join(trait_jae %>% select(accSppName, height_mi_jae), by = "accSppName") %>% 
  #left_join(trait_gh %>% select(accSppName, sla_mi_gh), by = "accSppName") %>% #no
  left_join(d_oz_trait %>% select(accSppName, height_oz), by = "accSppName") %>%
  left_join(trait_tr %>% 
              filter(TraitName == "Plant height vegetative") %>%
              select(AccSpeciesName, meanStVal), by = c("accSppName" = "AccSpeciesName")) %>%
  rename_at("meanStVal",~"height_tr")

## Get out the dublicate species rows that came in with the last merge
heightSppList <- heightSppList %>%
  group_by(accSppName)%>%
  summarize(height_wi = mean(height_wi), 
            height_mi_jae = mean(height_mi_jae), 
            height_oz = mean(height_oz),
            height_tr = mean(height_tr)*100)

## Merging all the height columns into one
heightSppList$height <- heightSppList$height_wi #make one height column; 211 species

heightSppList$height <- ifelse(is.na(heightSppList$height), #if sla data are missing; increase to 216
                         heightSppList$height_mi_jae, #true, use MI jae
                         heightSppList$height) #false, leave the NA

heightSppList$height <- ifelse(is.na(heightSppList$height), #if sla data are missing; increase to 226
                               heightSppList$height_oz, #true, use oz
                               heightSppList$height) #false, leave the NA

heightSppList$height <- ifelse(is.na(heightSppList$height), #if sla data are missing; increase to 335
                               heightSppList$height_tr, #true, use TRY
                               heightSppList$height) #false, leave the NA




###### SAVING CONSOLIDATED TRAIT DATA #########
###############################################
#save(SeedMassSppList, slaSppList, heightSppList, file = "output/traits_condensed.RData")









# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
###### OLD CODE THAT MAY STILL BE GOOD #######################
################################
### Organizing trait datasets ##
################################

######### Organizing Damschen Traits
glimpse(d_trait)

d_trait <- d_trait %>% 
  subset(select =c(Genus, Species, WorZ_SppCode, seedmass_mg, leaf_height_cm, sla)) %>%# just pull out traits of interest. 
  separate(WorZ_SppCode, into = c("state", "sppcode"), sep = 1)# Separate the W or Z off the front of the species code, make it it's own column 

# change data in "state" column. if pre=W state=WI, if pre=Z state=MO


#d_trait$dataset <- "Damschen" # Add a "dataset" column 
#d_trait <- select(d_trait, -c(Genus, Species)) #getting rid of Genus and Species to match other traits

#trait_w_name <- left_join(d_trait, nameup, by="sppcode") #merging traits with name corrections

#trait_w_name <- trait_w_name %>%
#  mutate(sppcode = if_else(merge_status == "Absolute", merge_code, sppcode)) #updates sppcode if the merge type is "Absolute", otherwise it just leaves the sppcode alone. Not sure if this code is working as it should. 

d_trait <- select(trait_w_name, c(state, sppcode, seedmass_mg, leaf_height_cm, sla, species, dataset)) #keeping only the relivant columns

###### Organizing Zirbel et al 2017 traits - Height, Seed mass, SLA
glimpse(trait_jae)
# has multiple entries per species, so summarize to get mean and varience of valuse and rename to match Damschen trait columns
trait_jae <- trait_jae %>%
  group_by(species) %>%
  summarize(leaf_height_cm  = mean(plant.height, na.rm=TRUE),
            sla = mean(SLA, na.rm=TRUE),
            seedmass_mg = mean(seed.mass, na.rm=TRUE),
            kew_seed = max(kew.seed))

trait_jae$state <- "MI" # add a "state" column and enter MI
trait_jae$dataset <- "Zirbel_jae" # add a "dataset" column and enter Zirbel_jae




##### Organizing Zirbel greenhouse traits - SLA
glimpse(trait_gh)

#All source=NA are from MI. Adding this
trait_gh$source[is.na(trait_gh$source)]<-"MI"

# has multiple entries per species, so summarize to get mean and varience of valuse and rename to match Damschen trait data
trait_gh <- trait_gh %>%
  group_by(species,source) %>% #added source here so we don't average traits from different seed sources
  summarize(sla = mean(sla, na.rm=TRUE),
            state = max(source))

#remove the source column
trait_gh$source<-NULL
           
# Note about "state" column (which was "source") it has MI=Michigan, MW=Midwest, SO=Southern. We may want to disregard the Southern species. Or, if we decide to use them then we should also use the SW traits from the Damschen trait dataset
trait_gh$dataset <- "Zirbel_gh" # add a "dataset" column 

####### Merging datasets
# Adding data end to end to make one long dataset. Then when merging it with community dataset, conditionally merge based on "state" column. So first use traits WI, if emlty use MI, if empty use MW, if empty use Z, if empty use SO
a <- bind_rows(d_trait, trait_jae)
traits <- bind_rows(a, trait_gh)

####### Saving data
# I don't know if saving it as a csv is the best way, but it's what I know how to do
write_csv(traits, path ="output/Traits_compiled_20190425.csv")
