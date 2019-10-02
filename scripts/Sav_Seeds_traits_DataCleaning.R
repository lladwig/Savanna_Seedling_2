## WI Savanna traits and restoration
## February 9, 2019
## Laura Ladwig 
## Postdoc at UW Madison


library(tidyverse)

#setwd("./analysis") #it would set a working directory
#getwd()


##### ~*~*~*~*~Read in data~*~*~*~*~*~ ####

## Seed List data
## These are seeding lists I found for savanna restorations in WI and the surrounding states
seed_path <- file.path(".", "data", "seedLists", "SeedingList_20190415.csv") # "file.path" tells you where to look for a dataset and sets the deliminators between files
#print(seed_path)
seed <- read_csv(seed_path) #reading in data
#glimpse(seed)

### Seed List Name Updates
## This has corrections to species names so the species on the seed list match both each other and the trait dataset in the lab
seedupdate_path <- file.path(".", "data", "seedLists", "SeedList_NameUpdate_20190307.csv")
seedupdate <- read_csv(seedupdate_path)
#glimpse(seedupdate)



####### ~*~*~*~*~*~*~*~ Clean Data ~*~*~*~*~
seed <- seed %>% select(-Notes) #deletes Notes column
seed$name <- paste(seed$genus, seed$species, sep = "_") #make a combined name

## Updating some species names in the seed list to make them consistant with others in the seed list and the trait list. 

seedup <- full_join (seed, seedupdate, by = c("name" = "old")) #merged the seed list and name updates
#glimpse(seedup)
seedup <- seedup %>% mutate (new = ifelse(is.na(new), name, new)) #this changes the "new" column to the correct species name
seedup <- dplyr::select(seedup, -c("name", "genus", "species")) # gets rid of columns associated with the old names
seedup$fullname <- seedup$new #make a new name column
seedup <- seedup %>% separate(new, into = c("genera", "species"), sep = "_") #separated a column to make a genus and species column

library(taxize) #you'll need to install if you don't have it

seedup <- seedup %>% 
  mutate(ourName = paste(genera, species)) #Create Species ID column with Genus and specific epithet
seedlistCheck <- tnrs(query = seedup$ourName, source = "iPlant_TNRS")

seedlistCheck$accSppName <- ifelse(seedlistCheck$acceptedname =="", seedlistCheck$matchedname, seedlistCheck$acceptedname)
seedlistCheck$accSppName <- ifelse(seedlistCheck$accSppName == "Koeleria cristata", "Koeleria macrantha", seedlistCheck$accSppName)
seedup <- seedup %>% 
  left_join(seedlistCheck %>% select(accSppName, submittedname), by = c("ourName" = "submittedname"))


save(seedup, file = "output/seedListCleaned.RData")

####### ~*~*~*~*~*~*~*~ Merging trait and seed data ~*~*~*~*~

### HELP ### THIS IS THE NEXT STEP THAT NEEDS WORK #############
## See similar merging notes at this step with the community data (Sav_Comm_traits_DataCleaning.R)






### Functional Trait Data ###########
trait_path <- file.path(".", "output", "Traits_compiled_20190425.csv")
traits <- read_csv(trait_path) #reading in data




### NOTE: this code below doesn't work now that traits and seed lists are not merged, but I kept it here for reference
## Questions to examine................
## Q: What's the trait coverage of the seed data?
## A: Poor, but it shouldn't be. Exported a csv so I can look through the issues in excel

seedlist_w_seed_traits <- data %>%
  group_by(fullname) %>%
  summarize(common_name = max(common),
            mean_seedmass = mean(seedmass_mg, na.rm=TRUE),
            mean_flwr_height = mean(flwr_height_cm, na.rm=TRUE),
            mean_sla = mean(sla, na.rm=TRUE),
            poll_mode = max(PollMode1),
            habit = max(USDAHabit))

write_csv(seedlist_w_seed_traits, path ="output/Seeds_w_Traits_20190211_withOzarks.csv")
          
write_csv(seedlist_w_seed_traits, file.path (".", "results", "seeds_W_traits_20190211.csv")) ## This should work. The computer automatially looks down to find the folders, so I don't really need a dot here (and if I want to use one, I could just use one)

          
          
          
          
          











###GRAVEYARD#####
#Was updating species names individually, but then decided that was a poor idea
#seed <- seed %>% mutate (name = replace(name, name == "Hystrix_patula", "Elymus_hystrix"))

# if seed$name = seedupdate#old then seed$name = seedupdate$new
seed <- seed %>% mutate (factor(ifelse(seed$name == seedupdate$old, seedupdate$new, seed$name )))

seed %>% mutate (name = replace(name, name))

## NOTE: WE ARE USING MULTIPLE TRAIT DATASETS SO THEY SHOULD BE MERGED IN A SEPARATE SCRIPT AND THEN THE TRAIT FILE CREATED IN THAT SCRIPT SHOULD BE IMPORTED HERE - 4/15/2019 LML
## Damschen lab trait data - just some select traits
trait_path <- file.path(".", "data", "cc_trait_20190117.csv")
trait <- read_csv(trait_path)
glimpse(trait)

## Damschen lab trait data - species name updates
nameup_path <- file.path(".", "data", "WIsppnames_merging_list.csv")
nameup <- read_csv(nameup_path)

## Traits from Zirbel et al. 2017 JAE
trait_JAE_path <- file.path(".", "data", "trait_data_field_Zirbel_2017_dryad.csv")
trait_JAE <- read_csv(trait_JAE_path)

## Traits from Chad's greenhouse project
trait_gh_path <- file.path(".", "data", "cle_prairie_trait_data_2016.csv")
trait_gh <- read_csv(trait_gh_path)



### Cleaning trait data
##Getting rid of the Z species and data
trait_w <- filter(trait, !grepl("^Z", WorZ_SppCode)) #gets rid of Ozark (Z) species data

# but some spp are only in the Ozark dataset, so I want to use them
trait_z <- filter(trait, !grepl("^W", WorZ_SppCode)) #gets rid of WI (W) species


trait_w$fullname <- paste(trait_w$Genus, trait_w$Species, sep = "_") #make a combined name in trait data for merging

#data.all <- full_join (seedup, trait_w, by = "fullname") #merged the seed list and trait data

data <-left_join(seedup, trait_w, by = "fullname")

###### TO DO!!!  LOOK HERE!!!############################
## make two dataframes based on if WI traits are present or not. 
#Dataset 1:Filter out species that have "NA" for USDAHabit" becasue that means we have no traits for that speceis in the  WI databse (because USDAHabit is the most basic of traits). 
#Dataset 2: filter out all species where USDAHabitat /= NA
#Then merge NA species list with the Ozark trait database. 
#Then merge together the two datasets so there is a complete species list again


