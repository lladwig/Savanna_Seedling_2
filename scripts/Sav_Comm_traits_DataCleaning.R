## Seeing what traits we have for species from that dissappered Bray's savannas between 1950 and 2010
# Laura Ladwig
# April 2019

library(tidyverse)

### IMPORT DATA ######
load("output/commCleaned_ORIGINALdata_20190826.RData") #new list created in "Sav_seed_Comm_listCreation_fromOriginal.R". dataset is called savanna

## Created in Sav_names-codeCleaning.R
load("output/WIsppnames_mergingList_Clean.RData") #object called names

comm_sav_names <- left_join(savanna, names %>% dplyr::select(sppcode, merge_status, merge_code), by= "sppcode")#merging ext and names to get updated species codes


# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~ OLD DATA USED HERE ~*~*~*~*~*~*~*~*~*~*~*~*~*~
#### OLD DATA: THIS USED DATA THAT WAS DIFFERENT THAN DATA USED IN PHYLO ANALYSIS, SO SPP LIST WAS REMADE ABOVE FROM ORGINAL DATA
## Community data
## These data were complied for the first part of the WI Winter Climate Change ms
#comm_path <- file.path(".", "data", "Comm Survey", "SpeciesList_SavPrairie.csv")
#comm <- read_csv(comm_path) #reading in data
#glimpse(comm)

# Load WI sppcodes and names
#load("output/WIsppnames_mergingList_Clean.RData") #object called names

#### ARRANGE DATA #######
# pull out just the savanna sites, generally site names that start with a CB and have a 4 digit number
#unique(comm$site) #look to see unique site names

#comm_cb <- filter(comm, !grepl("^W", site)) #getting rid of the Whitford sites

#comm_cb <- comm_cb %>% separate(site, into = c("sampler", "siteid"), sep = 2) #sparated site column after the second character and turn into columns called "sampler" and "siteid"

#comm_cb$siteid <- as.integer(comm_cb$siteid) #change siteid to a number

#comm_sav <- filter(comm_cb, siteid >= 1000)# Filter to only keep savanna sites (which are siteid >1000)

#comm_sav_names <- left_join(comm_sav, names %>% select(sppcode, merge_status, merge_code), by=c("spp" = "sppcode"))#merging ext and names to get updated species codes
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~                            

## Update sppcode with a multi-step conditional statement:
#comm_sav_names$merge_status[is.na(comm_sav_names$merge_status)] <- "Empty" #renaming mergestatuses that are NA to "empty" because otherwise they get dropped in the filter step in the code below. This is probably the step where Quinn and I were loosing the species that were only found in Chad's phylo dataset. (still not sure how Chad missed all the species we had, but that's why I'm remaking this dataset from scratch)

comm_sav_corr <- comm_sav_names %>%
  filter(merge_status != "delete") %>% #takes out the bad species codes
  mutate(sppcode = if_else(merge_status == "Absolute", merge_code, sppcode)) #updates sppcode if the merge type is "Absolute", otherwise it just leaves the sppcode alone

comm_sav_names <- comm_sav_corr %>% left_join(names %>% dplyr::select(sppcode, accSppName), by = "sppcode")

#unique(comm_sav_names$accSppName)

## summarize things in the dataset to make it easier to handle
d1950 <- comm_sav_names %>%
  filter (freq >0) %>% #make sure we're only working with presence data
  filter (year == 1950) %>%
  select (year, accSppName) %>%# keeping only what we really need
  mutate(past_survey = if_else(year == "1950", "yes", "unk")) %>%
  distinct(accSppName, .keep_all=TRUE) %>% #get unique spp
  select (accSppName, past_survey) #keep only the variables we need

d2014 <- comm_sav_names %>%
  filter (freq >0) %>% #make sure we're only working with presence data
  filter (year == 2014) %>%
  select (year, accSppName) %>%# keeping only what we really need
  mutate(current_survey = if_else(year == "2014", "yes", "unk")) %>%
  distinct(accSppName, .keep_all=TRUE) %>% #get unique spp
  select (accSppName, current_survey) #keep only the variables we need

## Merge past and current lists
comm_sav_names2 <- full_join(d1950, d2014) #it has 355 spp, good!
comm_sav_names2 [is.na(bothYears)] <- "no" # replace NA with "no"

## I don't know what I should call this updated file. A new a name? a new version number? I don't now what's best, so for now I'll just save it as a new version
  
save(comm_sav_names2, file = "output/commCleaned_2.RData")
##save(comm_sav_names, file = "output/commCleaned.RData") #old version to some degree

# also want a dataset that has the site information, so cleaning that up and sending it out
long_sav <- dplyr::select (comm_sav_names, c(site, year, accSppName))

save(long_sav, file = "output/LongSavannaDataWithSites.RData")












###### Merging extinction list to traits list. 
## The ideal: Merge datasets by sppcode or full species name, but also conditionally merge based on "state" column. So first use traits WI, if empty use MI, if empty use MW, if empty use Z, if empty use SO.
## Lingering questions: How to do the species match-up. I was merging the datasets by sppcode but new Zirbel data doesn't have Damschen lab sppcodes. So we could either 1) add species codes to Zirbel data, but although most of the time sppcodes are just first three letters of genus and species names, they are not always. OR 2) match by full species names, but I think we have some old species names in our community dataset. It might be best to do the merge first based on sppcode (since WI traits are the first ones to be matched up anyway) and then merge by full species names. Then manually go through the list to see if missing species are indeed there but just name changes.

# Trait data
trait_path <- file.path(".", "data", "Traits_compiled_20190425.csv")
traits <- read_csv(trait_path) #reading in data


#make trait data long
traits.long<-traits%>%
  gather(trait, value, seedmass_mg:sla)
#remove all of the missing traits
traits.long<-traits.long[!is.na(traits.long$value),]
#order the levels of state in the order we want to select for the traits
traits.long$state <- factor(traits.long$state, levels = c("W", "MI", "MW", "Z", "SO"))
#reorder the data frame based on the levels of state
traits.long <- traits.long[order(traits.long$state),]
#for each species by trait select the first available trait value
traits.long<-traits.long[!duplicated(traits.long[,c("species", "trait")]),]

#make the data wide again
traits<-spread(traits.long,trait, value)

#aggregate to the species level (removing state)
traits.ag<-traits%>%
  group_by(species)%>% #add other columns (e.g. kew_seed) here if you want to keep them
  summarise_at(c("leaf_height_cm", "seedmass_mg", "sla"),funs(mean))




####### CODE GRAVEYARD ###################

# filter just habitat == sav
# then name a new column that is "status",  = "stay" if Year==1950, pres==1, Year == 2010, pres == 1 (now sure how to do that since that's all just in 2 columns of data)
## Or, just look at species that left. so if Year ==


## NOTE: WE ARE USING MULTIPLE TRAIT DATASETS SO THEY SHOULD BE MERGED IN A SEPARATE SCRIPT AND THEN THE TRAIT FILE CREATED IN THAT SCRIPT SHOULD BE IMPORTED HERE - 4/15/2019 LML
# Trait data present in the WI traits database
### Old data that I'm keeping aroudn until the new code works
trait_path <- file.path(".", "data", "cc_trait_20190117.csv")
trait <- read_csv(trait_path) #reading in data
glimpse(trait)

# Preparing trait dataset so it is ready to merge with extinction dataset
trait_w <- filter(trait, !grepl("^Z", WorZ_SppCode)) #gets rid of Ozark (Z) species data
trait_w <- trait_w %>% separate(WorZ_SppCode, into = c("state", "sppcode"), sep = 1)#making a sppcode to merge with other datasets by taking off the W at the beginning of the code

## TO DO !!!! Check to see if species that don't have traits in the WI trait database might be in the Ozark (Z) plant database. 
# What spp do we not have WI traits for? so if NA for seedmass_mg, leaf_height_cm, and/or dry_mass_g
missing_traits_w <- filter(ext_3ymas_trait, is.na(seedmass_mg) | is.na(leaf_height_cm) | is.na(dry_mass_g))

write_csv(missing_traits_w, path ="results/SavCommunity_missing_traits_WI.csv") #write this list as a csv

#Make an Ozark only trait dataset 
trait_z <- filter(trait, !grepl("^W", WorZ_SppCode)) #gets rid of WI (W) species data
trait_z <- trait_z %>% separate(WorZ_SppCode, into = c("state", "sppcode"), sep = 1)#making a sppcode to merge with other datasets by taking off the Z at the beginning of the code

z_trait_check <-left_join(missing_traits_w, trait_z, by = "sppcode") ## Merging the species with missing WI trait data with Z traits. Note, this creates a rather wide, slightly unrulely dataframe


## TO DO!!!!! make a dataset of species that do not have WI or Z traits, so if NA for seedmass_mg, leaf_height_cm, and/or dry_mass_g. These are the species I'd want to ask Lars about
#missing_traits <- filter(z_trait_check,  if seedmass_mg == NA or leaf_height_cm = NA or dry_mass_g ==NA)

ext_3ymas_trait <-left_join(ext_3ymas_names, traits, by = "sppcode") ## Merging the filtered list of species that went extinct in savannas with the traits list.

#write_csv(ext_3ymas_trait, path ="results/SavannaExtinctions_w_traits_v2.csv") #write this list as a csv


#### NOTE: TAPPING OUT HERE!! EXPORTING THE MISSING DATA TABLE CREATED ABOVE AND DOING THE REST IN EXCEL BECAUSE THIS HAS TURNED INTO A TANGLED MIND TRAP #######


##### CODE GRAVEYARD ----------------------------------------------------------------------

# Extincion data
# This was also compiled for WI Winter Climate change ms, it notes which species went extinct at each site over the past 60 years
ext_path <- file.path(".", "data", "Comm Survey", "species_extinctions.csv")
ext <- read_csv(ext_path) #reading in data
#glimpse(ext)


ext_sav <- filter(ext_sav, extinct == 1)#now only pick species that when extinct at one of the savanna sites, so "extinct == 1"

## This summary has the number of savanna sites that a species went extinct at. Has 215 spp
ext_summ <- ext_sav %>%
  group_by(spp) %>%
  summarize(count = sum(extinct))

ext_2ymas <- filter(ext_summ, count >= 2) # 122 spp went extinct at 2 or more sites
ext_3ymas <- filter(ext_summ, count >= 3) # 84 spp went extinct at 3 or more sites

