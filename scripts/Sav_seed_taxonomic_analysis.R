## Taxonomic analysis for the Savanna Seed project
# 8/29/2019
# started by LML

## load packages
library(vegan)
library(MASS) 
library(tidyverse)
library(dplyr)
library(reshape2)
library(data.table)

### load data
## loads cleaned species list created in Sav_Seeds_traits_DataCleaning.R and the dataset is called seedup
load("output/seedListCleaned.RData")

## Loads long format data of survey data with corrected species names. Created in Sav_Comm_traits_DataCleaning.R and the data is called long_sav
load("output/LongSavannaDataWithSites.RData")

# importing habit because we just wnat to look at herbaceous species
herbaceous_v_woody <- read_csv("data/Traits/herbaceous v woody.csv")

## Individual site info is needed for the ordination, so we can't use this data. But I kept it in because it could still be useful for a different analysis.
#Species list with species presense/absence. Created in SppList_Master.R and the dataset is called spplist_complete
#load("output/spplist_complete.RData")

### Data organization for ordination -----------------------------
#the ordination below assumes your data has 3 columns: site, species, and abundance
#with column headers "site", "species", "libabund"

## seed mixes
seedup1 <- seedup #making a copy to work with
seedup1 <- dplyr::select(seedup1, c(Source, accSppName)) #selecting just the data we need
seedup1$presence <- 1 # add a presence column

## community surveys
long_sav1 <- long_sav #making a copy so I don't mess up the original file
long_sav1$presence <- 1 # add a presence column
long_sav1$Source <-paste (long_sav1$site, long_sav1$year, sep= "_") # combining site and year into one name
long_sav1 <- dplyr::select(long_sav1, c(Source, accSppName, presence)) #selecting just what I want

# Merge data
# adds it end to end
long <- bind_rows(seedup1, long_sav1)

# renaming Source to site
long <- dplyr::rename(long, site = Source)

# matching up the habit data, since we're only looking at herbaceous species
addHabit <- left_join(long, herbaceous_v_woody, by = "accSppName")

long_h <- filter(addHabit, habit == "herbaceous") #Filter to just get herbaceous
long_h <- long_h %>% dplyr::select(-habit) # take habit out

long_h <- distinct(long_h) #taking out duplicate rows. I don't know why they are in there, but they are.

short <- dcast(long_h, site~accSppName) #make it wide


### Data analysis -------------------------------

## Ordination of taxonomic data
short[is.na(short)] = 0 #replaces NAs with 0s
rownames(short)<-short$site #set rownames to site names
short=short[,-1] 		#drop site names column

m1 <- metaMDS(short, distance = "jaccard", k = 2, trymax = 500,
              autotransform =FALSE,  
              wascores = TRUE, expand = TRUE,
              trace = 1, plot = FALSE,)

#plot ordination
# The solution is only 2 dimensions, so don't need to look at 3
plot(m1, display = c("sites"), choices = c(1,2),
     type = "t", shrink = TRUE)
#plot(m1, display = c("sites"), choices = c(1,3),
#     type = "t", shrink = TRUE)
#plot(m1, display = c("sites"), choices = c(2,3),
#     type = "t", shrink = TRUE)

scores(m1, display = c("sites", "species"), shrink = FALSE)
print (m1)
str(m1)     
m1$species
m1$points

## PERMANOVA analysis
#create a group vector that has our three groups (current, past, and seed mixes)
survey.group <- ifelse(grepl("1950", rownames(short)), "past",
              ifelse(grepl("2014", rownames(short)), "current", "seed.mix"))

#run the PERMANOVA
adonis(vegdist(short)~survey.group,permutations=10000)
#significant difference in speices composition between groups

#test for significance of homogeneity of 
#multivariate dispersions
test.beta<-betadisper(vegdist(short),survey.group) 
anova(test.beta) #non-sig! Whew thank goodness! Glad we don't have to deal with that nightmare

#similarity percentages between groups
#This can tell us which species are driving differences between the three groups.
sim.savseed<-simper(short,survey.group)
summary(sim.savseed,ordered=TRUE)

## making a nice dataset that can be saved and graphed eleswhere (Sav_seed_taxonomic_plotting.R)
tax_nmds_points <- m1$points
tax_nmds_points <- as.data.frame(tax_nmds_points)
setDT(tax_nmds_points, keep.rownames = TRUE)[]


## Are dominant/common species similar?
# organizing data before merging to summarize
# comm surveys
long_sav2 <- long_sav #making a copy of original data to work with
long_sav2$presence <- 1 #adding a presence column
colnames(long_sav2) <- c("Source", "year", "accSppName", "presence") #renaming so they match seed
long_sav2$Source <- as.character(long_sav2$Source)

#seed mixes
seedup2 <- seedup #making a cipy to wirk iwith
seedup2 <- dplyr::select(seedup2, c("Source", "accSppName")) #picking what I want
seedup2$presence <- 1 #adding a presence column
seedup2$year <- "seed" #adding a column that says this is seed data
seedup2 <- dplyr::select(seedup2, c("Source", "year", "accSppName", "presence")) #reorder to match

# merge datasets
long2 <- bind_rows(seedup2, long_sav2)

# merge in habitat data
long2 <- left_join(long2, herbaceous_v_woody, by = "accSppName")

long2 <- long2 %>% distinct() #takes out duplicates. For some reason there are lots of duplicates in there!


# sumarising data
# for each summary (sum or count) for each year
sums <- long2 %>%
  filter(habit == "herbaceous") %>% #just herbaceous species
  filter(year != 2014) %>% #because we don't really care about degrated ones here
  group_by(accSppName, year) %>% # group by these two factors
  summarize(freq = sum(presence)) #sum the number of occurances across all sites/seed sources

freq_pres <- tidyr::spread(sums, year, freq)
colnames(freq_pres) <- c("spp", "past", "seedmix") #renaming columns for conveneince but also because "1950" was problematic becauseit was a nubmer

freq_pres[is.na(freq_pres)] <- 0

# calculate proportion of occurance, since there are slight more savanna sites (16 savanna sites, 14 seed mixes)
freq_pres$past_p <- freq_pres$past /16
freq_pres$seedmix_p <- freq_pres$seedmix /14


## Statistical Testing

## First looking at data
par(mfrow=c(1,1))

# Past data
hist(freq_pres$past, breaks = 10) #check what raw data look like
hist(log(freq_pres$past)) #check what 
shapiro.test(freq_pres$past) #test




# Linear regression
lm_tax <- lm(past_p ~ seedmix_p, data = freq_pres) # frequency of occurance in historic savannas and seed mixes is not related
summary(lm_tax)

# logistic regression
loglm_tax <- glm(past_p ~ seedmix_p, data = freq_pres, family = "binomial") #not significant, but I don't think it's run correctly
summary(loglm_tax)

## This logistic regression accounts for the size of the dataset (n-1), but I'm not really sure how it encorporates it.
past_fail <- 15
logit_tax <- glm(cbind(past,past_fail)~seedmix_p, data=freq_pres, family="binomial") #where is past_fail= 16-past
summary(logit_tax)

# logistic regression w logit (I'm not sure what the difference is...)
#logllm_tax <- glm(past_p ~ seedmix_p, data = freq_pres, family = binomial(link='logit')) #not significant
#summary(logllm_tax)


#Saving datasets from this file so I can graph them elsewhere (Sav_seed_taxonomix_plotting.R)
save(long2, freq_pres,tax_nmds_points, file = "output/TaxDataForGraphing.RData")







######################### CODE GRAVEYARD ##################################
# make data a bit longer
spplong <- tidyr::gather(spplist_complete, "location", "present", 2:4)

# changes to presense data from words to numbers
spplong <- mutate(spplong, 
                  present = dplyr::recode(present, 
                                          yes = 1, 
                                          no = 0))

## Reordering and renaming columns by just making an new dataset. Looking back, I'm not sure the columns need to be renamed, but I guess it doesn't hurt
short <- 0
short$site <- spplong$location
short$species <- spplong$accSppName
short$libabund <- spplong$present
short <- data.frame(short) #turning it into a dataframe
short <- select(short, c(site, species, libabund)) #just keeping the columns I want

#write.table(short,"/Users/laura/Desktop/Writing Projects/Savannas - WI/R/Results/OrdinationScoresAllCommunities_20170509.txt", sep="\t") 