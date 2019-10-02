# Statistial analysis of functional traits for Savanna Seeding ms
# August 26, 2019
# Started by LML, 

# Lingering concerns: How much trait coverage do we have? How does it compare between locations? (I would assume we have more traits for 2014 and 1950 savannas compared to seed mixes). If there's a difference in coverage, is there some way to correct for it?


#install.packages("beanplot")
library(tidyverse)
library(car) #for Anova tables
library(lme4)
library(graphics)


## Load data ---------------------------
#trait coverage for all species in the project. Created in Trait_Merging.R and has 3 datasets: SeedMassSppList, slaSppList, heightSppL
load("output/traits_condensed.RData") 

## Species list with species presense/absence. Created in SppList_Master.R and the dataset is called spplist_complete
load("output/spplist_complete.RData")

### should limit to herbacious species!!!!!!
## Loading it in!! THis is the wrong place to add in herbaceous v woody, but I'm not sure where else to put it right now. Also, I don't know if the new csv with this data will save in the GIT repository or if it's just on my computer. For me, there's still a glimmer of magic in this process.
habit <- read_csv("data/Traits/herbaceous v woody.csv")


## Data Organization ----------------------
# merging traits on to presense/absence data
sp_trait <- spplist_complete %>%
  left_join(heightSppList, by = "accSppName") %>% #adding height
  select (-c(height_wi, height_mi_jae, height_oz, height_tr)) %>% #taking out data from separate trait datasets
  left_join(SeedMassSppList, by = "accSppName") %>% #adding seed mass
  select (-c(seed_wi, seed_jae, seed_oz, seed_tr)) %>% #taking out data from separate trait datasets
  left_join(slaSppList, by = "accSppName") %>% #adding sla
  select(-c(sla_wi, sla_mn, sla_mi_gh, sla_oz, sla_tr)) %>%
  left_join(habit, by = "accSppName") #adding growth habit

# make data a bit longer
sp_trait_l <- tidyr::gather(sp_trait, "location", "present", 2:4)

sp_trait_herb <- sp_trait_l %>% filter(habit == "herbaceous",#only herbaceous speice
                                       present == "yes") # only present speccies

#save(sp_trait_herb, file = "output/HerbaceousDataForGraphing.RData")


## Gathering a few summary values
spp_trait_count <- sp_trait_herb %>% 
  dplyr::filter(!duplicated(accSppName)) %>%
  add_tally(!is.na(height)) %>% 
  add_tally(!is.na(sla)) %>% 
  add_tally(!is.na(seed))




## Data analysis
## Checking for normality
par(mfrow=c(1,1))

# Seeds
# just checking out density
plot(density(sp_trait_herb$seed))
hist(sp_trait_herb$seed)
hist(log(sp_trait_herb$seed))


# Seed raw
shapiro.test(sp_trait_herb$seed) #p<0.001
qqnorm(sp_trait_herb$seed)
qqline(sp_trait_herb$seed)

# Seed natural log - yes, this looks better than raw data
shapiro.test(log(sp_trait_herb$seed)) # p=<0.4
qqnorm(log(sp_trait_herb$seed))
qqline(log(sp_trait_herb$seed))



# SLA visualization
hist(sp_trait_herb$sla)
hist(log(sp_trait_herb$seed)) #log does make it better, even though the test below isn't much better with log data


# SLA raw
shapiro.test(sp_trait_herb$sla)
qqnorm(sp_trait_herb$sla)
qqline(sp_trait_herb$sla)

# SLA natural log ## THESE LOOK THE MOST NORMAL, but the stats don't back it
shapiro.test(log(sp_trait_herb$sla))
qqnorm(log(sp_trait_herb$sla))
qqline(log(sp_trait_herb$sla))

# SLA log base 10 (but looks the same as natural log)
shapiro.test(log10(sp_trait_herb$sla))
qqnorm(log10(sp_trait_herb$sla))
qqline(log10(sp_trait_herb$sla))



# Height visualization ## Messed up because I think there is still a mix of cm and m measurements in this dataset..... I thought I fixed that. I did fix it, but then the TRY data were added and they are in meters...
hist(sp_trait_herb$height, breaks = 50) 
hist(log(sp_trait_herb$height))
#hist(exp(sp_trait_herb$height))


# Height raw
shapiro.test(sp_trait_herb$height)
qqnorm(sp_trait_herb$height)
qqline(sp_trait_herb$height)

# Height natural log
shapiro.test(log(sp_trait_herb$height)) ## I guess this one is better???
qqnorm(log(sp_trait_herb$height))
qqline(log(sp_trait_herb$height))

# Height log 2, but the data distribution does not change
shapiro.test(log2(sp_trait_herb$height))
qqnorm(log2(sp_trait_herb$height))
qqline(log2(sp_trait_herb$height))


## Analysis ----------------------------
# To Do: run an anova for each trait comparing the three classes graphed above

lm_seed <- lm(log(seed) ~ location, data = sp_trait_herb) # no, p = 0.08
summary(lm_seed)
#Anova(lm_seed, type = 3)

aov_seed <- aov(log(seed) ~ location, data = sp_trait_herb)
summary(aov_seed)
TukeyHSD(aov_seed)



lm_sla <- lm(sla ~ location, data = sp_trait_herb) # Yes! significant differences
summary(lm_sla)
#Anova(lm_sla, type = 3)

aov_sla <- aov(sla ~ location, data = sp_trait_herb)
summary(aov_sla)
TukeyHSD(aov_sla)

#lm_sla <- lm(log(sla) ~ location, data = sp_trait_herb) # but not when using the data that look more normal to me (but the shapiro.test doesn't always agree with me)
#summary(lm_sla)
#Anova(lm_sla, type = 3)


lm_height <- lm(log(height) ~ location, data = sp_trait_herb) # No difference in height between savannas and mixes
summary(lm_height)
#Anova(lm_height, type = 3)

## running the anova so I can get the tukey test results that I can interpret
aov_height <- aov(log(height) ~ location, data = sp_trait_herb)
summary(aov_height)
TukeyHSD(aov_height)









########### CODE GRAVEYARD #############

load("output/commCleaned.RData")
load("output/seedListCleaned.RData")

#species list with families, seed mix, and survey data
#NOTE: this species list got updated so we shouldn't use this one anymore
sp_list <-read.csv("data/Phylogeny/savseed_sp_list.csv") 


## Formatting data ------------------------
#To do: Merged trait and spp presnse lists, check to make sure that species matched up well
# Taking the underscores out of the species names in sp.list
sp_list$species <- gsub("_", " ", sp_list$species)

# The species names in sp_list and the trait data don't match very well. Sending sp_list through the name corrections that Quinn did for trait data so both are processed similarly.
sp_list$ourName <-sp_list$species #Create Species ID column that matches code below
namesCheck <- tnrs(query = sp_list$ourName, source = "iPlant_TNRS") #creates list of accepted names. Visually looked at "namesCheck" and all matches at score >0.89 looked great
namesCheck$accSppName <- ifelse(namesCheck$acceptedname == "", namesCheck$matchedname, namesCheck$acceptedname) #creates a column of accepted names, but uses original name if the accepted name is left blank.
namesCheck$accSppName <- ifelse(namesCheck$score == "0.9", namesCheck$submittedname, namesCheck$accSppName)

sp_list <- sp_list %>% 
  left_join(namesCheck %>% select(accSppName, submittedname), by = c("ourName" = "submittedname")) #joins the accepted name to the names file



# Merge files together NOTE: currently doesn't match well, even after name corrections...
test <- dplyr::full_join(sp_list, SeedMassSppList, by = "accSppName")

### Checking for inconsistentices between species lists
# comm-sav_names was the data that was merged with the trait data, so lets see how that compares to Chads data (sp_list) also need to merge with seedup
test2 <- comm_sav_names$accSppName #community data
test2 <- as.data.frame(test2)
test3 <- distinct(test2)
colnames(test3) <- c("accSppName")

test4 <- seedup$accSppName # seed list data
test4 <- as.data.frame(test4)
test5 <- distinct(test4)
colnames(test5) <- c("accSppName")

test6 <- bind_rows(test3, test5) #sticking comm and seed lists together
test7 <- distinct(test6)
colnames(test7) <- c("accSppName")


test3[test3$accSppName %nin% test5$accSppName,] #returns test3 rows that are not in test5

test7[test7$accSppName %nin% sp_list$accSppName,] #returns test 7 (quinn and laura's spp list) that are not in the dataset Chad used

sp_list[sp_list$accSppName %nin% test7$accSppName,] #returns species in Chad's dataset that aren't in test 7 (quinn and laura's spp list) 
