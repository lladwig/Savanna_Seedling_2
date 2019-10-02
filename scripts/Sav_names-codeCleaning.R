
# Species names
# these are the merging codes to merge species codes with species names for the traits database
names_path <- file.path(".", "data", "Traits", "WIsppnames_merging_list.csv")
names_raw <- read_csv(names_path) #reading in data

# This needs to be added onto the longer name correciton list. These are corections or additional species that relate to savannas. Not adding these drops the associated data from analysis
names_SAV_UPDATE <- read.csv("data/Traits/WIsppnames_merging_list_SAVANNA_ADDITIONS.csv")

names <- bind_rows(names_raw, names_SAV_UPDATE)


library(taxize) #you'll need to install if you don't have it

names <- names %>% 
  mutate(ourName = paste(Genus, Species)) #Create Species ID column with Genus and specific epithet
namesCheck <- tnrs(query = names$ourName, source = "iPlant_TNRS") #creates list of accepted names

namesCheck$accSppName <- ifelse(namesCheck$acceptedname == "", namesCheck$matchedname, namesCheck$acceptedname) #creates a column of accepted names, but uses original name if the accepted name is left blank.
namesCheck$accSppName <- ifelse(namesCheck$accSppName == "Koeleria cristata", "Koeleria macrantha", namesCheck$accSppName)
namesCheck$accSppName <- ifelse(namesCheck$accSppName == "Populus virginiana", "Populus deltoides", namesCheck$accSppName)
namesCheck$accSppName <- ifelse(namesCheck$score >= "0.86", namesCheck$submittedname, namesCheck$accSppName)


## ~40 rows get added in this step and I don't know why
names <- names %>% 
  left_join(namesCheck %>% select(accSppName, submittedname), by = c("ourName" = "submittedname")) #joins the accepted name to the names file

#save(names, file = "output/WIsppnames_mergingList_Clean.RData")
