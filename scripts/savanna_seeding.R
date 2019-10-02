#==================================================================#
#                                                                  #
# A taxonomic and functional comparison of restoration seed mixes  #
# and historical species loss in Midwestern Oak Savannas           #
# Created by: Laura Ladwig & Chad Zirbel                           #
# Created on: 4/16/2019                                            #
#                                                                  #
#==================================================================#

## Lets get started -----------------------------------------------

#to do
#

## Load packages
#install needed packages that are not currently installed (will not update packages)

##WARNING## This will install new packages on your system!
#list.of.packages <- c("googledrive")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

#lapply(list.of.packages, require, character.only = TRUE)

##if the code below isn't working update your purrr package version
#install.packages("purrr")

## Download data from drive
#when you run the following functions you should be prompted in your browser to allow R
#access to your drive account if that doesn't happen run the following line:
#drive_auth()

#the following code will download data from google drive into your current working directory
#You do not need to run this code if you have the most up-to-date data files
#running this code will overwrite the data files you have stored locally in your current working directory

#Trait data
#drive_download("cc_trait_20190117.csv", overwrite = T)
#drive_download("cle_prairie_trait_data_2016.csv", overwrite = T)
#drive_download("trait_data_field_Zirbel_2017_dryad.csv", overwrite = T)
#drive_download("WIsppnames_merging_list.csv", overwrite = T)

#Composition
#drive_download("species_extinctions.csv", overwrite = T)
#drive_download("SpeciesList_SavPrairie.csv", overwrite = T)

#Seed list
#drive_download("SeedingList_20190415.csv", overwrite = T)
#drive_download("SeedList_NameUpdate_20190307.csv", overwrite = T)

## Load data
#Traits
#cle.trait<-read.csv("cle_prairie_trait_data_2016.csv")
#cc.trait<-read.csv("cc_trait_20190117.csv")
#dryad.trait<-read.csv("trait_data_field_Zirbel_2017_dryad.csv")
#wi.sp.names<-read.csv("WIsppnames_merging_list.csv")

#Composition
#sp.ext<-read.csv("species_extinctions.csv")
#sp.lst.sav.pra<-read.csv("SpeciesList_SavPrairie.csv")

#Seedlist
#seed.lst<-read.csv("SeedingList_20190415.csv")
#seed.lst.name<-read.csv("SeedList_NameUpdate_20190307.csv")

## Data cleaning & merging---------------------------------------
