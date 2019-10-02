## Savanna seed mix phylogeny Rscript
## By: Chad Zirbel
## Created on: 8/2/2019

#load packages
library(tidyverse)
library(ggtree)
library(phytools)
library(ape)
library(phytools)
library(caper)

## Load data
#Phylogeny
phylo<-read.tree("data/Phylogeny/savseed_phylotree.txt") #subset tree from Zanne et al. 2014
#with altered Spermatophor divergence time to make binding trees easier
#phylogeny of Pteridophytes that were missing from the Zanne et al. tree
phylo.missing<-read.tree("data/Phylogeny/savseed_phylotree_missing.nwk")
#species list with families, seed mix, and survey data
sp.lst<-read.csv("output/spplist_complete.csv")
sp.lst.phylo<-read.csv("data/Phylogeny/spplist_phylo.csv")

## Build the Phylogeny -----------------------
##combine the pruned Zanne et al. tree with the Pteridophte tree to create
#a tree with all species in the surveys/seed mixes

#drop pteridophyte clade from the large tree
phylo<-drop.tip(phylo,tip=c("Equisetum_laevigatum", "Adiantum_pedatum",
                 "Pteridium_aquilinum","Cystopteris_fragilis",
                 "Polypodium_virginianum", "Dryopteris_marginalis"), trim.internal = F)

#add full pteridophyte clade back in with divergence times from timetree.org
#phylo<-bind.tree(phylo.missing,phylo)
phylo2<-bind.tree(phylo.missing,phylo, where=15, position=18.07089)
phylo2<-drop.tip(phylo2,tip="", trim.internal = F) #drop hanging tip

#merge phylo and complete species list
sp.lst.final<-merge(sp.lst, sp.lst.phylo,by="accSppName")
#drop woody species
sp.lst.final<-sp.lst.final[!(sp.lst.final$habit%in%"woody"),]

##Create a list of all families. Use this in the loop below
families<-unique(sp.lst.final$Accepted_name_family)

#create any empty matrix to hold plant families and their node in the phylogeny
output <- matrix(ncol=2, nrow=length(families))

#Loop finds the most recent common ancestor node in phylo for each plant family
#This currently fails on families with a single species
for(i in 1:length(families)){
  tryCatch({ #this prevents singletons from causing the loop to fail
  output[i,1]<-as.character(families[i])
  output[i,2]<-findMRCA(phylo2,
                tips=as.vector(sp.lst.final$Name_submitted[sp.lst.final$Accepted_name_family%in%families[i]]),
                type="node")
  }, error=function(e){})
}

#rename and clean up the output matrix from the loop
family.node<-as.data.frame(output)
names(family.node)<-c("family","node")
family.node$node<-as.numeric(as.character(family.node$node))
family.node<-na.omit(family.node)

## create a matrix that only contains the survey/seed mix info as binary variables
# Use this to create the colored bands on the phylogeny
sp.lst.cut<-sp.lst.final[c("Name_submitted","past_survey","current_survey","seed_mix")]
sp.lst.cut <- data.frame(lapply(sp.lst.cut, as.character), stringsAsFactors=FALSE)
rownames(sp.lst.cut)<-sp.lst.cut$Name_submitted
sp.lst.cut$Name_submitted<-NULL

#make each "yes" unique to the variable to trick ggplots color pallette
sp.lst.cut$past_survey[sp.lst.cut$past_survey%in%"yes"]<-"yes.1950"
sp.lst.cut$current_survey[sp.lst.cut$current_survey%in%"yes"]<-"yes.current"
sp.lst.cut$seed_mix[sp.lst.cut$seed_mix%in%"yes"]<-"yes.seed"

## Plotting ----------------------------------------------
##plot the phylogeny
pdf("figures/savseed_phylo_fig.pdf", width = 8.5, height = 11)
p<-ggtree(phylo2, branch.length="none", layout='circular',ladderize = F)

#Loop to label each family on the phylo by its node position
for(j in 1:dim(family.node)[1]){
  #Then add each clade label
p<-p + geom_cladelabel(node=family.node$node[j],
                       label=family.node$family[j], offset = 2.5,
                       angle="auto", fontsize = 2)
}
#Plot binary data for the surveys/seed mixes on the phylo
gheatmap(p, sp.lst.cut, offset = .05, width=0.08, colnames = F)+
  scale_fill_manual(breaks=c("yes.1950", "yes.current", "yes.seed", "no"), 
                    values=c("white", "chartreuse4", "navajowhite2", "plum3"),
                    labels = c("Historic data", "Current data", "Seed mix", ""))
dev.off()

## Phylogenetic D Statistic --------
#make data binary
sp.lst.bin<-sp.lst.final
sp.lst.bin$past_survey<-ifelse(sp.lst.bin$past_survey%in%"yes",1,0)
sp.lst.bin$current_survey<-ifelse(sp.lst.bin$current_survey%in%"yes",1,0)
sp.lst.bin$seed_mix<-ifelse(sp.lst.bin$seed_mix%in%"yes",1,0)

#remove node labels to make things easier
phylo2$node.label<-NULL

#calculate D statistic for each variable
seed.phyloD<- phylo.d(data=sp.lst.bin, phy=phylo2, names.col=Name_submitted,
                      binvar=seed_mix, permut=10000)

current.phyloD<- phylo.d(data=sp.lst.bin, phy=phylo2, names.col=Name_submitted,
                      binvar=current_survey, permut=10000)

past.phyloD<- phylo.d(data=sp.lst.bin, phy=phylo2, names.col=Name_submitted,
                      binvar=past_survey, permut=10000)

## Family count for each group
length(unique(sp.lst.final$Accepted_name_family))

sp.lst.past<-sp.lst.final[sp.lst.final$past_survey%in%"yes",]
length(unique(sp.lst.past$Accepted_name_family))

sp.lst.current<-sp.lst.final[sp.lst.final$current_survey%in%"yes",]
length(unique(sp.lst.current$Accepted_name_family))

sp.lst.seed<-sp.lst.final[sp.lst.final$seed_mix%in%"yes",]
length(unique(sp.lst.seed$Accepted_name_family))
## The garbage heap --------------------------------------
#viewClade(ggtree(phylo)+geom_tiplab(size=1.5), node=477)

ggtree(phylo.missing,layout="circular")+geom_tiplab2(size=1.5)+ geom_text2(aes(subset=!isTip, label=node),size=1.7)
ggtree(phylo2,layout="circular")

##maths for phylogeny adjustment tests
#29.17450112
#163.951+49.571(from juniper)-29.17450112 (For not Juniper branch)
#401.80588650-4.18191-29.17450112 (for juniper)
plot(phylo2,type="fan", cex=.5)
edgelabels(phylo$edge.length, col="black", font=1, cex=.5, frame="none")
