## Plotting Trait data
## Aug 28, 2019
## LML, CRZ

# import data. 
## These data were created within Sav_seed_trait_analysis.R and the dataset is sp_trait_herbaceous
# It only has herbaceous species
load("output/HerbaceousDataForGraphing.RData")

library(ggplot2)
library(beanplot)
library(tidyverse)
library(cowplot)

## Data Organizing
# re-roder locations
sp_trait_herb <- sp_trait_herb %>% 
  mutate(location = factor(location, levels = c("seed_mix", "past_survey", "current_survey")))

sp_trait_herb_cut<-sp_trait_herb %>% filter(present=="yes")

## Getting mean values to add to table 2 in version R2 of the  ms

group_means <- sp_trait_herb_cut %>%
  group_by(location) %>%
  dplyr::summarise(height_mean = log(mean(height, na.rm = TRUE)),
            sla_mean = mean(sla, na.rm=TRUE),
            seed_mean = log(mean(seed, na.rm=TRUE)))

## Plotting --------------------------
# To Do: Separate plots for each trait. within each plot graph the distribution of the trait of choice from the three classes (past savanna, seed mix, degraded savanna)

## Bean plots
## To do: make y axis numbers closer to axis so title can go on the outside; make all three graphs the same size (espxially watch out that they aren't different heights); label axes, set size; rename groups so they are more intuative (seed mix, 1950 savanna, 2014 encroached savanna)

## Sets it up to stack the graphs
par(mfrow=c(3,1), 
    mar=c(1,5,4,1),#Sets margins (bottom, left, top, right)
    #mai =c(0.2, 0.75, 0.1, 0.3), #margin (in inches) 
    cex.axis = 1.25
    #mgp = c(0,2,0) # this changes the spacing with axes and labels, but this is only working for the x axis, it messes up the y axis...
    ) 
# seed mass
beanplot(log(seed) ~ location, data = sp_trait_herb %>% filter(present=="yes"), 
         ll = 0.05, #makes datapoints smaller,
         col = list("plum3","chartreuse4", "navajowhite2"),
         #border = "purple",
         #axes = T, #not sure what this does
         names = c("seed\nmixes", "1950\nsavanna", "degraded\nsavanna"),
         show.names = F,
         beanlines = "mean" #do we want the median??
         )
title(ylab= expression('Log Seed Mass (mg)'), # "mg" isn't super helpful because it's log
      line = 0.75, #distance between title and axis
      cex.axis = 1.2,
      cex.lab = 1.5
      #font.lab = 2 #this command isn't working, isnt' bold
      ) 

# SLA
par(mar=c(1,5,2,1),#Sets margins (bottom, left, top, right)
    #mai =c(0.2, 0.75, 0.1, 0.3), #margin (in inches) 
    cex.axis = 1.25
    #mgp = c(0,2,0) # this changes the spacing with axes and labels, but this is only working for the x axis, it messes up the y axis...
) 

beanplot(sla ~ location, data = sp_trait_herb %>% filter(present=="yes"), 
         ll = 0.05, #makes datapoints smaller,
         col = list("plum3","chartreuse4", "navajowhite2"),
         #border = "purple",
         #axes = T, #not sure what this does
         names = c("seed\nmixes", "1950\nsavanna", "degraded\nsavanna"),
         show.names = F,
         beanlines = "mean" #do we want the median??
         #main = "SLA"
         )
title(ylab= expression('SLA (g/cm'^{2}')'), 
      line = 0.75, 
      cex.axis = 1.2,
      cex.lab = 1.5
      #font.lab = 2 #this command isn't working, isnt' bold
      ) 

# plant height
par(mar=c(4,5,1.5,1),#Sets margins (bottom, left, top, right)
    #mai =c(0.2, 0.75, 0.1, 0.3), #margin (in inches) 
    cex.axis = 1.25,
    mgp = c(0,2,0) # this changes the spacing with axes and labels, but this is only working for the x axis, it messes up the y axis...
) 
beanplot(log(height) ~ location, data = sp_trait_herb %>% filter(present=="yes"), 
         ll = 0.05, #makes datapoints smaller,
         col = list("plum3","chartreuse4", "navajowhite2"),
         #border = "purple",
         #axes = T, #not sure what this does
         names = c("seed\nmixes", "1950\nsavanna", "degraded\nsavanna"),
         beanlines = "mean" #do we want the median??
         #main = "SLA"
         )
title(ylab= expression('Log Plant Height (cm)'), 
      line = 0.75, 
      cex.axis = 1.2,
      cex.lab = 1.5
      #font.lab = 2 #this command isn't working, isnt' bold
      )
 

     
## ggplot
p.seed<-ggplot(data = sp_trait_herb_cut, aes(location, log(seed),fill=location))+
  geom_violin(trim=F, size=0.5, color="black")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.3, fill="black")+
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=.75, width = 0.5)+
  geom_hline(linetype="dashed",yintercept=mean(log(sp_trait_herb_cut$seed),na.rm=T))+
  scale_fill_manual(values=c("plum3", "chartreuse4","navajowhite2"))+
  labs(x="", y="Log Seed Mass (mg)")+
  annotate("text", label="A",size=5, x=0.6, y=10.5)+
  annotate("text", label = " p = 0.08", y = -7.5, x = 0.8) +
  theme_bw() +
  theme(text = element_text(size=16),axis.text=element_text(color="black"),panel.background=element_blank(),
      panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.7),
      axis.text.x= element_blank(), axis.ticks.x= element_blank(), axis.line = element_blank(),legend.position = "none")

p.sla<-ggplot(data = sp_trait_herb_cut, aes(location, sla,fill=location))+
  geom_violin(trim=F, size=0.5, color="black")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.3, fill="black")+
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=.75, width = 0.5)+
  geom_hline(linetype="dashed",yintercept=mean(sp_trait_herb_cut$sla,na.rm=T))+
  scale_fill_manual(values=c("plum3", "chartreuse4","navajowhite2"))+
  labs(x="", y=bquote('SLA ('*mm^2~'/mg)'))+
  annotate("text", label="B",size=5, x=0.6, y=730)+
  annotate("text", label = " p < 0.001", y = -140, x = 0.8) +
  annotate("text", label = "b", x=1.8, y=500) +
  annotate("text", label = "b", x=0.8, y=500) +
  annotate("text", label = "a", x=2.7, y=500) +
  theme_bw() +
  theme(text = element_text(size=16),axis.text=element_text(color="black"),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.7),
        axis.text.x = element_blank(),axis.ticks.x= element_blank(), axis.line = element_blank(),legend.position = "none")

p.height<-ggplot(data = sp_trait_herb_cut, aes(location, log(height),fill=location))+
  geom_violin(trim=F, size=0.5, color="black")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.3, fill="black")+
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=.75, width = 0.5)+
  geom_hline(linetype="dashed",yintercept=mean(log(sp_trait_herb_cut$height),na.rm=T))+
  scale_fill_manual(values=c("plum3", "chartreuse4","navajowhite2"))+
  scale_x_discrete(labels=c("current_survey"="2014\nsurvey", "past_survey"="1950s\nsurvey", "seed_mix"="seed\nmixes"))+
  labs(x="", y="Log Plant Height (cm)")+
  annotate("text", label="C",size=5, x=0.6, y=8) +
  annotate("text", label = " p < 0.001", y = 1.2, x = 0.8) +
  annotate("text", label = "b", x=1.8, y=6) +
  annotate("text", label = "a", x=0.8, y=6) +
  annotate("text", label = "a", x=2.8, y=6) +
  theme_bw() +
  theme(text = element_text(size=16),
        axis.text=element_text(color="black"),
        #panel.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
       # axis.line = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, size = .7), 
        legend.position = "none")

pdf("figures/figure3_cz.pdf", height=10, width=4)
plot_grid(p.seed, p.sla, p.height, align="v", ncol=1)
dev.off()

############################################### 
############### CODE GRAVEYARD ################
###############################################
### Box plots
# seed mass
box_seed <- ggplot((data = sp_trait_herb %>% filter(present=="yes")),
                   mapping = aes(x = location, y = seed, color = location, group = location)) +
  geom_boxplot() +
  #xlab("Month") +
  #ylab("Temperature (C)") +
  scale_y_continuous(trans='log10') +
  theme_classic()
box_seed

# SLA
box_sla <- ggplot((data = sp_trait_herb %>% filter(present=="yes")),
                  mapping = aes(x = location, y = sla, color = location, group = location)) +
  geom_boxplot() +
  #scale_y_continuous(trans='log10') +
  theme_classic()
box_sla

# Height
box_height <- ggplot((data = sp_trait_herb %>% filter(present=="yes")),
                     mapping = aes(x = location, y = height, color = location, group = location)) +
  geom_boxplot() +
  #scale_y_continuous(trans='log10') +
  theme_classic()
box_height


########### guide code ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~

pdf("/Users/laura/Desktop/Writing Projects/Nettesheim - Root Cold Tolerance/R/Results/Temp_ColdTol_Figure2_20190328.pdf", width = 10, height = 9) #This saves the pdf
## Graphing code goes here
dev.off() 

####### Base R example 
par(mfrow=c(1,1), 
    mar=c(4,5,1.5,1) #Sets margins (bottom, left, top, right)
)

boxplot(PEL_lt50 ~ SppCode*tissue, 
        data=DOY270, 
        cex.axis = 1.2,
        at = c(1:2, 4:5), #Inserts a space between tissue types
        ylim = c(-17, 1),
        col=c("yellow3", "mediumpurple3"), 
        boxwex=0.7, #controls width of boxes
        range = 0, #this makes wiskers go to extremes
        xaxt = 'n',
        main = NA, xlab =NA, ylab =NA #take out lables bc i do it below
)
title(xlab='           Leaf                               Root', #spaced funny because I want it to line up under the bars
      adj = 0, #this makes label left justified
      line=0.6, #spacing etween label and axis
      cex.lab=1.5 #font size of label 
      #font.lab = 2 # turns axis label bold 
) 
title(ylab= expression('Cold tolerance (LT'[50]*~degree~'C)'), 
      line = 2.2, 
      cex.lab=1.2,
      cex.lab = 1.5,
      font.lab = 2 #this command isn't working, isnt' bold
) 
legend (0.5, 1, #x y coodinates for the legend
        legend = expression(italic('Silphium integrifolium'), italic('Solidago rigida')),
        fill = c("yellow3", "mediumpurple3"), # adds boxes and color to legend,
        bty="n",#takes away boxed outline
        cex = 1.2
)
dev.off() #ends work to the pdf (I think?)


######### ggplot example 
temp <-ggplot((data = long_means), #Set dataset
              mapping = aes(x = month, y = t_ave, color = site, group = site)) + 
  geom_point(aes(color = site), size = 0) + #this plots points
  geom_path(aes(color = site), size = 1) + #this is connects the dots
  ylim (-10, 30) +
  xlab("Month") +
  ylab("Temperature (C)") +
  boolScale +
  facet_wrap(~site, nrow = 5) +
  theme (panel.background = element_rect(fill = "white"),
         strip.background = element_rect(fill = "white"),
         panel.border = element_rect(color = "black", fill = NA),
         legend.key = element_rect(fill = "white")
  )
temp
png("Graphs/monthly_temp.png", width = 400, height = 1000, 
    units ="px", bg="transparent")
print(temp)
dev.off()

ggsave(temp, "Graphs/monthly_temp.png", bg = "transparent")
#~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
