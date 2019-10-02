## For plotting the tanxomonic data. 
## Specifically, we'd like a pretty NMDS, RA curve with 1950s spp colorcoded by includsion in seed mixes, and scatterplot for the non-association between occurance in 1950 savannas and seed mixes.

## For plotting the tanxomonic data. 
## Specifically, we'd like a pretty NMDS, RA curve with 1950s spp colorcoded by includsion in seed mixes, and scatterplot for the non-association between occurance in 1950 savannas and seed mixes.

library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(pryr) #this helps assign the base r graph as a psudo object

## Loading data. Files created in Sav_seed_taxonomic_analysis.R. 
#long2 is the data in long format 
#freq_pres is the presence of species in each type
# tax_nmds_points are NMDS scores from the taxonomic data
load("output/TaxDataForGraphing.RData")
freq_pres <- ungroup(freq_pres) # because it's carring past ghost of some previous grouping. Why? I have no fucking idea, but it messed up the code below and OMG is it annoying.

## Figure1: Ordination
# Need to make a colum that has the three groups
nmds_pts <- tax_nmds_points %>%
  separate(rn, c("site", "year"))  #separates the joined site name

# I know this is the wrong way to do it, but nothing else is working and I'm desperate
nmds_pts$location <- c("past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded", "seed", "seed", "seed","seed", "seed", "seed","seed", "seed", "seed","seed", "seed", "seed","seed", "seed")

## Keeping around as proof that I tried to do it in tidiverse
#  mutate(location = if_else(site =="PNN", "seed",
#                            if_else(site =="PRI", "seed",
#                                    if_else(year == "1950", "past",
#                                            if_else(year == "2014", "degraded", "seed")))))
#
#  mutate(location = case_when(
#    year == "50" ~ "past",
#    ends_with(year, "14") ~ "degraded",
#    ends_with(year, "S") ~ "seed",
#    starts_with(year, "S") ~ "seed",
#    contains(site, "P") ~ "seed",
#    TRUE ~ NA)) 

nmds_pts$location <- factor(nmds_pts$location, levels = c("seed", "past", "degraded")) #reordering so the legend is the correct order

#renaming locations so they look better in the Legend
nmds_pts <- nmds_pts %>% mutate(location = recode(location,
                         seed = "Seed mix  ",
                         past = "1950s survey  ",
                         degraded = "2014 survey       "))

# this sets the color scheme for graphs, with color based by site
boolColors <- as.character(c("seed" = "plum3", "past" = "chartreuse4", "degraded" = "navajowhite2"))
boolScale <- scale_colour_manual(name="location", values=boolColors)


## Graphing
## TO DO: a working legend; 
ord_fig <- ggplot((data = nmds_pts), mapping = aes(x = MDS1, y = MDS2)) +
  geom_point(aes(color = location, fill = location, shape = location), size = 2) + #this colorcodign will work better once I can figure out how to designate shapes based on location
  #boolScale +
  scale_shape_manual(values = c(21, 22, 24)) +
  scale_fill_manual(values = c("plum3", "chartreuse4", "navajowhite2")) +
  scale_color_manual(values = c("black", "black", "black")) +
  scale_y_continuous(breaks = seq(-1, 1.5, 0.5)) +
  scale_x_continuous(breaks = seq(-1, 2, 0.5)) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", size = 1, fill = NA),#black line all around graph
        text = element_text(size = 12),
        #legend.key.size = unit(4, "lines"),
        #legend.spacing = unit(0.5, "lines"),
        legend.title = element_blank(),
        legend.position = "top"
        )

pdf("figures/figure1.pdf", height=3.7, width=3.5)
plot_grid(ord_fig)
dev.off()
    


##Figure 2A: Rank abundance curve
## Need to calculate ranks
ra_data <- freq_pres %>%
  #mutate(rank_past = arrange(past_p)) %>%
  #mutate(rank_past = dense_rank(desc(past_p))) %>% # these ranks are too chunky
  mutate(seed_pres = if_else(seedmix >0, "plum3", "black"))

# Code above to make ranks wasn't quite right, so doing it a round about way
ra_data <- arrange(ra_data, desc(past_p))
ra_data$rank_past <- 1:nrow(ra_data)

ra_data <- filter(ra_data, past_p > 0)

## Turn on this code if want to save just this image as pdf. Also tunr on the dev.off() at the end
#dev.off() #this clears something
#pdf("figures/Figure2A_RA.pdf", width = 7, height = 3.5)
pdf("figures/Figure2A_RA_sm.pdf", width = 4.5, height = 3)

par(mfrow=c(1,1),
    mar =c(2.5,2.5,2.5,0.1)#Bottom, left top right. inner margins? Not sure i need this?
    ) 

plot(ra_data$past_p ~ ra_data$rank_past,
     pch="|",
     cex=1.25, #size of bars
     xlab = "Rank in 1950s savanna surveys", 
     ylab = "Presense in 1950s surveys (%)", 
     col = ra_data$seed_pres,
     tcl = NA, #makes ticks smaller
     mgp = c(1.2,0.2,0), #alters margins with regard to axis title, axis label, axis line
     cex.axis = 0.7, # size of axis numbers
     cex.lab = .8 #label font size relative to graph
     ) 

legend(120, 94, c(" Present in a seed mix", " Absent from seed mixes"), 
       pch=15, #makes points in the legend squares
       pt.cex=1.25, #enlarges the size of the squares
       #pch=c("|", "|", "|"), #makes ledgend points lines
       col=c("plum3", "black"),  
       cex=0.75, #size of font
       bty="n",
       x.intersp = 0.5
       )

dev.off()



## Figure 2B: graphing association between presence in savannas and seed mixes
#TO DO: change ledgend title to something useful - can't get the code below to work with that
#dev.off() #this clears something
#pdf("figures/Figure2B_regression.pdf", width = 6, height = 5)
#pdf("figures/Figure2B_regression_test.pdf", width = 6, height = 5)

#fig2b <-ggplot((data = freq_pres), mapping = aes(x = past_p, y = seedmix_p)) +
#  #geom_abline(intercept = 0, slope = 1, size = 45, color = "seashell2") +
#  geom_abline(intercept = 0, slope = 1, size = 1, color = "pink4") +
#  geom_count(shape = 21, fill = "plum3", color = "black") +
#  #scale_size_area() + #makes sure zeros aren't graphed
#  xlim (0, 100) +
#  ylim (0, 100) +
#  xlab ("Savanna sites occupied (%)") +
#  ylab ("Inclusion in seed mixes (%)") +
#  #guides(fill=guide_legend(title="Count of species")) +
#  #scale_fill_discrete(name = "Count of Species") +
#  annotate("text", x=88, y = 77, label = "underrepresented\nin seed mixes", fontface = "italic", cex =4, color = "palevioletred4", angle = 45) +
#  annotate("text", x=75, y = 89, label = "overrepresented\nin seed mixes", fontface = "italic", cex = 4, color = "palevioletred4", angle = 45) +
#  theme_classic() +
#  labs(size = "Count of\nspecies")

#fig2b  
#dev.off() #ends work to the pdf (I think?)



## Small version of Fig 2b
pdf("figures/Figure2B_regression_sm.pdf", width = 2.5, height = 3)

ggplot((data = freq_pres), mapping = aes(x = past_p, y = seedmix_p)) +
  #geom_abline(intercept = 0, slope = 1, size = 45, color = "seashell2") +
  geom_abline(intercept = 0, slope = 1, size = 1, color = "pink4") +
  geom_count(shape = 21, fill = "plum3", color = "black") +
  #scale_size_area() + #makes sure zeros aren't graphed
  xlim (0, 100) +
  ylim (0, 100) +
  xlab ("Savanna sites occupied (%)") +
  ylab ("Inclusion in seed mixes (%)") +
  #guides(fill=guide_legend(title="Count of species")) +
  #scale_fill_discrete(name = "Count of Species") +
  annotate("text", x=85, y = 74, 
           label = "underrepresented\nin seed mixes", fontface = "italic", 
           cex = 2.5, color = "grey15", angle = 45) +
  annotate("text", x=71, y = 85, label = "overrepresented\nin seed mixes", fontface = "italic", 
           cex = 2.5, color = "grey15", angle = 45) +
  #annotate("text", x = 3, y = 98, label = "B") +
  theme_bw() +
  labs(size = "Count of\nspecies") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        #axis.text=element_text(color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
        legend.title = element_text(size = 8),
        legend.position = "top")

dev.off() #ends work to the pdf (I think?)




## Want to group Fig 2a and Fig 2b in the same figure
## Isn't working well because one graph is ggplot and the other is base R....
#this is the ggplot solution...
ggarrange(fig2b, test_f2,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)






### There's some wierd duplication going in on in the text below this line. Not sure what's going on, but most of it (I think) is just reference code anyway

# ~*~*~*~*~*~*~*~*~*~*~ START SAMPLE CODE ~*~*~*~*~*~*~*~*~*~*~*~*~*~
## Exporting the figure....
jpeg('SavannaRA.jpg')
par(mfrow=c(2,1),
    oma =c(3,3,1,1), #outer margins
    mai =c(0.1, 0.3, 0.1, 0.3) #margin (in inches) between panels
    #mar =c(4,3,0,0.5) #inner margins?
)

#par(mar=0,3,1,1) #bottom, left, top, right?
occur <-c("purple", "sienna2", "grey75", "lightsteelblue3", "saddlebrown")
# grey30 = savanna both times
# sienna2 = local extinction 
# sienna4 = Regional extinction
# green = SEE ABOVE NOTE not in 2010 savanna, 2010 forest
plot(rathen.s$then_logAbun ~ rathen.s$rank_running,
     pch="|",
     cex=1.5,
     col = occur[rathen.s$status], #color based on all 3 habs separate
     axes=FALSE, frame.plot=TRUE,
     xlab = "",
     ylab = "",
     xlim = c(0, 230),
     ylim = c(0.25, 3)
     #cex.lab = 1.25 #label font size relative to graph
)
axis(side=1, labels=FALSE, tck=0.05)
axis(side=2, labels=TRUE, tck =-0.05)
text(175,2.7, "Species extinctions since 1950s", cex =1.25)
legend(200, 2.5, c("local", "regional", "persist"), 
       pch=15, #makes points in the legend squares
       pt.cex=1.1, #enlarges the size of the squares
       #pch=c("|", "|", "|"), #makes ledgend points lines
       col=c("sienna2", "saddlebrown", "grey75"),  cex=0.75, bty="n")

#par(mar=c(3,3,-1,1))
occur <-c( "darkorchid1", "red", "grey75", "purple4", "brown") #setting colors
# grey30 = savanna both times
# purple4 = regional colonization in savannas
# thistle = local colonization in savannas
plot(ranow.s$now_logAbun ~ ranow.s$rank_running,
     pch="|",
     cex = 1.5,
     col = occur[ranow.s$status], #color based on all 3 habs separate
     #axes=FALSE, frame.plot=TRUE,
     xlab = "", #got rid of default label
     ylab = "",
     xlim = c(0, 230),
     ylim = c(0.25,3),
     cex.lab = 1.25 #label font size relative to graph
)
axis(side=1, labels=FALSE, tck=0.05)
axis(side=2, labels=TRUE, tck =-0.05)
text(175,2.7, "Species colonizations in 2010s", cex =1.25)
#text(50, 4, "log Abundance", cex =1)
legend(200, 2.5, c("local", "regional", "persist"), pch=15, pt.cex=1.1, col=c("darkorchid1", "purple4", "grey75"),  cex=0.75, bty="n")

mtext(text ="log Abundance", side =2, line = 1, outer=TRUE, cex =1.25)  #overall y axis
mtext(text="Rank", side = 1, line =2, outer=TRUE, cex=1.25)

dev.off()
# ~*~*~*~*~*~*~*~*~*~*~ END SAMPLE CODE ~*~*~*~*~*~*~*~*~*~*~*~*~*~



## Loading data. Files created in Sav_seed_taxonomic_analysis.R. 
#long2 is the data in long format 
#freq_pres is the presence of species in each type
# tax_nmds_points are NMDS scores from the taxonomic data
load("output/TaxDataForGraphing.RData")
freq_pres <- ungroup(freq_pres) # because it's carring past ghost of some previous grouping. Why? I have no fucking idea, but it messed up the code below and OMG is it annoying.

## Figure1: Ordination
# Need to make a colum that has the three groups
nmds_pts <- tax_nmds_points %>%
  separate(rn, c("site", "year"))  #separates the joined site name

# I know this is the wrong way to do it, but nothing else is working and I'm desperate
nmds_pts$location <- c("past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded","past", "degraded", "seed", "seed", "seed","seed", "seed", "seed","seed", "seed", "seed","seed", "seed", "seed","seed", "seed")

## Keeping around as proof that I tried to do it in tidiverse
#  mutate(location = if_else(site =="PNN", "seed",
#                            if_else(site =="PRI", "seed",
#                                    if_else(year == "1950", "past",
#                                            if_else(year == "2014", "degraded", "seed")))))
#
#  mutate(location = case_when(
#    year == "50" ~ "past",
#    ends_with(year, "14") ~ "degraded",
#    ends_with(year, "S") ~ "seed",
#    starts_with(year, "S") ~ "seed",
#    contains(site, "P") ~ "seed",
#    TRUE ~ NA)) 
  

# this sets the color scheme for graphs, with color based by site
boolColors <- as.character(c("seed" = "plum3", "past" = "chartreuse4", "degraded" = "navajowhite2"))
boolScale <- scale_colour_manual(name="location", values=boolColors)


## Graphing
## TO DO: pick shapes based on location (seed = 21, past = 22, degraded = 24); black line going all around the graph, not just two sides; a working legend; make axis ticks consistent across axes
ggplot((data = nmds_pts), mapping = aes(x = MDS1, y = MDS2)) +
  geom_point(aes(size = 1, color = "black", fill = location, shape = location)) + #this colorcodign will work better once I can figure out how to designate shapes based on location
  boolScale +
  #scale_color_manual(values = location, aesthetics = c("colour", "fill")) +
  theme_classic()


##Figure 2A: Rank abundance curve
## Need to calculate ranks
ra_data <- freq_pres %>%
  #mutate(rank_past = arrange(past_p)) %>%
  #mutate(rank_past = dense_rank(desc(past_p))) %>% # these ranks are too chunky
  mutate(seed_pres = if_else(seedmix >0, "black", "red"))

# Code above to make ranks wasn't quite right, so doing it a round about way
ra_data <- arrange(ra_data, desc(past_p))
ra_data$rank_past <- 1:nrow(ra_data)

ra_data <- filter(ra_data, past_p > 0)

par(mfrow=c(1,1),
    oma =c(3,3,1,1) #outer margins
    #mai =c(0.1, 0.3, 0.1, 0.3) #margin (in inches) between panels
    #mar =c(4,3,0,0.5) #inner margins?
    )
    
plot(ra_data$past_p ~ ra_data$rank_past,
     pch="|",
     cex=1.5,
     xlab = "Rank", #it is ignoring this and I don't know why
     ylab = "Occurance in 1950s savannas (%)", #It is ignoring this and I don't know why
     col = ra_data$seed_pres,
     cex.lab = 1.25 #label font size relative to graph
     )




# ~*~*~*~*~*~*~*~*~*~*~ START SAMPLE CODE ~*~*~*~*~*~*~*~*~*~*~*~*~*~
## Exporting the figure....
jpeg('SavannaRA.jpg')
par(mfrow=c(2,1),
    oma =c(3,3,1,1), #outer margins
    mai =c(0.1, 0.3, 0.1, 0.3) #margin (in inches) between panels
    #mar =c(4,3,0,0.5) #inner margins?
)

#par(mar=0,3,1,1) #bottom, left, top, right?
occur <-c("purple", "sienna2", "grey75", "lightsteelblue3", "saddlebrown")
# grey30 = savanna both times
# sienna2 = local extinction 
# sienna4 = Regional extinction
# green = SEE ABOVE NOTE not in 2010 savanna, 2010 forest
plot(rathen.s$then_logAbun ~ rathen.s$rank_running,
     pch="|",
     cex=1.5,
     col = occur[rathen.s$status], #color based on all 3 habs separate
     axes=FALSE, frame.plot=TRUE,
     xlab = "",
     ylab = "",
     xlim = c(0, 230),
     ylim = c(0.25, 3)
     #cex.lab = 1.25 #label font size relative to graph
)
axis(side=1, labels=FALSE, tck=0.05)
axis(side=2, labels=TRUE, tck =-0.05)
text(175,2.7, "Species extinctions since 1950s", cex =1.25)
legend(200, 2.5, c("local", "regional", "persist"), 
       pch=15, #makes points in the legend squares
       pt.cex=1.1, #enlarges the size of the squares
       #pch=c("|", "|", "|"), #makes ledgend points lines
       col=c("sienna2", "saddlebrown", "grey75"),  cex=0.75, bty="n")

#par(mar=c(3,3,-1,1))
occur <-c( "darkorchid1", "red", "grey75", "purple4", "brown") #setting colors
# grey30 = savanna both times
# purple4 = regional colonization in savannas
# thistle = local colonization in savannas
plot(ranow.s$now_logAbun ~ ranow.s$rank_running,
     pch="|",
     cex = 1.5,
     col = occur[ranow.s$status], #color based on all 3 habs separate
     #axes=FALSE, frame.plot=TRUE,
     xlab = "", #got rid of default label
     ylab = "",
     xlim = c(0, 230),
     ylim = c(0.25,3),
     cex.lab = 1.25 #label font size relative to graph
)
axis(side=1, labels=FALSE, tck=0.05)
axis(side=2, labels=TRUE, tck =-0.05)
text(175,2.7, "Species colonizations in 2010s", cex =1.25)
#text(50, 4, "log Abundance", cex =1)
legend(200, 2.5, c("local", "regional", "persist"), pch=15, pt.cex=1.1, col=c("darkorchid1", "purple4", "grey75"),  cex=0.75, bty="n")

mtext(text ="log Abundance", side =2, line = 1, outer=TRUE, cex =1.25)  #overall y axis
mtext(text="Rank", side = 1, line =2, outer=TRUE, cex=1.25)

dev.off()
# ~*~*~*~*~*~*~*~*~*~*~ END SAMPLE CODE ~*~*~*~*~*~*~*~*~*~*~*~*~*~






