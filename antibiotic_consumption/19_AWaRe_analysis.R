#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot out the proportion of antibiotics by AWARE category ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list =ls())
library(data.table)
library(raster)
library(ggplot2)
library(viridis)
library(sp)
source('H:/Functions/seqlast.R')

setwd('Z:/AMR/Covariates/antibiotic_use/')

#1. Read in and prep datasets ####
IQVIA <- data.table(read.csv('IQVIA/imputation/imputed_ddd_per_1000_2000_2018.csv', stringsAsFactors = F))
IQVIA <-  IQVIA[IQVIA$ATC3 != 'J04A',]

AWaRe <- read.csv('IQVIA/lookup_tables/AWARE_categories.csv', stringsAsFactors = F)
IQVIA <-  merge(IQVIA, AWaRe, by = c('ATC5'), all.x = T, all.y = F)
IQVIA$AWARE[is.na(IQVIA$AWARE)] <-  'Other'
IQVIA$AWARE[IQVIA$AWARE==""] <-  'Other'
IQVIA <- data.table(IQVIA)
IQVIA <- IQVIA[,.(ddd_per_1000=total_ddd_per_1000_pop),
               by = c("super_region", "region", "country", "iso3", "loc_id", "year", "AWARE")]

WHO <- read.csv("WHO/WHO_AWaRE_clean.csv")
WHO <- WHO[!(WHO$iso3 %in% IQVIA$iso3),]
colnames(WHO)[colnames(WHO)=='access'] <- 'Access'
colnames(WHO)[colnames(WHO)=='watch'] <- 'Watch'
colnames(WHO)[colnames(WHO)=='reserve'] <- 'Reserve'

WHO$Other <- WHO$ddd_per_1000 - WHO$Access - WHO$Watch - WHO$Reserve
WHO <- data.table(WHO)
WHO <- melt(WHO, id.vars = c("super_region", "region", "country", "iso3", "loc_id", "year"),
            measure.vars = c("Access", "Watch", "Reserve", "Other"),
            variable.name = 'AWARE',
            value.name = 'ddd_per_1000')

mydata <- rbind(IQVIA, WHO)
rm(IQVIA, WHO, AWaRe)

ddd_per_1000 <- mydata[,.(ddd_per_1000 = sum(ddd_per_1000)),
                       by = c("super_region", "region", "country", "iso3", "loc_id", "year", "AWARE")]

sum_ddd_per_1000 <- mydata[,.(total_ddd_per_1000 = sum(ddd_per_1000)),
                       by = c("super_region", "region", "country", "iso3", "loc_id", "year")]

mydata <- merge(ddd_per_1000, sum_ddd_per_1000)
mydata$prop_aware <- mydata$ddd_per_1000/mydata$total_ddd_per_1000

#add data for Algeria reserve abx
DZA <- mydata[mydata$iso3 == 'DZA' & mydata$AWARE == 'Watch',]
DZA$AWARE <-  'Reserve'
DZA$prop_aware <- 0
DZA$total_ddd_per_1000 <-  0
DZA$ddd_per_1000 <-  0
mydata <-  rbind(mydata, DZA)
rm(DZA)

#2. Plot map of proportion of AWaRE antibitoics ####
shp <- st_read('Z:/AMR/Shapefiles/IQVIA_analysis.shp')
shp <- shp[shp$level == 3 | shp$loc_id == 44533 | shp$loc_id == 354,]
shp <- shp[shp$loc_id != 6,]

#merge on the data
my_shp <- merge(shp, mydata, by = 'loc_id') 

background_shp <- shp[!(shp$loc_id %in% my_shp$loc_id[my_shp$year == 2000]) | shp$ihme_lc_id == 'KEN' | shp$ihme_lc_id == 'VEN',]

my_shp$AWARE <- factor(my_shp$AWARE, levels = c("Access", "Watch", 'Reserve', 'Other'))  

#a. For 2018
png('results/figures/maps/AWaRe_2018.png',
     height = 20, width = 20, units = 'cm', res = 200)
ggplot()+
  geom_sf(data = background_shp, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp[my_shp$year == 2018,], aes(fill = prop_aware),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt')+
  facet_wrap(~AWARE, ncol = 2)+
  labs(fill = 'Proportion of total antibiotics')
dev.off()

#b. For 2000
png('results/figures/maps/AWaRe_2000.png',
    height = 20, width = 20, units = 'cm', res = 200)
ggplot()+
  geom_sf(data = background_shp, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp[my_shp$year == 2000,], aes(fill = prop_aware),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt')+
  facet_wrap(~AWARE, ncol = 2)+
  labs(fill = 'Proportion of total antibiotics')
dev.off()

#c. For 2005
png('results/figures/maps/AWaRe_2005.png',
    height = 20, width = 20, units = 'cm', res = 200)
ggplot()+
  geom_sf(data = background_shp, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp[my_shp$year == 2005,], aes(fill = prop_aware),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt')+
  facet_wrap(~AWARE, ncol = 2)+
  labs(fill = 'Proportion of total antibiotics')
dev.off()

#d. For 2010
png('results/figures/maps/AWaRe_2010.png',
    height = 20, width = 20, units = 'cm', res = 200)
ggplot()+
  geom_sf(data = background_shp, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp[my_shp$year == 2010,], aes(fill = prop_aware),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt')+
  facet_wrap(~AWARE, ncol = 2)+
  labs(fill = 'Proportion of total antibiotics')
dev.off()

#e. For 2015
png('results/figures/maps/AWaRe_2015.png',
    height = 20, width = 20, units = 'cm', res = 200)
ggplot()+
  geom_sf(data = background_shp, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp[my_shp$year == 2015,], aes(fill = prop_aware),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, trans = 'sqrt')+
  facet_wrap(~AWARE, ncol = 2)+
  labs(fill = 'Proportion of total antibiotics')
dev.off()

#Order the levels
mydata$AWARE <- factor(mydata$AWARE, levels = c("Other", "Reserve", "Watch", "Access"))

#3. Plot as a bar chart
mydata$prop_aware[mydata$prop_aware<0] <-  NA

#a. GLobal
png('results/figures/prop_AWARE_stacked_bars.png',
    height = 10, width = 15, units = 'cm', res = 300)
ggplot(mydata, aes(x = year, y = prop_aware, fill =AWARE))+
  geom_bar(position="fill", stat="identity")+
  labs(x = 'Year', y = 'Proportion of antibiotics consumed', fill = NULL)+
  theme_bw()

#b. by country
pdf('results/figures/plots/AWARE.pdf',
    height = 8.3, width = 11.7)
for(i in 1:length(unique(mydata$super_region))){
  subset <- mydata[mydata$super_region == unique(mydata$super_region)[i],]
  print(ggplot(subset, aes(x = year, y = prop_aware, fill =AWARE))+
          geom_bar(position="fill", stat="identity")+
          labs(x = 'Year', y = 'Proportion of antibiotics consumed', fill = NULL)+
          facet_wrap(~country)
        )}
dev.off()

#~~~~~#
# END #
#~~~~~#