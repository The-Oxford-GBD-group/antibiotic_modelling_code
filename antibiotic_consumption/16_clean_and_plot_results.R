#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Combined HIC and LMIC model results #
# Plot maps, figures and tables of    #
# antibitoic consumption results      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list =ls())
library(data.table)
library(ggplot2)
library(raster)
library(sf)
library(rgeos)
library(RColorBrewer)
library(readxl)
source('H:/Functions/seqlast.R')

setwd('Z:/AMR/Covariates/antibiotic_use')

# 1. Read in and merge the model results ####
#a. Read in the model results
HIC <- data.table(read.csv('results/imputed_HIC_J01.csv', stringsAsFactors = F))
HIC <- HIC[HIC$loc_id!=354,]
HIC <- HIC[HIC$loc_id!=44533,]
lmic <- data.table(read.csv("results/model_estimates.csv", stringsAsFactors = F))

#b. Read in the shapefile and limit top required locations
shp <- st_read('Z:/AMR/Shapefiles/GBD2019/GBD2019_analysis_final.shp')
shp <- shp[shp$level == 3,] 

lmic <- lmic[!(lmic$location_id %in% HIC$loc_id),]
lmic <- lmic[lmic$location_id %in% shp$loc_id,]
lmic <- unique(lmic)
HIC <- HIC[HIC$loc_id %in% shp$loc_id,]

#c. Combine HIC and LMIC model results
mydata <- rbind(HIC[,.(loc_id, year, ddd_per_1000)], 
                lmic[,.(loc_id = location_id, year = year_id, ddd_per_1000 = gpr_mean)])

mydata <- mydata[mydata$year >=2000,]

#d. Add on the population
pop <- read.csv('Z:/AMR/Misc/GBD_populations/GBD_total_populations.csv', stringsAsFactors = F)
mydata <- merge(mydata, pop, by.x = c('loc_id', 'year'), by.y = c('location_id', 'year_id'), all.x = T, all.y = F)
rm(pop)

#e. Merge on all location info to the data
locs <- read.csv('Z:/AMR/Misc/IHME_location_hierarchy/cleaned_ihme_hierarchy.csv', stringsAsFactors = F)
locs <- locs[1:3]
mydata <- merge(locs, mydata, by.x =c('location_id'), by.y = c('loc_id'), all.x = F, all.y = F)
colnames(mydata)[2] <-  'Country'
colnames(mydata)[3] <-  'reg'
mydata <- merge(locs, mydata, by.x =c('location_id'), by.y = c('reg'), all.x = F, all.y = T)
colnames(mydata)[2] <-  'Region'

colnames(mydata)[3] <-  'spr'
mydata <- merge(locs, mydata, by.x =c('location_id'), by.y = c('spr'), all.x = F, all.y = T)
colnames(mydata)[2] <-  'Super region'

#f. Calculate metrics
mydata$ddd_per_1000_per_day <- mydata$ddd_per_1000/365
mydata$ddd <- mydata$ddd_per_1000*(mydata$population/1000)

mydata <- mydata[c('Super region', 'Region', 'Country', 'year', 'ddd_per_1000', 'population', 'ddd_per_1000_per_day', 'ddd')]

#g. Save out ther results
write.csv(mydata, 'results/all_results.csv', row.names = F)
ddd_per_day <- dcast(mydata, Super.region + Region + Country ~ year, value.var='ddd_per_1000_per_day')
write.csv(ddd_per_day, 'results/ddd_per_1000_per_day.csv', row.names = F)

#2. Create  maps ####
#get a background shapefile to mask western sahara and french guyana
background <- st_read('Z:/AMR/Shapefiles/admin2013_0.shp')
background <- background[background$name == 'Western Sahara' | background$name == 'French Guiana',]
background <- st_simplify(background, dTolerance = 0.1, preserveTopology = T)

#merge data and geometry info
my_shp <- merge(shp, mydata, by.y = 'Country', by.x = 'loc_name') 

#Select colour scheme
myPalette <- colorRampPalette(brewer.pal(9, "YlGnBu"))
mycolours = myPalette(100)

#a. Map of total antibiotic consumption per 1000 population per day for 2018
png('results/figures/maps/J01_DDD__per_day_2018.png',
     height = 20, width = 40, units = 'cm', res = 300)
ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.25)+
  geom_sf(data = my_shp[my_shp$year == 2018,], aes(fill = `ddd_per_1000_per_day`),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colours = mycolours, limit = c(0, 50), labels = seqlast(0, 50, 10), breaks = seq(0, 50, 10))+
  labs(fill = 'DDD/1000/day')
dev.off()

#b. Map of total antibiotic consumption per 1000 population per day for all years
png('results/figures/maps/J01_DDD__per_day_all_years.png',
     height = 20, width = 40, units = 'cm', res = 300)
ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = my_shp, aes(fill = `ddd_per_1000_per_day`),colour = 'black', size = 0.15)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colours = mycolours, limit = c(0, 50), labels = seqlast(0, 50, 10), breaks = seq(0, 50, 10))+
  labs(fill = 'DDD/1000/day')+
  facet_wrap(~year, ncol = 5)
dev.off()

#c. Map of total antibiotic consumption in the caribbean and pacific islands seperatley 
# (as cannot see them on the large map), 2018
png('results/figures/maps/oceania_2018.png')
ggplot()+
  geom_sf(data = my_shp[my_shp$year == 2018 & my_shp$region_id == 21,], aes(fill = `ddd_per_1000_per_day`),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colours = mycolours, limit = c(0, 50), labels = seqlast(0, 50, 10), breaks = seq(0, 50, 10))+
  labs(fill = 'DDD/1000/day')
dev.off()

png('results/figures/maps/carribean_2018.png')
ggplot()+
  geom_sf(data = my_shp[my_shp$year == 2018 & my_shp$region_id == 104,], aes(fill = `ddd_per_1000_per_day`),colour = 'black', size = 0.25)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colours = mycolours, limit = c(0, 50), labels = seqlast(0, 50, 10), breaks = seq(0, 50, 10))+
  labs(fill = 'DDD/1000/day')
dev.off()

#d. Map of total antibiotic consumption per 1000 population per day for every 5 years
png('results/figures/maps/J01_DDD_per_day_5_yearly.png',
    height = 20, width = 40, units = 'cm', res = 300)
ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = my_shp[my_shp$year == 2000 |my_shp$year == 2005 |my_shp$year == 2010 |my_shp$year == 2015,], aes(fill = `ddd_per_1000_per_day`),colour = 'black', size = 0.15)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colours = mycolours, limit = c(0, 50), labels = seqlast(0, 50, 10), breaks = seq(0, 50, 10))+
  labs(fill = 'DDD/1000/day')+
  facet_wrap(~year, ncol = 2)
dev.off()

#3. Create plots of antibitoic use over time by income group and super region ####
#a. calculatethe total DDD/1000/day for LMIC and HICs
mydata <- data.table(mydata)
income <- mydata
income$income <- 'Low and middle income countries'
income$income[toupper(income$Country) %in% HIC$country] <-  'High income countries'
income$income[income$Country == 'China' | income$Country == 'Lebanon' | income$Country == 'Russia'] <- 'Low and middle income countries'
income$ddd <- income$ddd_per_1000*(income$population/1000)

income <- income[,.(ddd = sum(ddd),
                    ddd_per_1000_per_day = (sum(ddd)/(sum(population)/1000))/365),
                 by = c('income', 'year')]

income2 <- dcast(income, year ~ income, value.var = c('ddd', 'ddd_per_1000_per_day'))
write.csv(income2, "results/tables/ddd_by_income_group.csv", row.names = F)

#b. Calculate DDD/1000/day by super refion
spr_reg <- mydata[,.(ddd = sum(ddd),
                     ddd_per_1000_per_day = (sum(ddd)/(sum(population)/1000))/365),
                  by = c('Super region', 'year')]

#c. Plot graph of the DDD/1000 pop/year for each super-region and world bank income group for each year 2000-2018
png('results/figures/plots/super_regional_trends+income.png',
    height = 20, width = 30, units = 'cm', res = 300)
ggplot()+
  geom_line(data = spr_reg, aes(x = year, y = ddd_per_1000_per_day, group = `Super region`, colour = `Super region`), size = 1.5)+
  geom_line(data = income[income == 'High income countries',], aes(x = year, y = ddd_per_1000_per_day, group = income, colour = income), size = 1, linetype = "longdash")+
  geom_line(data = income[income != 'High income countries',], aes(x = year, y = ddd_per_1000_per_day, group = income, colour = income), size = 1, linetype = "dotdash")+
  scale_colour_manual(values = c("#fe9929", #central europe
                                 "#8c2d04", #HI
                                 '#000000', #WB HIC
                                 "#e78ac3", #latin America
                                 '#000000', #WB LMIC
                                 "#984ea3", #NAME
                                 "#4daf4a", #South Asia
                                 "#377eb8", # SE Asia
                                 "#e41a1c"))+ #SSA
  theme_bw()+
  labs(x = 'Year', y = 'Daily defined doses per 1000 population per day', colour = NULL)+
  ylim(0,30)
dev.off()

#4. Create table 1 of DDD/1000/pop for each region and super region for 2018####
mydata <-  mydata[mydata$year == 2018,]

totalddd <- sum(mydata$ddd)
table1_regions <- mydata[,.(`Total antibiotics consumed (DDDs)` = sum(ddd),
                            `Percentage of global antibiotic consumption` = round(sum(ddd)/totalddd*100,1),
                            `DDD/1000/day` = round((sum(ddd)/(sum(population)/1000))/365,1)),
                         by = c('Region')]


table1_spr_regions <- mydata[,.(`Total antibiotics consumed (DDDs)` = sum(ddd),
                            `Percentage of global antibiotic consumption` = round(sum(ddd)/totalddd*100,1),
                            `DDD/1000/day` = round((sum(ddd)/(sum(population)/1000))/365,1)),
                         by = c('Super region')]

table1_global <- mydata[,.(`Total antibiotics consumed (DDDs)` = sum(ddd),
                                `Percentage of global antibiotic consumption` = round(sum(ddd)/totalddd*100,1),
                                `DDD/1000/day` = round((sum(ddd)/(sum(population)/1000))/365,1))]

table1_global$`Super region` <-  'Global' 
table1_global$Region <- ""
table1_spr_regions$Region <- ""
table1_regions$`Super region` <- ""

table1 <- rbind(table1_regions, table1_spr_regions, table1_global)

write.csv(table1, 'results/tables/table1.csv', row.names = F)
#~~~~~#
# END #
#~~~~~#

