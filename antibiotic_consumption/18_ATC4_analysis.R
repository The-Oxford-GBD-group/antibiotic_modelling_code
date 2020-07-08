#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Calculate the proportion of antibitoics by select ATC4 class #
# and apply to the modelled estimates                          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())
library(ggplot2)
library(data.table)

setwd('Z:/AMR/Covariates/antibiotic_use/')

#1. Read in the atc4 file (IQVIA only) ####
mydata <- data.table(read.csv('IQVIA/imputation/imputed_ddd_per_1000_2000_2018.csv', stringsAsFactors = F))

#exclude pre 2000 and tb drugs
mydata <- mydata[mydata$year>= 2000,]
mydata <- mydata[mydata$ATC3 != 'J04A',]

#exclude J01R and X classes
mydata <- mydata[mydata$ATC3 != 'J01X',]
mydata <- mydata[mydata$ATC3 != 'J01R',]

# collapse the uncollapsed datasets
mydata <- mydata[,.(total_ddd = sum(total_ddd),
                  total_ddd_per_1000_pop = sum(total_ddd_per_1000_pop)),
               by = c("super_region", "region", "country", "iso3", "loc_id", "GAUL_CODE", "year", "ATC4", "pop")] 

#limit to required variables
mydata <- mydata[,.(country, year, ATC4, ATC4_ddd = total_ddd, ATC4_ddd_per_1000 = total_ddd_per_1000_pop)]

#restrict to ATC4 classes of interest
mydata <- mydata[mydata$ATC4 == 'J01DH'| #carbapenems
                   mydata$ATC4 == 'J01MA'| # fluoroquinolones
                   mydata$ATC4 == 'J01CA'| #Broad spec penicillins
                   mydata$ATC4 == 'J01DD',] # 3rd gen cephalosporins

#reshape wide
mydata <- dcast(mydata, country+year~ATC4, value.var = 'ATC4_ddd')

#2. Get the input totals and calculate the proportion in each class ####
inputs <- data.table(read.csv('combined_sources/J01_DDD_2000_2018.csv', stringsAsFactors = F))
inputs <- inputs[,.(super_region, region, country, year, total_ddd = ddd, total_ddd_per_1000 = ddd_per_1000, population = pop)]

#change the codes to names for the super regions and regions
regs <- read.csv('D:/region_code_lookup.csv', stringsAsFactors = F)
regs <-  regs[,1:4]
inputs <- merge(inputs, regs, by.x = c('super_region', 'region'), by.y =c('super_region_code', 'region_code'))
inputs$super_region <- NULL
inputs$region <- NULL
names(inputs)[names(inputs) == 'super_region.y'] <- 'super_region'
names(inputs)[names(inputs) == 'region.y'] <- 'region'
rm(regs)

# # French west africa and central america are combined in the ATC3 file but have country level estimates for total J01 DDDs
# # Calculate ratios of antibiotics in these regions and apply the proportions to the country totals estimates
FWA <-  inputs[inputs$country == 'BENIN'|
               inputs$country == 'BURKINA FASO' |
               inputs$country == 'CAMEROON' |
               inputs$country == "COTE D'IVOIRE" |
               inputs$country == 'GUINEA' |
               inputs$country == 'SENEGAL' |
               inputs$country == 'TOGO' |
               inputs$country == 'CONGO' |
               inputs$country == 'GABON' |
               inputs$country == 'MALI',]

FWA <- FWA[,.(region = NA,
              population = sum(population),
              total_ddd = sum(total_ddd)),
           by = c('super_region', 'year')]

FWA$total_ddd_per_1000 <- FWA$total_ddd/(FWA$population/1000)
FWA$country <- 'FRENCH WEST AFRICA'
FWA$region <- NA

CA <-  inputs[inputs$country == 'COSTA RICA'|
              inputs$country == 'EL SALVADOR' |
              inputs$country == 'GUATEMALA' |
              inputs$country == "HONDURAS" |
              inputs$country == 'NICARAGUA' |
              inputs$country == 'PANAMA',]

CA <- CA[,.(population = sum(population),
            total_ddd = sum(total_ddd)),
         by = c('super_region','region', 'year')]

CA$total_ddd_per_1000 <- CA$total_ddd/(CA$population/1000)
CA$country <-  'CENTRAL AMERICA'
inputs <- inputs[!(inputs$country == 'BENIN'|
                   inputs$country == 'BURKINA FASO' |
                   inputs$country == 'CAMEROON' |
                   inputs$country == "COTE D'IVOIRE" |
                   inputs$country == 'GUINEA' |
                   inputs$country == 'SENEGAL' |
                   inputs$country == 'TOGO' |
                   inputs$country == 'CONGO' |
                   inputs$country == 'GABON' |
                   inputs$country == 'MALI' |
                   inputs$country == 'COSTA RICA'|
                   inputs$country == 'EL SALVADOR' |
                   inputs$country == 'GUATEMALA' |
                   inputs$country == "HONDURAS" |
                   inputs$country == 'NICARAGUA' |
                   inputs$country == 'PANAMA'),]



inputs <- rbind(inputs, FWA, CA)
rm(CA, FWA)

mydata <- merge(inputs, mydata, by= c('country', 'year'), all.x = F, all.y = T)

#3. Calcaulte the proportions of each antibitoic class for the regional proportions ####
regions <- mydata[,.(reg_J01CA = sum(J01CA, na.rm = T)/sum(total_ddd),
                     reg_J01DD = sum(J01DD, na.rm = T)/sum(total_ddd),
                     reg_J01DH = sum(J01DH, na.rm = T)/sum(total_ddd),
                     reg_J01MA = sum(J01MA, na.rm = T)/sum(total_ddd)),
                  by = c('region', 'year')]


#make the french west africa both central and western sSA (remove the other western sSA as is just Burkina Faso WHO data)
cssa <- regions[is.na(regions$region),]
cssa$region <- 'Central Sub-Saharan Africa'
wssa <- regions[is.na(regions$region),]
wssa$region <- 'Western Sub-Saharan Africa'
regions <- regions[regions$region!='Western Sub-Saharan Africa',]
regions <- rbind(regions, cssa, wssa)
rm(cssa, wssa)

super_regions <- mydata[,.(spr_reg_J01CA = sum(J01CA, na.rm = T)/sum(total_ddd),
                           spr_reg_J01DD = sum(J01DD, na.rm = T)/sum(total_ddd),
                           spr_reg_J01DH = sum(J01DH, na.rm = T)/sum(total_ddd),
                           spr_reg_J01MA = sum(J01MA, na.rm = T)/sum(total_ddd)),
                        by = c('super_region', 'year')]


#calculate proportion of each antibiotic class
mydata <- mydata[,.(country, year, super_region, region,
                    J01CA = J01CA/total_ddd,
                    J01DD = J01DD/total_ddd,
                    J01DH = J01DH/total_ddd,
                    J01MA = J01MA/total_ddd)]



#4. Apply the proportions to the modelled estimates of DDD/1000/day ####
J01 <- data.table(read.csv('results/all_results.csv', stringsAsFactors = F))
J01 <- J01[,.(super_region = Super.region, region = Region, country = toupper(Country), year, total_ddd = ddd, total_ddd_per_1000 = ddd_per_1000, population)]

#remove FWA and central america from the proportions (as these will then get sorted by regions)
mydata <- mydata[!(mydata$country == 'FRENCH WEST AFRICA' | mydata$country == 'CENTRAL AMERICA' | mydata$country == 'HONG KONG'),]

all_data <- merge(J01, mydata, by= c('country', 'year'), all.x = T, all.y = T)
all_data$super_region.x[is.na(all_data$super_region.x)] <-  all_data$super_region.y[is.na(all_data$super_region.x)]
all_data$super_region.y <- NULL
colnames(all_data)[colnames(all_data)== 'super_region.x'] <-  'super_region'
all_data$region.x[is.na(all_data$region.x)] <-  all_data$region.y[is.na(all_data$region.x)]
all_data$region.y <- NULL
colnames(all_data)[colnames(all_data)== 'region.x'] <-  'region'

#change to factors as for some reason merge isnt working
all_data$super_region <-  as.factor(all_data$super_region)
all_data$region <-  as.factor(all_data$region)
super_regions$super_region <-  as.factor(super_regions$super_region)
regions$region <-  as.factor(regions$region)
levels(all_data$super_region) <-  levels(super_regions$super_region)
levels(regions$region) <- levels(all_data$region)

#match the region and super region proportions and replace where required
all_data <- merge(all_data, regions, by = c('region', 'year'), all.x = T)
all_data <- merge(all_data, super_regions, by = c('super_region', 'year'), all.x = T)

all_data$J01CA[is.na(all_data$J01CA)] <- all_data$reg_J01CA[is.na(all_data$J01CA)]
all_data$J01CA[is.na(all_data$J01CA)] <- all_data$spr_reg_J01CA[is.na(all_data$J01CA)]

all_data$J01DD[is.na(all_data$J01DD)] <- all_data$reg_J01DD[is.na(all_data$J01DD)]
all_data$J01DD[is.na(all_data$J01DD)] <- all_data$spr_reg_J01DD[is.na(all_data$J01DD)]

all_data$J01DH[is.na(all_data$J01DH)] <- all_data$reg_J01DH[is.na(all_data$J01DH)]
all_data$J01DH[is.na(all_data$J01DH)] <- all_data$spr_reg_J01DH[is.na(all_data$J01DH)]

all_data$J01MA[is.na(all_data$J01MA)] <- all_data$reg_J01MA[is.na(all_data$J01MA)]
all_data$J01MA[is.na(all_data$J01MA)] <- all_data$spr_reg_J01MA[is.na(all_data$J01MA)]


all_data$spr_reg_J01CA <-  NULL
all_data$spr_reg_J01DD <-  NULL
all_data$spr_reg_J01DH <-  NULL
all_data$spr_reg_J01MA <-  NULL
all_data$reg_J01CA <-  NULL
all_data$reg_J01DD <-  NULL
all_data$reg_J01DH <-  NULL
all_data$reg_J01MA <-  NULL

all_data$J01CA <- all_data$J01CA*all_data$total_ddd
all_data$J01DD <- all_data$J01DD*all_data$total_ddd
all_data$J01DH <- all_data$J01DH*all_data$total_ddd
all_data$J01MA <- all_data$J01MA*all_data$total_ddd

# Reshape long
all_data <- melt(all_data, id.vars = c('super_region', 'region', 'country', 'year', 'population'),
                 measure.vars = c("J01CA", "J01DD", "J01DH", "J01MA"),
                 variable.name = "ATC4", value.name = "ddd")


all_data$ddd_per_1000 <- all_data$ddd/(all_data$population/1000)
all_data$ddd_per_1000_per_day <- all_data$ddd_per_1000/365

write.csv(all_data, 'results/ATC4_total_DDDs.csv', row.names = F)
rm(inputs, J01, regions, super_regions)

#5 .Plot out the consumption of these select antibiotic classes for select regions ####
mydata <- all_data
mydata$ATC4 <-  as.character(mydata$ATC4)
mydata$super_region <-  as.character(mydata$super_region)

#match to HIC and LMICs
HIC <- read.csv('Z:/AMR/Misc/world_bank_regions/HICs.csv', stringsAsFactors = F)
mydata$income <- 'Low- and middle-income countries'
mydata$income[mydata$country %in% toupper(HIC$loc_name)] <- 'High-income countries'
rm(HIC)

mydata$ATC4[mydata$ATC4 == 'J01CA'] <- 'Broad spectrum penicillins'
mydata$ATC4[mydata$ATC4 == 'J01DD'] <- 'Third-generation cephalosporins'
mydata$ATC4[mydata$ATC4 == 'J01DH'] <- 'Carbapenems'
mydata$ATC4[mydata$ATC4 == 'J01MA'] <- 'Fluoroquinolones'


global <- mydata[,.(total_ddd = sum(ddd),
                    total_ddd_per_1000_pop = sum(ddd)/(sum(population)/1000),
                    total_ddd_per_1000_pop_per_day = (sum(ddd)/(sum(population)/1000))/365),
                 by = c('year', 'ATC4')]

income <- mydata[,.(total_ddd = sum(ddd),
                    total_ddd_per_1000_pop = sum(ddd)/(sum(population)/1000),
                    total_ddd_per_1000_pop_per_day = (sum(ddd)/(sum(population)/1000))/365),
                 by = c('income', 'year', 'ATC4')]


super_regions <- mydata[,.(total_ddd = sum(ddd),
                           total_ddd_per_1000_pop = sum(ddd)/(sum(population)/1000),
                           total_ddd_per_1000_pop_per_day = (sum(ddd)/(sum(population)/1000))/365),
                        by = c('super_region', 'year', 'ATC4')]

#a. By World Banl income groups
png('results/figures/plots/ATC4/ATC4_by_income.png',
    height = 10, width = 20, units = 'cm', res = 300)
ggplot(income)+
  geom_line(aes(x = year, y = total_ddd_per_1000_pop_per_day, group = income, colour = income))+
  facet_wrap(~ATC4, scales = "free_y")+
  theme_bw()+
  labs(y = 'DDD per 1000 per day', x = 'Year', colour = NULL)
dev.off()

#. By super regions
png('results/figures/plots/ATC4/ATC4_by_spr_reg.png',
    height = 10, width = 20, units = 'cm', res = 300)
ggplot(super_regions)+
  geom_line(aes(x = year, y = total_ddd_per_1000_pop_per_day, group = super_region, colour = super_region))+
  facet_wrap(~ATC4, scales = "free_y")+
  theme_bw()+
  labs(y = 'DDD per 1000 per day', x = 'Year', colour = NULL)
dev.off()

#c. By select super regions
# (for some reasons selecting based on the location name isnt working)
super_regions <-  data.frame(super_regions)
png('results/figures/plots/ATC4/ATC4_NAME_HIC_SSA_SA2.png',
    height = 10, width = 20, units = 'cm', res = 300)
ggplot(super_regions[super_regions$super_region == unique(super_regions$super_region)[4] |super_regions$super_region == unique(super_regions$super_region)[5] | super_regions$super_region == unique(super_regions$super_region)[7] |super_regions$super_region=="High-income",])+
  geom_line(aes(x = year, y = total_ddd_per_1000_pop_per_day, group = super_region, colour = super_region), size = 1)+
  facet_wrap(~ATC4, scales = "free_y")+
  scale_colour_manual(values = c("#8c2d04", #HI
                                 "#984ea3", #NAME
                                 "#4daf4a", #South Asia
                                 "#e41a1c"))+ #SSA
  expand_limits(y = 0)+
  theme_bw()+
  labs(y = 'Daily defined doses per 1000 population per day', x = 'Year', colour = 'GBD Super regions')
dev.off()

#~~~~~#
# END #
#~~~~~#