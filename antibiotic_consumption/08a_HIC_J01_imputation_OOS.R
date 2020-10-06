#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Impute data for the High Income Countries #
# Run with data held out to test full model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list =ls())
library(mice)
library(VIM)
library(sjmisc)
library(data.table)
library(ggplot2)
library(Metrics)
library(foreign)
library(RColorBrewer)

setwd('Z:/AMR/Covariates/antibiotic_use/')

#1. Data prep ###
# Read in sales data
mydata <- read.csv('combined_sources/J01_DDD_2000_2018.csv', stringsAsFactors = F)
mydata <- data.table(mydata)

#remove brunei as this is incomplete data and is far too low to be believable
mydata <- mydata[mydata$country != 'BRUNEI',]

#merge china and hongkong
CHN_HGK <- mydata[mydata$loc_id == 354 | mydata$loc_id == 44533, ]
CHN_HGK  <- CHN_HGK [,.(loc_id = 6,
                        super_region = 4,
                        region = 5,
                        country = 'China',
                        iso3 = 'CHN',
                        GAUL_CODE = 147295,
                        ddd = sum(ddd),
                        source = 'IQVIA',
                        pop = sum(pop)),
                     by = 'year'] 
CHN_HGK$ddd_per_1000 <- CHN_HGK$ddd/(CHN_HGK$pop/1000) 

# Read in the HIC countries
HIC <- read.csv('Z:/AMR/Misc/world_bank_regions/HICs.csv', stringsAsFactors = F)
HIC <- data.table(HIC)
HIC <- HIC[HIC$level == 3,]

# Limit the sales data to HICs
mydata <- mydata[mydata$loc_id %in% HIC$loc_id,]
mydata <-  rbind(mydata, CHN_HGK)

#split into folds
country <- unique(mydata[, country])
country <- country[sample(length(country))]
master_fold_id <- cut(seq(1,length(country)),breaks=5,labels=FALSE)
folds <- as.data.table(cbind(country, master_fold_id))
mydata <- merge(mydata, folds, by = c('country'))
mydata$master_fold_id <- as.numeric(mydata$master_fold_id)
rm(country, master_fold_id, folds, HIC, CHN_HGK)

#Imputation ####
for(i in 1:5){
  env.covs <- data.frame(mydata)
  env.covs$ddd_per_1000[env.covs$master_fold_id== i] <- NA
  env.covs$ddd <- NULL
  env.covs$iso3 <- NULL
  env.covs$loc_id <- NULL
  env.covs$GAUL_CODE <- NULL
  env.covs$pop <- NULL
  env.covs$master_fold_id <- NULL
  
  ##IMPUTE THE MISSING VALUES
  imputed.data <- mice(data = env.covs, m=5, maxit = 50, method = 'cart', seed = 1234)
  
  #MERGE WITH THE ORIGINAL DATA FRAME
  imputed.data <- merge_imputations(env.covs, imputed.data,
                                    ori = env.covs, summary = c("hist"))
  
  #EXTRACT THE IMPUTE VALUES FOR VARIABLES OF INTEREST
  imputed.data <- imputed.data$data
  
  #squeeze the imputations to the max and min of what is in the original dataset 
  imputed.data$ddd_per_1000_imp <- squeeze(imputed.data$ddd_per_1000_imp, c(min(imputed.data$ddd_per_1000, na.rm = T), max(imputed.data$ddd_per_1000, na.rm = T)))
  assign(paste0('holdout_', i), imputed.data$ddd_per_1000_imp)
}

#Join all together and get the OOS
mydata <- cbind(mydata, holdout_1, holdout_2, holdout_3, holdout_4, holdout_5)
mydata <- mydata[,.(location_id = loc_id, year_id = year, age_group_id = 22, sex_id = 3, 
                    master_fold_id,
                    holdout_1, holdout_2, holdout_3, holdout_4, holdout_5)]

#merge with the actual imputed data and set up fo STGPR
missing_imputed <- data.table(read.csv('results/imputed_HIC_J01.csv', stringsAsFactors = F))
cyp <- missing_imputed[missing_imputed$country == 'CYPRUS' & missing_imputed$year <2006,]
missing_imputed <- missing_imputed[!(missing_imputed$loc_id %in% mydata$location_id),] 
missing_imputed <- rbind(missing_imputed, cyp)
rm(cyp)

#split into folds
country <- unique(missing_imputed[, country])
country <- country[sample(length(country))]
master_fold_id <- cut(seq(1,length(country)),breaks=5,labels=FALSE)
folds <- as.data.table(cbind(country, master_fold_id))
missing_imputed <- merge(missing_imputed, folds, by = c('country'))
missing_imputed$master_fold_id <- as.numeric(missing_imputed$master_fold_id)
rm(country, master_fold_id, folds)

missing_imputed <- missing_imputed[,.(location_id = loc_id, year_id = year, age_group_id = 22, sex_id = 3, 
                                      master_fold_id,
                                      holdout_1 = ddd_per_1000, holdout_2 = ddd_per_1000, holdout_3 = ddd_per_1000,
                                      holdout_4 = ddd_per_1000, holdout_5 = ddd_per_1000)]

mydata <- rbind(mydata, missing_imputed)
mydata <- mydata[mydata$year_id>=2000,]

# Save the data
write.csv(mydata, 'results/OOS_imputed_HIC_J01.csv', row.names = F)

#~~~~~#
# END #
#~~~~~#