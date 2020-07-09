#' Make Custom Raking Shapefile
#'
#' @title Make Custom Raking Shapefile
#' 
#' @description A function used to generate a new shapefile where adm1 polygons in `countries` are replaced with their corresponding adm0 polygon
#' 
#' @param countries character vector of iso3 codes to change from subnational to national. Any subnational not in this list (out of "ETH", "KEN", "CHN", "ZAF", "BRA", "IND", "IDN", "IRN", "MEX", "NGA", "PAK", "PHL") will stay as ADM1
#' @param raking_shapefile_version standard shapefile version date (YYYY_MM_DD)
#' 
#' @export
#'
#' @return Returns a new raking shapefile with adm1 polygons switched out with adm0 polygons for the specified country. stops if invalid country iso3s are passed in.
#' 
make_custom_raking_shapefile <- function(countries,
                                         raking_shapefile_version) {
  
  #if countries is in condensed form (i.e. "CHN+BRA") separate into character vector
  if (all(grepl("+", countries, fixed = T))) {
    countries <- strsplit(countries, "[+]", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]]
  }
  
  #get mapping of gadm codes to ihme loc ids
  stage_list <- fread("/snfs1/WORK/11_geospatial/10_mbg/stage_master_list.csv")
  
  #check that countries recieved valid subnational iso3s
  subnational_countries <- c("ETH", "KEN", "CHN", "ZAF", "BRA", "IND", "IDN", "IRN", "MEX", "NGA", "PAK", "PHL")
  if (!all(toupper(countries) %in% subnational_countries)){
    stop("Invalid ISO3 passed to countries, must be one or several of the following: ", paste(subnational_countries, collapse = ", "))
  }
  
  message("Making custom raking shapefile with subnationals: ", paste(subnational_countries[!(subnational_countries %in% countries)], collapse = ", "))
  
  #subset subnational countries to the ones requested
  subnational_countries  <- subnational_countries[!(subnational_countries %in% countries)]
  #read in raking and adm0 shapefiles
  raking_shp <- readOGR(get_admin_shapefile(admin_level = 0, raking = T, version = raking_shapefile_version), stringsAsFactors = F, verbose = F)
  adm0_shp <- readOGR(get_admin_shapefile(admin_level = 0, raking = F, version = raking_shapefile_version), stringsAsFactors = F, verbose = F)
  
  #check that all expected subnationals are present in the shapfile
  subnational_country_codes <- data.table("ADM0_CODE" = as.numeric(unique(raking_shp@data[raking_shp@data$ad_level == 1,]$ADM0_CODE)))
  subnational_iso3s <- merge(stage_list[, c("iso3", "gadm_geoid")], subnational_country_codes, by.x = "gadm_geoid", by.y = "ADM0_CODE")
  if (!all(subnational_countries %in% subnational_iso3s$iso3)) {
    message("The following countries are not present subnationally in the raking shapefile and will be used nationally: ", paste(subnational_countries[!subnational_countries %in% subnational_iso3s$iso3], collapse = ", "))
    message("All subnationals are included in admin shapefiles from 4/2019 or later")
  }
  
  #get all subnational country codes that were not passed in to countries - 
  country_codes <- get_adm0_codes(countries, shapefile_version = raking_shapefile_version)
  
  #remove countries from raking shapefile, keep only those countries in adm0 shapefile
  raking_shp <- raking_shp[!(raking_shp$ADM0_CODE %in% country_codes),]
  adm0_shp <- adm0_shp[adm0_shp$ADM0_CODE %in% country_codes,]
  
  #add on fields to adm0 shapefile in order to merge to raking shapefile
  adm0_shp@data$ad_level <- 0
  adm0_shp@data$ADM1_CODE <- 0
  adm0_shp@data$ADM1_NAME <- "NA"
  adm0_shp <- sp::merge(adm0_shp, stage_list[,c("gadm_geoid", "loc_id")], by.x = "ADM0_CODE", by.y = "gadm_geoid", all.x = T)
  
  #merge shapefiles and return
  new_shp <- rbind(raking_shp, adm0_shp, makeUniqueIDs = TRUE)
  
  return(new_shp)
}