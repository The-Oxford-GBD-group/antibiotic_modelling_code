#' @title Merge link table and GBD geographies
#' @description This merges the link table and the list of GBD geographies so that we know what aggregations to use for raking.
#'               
#' @param rake_subnational T/F are we using the subnational targets for raking.  What targets get used is in the raking shp
#' @param link the link table
#' @param connector the table identifying what geographies go to what raking targets
#' @param nat_connector a connector object from `get_gbd_locs()` with `rake_subnational = F`. Used to replace subnationals specified in `countries_not_to_subnat_rake`
#' @param countries_not_to_subnat_rake a character vector of iso3 codes gotten from the config. All countries in this list will be raked nationally.
#'
#' @return a link table that has not only the spatial hierarchy in it but also a column for raking geographies.
#'
#' @export
sub_nat_link_merge <- function(rake_subnational,
                               link,
                               connector,
                               nat_connector = NULL,
                               countries_not_to_subnat_rake = NULL){
  
  if (rake_subnational == T) {
    connector0 <- connector[rak_level == 0, ]
    connector1 <- connector[rak_level == 1, ]
    
    #move countries from subnat connector to national connector
    if(!is.null(countries_not_to_subnat_rake)){
      if(is.null(nat_connector)) {
        stop("A connector table built with get_gbd_locs() with rake_subnational = F must be passed in when countries_not_to_subnat_rake is not null.")
      } else {
        nat_connector <- nat_connector[ADM_CODE %in% get_adm0_codes(countries_not_to_subnat_rake, shapefile_version = modeling_shapefile_version),]
        nat_connector[, rak_level := 0]
        nat_connector[, ADM1_CODE := 0]
        nat_connector[, ADM0_CODE := ADM_CODE]
        
        connector0 <- rbind(connector0, nat_connector)
        connector1 <- connector1[!(ADM0_CODE %in% get_adm0_codes(countries_not_to_subnat_rake, shapefile_version = modeling_shapefile_version)),]
        
      }
    }
    
    # Remove any duplicated rows
    connector0 <- unique(connector0)
    connector1 <- unique(connector1)
    
    # Ensure that all subnationals are properly assigned to connectors
    # When the raking shapefile version does not have all subnationals, 
    # there are possible scenarios where requested subnationals will not be present.
    subnational_countries <- c("POL+RUS+UKR+NZL+JPN+USA+ITA+NOR+SWE+GBR+MEX+BRA+IRN+IND+PAK+CHN+IDN+PHL+ETH+KEN+ZAF+NGA")
    
    #invert countries_not_to_subnat_rake
    subnat_countries_to_rake <- setdiff(get_adm0_codes(subnational_countries, shapefile_version = modeling_shapefile_version), 
                                        get_adm0_codes(countries_not_to_subnat_rake, shapefile_version = modeling_shapefile_version))
    
    #check to see if any requested subnationals are in the national connector
    if(any(unique(connector0[, ADM0_CODE]) %in% subnat_countries_to_rake)){
      stage_list <- fread("/snfs1/WORK/11_geospatial/10_mbg/stage_master_list.csv")
      stage_list <- stage_list[, c("iso3", "loc_id")]
      con_error <- connector0[connector0[, ADM0_CODE] %in% subnat_countries_to_rake,]
      
      con_error <- merge(con_error, stage_list, by.x = "location_id", by.y = "loc_id")
      
      message("The following countries are not present subnationally in the raking shapefile and will be raked nationally: ", paste(unique(con_error$iso3), collapse = ", "))
      message("All subnationals are included in admin shapefiles from 4/2019 or later")
    }
    
    connector0$ADM1_CODE <- NULL
    connector1$ADM0_CODE <- NULL
    
    link0 <- merge(link, connector0, by = c("ADM0_CODE"))
    link1 <- merge(link, connector1, by = c("ADM1_CODE"))
    link <- rbind(link0, link1)
  } else {
    link <- merge(link, connector, by.x = c("ADM0_CODE"), by.y = c("ADM_CODE"))
  }
  return(link)
}
