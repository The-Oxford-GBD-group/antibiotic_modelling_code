#' @title Holdout Function Notes
#' @return Notes
#' @export
holdout_function_notes <- function() {
  cat("
        ## this file contains functions to make different types of holdouts for spatio-temporal data\n
        ## some of the holdout methods are spatial, some temporal, and some do both\n
        ## written by aoz\n
        ## last editted on OCT 27 2016\n
        \n
        \n
        ## USAGE NOTES:\n
        \n
        ## 1) all of these options takes in data that is already cleaned and processed\n
        ##\n
        ## 2) if you specify strata with little data, things may break but it will hopefully warn you\n
        ##\n
        ## 3) this is still a work in progress. I know some things won't work\n
        ##\n
        ## 4) source this whole file then proceed to make folds\n
        ##\n
        ## 5) I would suggest temp_strat=\"prop\"\n
        ##    and spat_strat=\"poly\" or \"qt\"\n
        ##\n
        ## 6) make_folds() returns a named list. each item is a data strata and will have a folds\n
        ##    column in it. the name of the list element tells you the strata\n
        \n
        ##  here are some examples which I've tested - may take a while (30-60min?):\n
        \n
        #df <- fread('J:/temp/geospatial/U5M_africa/data/clean/fully_processed.csv',\n
        #            stringsAsFactors = FALSE)\n
        ## clean the df\n
        #df$long <- as.numeric(as.character(gsub(\",\", \"\", df$long)))\n
        #df$lat  <- as.numeric(as.character(gsub(\",\", \"\", df$lat)))\n
        #df <- df[-which(df$lat > 90), ]\n
        #data <- df\n
        #data <- data[-which(data$year == 2011), ]\n
        #data <- data[, fold:= NULL]\n
        #dim(data)\n
        \n
        ## with quad_tree\n
        #system.time(\n
        #stratum_qt <- make_folds(data = data, n_folds = 5, spat_strat = 'qt',\n
        #                         temp_strat = \"prop\", strat_cols = \"age_bin\",\n
        #                         ts = 1e5, mb = 10, lat_col = 'lat', long_col = 'long',\n
        #                         ss_col = 'exposed', yr_col = 'year')\n
        #)\n
        #str(stratum_qt)\n
        \n
        ## pdf(\"C:/Users/azimmer/Documents/U5M_africa/Holdouts/qt_tests.pdf\")\n
        ## for(i in 1:length(stratum_qt)){\n
        ##   library(scales)\n
        ##   d  <- stratum_qt[[i]]\n
        ##   for(yr in sort(unique(d$year))){\n
        ##     r <- which(d$year == yr)\n
        ##     x <- d$long[r]\n
        ##     y <- d$lat[ r]\n
        ##     col <- d$fold[r]\n
        ##     main <- paste0(\"Strata: \", names(stratum_qt)[i], \" Year: \", yr)\n
        ##     plot(x, y, col = alpha(col, alpha = 0.25), pch = \".\", main = main)\n
        ##   }\n
        ## }\n
        ## dev.off()\n
        \n
        ## ## with ad2\n
        ## stratum_ad2 <- make_folds(data = data, n_folds = 5, spat_strat = 'poly',\n
        ##                           temp_strat = \"prop\", strat_cols = \"age_bin\",\n
        ##                           admin_shps='J:/temp/geospatial/U5M_africa/data/clean/shapefiles/ad2_raster.grd',\n
        ##                           shape_ident=\"gaul_code\",\n
        ##                           admin_raster='J:/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad2.shp',\n
        ##                           mask_shape='J:/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_simple.shp',\n
        ##                           mask_raster='J:/temp/geospatial/U5M_africa/data/clean/shapefiles/ad0_raster',\n
        ##                           lat_col = 'lat', long_col = 'long',\n
        ##                           ss_col = 'exposed', yr_col = 'year')\n
        \n
        ## ad2 <- shapefile('J:/temp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad2.shp')\n
        ## pdf(\"C:/Users/azimmer/Documents/U5M_africa/Holdouts/ad2_tests.pdf\")\n
        ## for(i in 1:length(stratum_ad2)){\n
        ##   library(scales)\n
        ##   d  <- stratum_ad2[[i]]\n
        ##   for(yr in sort(unique(d$year))){\n
        ##     r <- which(d$year == yr)\n
        ##     x <- d$long[r]\n
        ##     y <- d$lat[ r]\n
        ##     col <- d$fold[r]\n
        ##     main <- paste0(\"Strata: \", names(stratum_ad2)[i], \" Year: \", yr)\n
        ##     plot(x, y, col = alpha(col, alpha = 0.25), pch = \".\", main = main)\n
        ##     plot(ad2, add = T)\n
        ##   }\n
        ## }\n
        ## dev.off()\n
        \n
        \n
        #######################\n
        #######################\n
        ### HOLDOUT OPTIONS ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n
        #######################\n
        #######################\n
        \n
        \n
        ####################\n
        ## TOTALLY RANDOM ##\n
        ####################\n
        \n
        ## 1) As it sounds, totally random points across space and time\n
        \n
        \n
        ##############\n
        ## IN SPACE ##\n
        ##############\n
        \n
        ## 1) random in space (this is just totally random with one timepoint)\n
        ## 2) small 'tesselations' aggregated up until certain population is reached\n
        ##    a) with quadtree\n
        ##    b) NOT DONE: with weighted k-means (weights inversely proportional to sample size)\n
        ## 3) by admin2\n
        ## 4) countries\n
        ## 5) larger regions (e.g. East Africa, West Africa, ...)\n
        \n
        \n
        ##############\n
        ## IN  TIME ##\n
        ##############\n
        \n
        ## 1) random in time\n
        ## 2) proportional to data amount in the period\n
        ## 3) full years, randomly selected across the duration of data\n
        ## 4) build up different datasets as if we were moving chronologically in time\n
        ##    i.e. first set is only yr 1, second set is yrs 1 & 2, third set is yrs 1, 2, 3 ...\n
        \n
        \n
        ##############\n
        ## IN   S-T ##\n
        ##############\n
        \n
        ## all the combos of the space time sets!\n
        \n
        ## quadtree functions hacked from: \n
        ## http://gis.stackexchange.com/questions/31236/how-can-i-generate-irregular-grid-containing-minimum-n-points
")
}
