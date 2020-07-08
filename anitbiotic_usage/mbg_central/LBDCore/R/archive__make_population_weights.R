#' @rdname make_population_weights
#' @export
archive__make_population_weights <- function(admin_level,
                                             simple_raster,
                                             pop_raster,
                                             gaul_list) {
  if (!admin_level %in% c(0, 1, 2)) stop("admin_level must be either 0, 1, or 2")


  # load admin raster, crop down to simple_raster
  adm <- load_admin_raster(admin_level)
  adm <- crop(adm, simple_raster)

  if (admin_level == 0) adm[adm == 1013965] <- 227
  # load population data
  # pop <- get_population_data(simple_raster)
  pop <- pop_raster

  # do using cell index (vectorized)
  cell_idx <- notMissingIdx(simple_raster)
  adm_cell <- extract(adm, cell_idx)

  # need gaul-country code linkage - get from shapefile
  # TODO: needs to be from a central place, Lucas
  sadm <- shapefile(sprintf("%stemp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad%i.shp", root, admin_level))@data[c("name", "gaul_code", "country_id")]
  sadm$gaul_code[sadm$gaul_code == 1013965] <- 227 # ZAF
  # sadm <- shapefile(paste0(root,"DATA/SHAPE_FILES/GBD_geographies/master/GBD_2016/master/shapefiles/GBD2016_analysis_final.shp"))
  sadm0 <- shapefile(sprintf("%stemp/geospatial/U5M_africa/data/clean/shapefiles/africa_ad%i.shp", root, 0))@data[c("name", "gaul_code", "country_id")]
  sadm0$gaul_code[sadm0$gaul_code == 1013965] <- 227 # ZAF
  isolist <- sadm0$country_id[which(sadm0$gaul_code %in% gaul_list)]
  # only keep countries we are making estimates for
  sadm <- sadm[sadm$country_id %in% isolist, ]
  # sadm <- sadm[sadm@data$GAUL_CODE %in% gaul_list, ]

  # patch in for countries whose names have problematic characters
  if (admin_level == 0) {
    sadm$name[sadm$gaul_code == 66] <- "Cote dIvoire"
    sadm$gaul_code[sadm$name == "Abyei"] <- 6 # sudan
    sadm$gaul_code[sadm$name == "Hala'ib triangle"] <- 6 # sudan
    sadm$gaul_code[sadm$name == "Ma'tan al-Sarra"] <- 6
    sadm$gaul_code [sadm$name == "Ilemi triangle"] <- 74 # s.sudan
  }
  # get gaul code for each cell in the admin raster for level <<admin_level>>
  # use name if national level
  adm_code <- match(adm_cell, sadm$gaul_code)
  # if(admin_level==0) adm_code <- sadm$name[adm_code]
  if (admin_level == 0) adm_code <- sadm$gaul_code[adm_code]

  # Get populations at the cells, only for the chosen year
  pop_cell <- extract(pop, cell_idx)

  # count population as zero if it is NA.
  pop_cell[is.na(pop_cell)] <- 0
  pop_cell[is.na(adm_code), ] <- 0

  ## get population totals in all cells by admin
  pop_totals_adm <- aggregate(pop_cell ~ adm_code, FUN = sum)

  # in some cases worldpop has zero totl pop for some admin 2s. This causes
  # an issue in weighting. If thats the case, we give a small homogenous
  # population to those districts
  for (i in 2:ncol(pop_totals_adm))
    pop_cell[, (i - 1)][adm_code %in% pop_totals_adm$adm_code[pop_totals_adm[, i] == 0]] <- .0001

  ## replicate totals for all cells in area
  pop_totals_adm_cell <- as.vector(pop_totals_adm)[match(
    adm_code,
    pop_totals_adm$adm_code
  ), ]

  pop_totals_adm_cell$adm_code <- NULL

  # for all cells with some population, but unknown country, set to 0/1
  # this shouldn't affect totals, since poopulations in NA country cells already set to 0
  disowned_adm <- which(is.na(pop_totals_adm_cell[, 2]))
  pop_totals_adm_cell[disowned_adm, ] <- 1
  pop_cell[disowned_adm, ] <- 0

  # get  population weights for each cell
  pop_wt_adm <- pop_cell / pop_totals_adm_cell

  # return
  return(list(
    "pop_wt" = as.matrix(pop_wt_adm),
    "admin_level" = admin_level,
    "adm_code" = adm_code
  ))
}
