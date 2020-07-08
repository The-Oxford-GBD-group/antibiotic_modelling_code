#' @title Merge shape hierarchy
#' @description Takes a data frame or data table with a column of administrative codes
#' and merges on the accompanying admin code names, as well as any higher
#' order administrative codes within which that admin code is nested
#'
#' @param df Data frame or data table with a column of admin codes
#' @param admin_level The administrative level of the codes in the
#'   \code{df} object.  Integer; 0, 1, or 2.
#' @param idx_col which column in \code{df} contains the administrative
#'   codes?  A character.
#' @param sp_h Spatial hierarchy object generated with
#'   \code{get_sp_hierarchy()}. If \code{NULL}, then will be generated
#'   within the function. If using this function several times, it may
#'   save time to generate the sp_h object once, then pass it to this
#'   function
#' @param shapefile_version String specifying shapefile version to pull of is.null(sp_h)
#' @return Data table with sp_hierarchy added on.  Note that the
#'   original admin_code column will be renamed to conform with
#'   the "ADMX_CODE" convention.
#' @examples
#' \dontrun{
#' # add spatial hierarchy information to df, which contains
#' # admin 2 codes in the column "spatial_idx", using a pre-made
#' # spatial hierarchy object stored in sp_h
#' df2 <- merge_sp_hierarchy(
#'   df = df,
#'   admin_level = 2,
#'   idx_col = "spatial_idx",
#'   sp_h = sp_h
#' )
#' }
#' 
#' @rdname merge_sp_hierarchy
#'
#' @export
merge_sp_hierarchy <- function(df, admin_level, idx_col, sp_h = NULL, shapefile_version = "current") {

  # Grab the spatial hierarchy
  if (is.null(sp_h)) sp_h <- get_sp_hierarchy(shapefile_version = shapefile_version)

  df <- as.data.table(df) %>%
    setnames(., idx_col, paste0("ADM", admin_level, "_CODE"))

  # grab df names but not the spatial idx
  df_names <- names(df)[names(df) != paste0("ADM", admin_level, "_CODE")]

  sp_idx_table <- sp_h[[paste0("ADM", admin_level)]]

  df <- merge(sp_idx_table, df,
    by = c(paste0("ADM", admin_level, "_CODE")),
    all.y = T, all.x = F
  )

  idx_names <- sort(names(sp_idx_table))

  setcolorder(df, c(idx_names, df_names))
  setorderv(df, c(paste0("ADM", 0:admin_level, "_CODE")))

  return(df)
}
