#' @title Pred cell pred
#' @description This converts the cell pred to a data.table and sets up the variables needed for merging
#'
#' @param cell_pred the cell pred object that will be raked and aggregated
#' @param cell_ids the cell ids from the global simple raster, used to connect the data to the link table
#' @param pixel_ids the cell ids from the simple raster
#' @param covdt the data table containing the populations and possibly the staker covariates as well
#'
#' @return returns the cell_pred as a dt with additional columns for cell_id and pixel_id wich allows for
#' merging to the link table.  Also adds the population and covariate columns if you would like.
#'
#' @export
prep_cell_pred <- function(cell_pred = cell_pred,
                           cell_ids = cell_ids,
                           pixel_id = pixel_id,
                           covdt = covdt) {
  # set cell pred as a data table, and rename things
  if (!inherits(x = cell_pred, "data.table")) {
    cell_pred <- as.data.table(cell_pred)
    names(cell_pred) <- paste0("V", 1:ncol(cell_pred))
  }
  
  cell_pred[, cell_pred_id := .I] #cell_pred object ID
  cell_pred[,cell_id := rep(cell_ids, times = nrow(cell_pred) / length(cell_ids))]  #cell id references the africa map
  cell_pred[,pixel_id := rep(pixel_id, times = nrow(cell_pred) / length(pixel_id))] #pixel id references the regional map
  
  #add population, year and potentially the stackers
  cell_pred = cbind(cell_pred, covdt[,c('year', 'pop'),with = F])
  
  #make sure it behaved
  stopifnot(any(!(cell_pred[,pixel_id] != rep.int(covdt[,pixel_id], 18))))
  
  return(cell_pred)
}
