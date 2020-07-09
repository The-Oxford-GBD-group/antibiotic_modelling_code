#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname getNewChunksWrapper
#' @export
getNewChunksWrapper <- function(x) {
  # data for this chunk (shapefile/age bin combo)
  chunk <- chunks[[x]]
  points <- chunk_points[[x]]

  if (nrow(points) == 0) {
    new_chunk <- chunk[0, ]
    warning(paste("Chunk", x, "missing spatial info and will be dropped."))
  } else {
    dupRecords <- function(j) {
      record <- chunk[j, , drop = FALSE] # pull out the record
      # drop columns also in "points"
      record <- record[, !(colnames(record) %in% colnames(points)), with = FALSE]
      # faster than rbind / replicate
      record_dup <- cbind(record, points)

      return(record_dup)
    }

    duped_records <- lapply(1:nrow(chunk), dupRecords)

    # append the duplicated data
    new_chunk <- rbindlist(duped_records)
  }
  return(new_chunk)
}
