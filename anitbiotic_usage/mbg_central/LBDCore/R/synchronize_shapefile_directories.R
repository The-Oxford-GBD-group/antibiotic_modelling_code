
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cores PARAM_DESCRIPTION, Default: NULL
#' @param verbose PARAM_DESCRIPTION, Default: F
#' @param in_dir PARAM_DESCRIPTION, Default: '/home/j/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/'
#' @param out_dir PARAM_DESCRIPTION, Default: '/share/geospatial/rds_shapefiles/'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[stringr]{str_match}}
#' @rdname synchronize_shapefile_directories
#' @export
#' @importFrom stringr str_match
synchronize_shapefile_directories <- function(cores = NULL,
                                              verbose = F,
                                              in_dir = "/home/j/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/",
                                              out_dir = "/share/geospatial/rds_shapefiles/") {
  # Synchronize the two shapefile directories
  # Only new or more recently modified files will be changed (for speed)

  # This function performs a one-way sync from in_dir --> out_dir
  # Run this before using any of the fast shapefile functions

  message("Checking to see if shapefile synchronization needed...")

  # Load one function from stringr
  str_match <- stringr::str_match

  # Make list of input files from in_dir with last modified time
  in_files <- list.files(in_dir, pattern = ".shp$", full.names = T)
  in_files <- as.data.table(in_files) %>% setnames(., "in_files", "filename_in")
  in_files[, last_modified_in := file.mtime(filename_in)]
  in_files[, shape := str_match(filename_in, "//(.*).shp$")[, 2]]

  # Make list of output files from out_dir with last modified time
  out_files <- list.files(out_dir, pattern = ".rds$", full.names = T)
  out_files <- as.data.table(out_files) %>% setnames(., "out_files", "filename_out")
  out_files[, last_modified_out := file.mtime(filename_out)]
  out_files[, shape := str_match(filename_out, "//(.*).rds$")[, 2]]

  out_files <- subset(out_files, !grepl("_lockfile", shape))

  # create a single master list
  in_files <- merge(in_files, out_files, all.x = T, all.y = F, by = "shape")

  # Get list of files in the in_dir but not the out_dir
  files_to_synchronize <- subset(in_files, is.na(filename_out) | last_modified_out < last_modified_in)
  files_to_synchronize <- unique(files_to_synchronize$filename_in)

  if (length(files_to_synchronize) > 0) {

    # Simplify shapes
    message(paste0("\nSynchronizing ", length(files_to_synchronize), " shapefiles..."))

    if (is.null(cores)) {
      results <- lapply(files_to_synchronize, factory(save_shapefile_as_rds),
        in_dir = in_dir, out_dir = out_dir, verbose = verbose
      )
    } else if (!is.null(cores)) {
      # Set multithreading to serial for `mclapply()`:
      set_serial_threads()
      results <- mclapply(files_to_synchronize, factory(save_shapefile_as_rds),
        in_dir = in_dir, out_dir = out_dir, verbose = verbose,
        mc.cores = cores
      )
      # Return to multithreading (if any):
      set_original_threads()
    }

    # Make & format a results table
    pull_result <- function(x) x[[1]] %>% as.character()

    out_table <- results %>%
      lapply(., function(x) as.character(x[[1]])) %>%
      unlist() %>%
      as.data.table() %>%
      setnames(., c("."), c("result")) %>%
      .[, shape := str_match(files_to_synchronize, "//(.*).shp$")[, 2]] %>%
      setcolorder(., c("shape", "result"))

    out_table <- subset(out_table, result != "success")

    if (nrow(out_table) > 0) {
      warning(paste0(
        "The following shapefiles failed to synchronize:\n   ",
        paste(out_table$shape, collapse = "\n   "),
        "\nSee the output of this function for a summary of errors."
      ))
      return(out_table)
    } else {
      message(paste0("Successfully synchronized ", length(files_to_synchronize), " shapes."))
      return_table <- as.data.table(c(
        paste0(
          "Successfully synchronized ",
          length(files_to_synchronize),
          " shapes."
        ),
        files_to_synchronize
      ))
      return(return_table)
    }
  } else {
    message("No files to synchronize")
    return(as.data.table("No files to synchronize"))
  }
}
