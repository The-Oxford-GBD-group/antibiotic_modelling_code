#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname get_qstat_table
#' @export
get_qstat_table <- function() {
  qs <- system("qstat", intern = T)
  qs <- qs[3:length(qs)] # Trim headers
  qs <- lapply(qs, function(x) gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)) %>% unlist()
  qs <- lapply(qs, function(x) unlist(strsplit(x, " ", fixed = T)))
  qs <- lapply(qs, function(x) return(x[1:5])) # Trim to just the useful stuff
  qs <- rbindlist(lapply(qs, function(x) setDT(as.list(x))[]))
  names(qs) <- c("jobid", "prior", "name", "user", "state")
  qs[, jobid := as.numeric(jobid)]
  return(qs)
}
