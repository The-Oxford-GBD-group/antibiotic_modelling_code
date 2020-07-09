#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param lv PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname update_lv_table
#' @export
update_lv_table <- function(lv) {

  # Grab and parse qstat
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

  qstat <- get_qstat_table()
  if ("state" %in% names(lv)) lv[, state := NULL] # clear state if present
  lv <- merge(lv, subset(qstat, select = c("jobid", "state")), by = "jobid", all.x = T, all.y = F)

  # For any jobs without an exit status that have closed, grab exit status
  get_qacct_exit_status <- function(jobid) {
    qa <- system(paste0("qacct -j ", jobid), intern = T)
    qa <- str_match(qa, "exit_status\\s+([0-9]*)")[, 2]
    qa <- as.numeric(qa[!is.na(qa)])
    return(qa)
  }

  get_qa_wrapper <- function(jobids) {
    Sys.sleep(30) # Give a bit of time to make sure that exit status generated
    return(sapply(jobids, get_qacct_exit_status))
  }

  lv[
    is.na(state) & is.na(exit_status),
    exit_status := get_qa_wrapper(jobid)
  ]

  # update states
  lv[is.na(state), state := "x"]

  return(lv)
}
