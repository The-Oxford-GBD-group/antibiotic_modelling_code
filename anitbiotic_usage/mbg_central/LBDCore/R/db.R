#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param machine PARAM_DESCRIPTION, Default: c("local")
#' @param user PARAM_DESCRIPTION, Default: 'lina1864'
#' @param warnings PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname db
#' @export
db <- function(machine = c("local"), user = "lina1864", warnings = FALSE) {
  # check dropbox syncing status, optionally on multiple remote machines
  # requies key-pairs and firewall acces tho...
  # e.g.:
  #   synced <- db(c('local',
  #                  'zoo-mapbig02.zoo.ox.ac.uk',
  #                  'big04.well.ox.ac.uk',
  #                  'big05.well.ox.ac.uk',
  #                  'big06.well.ox.ac.uk',
  #                  'zoo-seeg02.zoo.ox.ac.uk')))

  # vectorize by recursion
  if (length(machine) > 1) {
    ans <- sapply(machine, db)
  } else {

    # for local machine
    if (machine == "local") {
      call <- "dropbox status"
    } else {
      call <- sprintf(
        "ssh %s@%s 'dropbox status'",
        user,
        machine
      )
    }

    # run the system call
    result <- tryCatch({
      system(call,
        intern = TRUE,
        ignore.stderr = TRUE
      )
    },
    error = function(e) "nope"
    )

    # if the user wants warnings
    if (warnings) {

      # warn if dropbox is switched off
      if (result == "Dropbox isn't running!") {
        warning(sprintf(
          "dropbox not running on %s",
          machine
        ))
      }

      # warn if the system call errored
      if (result == "nope") {
        warning(sprintf(
          "some sort of error occurred",
          machine
        ))
      }
    }

    # check if it's up to date
    ans <- result == "Up to date"
  }

  # return the result
  return(ans)
}
