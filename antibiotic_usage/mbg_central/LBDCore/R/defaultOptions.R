#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param resolution PARAM_DESCRIPTION, Default: 5
#' @param location PARAM_DESCRIPTION, Default: loc()
#' @param cores PARAM_DESCRIPTION, Default: switch(loc(), oxford = 60, seattle = 60)
#' @param spacetime PARAM_DESCRIPTION, Default: TRUE
#' @param start PARAM_DESCRIPTION, Default: Sys.time()
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname defaultOptions
#' @export
defaultOptions <- function(resolution = 5,
                           location = loc(),
                           cores = switch(loc(), oxford = 60, seattle = 60),
                           spacetime = TRUE,
                           start = Sys.time()) {

  # built in check for if they selected more than one country

  # if these options are not already set, set them at these default values

  # list all visible objects in this environment
  object_names <- ls()

  # get them in a named list
  objects <- lapply(object_names,
    get,
    envir = environment()
  )
  names(objects) <- object_names

  # empty vector of the options to keep
  keep <- c()

  # loop through identifying those that don't already exist
  for (i in 1:length(objects)) {

    # if doesn't exist yet...
    if (is.null(options(names(objects)[i])[[1]])) {

      # add it to the list
      keep <- c(keep, i)
    }
  }

  # keep the new options
  objects <- objects[keep]

  if (length(objects) > 0) {
    # notify the user
    message("\nThe following options were not defined and have been set to their defaults:\n")
    for (i in 1:length(objects)) {
      message(sprintf(
        "    %s = %s",
        names(objects)[i],
        paste(objects[[i]], collapse = ", ")
      ))
    }
    message("\n")
  }

  # add to options
  options(objects)
}
