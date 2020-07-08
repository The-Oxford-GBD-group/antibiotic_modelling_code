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
#' @rdname loc
#' @export
loc <- function() {
  # detect location based on username

  user <- system("echo $USER",
    intern = TRUE
  )

  # use this to work out our location
  location <- switch(user,
    lina1864 = "oxford",
    goldingn = "seattle",
    royburst = "seattle"
  )

  return(location)
}
