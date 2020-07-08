#' @title Adds a goal to a goal object, or creates a new one
#'
#' @description This function creates or modifies a "goal object" - which is just a data table
#' that contains all of the information in each row to check a given target.  This
#' setup is useful in case you want to check a series of different targets, or
#' run the same analysis for a series of target years.  (Note that all target
#' years must have projection objects created by first running \code{make_proj()})
#'
#' @param goal_obj An existing goal object. If \code{NULL}, will create a new goal object.
#' @param target_year Year for which the goal is meant. Must be a future (projected) year
#'   and the year projections must already be generated for this target_year using
#'   \code{make_proj()}
#' @param target Target (e.g. 0.8 for 80\% vaccine coverage); numeric
#' @param target_type Type of target ('greater' or 'less'); character
#' @param abs_rel Is this an 'absolute' or 'relative' goal?
#' @param baseline_year For relative goals, what year should we compare to?
#' @param proj Placeholder - for now, always use projected years.  In the future this
#'   may support modeled years as well, but still need to code
#' @param pred_type Type of aroc to create. Options include \code{cell}, \code{admin},
#'   or \code{c('cell', 'admin')} for both.
#' @return A "goal object" (data.table in a specific format for use in
#'   \code{compare_to_target()})
#' @examples
#' \dontrun{
#' # Define goals: start by initializing goal object
#' goals <- add_goal(
#'   target_year = 2030,
#'   target = 0.8,
#'   target_type = "greater",
#'   abs_rel = "absolute",
#'   pred_type = c("cell", "admin")
#' )
#' 
#' # Add goals to existing goal object by specifying goal_obj
#' goals <- add_goal(
#'   goal_obj = goals,
#'   target_year = 2020,
#'   target = 0.8,
#'   target_type = "greater",
#'   abs_rel = "absolute",
#'   pred_type = c("cell", "admin")
#' )
#' }
#' @export
add_goal <- function(goal_obj = NULL,
                     target_year,
                     target,
                     target_type,
                     abs_rel,
                     baseline_year = NA,
                     proj = TRUE,
                     pred_type = c("cell", "admin")) {
  if (!(abs_rel %in% c("absolute", "relative"))) {
    stop("abs_rel must be either \"absolute\" or \"relative\"")
  }

  if (!(target_type %in% c("greater", "less"))) {
    stop("target_type must be either \"greater\" or \"less\"")
  }

  if (abs_rel == "relative" & is.na(baseline_year)) {
    stop("Must specify baseline_year if abs_rel == relative")
  } else if (abs_rel == "absolute" & !is.na(baseline_year)) {
    warning("For absolute targets, ignoring baseline_year")
  }

  if (length(pred_type) > 1) {
    pred_type <- paste0(
      "c(\"",
      paste(pred_type, collapse = "\", \""),
      "\")"
    )
  }

  new_goal <- data.table(
    target_year = target_year,
    target = target,
    target_type = target_type,
    abs_rel = abs_rel,
    baseline_year = baseline_year,
    proj = proj,
    pred_type = as.character(pred_type)
  )

  if (!is.null(goal_obj)) new_goal <- rbind(goal_obj, new_goal)
  return(new_goal)
}
