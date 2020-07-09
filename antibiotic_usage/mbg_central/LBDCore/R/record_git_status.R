#' @title Record Git Status
#' @description Retrieve information about current git status (eg, branch, commit,
#' uncommitted changes) for core repository and optionally a separate
#' indicator repository. A check can also be made to see if your core
#' repository is in sync with the LBD core repo
#' (\code{/share/code/geospatial/lbd_core}) if a fork is being used.
#'
#' Can be used at a minimum to print the git hash of the code being used
#' for posterity.
#'
#' @param core_repo file path to the lbd_core repo
#' @param indic_repo file path to an indicator-specific repo (optional)
#' @param show_diff logical. If there are uncomitted changes, should the
#'     output from git diff be shown?
#' @param check_core_repo logical. Will check whatever has been set as
#'     'core_repo' is the default LBD core code master repo and will give
#'     messages and warnings if not.
#' @param file file path to a text file where output should be saved. This
#'     is optional. If no file path is provided, the output will instead be
#'     printed to the screen.
#'
#' @return Git status
#'
#' @export
record_git_status <- function(core_repo,
                              indic_repo = NULL,
                              show_diff = FALSE,
                              check_core_repo = TRUE,
                              file = NULL) {

  # the core code repo directory
  lbd_core_repo <- "/share/code/geospatial/lbd_core"

  # if a file is specified, start a sink to record output
  if (!is.null(file)) sink(file)

  # print out the core_repo status
  cat(get_git_status(repo = core_repo, repo_name = "Core repo", show_diff = FALSE))

  # if the user wants to make sure their repo is up-to-date with LBD master
  if (check_core_repo) {
    check_repo_report <- "\n********** REPO CHECK **********\n"
    # The two repo paths are the same
    if (clean_path(core_repo) == lbd_core_repo) {
      check_repo_report <- paste0(c(
        check_repo_report,
        "'core_repo' set to default LBD core code master repo: '",
        core_repo, "'\n"
      ))
    } else {
      # Get the git hashes of the standard LBD core code repo and user repo and compare
      lbd_core_repo_hash <- system(paste0("cd ", lbd_core_repo, "; git rev-parse HEAD"),
        intern = TRUE
      )
      core_repo_hash <- system(paste0("cd ", core_repo, "; git rev-parse HEAD"),
        intern = TRUE
      )
      # The two repos paths are not the same, but the hash matches (separate, up-to-date clones)
      if (lbd_core_repo_hash == core_repo_hash) {
        check_repo_report <- paste0(c(
          check_repo_report,
          "Current 'core_repo' clone is up-to-date with LBD core code master repo:\n'",
          lbd_core_repo, "' == '", core_repo, "'\n"
        ))
        # The two repo paths are not the same and the hash doesn't match, repos are out of sync
      } else {
        # print out the LBD core code master repo information for reference against current
        # repo being used as a warning.
        check_repo_report <- paste0(c(
          check_repo_report,
          "WARNING: Current 'core_repo' clone is out of sync with the LBD core code master repo:\n'",
          lbd_core_repo, "' != '", core_repo, "'\n",
          "\n\n** LBD Core Code MASTER Repo Git Info **\n"
        ))
        check_repo_report <- paste0(c(
          check_repo_report,
          get_git_status(
            repo = lbd_core_repo,
            repo_name = "LBD Core Repo",
            show_diff = FALSE
          )
        ))
      }
    }
    cat(check_repo_report)
  }
  # run git log and git status on the indicator repo
  if (!is.null(indic_repo)) {
    cat(get_git_status(repo = indic_repo, repo_name = "Indicator repo", show_diff = FALSE))
  }
  # if a file is specified, end the sink recording output
  if (!is.null(file)) sink()
}
