#' @title Grabbing parallel model inputs
#' @description Get parallel model inputs from the command line arguments
#'
#' @param use_argparse Boolean. Was argparse used in the qsub? Default: FALSE
#'
#' @param load_prerun_image Boolean. Load pre-saved image? Default: TRUE
#'
#' @param file_format File format of pre-run image. Default = ".rds"
#'
#' @return A named \code{list} with the arguments: \code{reg, age, run_date, test, holdout, indicator, indicator_group, pathaddin, outputdir}
#'
#' @export
#'
setup_parallel_model_inputs <- function(use_argparse = FALSE, load_prerun_image = TRUE, file_format = ".rds") {
  if (!use_argparse) {
    reg <- as.character(commandArgs()[4])
    age <- as.numeric(commandArgs()[5])
    run_date <- as.character(commandArgs()[6])
    test <- as.numeric(commandArgs()[7])
    holdout <- as.numeric(commandArgs()[8])
    indicator <- as.character(commandArgs()[9])
    indicator_group <- as.character(commandArgs()[10])
    nodename <- as.character(commandArgs()[11])
    dag_hash <- as.character(commandArgs()[12])
  } else {
    parser <- ArgumentParser(description = "Process parallel model jobs")
    parser$add_argument("--reg",
      type = "character",
      help = "Region name"
    )
    parser$add_argument("--age",
      type = "integer",
      help = "Age group number"
    )
    parser$add_argument("--run_date",
      type = "character",
      help = "Run date"
    )
    parser$add_argument("--test",
      default = FALSE,
      help = "Test mode?"
    )
    parser$add_argument("--holdout",
      default = FALSE,
      help = "Holdout mode?"
    )
    parser$add_argument("--indicator",
      type = "character",
      help = "Indicator name"
    )
    parser$add_argument("--indicator_group",
      type = "character",
      help = "Indicator group name"
    )
    parser$add_argument("--nodename",
      type = "character",
      help = "Name of node"
    )
    parser$add_argument("--dag_hash",
      type = "character",
      help = "Hash string for DAG"
    )
    args <- parser$parse_args()
    print(args)
    ## Get the args out of the list, and also make sure
    ## that they're not NA or NULL
    for (varz in c(
      "reg", "age", "run_date", "test", "holdout",
      "indicator", "indicator_group", "dag_hash", "nodename"
    )) {
      stopifnot(!is.null(args[[varz]]))
      stopifnot(!is.na(args[[varz]]))
      assign(paste0(varz), args[[varz]], envir = .GlobalEnv)
    }
  }

  ## Load image?
  if (load_prerun_image) {
    return(
      load_pipeline_image(
        args$indicator_group,
        args$indicator,
        args$run_date,
        paste0("_bin", args$age, "_", args$reg, "_", args$holdout),
        file_format = file_format
      )
    )
  }
}
