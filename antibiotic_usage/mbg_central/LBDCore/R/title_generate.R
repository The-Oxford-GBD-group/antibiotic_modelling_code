#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ind_title PARAM_DESCRIPTION
#' @param year_list PARAM_DESCRIPTION
#' @param admin PARAM_DESCRIPTION, Default: 0
#' @param ad0_reg_title PARAM_DESCRIPTION, Default: ''
#' @param ctry PARAM_DESCRIPTION, Default: ''
#' @param ad1_df_ad1 PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname title_generate
#' @export
title_generate <- function(ind_title,
                           year_list,
                           admin = 0,
                           ad0_reg_title = "",
                           ctry = "",
                           ad1_df_ad1 = "") {
  # Title generate makes the title Grob that goes in the right hand corner of the pdf
  arrangeGrob(textGrob("", gp = gpar(fontsize = 30)),
    textGrob(str_wrap(ind_title, 18),
      gp = gpar(fontsize = title_grob_size, fontface = "bold")
    ),
    textGrob("", gp = gpar(fontsize = 10)),
    textGrob(if (admin == 0) ifelse(ad0_reg_title == "All countries", NULL, ad0_reg_title) else ctry,
      gp = gpar(fontsize = 20)
    ),
    textGrob("", gp = gpar(fontsize = 10)),
    textGrob(if (admin == 0) "National estimates" else if (admin == 1) "By First-level Administrative Unit" else paste0("Admin 1: ", unique(ad1_df_ad1$ADM1_NAME)),
      gp = gpar(fontsize = 15)
    ),
    textGrob("", gp = gpar(fontsize = 10)),
    textGrob(paste0(min(year_list), " - ", max(year_list)),
      gp = gpar(fontsize = 18)
    ),
    ncol = 1,
    heights = c(30, 25, 10, 25, 10, 15, 10, 20)
  )
}
