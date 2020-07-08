
#######################################################
## Building package and documentation for LBDCore
#######################################################

## Path setup
rm(list=ls())
## Point to personal directory (create if needed)
personal_lib <- sprintf(
  "~/R/x86_64-pc-linux-gnu-library/%s.%sgeo/",
  R.Version()$major, R.Version()$minor
)
Sys.setenv(R_LIBS_USER = personal_lib)
if (!dir.exists(personal_lib)) dir.create(personal_lib, recursive = TRUE)

## Set up .libPaths()
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

## Set up MKLROOT directory (needed if using RStudio)
Sys.setenv(MKLROOT = "/opt/intel/compilers_and_libraries/linux/mkl")

## Build and install LBDCore
user_path <- paste0("/share/code/geospatial/", Sys.info()["user"], "/lbd_core")
if (!dir.exists(user_path)) {
  message("The lbd_core repository clone does not exist in the expected place.")
  message("Please point to where you cloned the repo in order to install LBDCore.")
  stop("lbd_core repo not found")
}
system(paste0("rm ", user_path, "/mbg_central/LBDCore/*.so ", user_path, "/mbg_central/LBDCore/*.o ", user_path, "/mbg_central/LBDCore/*.gcda"))
system(paste0("rm -rf ", user_path, "/mbg_central/LBDCore/docs"))
system(paste0("rm -rf ", personal_lib, "LBDCore ", personal_lib, "00*"))

devtools::document(paste0(user_path, '/mbg_central/LBDCore/'))
devtools::install(paste0(user_path, '/mbg_central/LBDCore/'),
                  dependencies = FALSE, upgrade = FALSE)


