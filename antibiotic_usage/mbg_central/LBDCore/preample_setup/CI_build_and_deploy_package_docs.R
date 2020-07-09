
#######################################################
## Building package and documentation for LBDCore
#######################################################

## Install library to temp folder
tmpdir <- Sys.getenv("TMPDIR")
if (tmpdir == "") {
    tmpdir <- Sys.getenv("TMP")
}
if (tmpdir == "") {
    tmpdir <- "/tmp"
}
unlink(paste0(tmpdir, "/LBDCore"), recursive = TRUE)
.libPaths(c(tmpdir, .libPaths()))
library(devtools)
library(pkgdown)

devtools::document('mbg_central/LBDCore/')
devtools::install('mbg_central/LBDCore/', dependencies = FALSE, upgrade = FALSE)
library(LBDCore)

## Build static HTML dox
pkgdown::build_site(pkg = "mbg_central/LBDCore", examples = TRUE, document = FALSE)

## Copy to sandbox
htdoc_location <- "/ihme/homes/miker985/htdocs"
system(sprintf("rm -rf %s/LBDCore", htdoc_location))
system(sprintf("umask 0002; mkdir -p %s", htdoc_location))
system(sprintf("cp -rf mbg_central/LBDCore/docs %s/LBDCore", htdoc_location))
system(sprintf("chmod -R +rx %s/LBDCore", htdoc_location))
