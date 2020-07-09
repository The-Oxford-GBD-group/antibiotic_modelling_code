### onLoad and onAttach functions ###

.onLoad <- function(libname, pkgname) {
  assign(
    "MBG_ROOT", "/share/geospatial/mbg",
    envir = .GlobalEnv
  )
  assign(
    "SGE_OUTPUT_ROOT", "/share/temp/sgeoutput",
    envir = .GlobalEnv
  )
  assign(
    "CC_ENV_DIR", "/ihme/cc_resources/libraries/gbd_env/r",
    envir = .GlobalEnv
  )
  Sys.setenv(
    MKLROOT = "/opt/intel/compilers_and_libraries/linux/mkl"
  )
  fix_raster_tmpdir()
}
