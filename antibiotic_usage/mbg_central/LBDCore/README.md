# Local Burden of Disease Core Library

`LBDCore` contains the baseline set of functions to be able to run a model-based geostatistics (MBG) sequence.

## Documentation

Currently the documentation for `LBDCore` exists [here](http://sandbox-web.ihme.washington.edu/~miker985/LBDCore/) until we find it a stable home. Functions inside `LBDCore` have docstrings than can be viewed with `?function_name`.

## Installation

If you're using a Singularity image, you will need to point to a personal library to install `lbd_core`, for example like this (the `MKLROOT` path is necessary to install the threading functions):

```r
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

## Create the package documentation
devtools::document("/path/to/LBDCore")

## Install the LBDCore package
devtools::install("/path/to/LBDCore", dependencies = FALSE, upgrade = FALSE)
```


# Changes for modelers to implement

## Package setup 

1) Comment out any call to sourcing any explicit R scripts, like these. 

```r
# package_list <- c(t(read.csv('/share/geospatial/mbg/common_inputs/package_list.csv',header=FALSE)))
# source(paste0(core_repo, '/mbg_central/setup.R'))
# mbg_setup(package_list = package_list, repos = core_repo)
library(LBDCore, lib.loc = "/path/to/LBDCore")
```

2) Installing `LBDCore` to your personal library, and calling `LBDCore` as a package. See code snippet in the previous section.



## Child Stackers Script

Initially, the child stacker models were implemented by sourcing an R script once the child stacker model were defined in the vector `child_model_names`. In this package, the stacker model runs have been converted into the `run_child_stackers()` function. 

`run_child_stackers()` takes in two arguments: `model` which is the vector of child stacker models to run, and `input_data` which will be the input data frame used by each of the stacking sub-models, defaulted to `the_data` from the user's global environment (which was initially hardcoded into the stacker script).

Quoting from the general `parallel_model.R` script, the commented out part was what existed before, and the parts not commented are the new additions from the `LBDCore` package:

```r
# source(paste0(core_repo, '/mbg_central/share_scripts/run_child_stackers.R'))
child_mods <- run_child_stackers(models = child_model_names, stack_child = TRUE, input_data = the_data)

## combine the children models
# the_data  <- cbind(the_data, do.call(cbind, lapply(lapply(child_model_names, 'get'), function(x) x[[1]])))
the_data  <- cbind(the_data, child_mods)
child_model_objs <- setNames(lapply(lapply(child_model_names, 'get'), function(x) x[[2]]), child_model_names)
```

## Preserving Raster Names in Transformed Space

It turns out that for whatever reason, dividing the values in a raster with a log transformation ends up renaming the variables in the raster. Therefore, if there's a need for the rasters to be logit transformed, then there's a couple extra lines added on either side of the logit transformation (also in `parallel_model.R`):

```r
## transform the rasters
  for (ii in child_model_names) {
    
    ## Preserve variable names in the raster first
    tmp_rastvar <- names(cov_list[[ii]])
    
    ## Logit
    cov_list[[ii]] <- logit(cov_list[[ii]])
    
    ## Reassign names
    names(cov_list[[ii]]) <- tmp_rastvar
    rm(tmp_rastvar)
  }
```








