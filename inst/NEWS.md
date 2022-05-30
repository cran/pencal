# Introduction

The aim of this document is to keep track of the changes made to the
different versions of the `R` package `pencal`.

The numbering of package versions follows the convention a.b.c, where a
and b are non-negative integers, and c is a positive integer. When minor
changes are made to the package, a and b are kept fixed and only c is
increased. Major changes to the package, instead, are made apparent by
changing a or b.

Each section of this document corresponds to a major change in the
package - in other words, within a section you will find all those
package versions a.b.x where a and b are fixed whereas x = 1, 2, 3, …
Each subsection corresponds to a specific package version.

# 1.2.x

## pencal 1.2.0

-   Released: May 2022
-   Added `max.ymissing` argument to `fit_lmms`: with this change, it is
    now possible to estimate the LMMs within the PRC-LMM model even if
    there are subjects with missing measurements for some (but not all)
    of the longitudinal outcomes. Within `summarize_lmms`, the predicted
    random effects when a longitudinal outcome is missing for a given
    subject are set = 0 (marginal / population average). Setting
    `max.ymissing = 0` disables such additional feature
-   Added extra check to `summarize_lmms` on subjects without any
    longitudinal information available (i.e., 100% missing on all
    longitudinal variables used in step 1)
-   Introduced dependency on `purrr` (now required by `summarize_lmms`)
-   Fixed `CRAN` dependency issue with examples in
    `simulate_prclmm_data` and `simulate_prcmlpmm_data`

# 1.1.x

## pencal 1.1.1

-   Released: May 2022
-   Added `tau.age` argument to `simulate_prclmm_data` and
    `simulate_prclmm_data`
-   Minor fix of an error message inside `fit_lmms` (row 181)

## pencal 1.1.0

-   Released: March 2022
-   Fixed subject ids displayed in the output of `survpred_prclmm`
-   Fixed bug that made `survpred_prclmm` fail when new data for just 1
    subject were supplied (added missing `drop = FALSE`)
-   Added function call to the output of `survpred_prcmlpmm`

# 1.0.x

## pencal 1.0.2

-   Released: February 2022
-   Added volume, issue and page number to CITATION file, vignette and
    help pages
-   Updated vignette with more detailed installation instructions

## pencal 1.0.1

-   Released: December 2021
-   Minor correction to package description (it was still mentioning
    arXiv instead of the *Statistics in Medicine* publication)

## pencal 1.0.0

-   Released: September 2021
-   The article describing Penalized Regression Calibration is now
    published in *Statistics in Medicine*! The article is published with
    Open Access, so anybody can freely download it [from the website of
    Statistics in Medicine](https://doi.org/10.1002/sim.9178)
-   Updated package description and citation info
-   Updated vignette and help pages

# 0.4.x

## pencal 0.4.2

-   Released: May 2021
-   Function `survpred_prc` replaced by two distinct functions:
    `survpred_prclmm` for the PRC-LMM model, and `survpred_prcmlpmm` for
    the PRC-MLPMM model
-   Documentation and vignette updated accordingly

## pencal 0.4.1

-   Released: April 2021
-   `fit_lmms` is now more memory efficient (`keep.data = F` when
    calling lme)
-   `fit_mlpmms` is now faster (parallelization implemented also before
    the CBOCP is started)
-   Added functions `pencox_baseline` and `performance_pencox_baseline`
-   Minor updates to the vignette

# 0.3.x

## pencal 0.3.2

-   Released: March 2021
-   Fixed CRAN error in PRC MLPMM examples (replaced `T` with `TRUE`)
-   Corrected typos in vignette

## pencal 0.3.1

-   Released: March 2021
-   Added a set of functions that can be used to fit the PRC-MLPMM
    model(s): `simulate_prcmlpmm_data`, `fit_mlpmms`, `summarize_mlpmms`
    and `fit_prcmlpmm`
-   Renamed `performance_prclmm` to `performance_prc`, and
    `survpred_prclmm` to `survpred_prc` (the functions work both for the
    PRC-LMM, and the PRC-MLPMM)
-   Vignettes and documentation updated to reflect the changes

# 0.2.x

## pencal 0.2.2

-   Released: January 2021
-   Fixed CRAN error caused by parallel::detectCores()
-   Added link to arXiv preprint in package description and vignette
-   Added CITATION file
-   Vignettes updated and revised
-   Updated references in help pages

## pencal 0.2.1

-   Released: January 2021
-   Added vignette: “An introduction to the R package pencal”

# 0.1.x

## pencal 0.1.2

-   Released: December 2020
-   Added the function `survpred_prclmm`, which computes predicted
    survival probabilities from the fitted PRC-LMM model
-   Added `fitted_prclmm` data object and related documentation (it is
    used in the examples of `performance_prclmm`)
-   Several corrections and clarifications added to the documentation
-   Changed displaying style for function arguments in the documentation

## pencal 0.1.1

-   Released: November 2020
-   This is the first public release of the `pencal` package. It
    comprises the skeleton around which the rest of the R package will
    be built
-   This version comprises functions to perform the following tasks:
    1.  simulate data corresponding to the PRC-LMM model (functions
        `simulate_t_weibull` and `simulate_prclmm_data`);
    2.  estimate the PRC-LMM model and its associated cluster bootstrap
        optimism correction procedure (functions `fit_lmms`,
        `summarize_lmms` and `fit_prclmm`);
    3.  compute the optimism-corrected estimates of the C index and
        time-dependent AUC (function `performance_prclmm`)
-   Note: developing an `R` package that is user-friendly, comprehensive
    and well-documented is an effort that takes months, sometimes even
    years. This package is *currently under active development*, and
    many additional features and functionalities (including vignettes!)
    will be added incrementally with the next releases. If you notice a
    bug or something unclear in the documentation, feel free to get in
    touch with the maintainer of the package!
