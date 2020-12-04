Introduction
============

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

0.1.x
=====

pencal 0.1.2
------------

-   Released: December 2020
-   Added the function `survpred_prclmm`, which computes predicted
    survival probabilities from the fitted PRC-LMM model
-   Added `fitted_prclmm` data object and related documentation (it is
    used in the examples of `performance_prclmm`)
-   Several corrections and clarifications added to the documentation
-   Changed displaying style for function arguments in the documentation

pencal 0.1.1
------------

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
