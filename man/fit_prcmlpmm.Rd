% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_prcmlpmm.R
\name{fit_prcmlpmm}
\alias{fit_prcmlpmm}
\title{Step 3 of PRC-MLPMM (estimation of the penalized Cox model(s))}
\usage{
fit_prcmlpmm(object, surv.data, baseline.covs = NULL, include.b0s = TRUE,
  penalty = "ridge", standardize = TRUE, pfac.base.covs = 0,
  cv.seed = 19920207, n.alpha.elnet = 11, n.folds.elnet = 5,
  n.cores = 1, verbose = TRUE)
}
\arguments{
\item{object}{the output of step 2 of the PRC-MLPMM procedure, 
as produced by the \code{\link{summarize_mlpmms}} function}

\item{surv.data}{a data frame with the survival data and (if 
relevant) additional baseline covariates. \code{surv.data} should at least
contain a subject id (called \code{id}), the time to event outcome  
(\code{time}), and binary event variable (\code{event})}

\item{baseline.covs}{a formula specifying the variables 
(e.g., baseline age) in \code{surv.data} that should be included 
as baseline covariates in the penalized Cox model. Example:
\code{baseline.covs = '~ baseline.age'}. Default is \code{NULL}}

\item{include.b0s}{logical. If \code{TRUE}, the PRC-MLPMM(U+B) model
is estimated; if \code{FALSE}, the PRC-MLPMM(U) model is estimated. See
Signorelli et al. (2021) for details}

\item{penalty}{the type of penalty function used for regularization.
Default is \code{'ridge'}, other possible values are \code{'elasticnet'} 
and \code{'lasso'}}

\item{standardize}{logical argument: should the predicted random effects
be standardized when included in the penalized Cox model? Default is \code{TRUE}}

\item{pfac.base.covs}{a single value, or a vector of values, indicating
whether the baseline covariates (if any) should be penalized (1) or not (0).
Default is \code{pfac.base.covs = 0} (no penalization of all baseline covariates)}

\item{cv.seed}{value of the random seed to use for the cross-validation
done to select the optimal value of the tuning parameter}

\item{n.alpha.elnet}{number of alpha values for the two-dimensional 
grid of tuning parameteres in elasticnet.
Only relevant if \code{penalty = 'elasticnet'}. Default is 11,
so that the resulting alpha grid is c(1, 0.9, 0.8, ..., 0.1, 0)}

\item{n.folds.elnet}{number of folds to be used for the selection
of the tuning parameter in elasticnet. Only relevant if 
\code{penalty = 'elasticnet'}. Default is 5}

\item{n.cores}{number of cores to use to parallelize part of
the computations. If \code{ncores = 1} (default), no parallelization is done. 
Pro tip: you can use \code{parallel::detectCores()} to check 
how many cores are available on your computer}

\item{verbose}{if \code{TRUE} (default and recommended value), information
on the ongoing computations is printed in the console}
}
\value{
A list containing the following objects:
\itemize{
\item \code{call}: the function call
\item \code{pcox.orig}: the penalized Cox model fitted on the
original dataset;
\item \code{tuning}: the values of the tuning parameter(s) selected through 
cross-validation
\item \code{surv.data}: the supplied survival data (ordered by
subject id)
\item \code{n.boots}: number of bootstrap samples;
\item \code{boot.ids}: a list with the ids of bootstrapped subjects 
(when \code{n.boots > 0});
\item \code{pcox.boot}: a list where each element is a fitted penalized
Cox model for a given bootstrap sample (when \code{n.boots > 0}).
}
}
\description{
This function performs the third step for the estimation
of the PRC-MLPMM model proposed in Signorelli et al. (2021)
}
\examples{
\donttest{
# generate example data
set.seed(123)
n.items = c(4,2,2,3,4,2)
simdata = simulate_prcmlpmm_data(n = 100, p = length(n.items),  
             p.relev = 3, n.items = n.items, 
             type = 'u+b', seed = 1)
 
# specify options for cluster bootstrap optimism correction
# procedure and for parallel computing 
do.bootstrap = FALSE
# IMPORTANT: set do.bootstrap = TRUE to compute the optimism correction!
n.boots = ifelse(do.bootstrap, 100, 0)
more.cores = FALSE
# IMPORTANT: set more.cores = TRUE to speed computations up!
if (!more.cores) n.cores = 2
if (more.cores) {
   # identify number of available cores on your machine
   n.cores = parallel::detectCores()
   if (is.na(n.cores)) n.cores = 2
}

# step 1 of PRC-MLPMM: estimate the MLPMMs
y.names = vector('list', length(n.items))
for (i in 1:length(n.items)) {
  y.names[[i]] = paste('marker', i, '_', 1:n.items[i], sep = '')
}

#' # temporary patch v 2.2.3 (see NEWS file):
run = FALSE
if (run) {
step1 = fit_mlpmms(y.names, fixefs = ~ contrast(age),  
                 ranef.time = age, randint.items = TRUE, 
                 long.data = simdata$long.data, 
                 surv.data = simdata$surv.data,
                 t.from.base = t.from.base,
                 n.boots = n.boots, n.cores = n.cores)

# step 2 of PRC-MLPMM: compute the summaries 
step2 = summarize_mlpmms(object = step1, n.cores = n.cores)

# step 3 of PRC-LMM: fit the penalized Cox models
step3 = fit_prcmlpmm(object = step2, surv.data = simdata$surv.data,
                   baseline.covs = ~ baseline.age,
                   include.b0s = TRUE,
                   penalty = 'ridge', n.cores = n.cores)
summary(step3)
}
}
}
\references{
Signorelli, M. (2024). pencal: an R Package for the Dynamic 
Prediction of Survival with Many Longitudinal Predictors. 
To appear in: The R Journal. Preprint: arXiv:2309.15600

Signorelli, M., Spitali, P., Al-Khalili Szigyarto, C, 
The MARK-MD Consortium, Tsonaka, R. (2021). 
Penalized regression calibration: a method for the prediction 
of survival outcomes using complex longitudinal and 
high-dimensional data. Statistics in Medicine, 40 (27), 6178-6196.
DOI: 10.1002/sim.9178
}
\seealso{
\code{\link{fit_mlpmms}} (step 1), 
\code{\link{summarize_mlpmms}} (step 2),
\code{\link{performance_prc}}
}
\author{
Mirko Signorelli
}
