% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survplot_prc.R
\name{survplot_prc}
\alias{survplot_prc}
\title{Visualize survival predictions for a fitted PRC model}
\usage{
survplot_prc(step1, step2, step3, ids, tmax = 5, res = 0.01, lwd = 1,
  lty = 1, legend.title = "Subject", legend.inset = -0.3,
  legend.space = 1)
}
\arguments{
\item{step1}{the output of \code{\link{fit_lmms}} or \code{\link{fit_mlpmms}}}

\item{step2}{the output of \code{\link{summarize_lmms}} or
\code{\link{summarize_mlpmms}}}

\item{step3}{the output of \code{\link{fit_prclmm}} or \code{\link{fit_prcmlpmm}}}

\item{ids}{a vector with the identifiers of the subjects to show in the plot}

\item{tmax}{maximum prediction time to consider for the chart. Default is 5}

\item{res}{resolution at which to evaluate predictions for the chart. Default is 0.01}

\item{lwd}{line width}

\item{lty}{line type}

\item{legend.title}{legend title}

\item{legend.inset}{moves legend more to the left / right (default is -0.3)}

\item{legend.space}{interspace between lines in the legend (default is 1)}
}
\description{
Visualize survival predictions for a fitted PRC model
}
\examples{
# generate example data
simdata = simulate_prclmm_data(n = 100, p = 4, p.relev = 2, 
             t.values = c(0, 0.2, 0.5, 1, 1.5, 2),
             landmark = 2, seed = 123)
             
# estimate the PRC-LMM model
y.names = paste('marker', 1:4, sep = '')
step1 = fit_lmms(y.names = y.names, 
                 fixefs = ~ age, ranefs = ~ age | id, 
                 long.data = simdata$long.data, 
                 surv.data = simdata$surv.data,
                 t.from.base = t.from.base,
                 n.boots = 0)
step2 = summarize_lmms(object = step1)
step3 = fit_prclmm(object = step2, surv.data = simdata$surv.data,
                   baseline.covs = ~ baseline.age,
                   penalty = 'ridge')

# visualize the predicted survival for subjects 1, 3, 7 and 13                    
survplot_prc(step1, step2, step3, ids = c(1, 3, 7, 13), tmax = 6)
}
\references{
Signorelli, M. (2024). pencal: an R Package for the Dynamic 
Prediction of Survival with Many Longitudinal Predictors. 
To appear in: The R Journal. Preprint: arXiv:2309.15600
}
\author{
Mirko Signorelli
}
