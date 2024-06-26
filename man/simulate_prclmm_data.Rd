% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_prclmm_data.R
\name{simulate_prclmm_data}
\alias{simulate_prclmm_data}
\title{Simulate data that can be used to fit the PRC-LMM model}
\usage{
simulate_prclmm_data(n = 100, p = 10, p.relev = 4, t.values = c(0, 0.5,
  1, 2), landmark = max(t.values), seed = 1, lambda = 0.2, nu = 2,
  cens.range = c(landmark, 10), base.age.range = c(3, 5), tau.age = 0.2)
}
\arguments{
\item{n}{sample size}

\item{p}{number of longitudinal outcomes}

\item{p.relev}{number of longitudinal outcomes that
are associated with the survival outcome (min: 1, max: p)}

\item{t.values}{vector specifying the time points 
at which longitudinal measurements are collected
(NB: for simplicity, this function assumes a balanced 
designed; however, \code{pencal} is designed to work
both with balanced and with unbalanced designs!)}

\item{landmark}{the landmark time up until which all individuals survived.
Default is equal to \code{max(t.values)}}

\item{seed}{random seed (defaults to 1)}

\item{lambda}{Weibull location parameter, positive}

\item{nu}{Weibull scale parameter, positive}

\item{cens.range}{range for censoring times. By default, the minimum
of this range is equal to the \code{landmark} time}

\item{base.age.range}{range for age at baseline (set it
equal to c(0, 0) if you want all subjects to enter
the study at the same age)}

\item{tau.age}{the coefficient that multiplies baseline age
in the linear predictor (like in formula (6) from Signorelli 
et al. (2021))}
}
\value{
A list containing the following elements:
\itemize{
\item a dataframe \code{long.data} with data on the longitudinal 
predictors, comprehensive of a subject id (\code{id}),
baseline age (\code{base.age}), time from baseline
(\code{t.from.base}) and the longitudinal biomarkers;
\item a dataframe \code{surv.data} with the survival data: 
a subject id (\code{id}), baseline age (\code{baseline.age}),
the time to event outcome (\code{time}) and a binary vector
(\code{event}) that is 1 if the event
is observed, and 0 in case of right-censoring;
\item \code{perc.cens} the proportion of censored individuals 
in the simulated dataset;
\item \code{theta.true} a list containing the true parameter values
used to simulate data from the mixed model (beta0 and beta1) and
from the Weibull model (tau.age, gamma, delta)
}
}
\description{
This function allows to simulate a survival outcome
from longitudinal predictors following the PRC LMM model
presented in Signorelli et al. (2021). Specifically, the longitudinal
predictors are simulated from linear mixed models (LMMs), and 
the survival outcome from a Weibull model where the time
to event depends linearly on the baseline age and on the 
random effects from the LMMs.
}
\examples{
# generate example data
simdata = simulate_prclmm_data(n = 20, p = 10, p.relev = 4,
               t.values = c(0, 0.5, 1, 2), landmark = 2, 
               seed = 19931101)
# view the longitudinal markers:
if(requireNamespace("ptmixed")) {
  ptmixed::make.spaghetti(x = age, y = marker1, 
                 id = id, group = id,
                 data = simdata$long.data, 
                 legend.inset = - 1)
 }
# proportion of censored subjects
simdata$censoring.prop
# visualize KM estimate of survival
library(survival)
surv.obj = Surv(time = simdata$surv.data$time, 
                event = simdata$surv.data$event)
kaplan <- survfit(surv.obj ~ 1,  
                  type="kaplan-meier")
plot(kaplan)
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
\author{
Mirko Signorelli
}
