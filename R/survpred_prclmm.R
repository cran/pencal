#' Compute predictive survival probabilities from PRC-LMM
#'
#' This function computes the predictive survival probabilities 
#' for the PRC-LMM model proposed in Signorelli et al. (2020, in review)
#' 
#' @param step2 the output of \code{\link{summarize_lmms}} 
#' (step 2 of the estimation of the PRC-LMM model)
#' @param step3 the output of \code{\link{fit_prclmm}}
#' (step 3 of the estimation of the PRC-LMM model)
#' @param times numeric vector with the time points at which
#' to estimate the time-dependent AUC
#' 
#' @return A data frame with the predicted survival probabilities
#' computed at the supplied time points
#' 
#' @import foreach doParallel glmnet survival survivalROC survcomp
#' @export
#' 
#' @author Mirko Signorelli
#' @references 
#' Signorelli, M., Spitali, P., Tsonaka, R. (in review). 
#' On the prediction of survival outcomes using longitudinal 
#' and high-dimensional (omic) data.
#' @seealso \code{\link{fit_lmms}} (step 1),
#' \code{\link{summarize_lmms}} (step 2),
#' \code{\link{fit_prclmm}} (step 3)
#' 
#' @examples
#' # generate example data
#' set.seed(1234)
#' p = 4 # number of longitudinal predictors
#' simdata = simulate_prclmm_data(n = 100, p = p, p.relev = 2, 
#'              seed = 123, t.values = c(0, 0.2, 0.5, 1, 1.5, 2))
#'              
#' # step 1 of PRC-LMM: estimate the LMMs
#' y.names = paste('marker', 1:p, sep = '')
#' step1 = fit_lmms(y.names = y.names, 
#'                  fixefs = ~ age, ranefs = ~ age | id, 
#'                  long.data = simdata$long.data, 
#'                  surv.data = simdata$surv.data,
#'                  t.from.base = t.from.base,
#'                  n.boots = 0)
#'                  
#' # step 2 of PRC-LMM: compute the summaries 
#' # of the longitudinal outcomes
#' step2 = summarize_lmms(object = step1)
#' 
#' # step 3 of PRC-LMM: fit the penalized Cox models
#' step3 = fit_prclmm(object = step2, surv.data = simdata$surv.data,
#'                    baseline.covs = ~ baseline.age,
#'                    penalty = 'ridge')
#'                    
#' # predict survival probabilities at times 1, 2, 3
#' surv.probs = survpred_prclmm(step2, step3, times = 1:3)
#' head(surv.probs)

survpred_prclmm = function(step2, step3, times = 1) {
  call = match.call()
  # load namespaces
  requireNamespace('survival')
  requireNamespace('glmnet')
  
  ############################
  ##### CHECK THE INPUTS #####
  ############################
  if (!is.numeric(times)) stop('times should be numeric!')
  n.times = length(times)
  # checks on step 2 input
  temp = c('call', 'ranef.orig', 'n.boots')
  check1 = temp %in% ls(step2)
  mess1 = paste('step2 input should cointain:', do.call(paste, as.list(temp)) )
  if (sum(check1) != 3) stop(mess1)
  ranef.orig = step2$ranef.orig
  # checks on step 3 input
  temp = c('call', 'pcox.orig', 'surv.data', 'n.boots')
  check2 = temp %in% ls(step3)
  mess2 = paste('step2 input should cointain:', do.call(paste, as.list(temp)) )
  if (sum(check2) != 4) stop(mess2)
  baseline.covs = step3$call$baseline.covs
  pcox.orig = step3$pcox.orig
  surv.data = step3$surv.data
  n = length(unique(surv.data$id))
  
  ###############################
  ##### COMPUTE PREDICTIONS #####
  ###############################
  # reconstruct pieces  
  surv.orig = Surv(time = surv.data$time, event = surv.data$event)
  if (is.null(baseline.covs)) {
    X.orig = as.matrix(ranef.orig)
  }
  if (!is.null(baseline.covs)) {
    X0 = model.matrix(as.formula(baseline.covs), data = surv.data)
    X.orig = as.matrix(cbind(X0, ranef.orig))
    contains.int = '(Intercept)' %in% colnames(X.orig)
    if (contains.int) {
      X.orig = X.orig[ , -1] 
    }
  }
  beta.hat = coef(pcox.orig, s = 'lambda.min')
  temp = rownames(beta.hat)
  beta.hat = as.numeric(beta.hat)
  names(beta.hat) = temp
  # check if dimensionality correct
  if (ncol(X.orig) != length(beta.hat)) stop("Dimensions of beta.hat and X.train don't correspond")

  # convert glmnet Cox model to equivalent with survival package
  linpred.orig = X.orig %*% beta.hat
  f1 = as.formula(surv.orig ~ linpred.orig)
  cox.survival = coxph(f1, init = 1, 
     control = coxph.control(iter.max = 0))
  
  # compute survival probabilities
  temp.sfit = survfit(cox.survival,
       newdata = data.frame('linpred.orig' = linpred.orig),
                      se.fit = F, conf.int = F)
  shat.orig = t(summary(temp.sfit, 
                       times = times)$surv)
  out = data.frame(rownames(ranef.orig),
                   shat.orig)
  names(out) = c('id', paste('S(', times, ')', sep = '') )
  return(out)
}
