#' Predictive performance of the penalized Cox model
#' with baseline covariates
#'
#' This function computes the naive and optimism-corrected
#' measures of performance (C index and time-dependent AUC) 
#' for a penalized Cox model with baseline covariates as
#' presented in Signorelli et al. (2021, in review). 
#' The optimism correction is a bootstrap
#' optimism correction procedure
#' 
#' @param fitted_pencox the output of \code{\link{pencox_baseline}}
#' @param times numeric vector with the time points at which
#' to estimate the time-dependent AUC
#' @param n.cores number of cores to use to parallelize the computation
#' of the CBOCP procedure. If \code{ncores = 1} (default), 
#' no parallelization is done. Pro tip: you can use 
#' \code{parallel::detectCores()} to check how many 
#' cores are available on your computer
#' @param verbose if \code{TRUE} (default and recommended value), information
#' on the ongoing computations is printed in the console
#' 
#' @return A list containing the following objects:
#' \itemize{
#' \item \code{call}: the function call;
#' \item \code{concordance}: a data frame with the naive and
#' optimism-corrected estimates of the concordance (C) index;
#' \item \code{tdAUC}: a data frame with the naive and
#' optimism-corrected estimates of the time-dependent AUC
#' at the desired time points.
#' }
#' 
#' @import foreach doParallel glmnet survival survivalROC survcomp
#' @export
#' 
#' @author Mirko Signorelli
#' @references 
#' Signorelli, M., Spitali, P., Al-Khalili Szigyarto, C, 
#' The MARK-MD Consortium, Tsonaka, R. (2021). 
#' Penalized regression calibration: a method for the prediction 
#' of survival outcomes using complex longitudinal and 
#' high-dimensional data. arXiv preprint: arXiv:2101.04426.
#' 
#' @seealso \code{\link{pencox_baseline}}
#' 
#' @examples
#' # generate example data
#' set.seed(1234)
#' p = 4 # number of longitudinal predictors
#' simdata = simulate_prclmm_data(n = 100, p = p, p.relev = 2, 
#'              seed = 123, t.values = c(0, 0.2, 0.5, 1, 1.5, 2))
#' # create dataframe with baseline measurements only
#' baseline.visits = simdata$long.data[which(!duplicated(simdata$long.data$id)),]
#' df = cbind(simdata$surv.data, baseline.visits)
#' df = df[ , -c(5:7)]
#' 
#' do.bootstrap = FALSE
#' # IMPORTANT: set do.bootstrap = TRUE to compute the optimism correction!
#' n.boots = ifelse(do.bootstrap, 100, 0)
#' parallelize = FALSE
#' # IMPORTANT: set parallelize = TRUE to speed computations up!
#' if (!parallelize) n.cores = 1
#' if (parallelize) {
#'   # identify number of available cores on your machine
#'   n.cores = parallel::detectCores()
#'   if (is.na(n.cores)) n.cores = 1
#' }
#' 
#' form = as.formula(~ baseline.age + marker1 + marker2
#'                      + marker3 + marker4)
#' base.pcox = pencox_baseline(data = df, 
#'               formula = form, 
#'               n.boots = n.boots, n.cores = n.cores) 
#' ls(base.pcox)
#'                    
#' # compute the performance measures
#' perf = performance_pencox_baseline(fitted_pencox = base.pcox, 
#'           times = c(0.5, 1, 1.5, 2), n.cores = n.cores)
#' 
#' # concordance index:
#' perf$concordance
#' # time-dependent AUC:
#' perf$tdAUC

performance_pencox_baseline = function(fitted_pencox, times = 1,
                              n.cores = 1, verbose = TRUE) {
  call = match.call()
  # load namespaces
  requireNamespace('survival')
  requireNamespace('survcomp')
  requireNamespace('survivalROC')
  requireNamespace('glmnet')
  requireNamespace('foreach')
  # fix for 'no visible binding for global variable...' note
  i = b = NULL
  
  ############################
  ##### CHECK THE INPUTS #####
  ############################
  if (!is.numeric(times)) stop('times should be numeric!')
  n.times = length(times)
  c.out = data.frame(n.boots = NA, naive = NA,
                    cb.correction = NA, cb.performance = NA)
  tdauc.out = data.frame(pred.time = times, naive = NA,
                         cb.correction = NA, cb.performance = NA)
  # checks on fitted_pencox
  temp = c('call', 'pcox.orig', 'surv.data', 'X.orig', 'n.boots')
  check2 = temp %in% ls(fitted_pencox)
  mess2 = paste('fitted_pencox input should cointain:', 
                do.call(paste, as.list(temp)) )
  if (sum(check2) != 5) stop(mess2)
  X.orig = fitted_pencox$X.orig
  pcox.orig = fitted_pencox$pcox.orig
  surv.data = fitted_pencox$surv.data
  n = length(unique(surv.data$id))
  n.boots = fitted_pencox$n.boots
  # further checks
  if (n.boots == 0) {
    mess = paste('The bootstrap optimism correction has not',
            'been performed (n.boots = 0). Therefore, only the apparent',
            'values of the performance values will be returned.')
    warning(mess, immediate. = TRUE)
  }
  if (n.boots > 0) {
    temp = c('boot.ids', 'pcox.boot')
    check3 = temp %in% ls(fitted_pencox)
    mess3 = paste('fitted_pencox should cointain:', do.call(paste, as.list(temp)) )
    if (sum(check3) != 2) stop(mess3)
    boot.ids = fitted_pencox$boot.ids
    pcox.boot = fitted_pencox$pcox.boot
  }
  if (n.cores < 1) {
    warning('Input n.cores < 1, so we set n.cores = 1', immediate. = TRUE)
    n.cores = 1
  }
  # check how many cores are actually available for this computation
  if (n.boots > 0) {
    max.cores = parallel::detectCores()
    if (!is.na(max.cores)) {
      diff = max.cores - n.cores
      mess0 = paste('You requested', n.cores, 'cores for this computation.')
      mess1 = paste(mess0, 'It seems that your computer actually has',
                    max.cores, 'cores available.',
                    'Consider increasing n.cores accordingly to speed computations up! =)')
      if (diff > 0) warning(mess1, immediate. = TRUE)
      mess2 = paste(mess0, 'However, seems that your computer only has',
                    max.cores, 'cores available.',
                    'Therefore, most likely computations will be performed using only', 
                    max.cores, 'cores. =(')
      if (diff < 0)  warning(mess2, immediate. = TRUE)
    }
  }
  
  #############################
  ##### NAIVE PERFORMANCE #####
  #############################
  surv.orig = Surv(time = surv.data$time, event = surv.data$event)
  
  # C index on the original dataset
  relrisk.orig = predict(pcox.orig, 
                         newx = X.orig, 
                         s = 'lambda.min',
                         type = 'response')  
  c.naive = concordance.index(x = relrisk.orig, 
                    surv.time = surv.data$time,
                    surv.event = surv.data$event, 
                    method = "noether")
  c.out$n.boots = n.boots
  check = !inherits(c.naive, 'try-error')
  c.out$naive = ifelse (check, round(c.naive$c.index, 4), NA)
  
  # time-dependent AUC
  pmle.orig = as.numeric(coef(pcox.orig, s = 'lambda.min'))
  linpred.orig = X.orig %*% pmle.orig
  tdauc.naive = foreach(i = 1:n.times, .combine = 'c') %do% {
    auc = try(survivalROC(Stime = surv.data$time, 
                          status = surv.data$event, 
                          marker = linpred.orig, 
                          entry = rep(0, n), 
                          predict.time = times[i], 
                          cut.values = NULL,
                          method = "NNE", 
                          span = 0.25*n^(-0.20)))
    check = !inherits(auc, 'try-error') & !is.nan(auc$AUC)
    out = ifelse(check, round(auc$AUC, 4), NA)
  }
  tdauc.out$naive = tdauc.naive

  ###############################
  ##### OPTIMISM CORRECTION #####
  ###############################
  if (n.boots > 0) {
    if (verbose) cat('Computation of optimism correction started\n')
    mess = ifelse(n.cores >=2, paste('in parallel, using', n.cores, 'cores'),
                  'using a single core')
    if (verbose) cat(paste('This computation will be run', mess, '\n'))
    # set up environment for parallel computing
    cl = parallel::makeCluster(n.cores)
    doParallel::registerDoParallel(cl)
    
    booty = foreach(b = 1:n.boots, .combine = 'rbind',
       .packages = c('survival', 'survcomp', 'survivalROC',
                     'glmnet', 'foreach')) %dopar% {
      # prepare data for the training set
      surv.data.train = surv.data[boot.ids[[b]], ]
      X.train = X.orig[boot.ids[[b]], ]
      X.valid = X.orig
      
      # C index on boot.train
      relrisk.train = predict(pcox.boot[[b]], 
                              newx = X.train, 
                              s = 'lambda.min',
                              type = 'response')  
      c.train = concordance.index(x = relrisk.train, 
                                  surv.time = surv.data.train$time,
                                  surv.event = surv.data.train$event, 
                                  method = "noether")
      
      # C index on boot.valid
      relrisk.valid = predict(pcox.boot[[b]], 
                              newx = X.valid, 
                              s = 'lambda.min',
                              type = 'response')  
      c.valid = concordance.index(x = relrisk.valid, 
                                  surv.time = surv.data$time,
                                  surv.event = surv.data$event, 
                                  method = "noether")
      
      # tdAUC on boot.train
      pmle.train = as.numeric(coef(pcox.boot[[b]], s = 'lambda.min'))
      linpred.train = X.train %*% pmle.train
      tdauc.train = foreach(i = 1:n.times, .combine = 'c') %do% {
        auc = try(survivalROC(Stime = surv.data.train$time, 
                              status = surv.data.train$event, 
                              marker = linpred.train, 
                              entry = rep(0, n), 
                              predict.time = times[i], 
                              cut.values = NULL,
                              method = "NNE", 
                              span = 0.25*n^(-0.20)))
        check = !inherits(auc, 'try-error') & !is.nan(auc$AUC)
        out = ifelse(check, round(auc$AUC, 4), NA)
      }
      
      # tdAUC on boot.valid
      pmle.train = as.numeric(coef(pcox.boot[[b]], s = 'lambda.min'))
      # important: the pmle comes from the model fitted on the training set
      linpred.valid = X.valid %*% pmle.train
      tdauc.valid = foreach(i = 1:n.times, .combine = 'c') %do% {
        auc = try(survivalROC(Stime = surv.data$time, 
                              status = surv.data$event, 
                              marker = linpred.valid, 
                              entry = rep(0, n), 
                              predict.time = times[i], 
                              cut.values = NULL,
                              method = "NNE", 
                              span = 0.25*n^(-0.20)))
        check = !inherits(auc, 'try-error') & !is.nan(auc$AUC)
        out = ifelse(check, round(auc$AUC, 4), NA)
      }
      
      # define outputs of parallel computing
      check1 = !inherits(c.train, 'try-error')
      ct = ifelse (check1, round(c.train$c.index, 4), NA)
      check2 = !inherits(c.valid, 'try-error')
      cv = ifelse (check2, round(c.valid$c.index, 4), NA)
      out = data.frame(repl = NA, stat = NA, times = NA, train = NA, valid = NA)
      out[1, ] = c(b, NA, NA, ct, cv)
      out[2:(n.times + 1),] = cbind(b, NA, times, tdauc.train, tdauc.valid)
      out$stat = c('C', rep('tdAUC', n.times))
      out$optimism = out$valid - out$train
      return(out)
    }
    
    # close the cluster
    parallel::stopCluster(cl)
    
    # compute the optimism correction for the C index
    c.vals = booty[booty$stat == 'C', ]
    c.opt = mean(c.vals$optimism, na.rm = TRUE)
    c.out$cb.correction = round(c.opt, 4)
    c.out$cb.performance = c.out$naive + c.out$cb.correction
    
    # compute the optimism correction for the tdAUC
    tdauc.vals = booty[booty$stat == 'tdAUC', ]
    tdauc.opt = foreach(i = 1:n.times, .combine = 'c') %do% {
      temp = tdauc.vals[tdauc.vals$times == times[i], ]
      out = mean(temp$optimism, na.rm = TRUE)
      return(out)
    }
    tdauc.out$cb.correction = round(tdauc.opt, 4)
    tdauc.out$cb.performance = tdauc.out$naive + tdauc.out$cb.correction
    # closing message
    if (verbose) {
      cat('Computation of the optimism correction: finished :)\n')
    }
  }

  names(c.out) = c('n.boots', 'C.naive', 'cb.opt.corr', 'C.adjusted')
  names(tdauc.out) = c('pred.time', 'tdAUC.naive', 'cb.opt.corr', 'tdAUC.adjusted')
  
  out = list('call' = call, 'concordance' = c.out, 
             'tdAUC' = tdauc.out)
  return(out)
}
