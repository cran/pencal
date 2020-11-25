#' Step 1 of PRC-LMM: fits one linear mixed model for each response
#'
#' This function performs the first step for the estimation
#' of the PRC-LMM model proposed in Signorelli et al. (2020, 
#' in review)
#' 
#' @param y.names character vector with the names of the
#' response variables which the LMMs have to be fitted to
#' @param fixefs fixed effects formula for the model, example:
#' \code{~ time} (NB: do not include left-hand side!)
#' @param ranefs random effects formula for the model,
#' specified using the representation of random effect
#' structures of the \code{R} package \code{nlme}
#' @param long.data a data frame with the longitudinal predictors,
#' comprehensive of a variable called \code{id} with the subject 
#' ids, and of a variable measuring time from baseline
#' @param surv.data a data frame with the survival data and (if 
#' relevant) additional baseline, which should at least contain
#' a subject id (called \code{id}), the time to event outcome  
#' (\code{time}), and binary event variable (\code{event})
#' @param t.from.base name of the variable containing time from 
#' baseline in \code{long.data}
#' @param n.boots number of bootstrap samples; if 0, the random 
#' effects are computed only for the original dataset and no
#' bootstrapping is performed
#' @param n.cores number of cores to use to parallelize the computation
#' of the cluster bootstrap procedure. If \code{ncores = 1} (default), 
#' no parallelization is done. Tip: you can use \code{parallel::detectCores()}
#' to check how many cores are available on your computer
#' @param verbose if \code{TRUE} (default and recommended value), information
#' on the ongoing computations is printed in the console
#' 
#' @return A list containing the following objects:
#' \itemize{
#' \item \code{call.info}: a list containing the following function
#' call information: \code{call}, \code{y.names}, \code{fixefs},
#' \code{ranefs};
#' \item \code{lmm.fits.orig}: a list with the LMMs fitted on the
#' original dataset (it should comprise as many LMMs as the elements
#' of \code{y.names} are);
#' \item \code{df.sanitized}: a sanitized version of the supplied 
#' \code{long.data} dataframe, without the
#' longitudinal measurements that are taken after the event
#' or after censoring;
#' \item \code{n.boots} number of bootstrap samples;
#' \item \code{boot.ids} ids of bootstrapped subjects (when \code{n.boots > 0});
#' \item \code{lmms.fits.boot}: a list of lists, with the LMMs fitted 
#' on eacj bootstrapped datasets (when \code{n.boots > 0}).
#' }
#' 
#' @import nlme foreach doParallel stats
#' @importFrom dplyr arrange
#' @export
#' 
#' @author Mirko Signorelli
#' @references 
#' Signorelli, M., Spitali, P., Tsonaka, R. (in review). 
#' On the prediction of survival outcomes using longitudinal 
#' and high-dimensional (omic) data.
#' @seealso \code{\link{simulate_prclmm_data}},
#' \code{\link{summarize_lmms}} (step 2),
#' \code{\link{fit_prclmm}} (step 3),
#' \code{\link{performance_prclmm}}
#' 
#' @examples
#' # generate example data
#' set.seed(1234)
#' p = 4 # number of longitudinal predictors
#' simdata = simulate_prclmm_data(n = 100, p = p, p.relev = 2, 
#'              seed = 123, t.values = c(0, 0.2, 0.5, 1, 1.5, 2))
#'  
#' # specify options for cluster bootstrap optimism correction
#' # procedure and for parallel computing 
#' do.bootstrap = FALSE
#' # IMPORTANT: set do.bootstrap = TRUE to compute the optimism correction!
#' n.boots = ifelse(do.bootstrap, 100, 0)
#' parallelize = FALSE
#' # IMPORTANT: set parallelize = TRUE to speed computations up!
#' if (!parallelize) n.cores = 1
#' if (parallelize) {
#'    # identify number of available cores on your machine
#'    n.cores = parallel::detectCores()
#'    if (is.na(n.cores)) n.cores = 1
#' }
#' 
#' # step 1 of PRC-LMM: estimate the LMMs
#' y.names = paste('marker', 1:p, sep = '')
#' step1 = fit_lmms(y.names = y.names, 
#'                  fixefs = ~ age, ranefs = ~ age | id, 
#'                  long.data = simdata$long.data, 
#'                  surv.data = simdata$surv.data,
#'                  t.from.base = t.from.base,
#'                  n.boots = n.boots, n.cores = n.cores)

fit_lmms = function(y.names, fixefs, ranefs, long.data, 
                    surv.data, t.from.base, n.boots = 0, 
                    n.cores = 1, verbose = TRUE) {
  call = match.call()
  # load namespaces
  requireNamespace('nlme')
  requireNamespace('foreach')
  requireNamespace('doParallel')
  # fix for 'no visible binding for global variable...' note
  id = i = numeric.id = b = NULL
  # setup and checks
  p = length(y.names)
  if (n.boots < 0) {
    warning('Input n.boots < 0, so we set n.boots = 0', immediate. = TRUE)
    n.boots = 0
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
  
  # get longitudinal predictors
  check1 = is.data.frame(long.data)
  if (!check1) stop('long.data should be a dataframe')
  check2 = c('id') %in% names(long.data)
  if (sum(check2) != 1) stop("long.data should contain an 'id' variable with subject ids")
  if (verbose) cat('Sorting long.data by subject id\n')
  long.data = dplyr::arrange(long.data, id)
  # get survival data
  check2 = is.data.frame(surv.data)
  if (!check2) stop('surv.data should be a dataframe')
  check3 = c('id', 'time', 'event') %in% names(surv.data)
  if (sum(check3) != 3) stop("surv.data should contain at least: 'id', 'time', 'event' variables")
  if (verbose) cat('Sorting surv.data by subject id\n')
  surv.data = dplyr::arrange(surv.data, id)
  # check that id values are the same in the two datasets!
  temp1 = as.character(unique(long.data$id))
  temp2 = as.character(unique(surv.data$id))
  check4 = identical(temp1, temp2)
  if (!check4) stop('id values are different in long.data and surv.data')  
  # remove all longitudinal measurements after t
  temp = prepare_longdata(df = long.data, subj.id = id, 
            t.from.base = t.from.base, survtime = surv.data$time, 
            verbose = verbose)
  df = temp$df.sanitized
  # add to df a numeric.id variable to simplify the bootstrapping
  df$numeric.id = as.numeric(as.factor(df$id))
  
  ########################
  ### original dataset ###
  ########################
  # fit the LMMs
  if (verbose) cat('Estimating the LMMs on the original dataset...\n')
  fit.orig = foreach (i = 1:p) %do% {
    # check if NAs on response
    n1 = length(unique(df$numeric.id))
    nas = which(is.na(df[ , y.names[i]]))
    df.sub = df
    if (length(nas) > 0) df.sub = df[-nas, ]
    n2 = length(unique(df.sub$numeric.id))
    if (n1 != n2) {
      mess = paste('There is at least one subject without any information available for variable',
                   y.names[i], '- double-check your input long.data!')
      stop()
    }
    # fit LMM
    fixef.formula = as.formula(paste(y.names[i], deparse(fixefs)))
    ranef.formula = as.formula(deparse(ranefs))
    lmm = try( nlme::lme(fixed = fixef.formula, 
                    random = ranef.formula, data = df.sub),
                silent = TRUE)
    if (inherits(lmm, 'try-error')) { # retry with increased max number of iterations
      lmm = try( nlme::lme(fixed = fixef.formula, 
                     random = ranef.formula, data = df.sub,
                     control = list(maxIter = 1e4, msMaxIter = 1e3,
                                    niterEM = 1e3, msMaxEval = 1e3)),
                 silent = TRUE)
    }
    if (inherits(lmm, 'try-error')) {
      mess = paste('The desired model for response', y.names[i],
                   'did not converge. A simpler model comprising only a random
                   intercept was fitted for', y.names[i])
      warning(mess, immediate. = TRUE)
      lmm = try( nlme::lme(fixed = fixef.formula, 
                           random = ~ 1 | id, data = df.sub,
                           control = list(maxIter = 1e4, msMaxIter = 1e3,
                                          niterEM = 1e3, msMaxEval = 1e3)),
                 silent = TRUE)
    }
    if (inherits(temp, 'try-error')) {
      stop(paste('no model could be fitted for response', y.names[i]))
    }
    lmm
  }
  names(fit.orig) = y.names
  if (verbose) cat('...done\n')
  
  #######################
  ### start bootstrap ###
  #######################
  if (n.boots >= 1) {
    if (verbose) cat('Bootstrap procedure started\n')
    mess = ifelse(n.cores >=2, paste('in parallel, using', n.cores, 'cores'),
                  'using a single core')
    if (verbose) cat(paste('This computation will be run', mess, '\n'))
    # draw n bootstrap samples
    ids = unique(df$numeric.id)
    n = length(ids)
    boot.ids = foreach(i = 1:n.boots) %do% {
      set.seed(i)
      sort(sample(ids, n, TRUE))
    }
    # set up environment for parallel computing
    cl = parallel::makeCluster(n.cores)
    doParallel::registerDoParallel(cl)
    
    # compute fitted LMMS for each bootstrap sample (in parallel)
    fit.boots = foreach(b = 1:n.boots,
    .packages = c('foreach', 'pencal', 'nlme')) %dopar% {
      
      # bootstrap the longitudinal dataset for variable j
      boot.df = draw_cluster_bootstrap(df = df,
                        idvar = numeric.id, boot.ids = boot.ids[[b]])
      # important: overwrite id (that has repetitions) with new.id
      boot.df$id = boot.df$new.id
      
      # for each response, derive predicted ranefs for that given bootstrap sample
      this.fit = foreach(i = 1:p) %do% {
        # check if NAs on response
        nas = which(is.na(boot.df[ , y.names[i]]))
        df.sub = boot.df
        if (length(nas) > 0) df.sub = boot.df[-nas, ]
        # fit LMM
        fixef.formula = as.formula(paste(y.names[i], deparse(fixefs)))
        ranef.formula = as.formula(deparse(ranefs))
        lmm = try( nlme::lme(fixed = fixef.formula, 
                              random = ranef.formula, data = df.sub),
                    silent = TRUE)
        if (inherits(lmm, 'try-error')) { # retry with increased max number of iterations
          lmm = try( nlme::lme(fixed = fixef.formula, 
                               random = ranef.formula, data = df.sub,
                               control = list(maxIter = 1e4, msMaxIter = 1e3,
                                            niterEM = 1e3, msMaxEval = 1e3)),
                     silent = TRUE)
        }
        if (inherits(lmm, 'try-error')) {
          lmm = try( nlme::lme(fixed = fixef.formula, 
                               random = ~ 1 | id, data = df.sub,
                               control = list(maxIter = 1e4, msMaxIter = 1e3,
                                            niterEM = 1e3, msMaxEval = 1e3)),
                     silent = TRUE)
        }
        if (inherits(lmm, 'try-error')) {
          warning(paste('bootstrap sample', b,
            '= the LMM could be fitted for response', y.names[i]), 
            immediate. = TRUE)
        }
        lmm
      }
      # all proteins done for each b
      names(this.fit) = y.names
      this.fit
    }
    # close the cluster
    parallel::stopCluster(cl)
    if (verbose) {
      cat('Bootstrap procedure finished\n')
      cat('Computation of step 1: finished :)\n')
    }
  }
  
  
  call.info = list('call' = call, 'y.names' = y.names,
                   'fixefs' = fixefs, 'ranefs' = ranefs)
  out = list('call.info' = call.info, 'lmm.fits.orig' = fit.orig, 
             'df.sanitized' = df, 'n.boots' = n.boots)
  if (n.boots >= 1) {
    out[['boot.ids']] = boot.ids
    out[['lmm.fits.boot']] = fit.boots
  }
  return(out)
}
