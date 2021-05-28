#' Step 2 of PRC-LMM (computation of the predicted random effects)
#'
#' This function performs the second step for the estimation
#' of the PRC-LMM model proposed in Signorelli et al. (2021, 
#' in review)
#' 
#' @param object a list of objects as produced by \code{\link{fit_lmms}}
#' @param n.cores number of cores to use to parallelize the computation
#' of the cluster bootstrap optimism correction procedure. If 
#' \code{ncores = 1} (default), no parallelization is done. 
#' Pro tip: you can use \code{parallel::detectCores()} to check 
#' how many cores are available on your computer
#' @param verbose if \code{TRUE} (default and recommended value), information
#' on the ongoing computations is printed in the console
#'  
#' @return A list containing the following objects:
#' \itemize{
#' \item \code{call}: the function call
#' \item \code{ranef.orig}: a matrix with the predicted random effects
#' computed for the original data;
#' \item \code{n.boots}: number of bootstrap samples;
#' \item \code{boot.ids}: a list with the ids of bootstrapped subjects 
#' (when \code{n.boots > 0});
#' \item \code{ranef.boot.train}: a list where each element is a matrix that 
#' contains the predicted random effects for each bootstrap sample 
#' (when \code{n.boots > 0});
#' \item \code{ranef.boot.valid}: a list where each element is a matrix that 
#' contains the predicted random effects on the original data, based on the 
#' lmms fitted on the cluster bootstrap samples (when \code{n.boots > 0});
#' }
#' 
#' @import nlme foreach doParallel stats
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
#' @seealso \code{\link{fit_lmms}} (step 1), 
#' \code{\link{fit_prclmm}} (step 3),
#' \code{\link{performance_prc}}
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
#'                  
#' # step 2 of PRC-LMM: compute the summaries 
#' # of the longitudinal outcomes
#' step2 = summarize_lmms(object = step1, n.cores = n.cores)

summarize_lmms = function(object, n.cores = 1, verbose = TRUE) {
  call = match.call()
  # load namespaces
  requireNamespace('foreach')
  requireNamespace('doParallel')
  # fix for 'no visible binding for global variable...' note
  j = i = numeric.id = b = NULL
  
  # checks on step 1 output + retrieve info from it
  if (!is.list(object)) stop('object should be a list, as outputted from fit_lmms')
  minimal.elements = c('call.info', 'lmm.fits.orig', 'df.sanitized', 'n.boots')
  check1 = minimal.elements %in% ls(object)
  mess = paste('At least one of the following elements is missing in object:',
               paste(minimal.elements, collapse = ', '))
  if (!all(check1, TRUE)) stop(mess)
  data = object$df.sanitized
  y.names = object$call.info$y.names
  fixefs = object$call.info$fixefs
  ranefs = object$call.info$ranefs
  p = length(y.names)
  n.boots = object$n.boots
  if (n.boots < 0) {
    warning('Input n.boots < 0, so we set n.boots = 0', immediate. = TRUE)
    n.boots = 0
  }
  if (n.boots > 0) {
    extra.inputs = c('boot.ids', 'lmm.fits.boot')
    check2 = extra.inputs %in% ls(object)
    mess = paste('At least one of the following elements is missing in object:',
                 paste(extra.inputs, collapse = ', '))
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
  
  ########################
  ### original dataset ###
  ########################
  if (verbose) cat('Computing the predicted random effects on the original dataset...\n')
  # create matrix with predicted random effects computed on original dataset
  lmms = object$lmm.fits.orig
  ranef.orig = foreach(j = 1:p, .combine = 'cbind') %do% {
    x = ranef(lmms[[j]])
    # fix column names
    is.inter = (names(x) == '(Intercept)')
    if (sum(is.inter) > 0) names(x)[which(is.inter == 1)] = 'int'
    names(x) = paste(y.names[j], 'b', names(x), sep = '_')
    # return
    x
  }
  if (verbose) cat('...done\n')
  
  #######################
  ### start bootstrap ###
  #######################
  if (n.boots >= 1) {
    if (verbose) cat('Bootstrap procedure started\n')
    mess = ifelse(n.cores >=2, 
                  paste('in parallel, using', n.cores, 'cores'),
                  'using a single core')
    if (verbose) cat(paste('This computation will be run', mess, '\n'))
    # retrieve bootstrap ids
    boot.ids = object$boot.ids
    # set up environment for parallel computing
    cl = parallel::makeCluster(n.cores)
    doParallel::registerDoParallel(cl)
    
    # compute predicted ranefs for each bootstrap sample (training set)
    # and for the original dataset (validation set)
    
    # training set
    ranef.boot.train = foreach(b = 1:n.boots,
        .packages = c('foreach', 'pencal', 'nlme')) %dopar% {
      ranef.train = foreach(j = 1:p, .combine = 'cbind') %do% {
        x = ranef(object$lmm.fits.boot[[b]][[j]])
        # fix column names
        is.inter = (names(x) == '(Intercept)')
        if (sum(is.inter) > 0) names(x)[which(is.inter == 1)] = 'int'
        names(x) = paste(y.names[j], 'b', names(x), sep = '_')
        # return
        x
      }
      # return
      ranef.train
    }
    
    # validation set
    ranef.boot.valid = foreach(b = 1:n.boots,
              .packages = c('foreach', 'pencal', 'nlme')) %dopar% {
      ids = unique(data$id)
      n = length(ids)
      
      # for each response, derive predicted ranefs for that given bootstrap sample
      ranef.b = foreach(j = 1:p, .combine = 'cbind') %do% {
        # check if NAs on response
        new.df = data
        y = new.df[ , y.names[j]]
        nas = which(is.na(y))
        if (length(nas) > 0) {
          new.df = new.df[-nas, ]
          y = new.df[ , y.names[j]]
        }
        # retrieve the right pieces from lmm
        lmms = object$lmm.fits.boot[[b]]
        D.hat = getVarCov(lmms[[j]], type = 'random.effects')
        beta.hat = fixef(lmms[[j]])
        sigma2.hat = summary(lmms[[j]])$sigma^2 # error variance
        # create X and Z
        X = model.matrix(as.formula(fixefs), data = new.df)
        formYz = formula(lmms[[j]]$modelStruct$reStruct[[1]]) 
        mfZ = model.frame(terms(formYz), data = new.df)
        Z = model.matrix(formYz, mfZ)
        #####
        current = foreach(i = 1:n, .combine = 'rbind') %do% {
          rows = which(new.df$id == ids[i])
          Xi = X[rows, , drop = FALSE] 
          # drop = F prevents conversion to vector when rows has length 1
          yi = y[rows]
          Zi = Z[rows, , drop = FALSE]
          I.matr = diag(1, length(rows), length(rows))
          Vi = Zi %*% D.hat %*% t(Zi) + sigma2.hat * I.matr
          temp = yi - Xi %*% beta.hat
          t(D.hat %*% t(Zi) %*% solve(Vi) %*% temp)
        }
        # return
        current
      }
      rownames(ranef.b) = ids
      colnames(ranef.b) = names(ranef.boot.train[[b]])
      ranef.b
    }
    # close the cluster
    parallel::stopCluster(cl)
    if (verbose) {
      cat('Bootstrap procedure finished\n')
      cat('Computation of step 2: finished :)\n')
    }
  }
  
  out = list('call' = call, 'ranef.orig' = ranef.orig, 
             'n.boots' = n.boots)
  if (n.boots >= 1) {
    out[['boot.ids']] = boot.ids
    out[['ranef.boot.train']] = ranef.boot.train
    out[['ranef.boot.valid']] = ranef.boot.valid
  }
  return(out)
}
