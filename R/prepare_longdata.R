#' Prepare longitudinal data for PRC
#'
#' This function removes from a longitudinal dataframe
#' all measurements taken after the occurence of the event 
#' or after censoring. It is used internally by \code{\link{fit_lmms}}
#' and it assumes that \code{df} is sorted by \code{subj.id},
#' with survival times given in the same order by subject id
#' (\code{fit_lmms} automatically performs this sorting when
#' needed)
#' 
#' @param df dataframe with the longitudinal measurements
#' @param subj.id name of the subject id variable in \code{df}
#' @param t.from.base name of the variable containing time from 
#' baseline in \code{df}
#' @param survtime vector containing the survival time or censoring time
#' @param verbose if \code{TRUE}, a summary of the data manipulation
#' is printed
#'
#' @return A list containing: a reduced dataframe called \code{df.sanitized}, 
#' where only measurements taken before \code{t} are retained; the number of
#' measurements retained (\code{n.kept}) and removed (\code{n.removed})
#' from the input data frame
#' 
#' @keywords internal
#' 
#' @author Mirko Signorelli
#' @references 
#' Signorelli, M., Spitali, P., Tsonaka, R. (in review). 
#' On the prediction of survival outcomes using longitudinal 
#' and high-dimensional (omic) data.

prepare_longdata = function(df, t.from.base, subj.id, survtime, verbose = TRUE) {
  id = df[ , deparse(substitute(subj.id))]
  time = df[ , deparse(substitute(t.from.base))]
  id.vals = unique(id)
  n = length(id.vals)
  keep = c()
  for (i in 1:n) {
    rows = which(id == id.vals[i])
    time.i = time[rows]
    keep.i = which(time.i <= survtime[i])
    if (length(keep.i) < 1) {
      err = paste('No measurements before event for subject', id.vals[i])
      stop(err)
    }
    keep.i = rows[keep.i]
    keep = c(keep, keep.i)
  }
  n1 = nrow(df)
  n2 = length(keep)
  if (verbose) {
    n1 = nrow(df)
    n2 = length(keep)
    mess1 = 'Preliminary step: remove measurements taken after event / censoring.\n'
    mess2 = paste('Removed:', n1-n2, 'measurements. Retained:', 
                 n2, 'measurements.\n')
    if (verbose) {
      cat(mess1)
      cat(mess2)
    }
  }  
  out = list('df.sanitized' = df[keep, ],
             'n.kept' = n2, 'n.removed' = n1-n2)
  return(out)
}
