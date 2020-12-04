#' A fitted PRC LMM
#'
#' This list contains a fitted PRC LMM, where the CBOCP is
#' computed using 50 cluster bootstrap samples. It is
#' used to reduce the computing time in the example of
#' the function \code{performance_prclmm} 
#' 
#' @docType data
#' @keywords datasets
#' @usage data(fitted_prclmm)
#'
#' @format A list comprising step 2 and step 3 as obtained
#' during the estimation of the PRC LMM
#' 
#' @author Mirko Signorelli
#' @references 
#' Signorelli, M., Spitali, P., Tsonaka, R. (in review). 
#' On the prediction of survival outcomes using longitudinal 
#' and high-dimensional (omic) data.
#' @seealso \code{\link{performance_prclmm}}
#' 
#' @examples 
#' data(fitted_prclmm)
#' ls(fitted_prclmm)
"fitted_prclmm"