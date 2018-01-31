#' logit
#'
#' computes the logit (or its inverse) on a numerical vector
#'
#' @param x numerical
#' @return numerical
#'
#' @author Martin Schmettow
#' @export


logit <-
  function(x) log(x/(1-x))

#' @rdname logit
#' @export

inv_logit <-
  function(x) plogis(x)
