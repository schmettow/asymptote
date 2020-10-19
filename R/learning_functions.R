library(lazyeval)

#' Learning curve functions and formulas
#'
#' exponential learning curve functions and non-linear regression formulas for use with brms
#'
#' @param init initial performance
#' @param ampl amplitude (init - asym)
#' @param asym asymptote, max performance
#' @param rate rate of learning
#' @param exp previous experience
#' @param trial practice trial, starting at 0
#' @return numeric
#'
#' Computes the expected performance after practice trials using the exponential law of practice.
#' The following parametrizations are provided:
#'
#' \describe{
#' \item{ary}{ampl, rate, asym}
#' \item{ery}{pexp, rate, asym}
#' \item{iry}{init, rate, asym}
#' }
#'
#' In addition, regression formulas are provided for the Bayesian regression engine brms.
#' The right-hand side is always called \emph{perf}ormance.
#' By convention, formulas use upper case names, whereas the functions use lower-case.
#'
#' Furthermore, two-component learning functions are provided for cases where performance is composed of
#' a general skill component \emph{S} (e.g. learning to draw a figure using a mirror)
#' and a motor sequence component \emph{M} (e.g., learning to draw the exact same figure)
#'
#' \describe{
#' \item{arary}{ampl, rate, asym}
#' }
#'
#' All functions and formulas are also available with
#' link functions that put the parameters on a \emph{linear predictor scale} (supported
#' range \eqn{[-\infty; \infty]}) by log transformation. These formnulas/functions have the prefix \emph{L} (e.g., LARA)
#'
#'
#' @examples
#' ARY
#' ary(2, 0.3, 1, 1:5)
#' LARY
#' lary(log(2), log(0.3), log(1), 1:5)
#'
#' @author Martin Schmettow
#' @export

ARY <-
  formula(perf ~ ampl * exp(-rate * trial) + asym)

#' @rdname ARY
#' @export

ary <- function(ampl, rate, asym, trial){
  lazyeval::f_eval_rhs(ARY, data = as.list(environment()))
}

#' @rdname ARY
#' @export

SCOR <-
  formula(perf ~ scale * (exp(-rate * trial) + offset))

#' @rdname ARY
#' @export

scor <- function(scale, offset, rate, trial){
  lazyeval::f_eval_rhs(SCOR, data = as.list(environment()))
}

#' @rdname ARY
#' @export

LSCOR <-
  formula(perf ~ exp(scale) * (exp(-exp(rate) * trial) + exp(offset)))

#' @rdname ARY
#' @export

lscor <- function(scale, offset, rate, trial){
  lazyeval::f_eval_rhs(LSCOR, data = as.list(environment()))
}

#' @rdname ARY
#' @export




LARY <- formula(perf ~ exp(ampl) * exp(-exp(rate) * trial) + exp(asym))

#' @rdname ARY
#' @export

lary <- function(ampl, rate, trial){
  lazyeval::f_eval_rhs(LARY, data = as.list(environment()))
}




IRY <-
  formula(perf ~ (init - asym) * exp(-rate * trial) + asym)

#' @rdname ARY
#' @export

iry <- function(init, rate, asym, trial){
  lazyeval::f_eval_rhs(IRY, data = as.list(environment()))
}


#' @rdname ARY
#' @export

ERY <- formula(perf ~ exp(-rate * (trial + pexp)) + asym)


#' @rdname ARY
#' @export

ery <- function(pexp, rate, asym, trial){
  lazyeval::f_eval_rhs(ERY, data = as.list(environment()))
}


#' @rdname ARY
#' @export

LERY <- formula(perf ~ exp(-exp(rate) * (trial + pexp)) + exp(asym))


#' @rdname ARY
#' @export

lery <- function(pexp, rate, asym, trial){
  lazyeval::f_eval_rhs(LERY, data = as.list(environment()))
}

#' @rdname ARY
#' @export

ARARY <- formula(perf ~ asym + amplS * exp(-rateS * trialS) + amplM * exp(-rateM * trialM))

#' @rdname ARY
#' @export

arary <- function(amplS, rateS, amplM, rateM, asym, trialS, trialM){
  lazyeval::f_eval_rhs(ARARY, data = as.list(environment()))
}


#' @rdname ARY
#' @export

#LARARY <- formula(perf ~ exp(asym) + exp(amplS) * exp(-exp(rateS) * trialS) + exp(amplM) * exp(-exp(rateM) * trialM))
LARARY <- formula(perf ~ exp(amplS - exp(rateS) * trialS) + exp(amplM - exp(rateM) * trialM) + exp(asym))

#' @rdname ARY
#' @export

larary <- function(amplS, rateS, amplM, rateM, asym, trialS, trialM){
  lazyeval::f_eval_rhs(LARARY, data = as.list(environment()))
}




#
# F_irara <-
#   formula(perf ~ amplS * exp(-rateS*trialS) + amplM * exp(-rateM*trialM)+amplS+amplM+init)
#
#
#
#
# F_irara_lp <-
#   formula(perf ~ exp(amplS-(exp(rateS)*trialS)/(exp(rateS)+1))+exp(amplM-(exp(rateM)*trialM)/(exp(rateM)+1))+exp(amplS)+exp(amplM)+exp(init))
#
#
#
# F_ara1 <- formula(perf ~ asym + ampl * exp(-rate * trial))
# F_ara2 <- formula(perf ~ asym + amplS * exp(-rateS * trialS) + amplM * exp(-rateM * trialM))
# F_ara2_lp <-  formula(perf ~ exp(asym) +
#                         exp(amplS - exp(rateS)*trialS/(exp(rateS)+1))+
#                         exp(amplM - exp(rateM)*trialM/(exp(rateS)+1)))
#
#
#
#
#
#
# IRA2_lp <- function(init, rateS, amplS, rateM, amplM, trialS, trialM){
#   lazyeval::f_eval_rhs(F_ira2_lp, data = as.list(environment()))
# }
#
#
# ARA2 <- function(asym, rateS, amplS, rateM, amplM, trialS, trialM){
#   lazyeval::f_eval_rhs(F_ara2, data = as.list(environment()))
# }
#
# ARA2_lp <- function(asym, rateS, amplS, rateM, amplM, trialS, trialM){
#   lazyeval::f_eval_rhs(F_ara2_lp, data = as.list(environment()))
# }
#
#
#
#
#
#
