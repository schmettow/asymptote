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
#' \item{ary}{amplitude, rate, asymptote}
#' \item{ira}{initial performance, rate, amplitude}
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
#' \item{arary}{amplitude_S, rate_S, amplitude_M, rate_M, asymptote}
#' }
#'
#' All functions and formulas are also available with
#' link functions that put the parameters on a \emph{linear predictor scale, _lp} with a supported
#' range \eqn{[-\infty; \infty]}:
#'
#' \describe{
#' \item{init, ampl, asym}{exp() <-> log()}
#' \item{rate}{inv_logit() <-> logit()}
#' }

#'
#' @examples
#' ARY
#' ary(2, 0.3, 1, 1:5)
#'
#' @author Martin Schmettow
#' @export

ARY <-
  formula(perf ~ asym + ampl * exp(-rate * trial))

#' @rdname ARY
#' @export

ary <- function(ampl, rate, asym, trial){
  f_eval_rhs(ARY, data = as.list(environment()))
}

#' @rdname ARY
#' @export

IRA <-
  formula(perf ~ init - ampl * exp(-rate * trial))

#' @rdname ARY
#' @export

ira <- function(IRA, rate, ampl, trial){
  f_eval_rhs(F_ara1, data = as.list(environment()))
}

#' @rdname ARY
#' @export

ARARY <- formula(perf ~ asym + amplS * exp(-rateS * trialS) + amplM * exp(-rateM * trialM))

#' @rdname ARY
#' @export

arary <- function(amplS, rateS, amplM, rateM, asym, trialS, trialM){
  f_eval_rhs(ARARY, data = as.list(environment()))
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
