#' Smoother function
#'
#' quickly estimates predicted values of an ARY model
#'
#' @param  perf
#' @param  trial
#' @return fitted values
#'
#' Use nls() to estimate an ARY model and returns fitted values.
#' With grouped mutation one can easily estimate a bunch of curves.
#'
#'
#' @author Martin Schmettow
#' @usage smoothary(Laptrain$ToT, Laptrain$trial)
#'
#' @export

smoothary <- function(x, perf, trial){
  perf <- enquo(perf)
  trial <- enquo(trial)
  data = x %>%
    select(perf = !!perf,
           trial = !!trial)
  model <-
    nls(asymptote::ARY,
        start = list(ampl = 6, rate = .5, asym = 2.5),
        data = data)
  x %>%
    mutate(predict_ARY = predict(model))
}


# Laptrain %>%
#   group_split(Part) %>%
#   map_df(smoothary, perf = Duration, trial = trial) %>%
#   ggplot(aes(x = trial, y = predict_ARY, group = Part)) +
#   geom_smooth(se = F)


#' Quick estimation
#'
#' quickly estimates predicted values of an ARY model
#'
#' @param  perf
#' @param  trial
#' @return fitted values
#'
#' Use nls() to estimate an ARY model and returns coefficients.
#' Trial counter and performance variable are given as vectors.
#' This makes smoothary useful in conjunction with manipulation chains.
#' With grouped mutation one can easily estimate a bunch of curves.
#'
#'
#' @author Martin Schmettow
#' @usage smoothary(Laptrain$ToT, Laptrain$trial)
#'
#' @export
coefary <- function(x, perf, trial){
  perf <- enquo(perf)
  trial <- enquo(trial)
  data = x %>%
    select(perf = !!perf,
           trial = !!trial)
  model <-
    nls(asymptote::ARY,
        start = list(ampl = 6, rate = .5, asym = 2.5),
        data = data)
  coef(model)
}

coefery <- function(x, perf, trial){
  perf <- enquo(perf)
  trial <- enquo(trial)
  data = x %>%
    select(perf = !!perf,
           trial = !!trial)
  model <-
    nls(asymptote::ERY,
        start = list(pexp = 0.1, rate = .5, asym = 2.5),
        data = data)
  coef(model)
}





