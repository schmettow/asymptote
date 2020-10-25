## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tidyverse)
# devtools::install_github("schmettow/asymptote")
library(asymptote)

## -----------------------------------------------------------------------------
Laptrain

## -----------------------------------------------------------------------------
Laptrain %>% 
  filter(Part == "01") %>% 
  smoothary(perf = Duration, trial = trial)

## -----------------------------------------------------------------------------
Laptrain %>% 
  filter(Part == "01") %>% 
  coefary(perf = Duration, trial = trial)

## -----------------------------------------------------------------------------
Laptrain %>%
  group_split(Part) %>%
  map_df(smoothary, perf = Duration, trial = trial) %>%
  ggplot(aes(x = trial, y = predict_ARY, group = Part)) +
  geom_point(aes(y = Duration)) +
  geom_smooth(se = F) +
  facet_wrap(~Part)


## -----------------------------------------------------------------------------
Laptrain %>%
  group_split(Part) %>%
  map_df(coefary, perf = Duration, trial = trial) %>% 
  knitr::kable()

