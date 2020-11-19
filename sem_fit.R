## SEM relating species niche characteristics to range shifts

library(lavaan)
library(piecewiseSEM)
library(tidyverse)

## Read in and join niche breadth and range shifts data

niche_breadth <- read.csv("species_niche_breadths.csv", stringsAsFactors = F)

range_dyn <- read.csv("species_range_dynamics.csv", stringsAsFactors = F)

spp_data <- niche_breadth %>%
  left_join(range_dyn, by = c("aou" = "AOU")) %>%
  dplyr::select(aou, Species.Name, N.Routes, Diet, Tol, Trend, occ_change) %>%
  na.omit()
# 210 species

## piecewise SEM

spp_psem <- psem(
  lm(occ_change ~ Diet + Tol + Trend, data = spp_data),
  lm(Trend ~ Diet + Tol, data = spp_data),
  data = spp_data)

summary(spp_psem)

# Correlated range occ and pop trend
# Latent variable for niche breadth -- one for diet
# Individual SEMs for different range measurements

## global SEM

sem <- '
occ_change ~ Diet + Tol + Trend
Trend ~ Diet + Tol
'

spp_sem <- sem(sem, spp_data)
summary(spp_sem, standardize = T, rsq = T)
