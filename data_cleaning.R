## Collect dataset

library(tidyverse)
library(purrr)

#### Species niche characteristics: ####
## Habitat -- want to use eBird for real thing
## Climate **

clim_breadth <- read.csv("Master_RO_Correlates_20110610.csv", stringsAsFactors = F) %>%
  dplyr::select(AOU, Tol)

## Diet & Habitat **

species_list <- read.csv("species_list.csv", stringsAsFactors = F)

# EltonTraits 1.0

# birdfunc <- read.table("\\\\BioArk//HurlbertLab//Databases//Elton Traits//BirdFuncDat.txt", sep = "\t", header = T, quote = "\"", stringsAsFactors = F)
birdfunc <- read.table("/Volumes/HurlbertLab/Databases/Elton Traits/BirdFuncDat.txt", sep = "\t", header = T, quote = "\"", stringsAsFactors = F)

# BBS species traits with AOUs

binomial_join <- species_list %>%
  filter(!grepl("unid.", english_common_name)) %>%
  dplyr::select(genus, species, english_common_name, aou) %>%
  mutate(binomial = paste(genus, species, sep = " ")) %>%
  left_join(birdfunc, by = c("binomial" = "Scientific"))

common_name_join <- binomial_join %>%
  filter(is.na(Diet.5Cat)) %>%
  dplyr::select(genus, species, english_common_name, aou, binomial) %>%
  left_join(birdfunc, by = c("english_common_name" = "English"))

# Anything that's left is subspecies or Eastern Yellow Wagtail (Asian spp found in AK)

binomial_matches <- binomial_join %>%
  filter(!is.na(Diet.5Cat))

common_name_matches <- binomial_join %>%
  filter(!is.na(Diet.5Cat))

bbs_func <- binomial_matches %>%
  bind_rows(common_name_matches)

# Species distance matrix
# Keep diet composition, foraging height, diurnality, body size
bbs_traits <- bbs_func %>%
  arrange(aou) %>%
  dplyr::select(aou, Diet.Inv, Diet.Vend, Diet.Vect, Diet.Vfish, Diet.Vunk, Diet.Scav, Diet.Fruit, Diet.Nect, Diet.Seed, Diet.PlantO,
                ForStrat.watbelowsurf, ForStrat.wataroundsurf, ForStrat.ground, ForStrat.understory, ForStrat.midhigh, ForStrat.canopy,
                ForStrat.aerial, Nocturnal, BodyMass.Value) %>%
  distinct() %>%
  mutate_at(.fun = log10, .vars = "BodyMass.Value")

row.names(bbs_traits) <- bbs_traits$aou

# Breadth of niches (diet and foraging strata are fuzzy: proportions of categories)

niche_breadth <- bbs_traits %>%
  dplyr::select(-Nocturnal, -BodyMass.Value) %>%
  gather(key = "trait", value = "proportion", 2:18) %>%
  filter(proportion > 0) %>%
  mutate(category = word(trait, 1, 1, sep = "[.]")) %>%
  group_by(aou, category) %>%
  summarize(nTraits = n_distinct(trait)) %>%
  pivot_wider(names_from = category, values_from = nTraits) %>%
  right_join(clim_breadth, by = c("aou" = "AOU"))
write.csv(niche_breadth, "species_niche_breadths.csv", row.names = F)

#### Range shifts: ####
## Change in range wide pop size **

bbs_trend <- read.csv("BBS_1966-2017_core_trend_revised_v2.csv", stringsAsFactors = F) %>%
  filter(Region == "SU1")

## Change in range occupancy **

## BBS 2017 Version
routes <- read.csv("/Volumes/HurlbertLab/Databases/BBS/2017/bbs_routes_20170712.csv")
counts <- read.csv("/Volumes/hurlbertlab/Databases/BBS/2017/bbs_counts_20170712.csv")
species <- read.csv("/Volumes/hurlbertlab/Databases/BBS/2017/bbs_species_20170712.csv")
weather <- read.csv("/Volumes/hurlbertlab/Databases/BBS/2017/bbs_weather_20170712.csv")

routes$stateroute <- routes$statenum*1000 + routes$route
weather$stateroute <-weather$statenum*1000 + weather$route
RT1 <- subset(weather, runtype == 1, select = c("stateroute", "year"))
RT1.routes <- merge(RT1, routes[ , c("countrynum", "statenum", "stateroute", "latitude", "longitude","bcr")], by = "stateroute", all.x = TRUE)
counts$stateroute <- counts$statenum*1000 + counts$route

counts.subs <- counts %>%
  merge(RT1.routes, by = c("stateroute", "year")) %>%
  filter(rpid == 101)

range_occ <- counts.subs %>%
  group_by(aou) %>%
  mutate(total_rtes = n_distinct(stateroute)) %>%
  filter(total_rtes > 100) %>%
  group_by(aou, total_rtes) %>%
  summarize(early_rtes = n_distinct(stateroute[year <= 1975]),
            late_rtes = n_distinct(stateroute[year >= 2012])) %>%
  mutate(occ_change = late_rtes/total_rtes - early_rtes/total_rtes)

## Change in lat bounds of range 


## Range dynamics

range_dyn <- bbs_trend %>%
  inner_join(range_occ, by = c("AOU" = "aou"))
write.csv(range_dyn, "species_range_dynamics.csv", row.names = F)

