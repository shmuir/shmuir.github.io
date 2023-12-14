################################################################
# Final Project for EDS222: Statistics for Env. Data Science
# Sam Muir
# December 15, 2023
################################################################

## Data reading and cleaning ##

# IUCN data was obtained by download from the website
# https://www.iucnredlist.org/search?query=plant&searchType=species

## load packages ##
library(tidyverse)
library(BIEN)
library(rredlist)
library(janitor)

## leaf photosynthetic rate per leaf area data ##
lpr <- BIEN_trait_trait("leaf photosynthetic rate per leaf area") %>%
  rename(scientific_name = scrubbed_species_binomial)  %>%
  mutate(trait_value = as.numeric(trait_value)) %>%
  filter(trait_value < 70) 
# there's a few values there in the 200,000+ which don't make sense physiologically...
#these high values are also all from the same study, so there may be some incorrect calculations in the data/ unit conversions. 

## find avg photosynthetic rate for each species ##
lpr_summary <- lpr %>%
  group_by(scientific_name) %>%
  summarise(avg_photo = mean(trait_value, na.rm = TRUE))

## read in redlist data ##
redlist <- read_csv("data/redlist_species_data/assessments.csv") %>%
  clean_names() %>%
  filter(population_trend %in% c("Stable", "Increasing", "Decreasing")) %>% # remove data deficient observations
  # create new pop trend groupings
  mutate(pop_trend = case_when(population_trend == "Decreasing" ~ "Decreasing",
                               population_trend %in% c("Stable", "Increasing") ~ "Stable_Inc"),
         # binary values for pop trend
         pop_trend_binary = case_when(pop_trend == "Decreasing" ~ 1,
                                      pop_trend == "Stable_Inc" ~ 0)) 


#setdiff(lpr$scrubbed_species_binomial, redlist$scientific_name)

## join lpr with the redlist data, keeping species present in both dfs ##
join <- inner_join(lpr_summary, redlist)

## threat data ##
# climate threatened speices
climate_threatened <- read_csv("data/IUCN_climate_threatened.csv") %>%
  mutate(climate_threatened = "yes")

# human threatened species
# this includes both "human intrusions & disturbance" and "residential & commercial development" catagories from IUCN
human_res <- read_csv("data/IUCN_human_res.csv") %>%
  mutate(human_threatened = "yes")

## threat joins ##
# those not climate threatened will be catagorized as "no" under the climate_threatened col
climate_join <- left_join(join, climate_threatened) %>%
  mutate(climate_threatened = replace(climate_threatened, is.na(climate_threatened), "no"))

# those not human threatened will be catagorized as "no" under the climate_threatened col
clim_human_res <- left_join(climate_join, human_res) %>%
  mutate(human_threatened = replace(human_threatened, is.na(human_threatened), "no"))

