#################################
# Compute indoor and outdoor exposure to bites from West African data
#
# modified 15.11.2024 by Jeanne Lemant
#################################

# Install AnophelesModel

# devtools::install_github("SwissTPH/AnophelesModel",
#                          build_vignettes = FALSE,
#                          ref = "v1.0.0",
#                          force = TRUE)

library(tidyverse)
library(AnophelesModel)

activity_patterns %>% filter(country=="Benin") %>% distinct(sampling,species)
# data for Anopheles funestus and human indoors
# for humans in bed we'll take data from Cote d'Ivoire, nearest
# for Anopheles gambiae we'll take data from Ghana

activity_cycles_BEN = activity_patterns %>% filter(country=="Benin" | 
                                                     sampling == "BED" & country == "Cote D'Ivoire" |
                                                     species == "Anopheles gambiae" & country == "Ghana")

activity_cycles_BEN_Gambiae = activity_cycles_BEN %>%
  filter(species != "Anopheles funestus") %>%
  group_by(sampling,hour) %>% 
  summarise(value = mean(value)) %>%
  pivot_wider(names_from = sampling, values_from = value) %>%
  rename(humans_indoors = "IND", humans_in_bed = "BED")

exposure_Gambiae <- get_in_out_exp(activity_cycles = activity_cycles_BEN_Gambiae
               , vec_p = def_vector_params(mosquito_species = "Gambiae complex"))

activity_cycles_BEN_Funestus = activity_cycles_BEN %>%
  filter(species != "Anopheles gambiae") %>%
  group_by(sampling,hour) %>% 
  summarise(value = mean(value)) %>%
  pivot_wider(names_from = sampling, values_from = value) %>%
  rename(humans_indoors = "IND", humans_in_bed = "BED")

exposure_Funestus <- get_in_out_exp(activity_cycles = activity_cycles_BEN_Funestus
               , vec_p = def_vector_params(mosquito_species = "Anopheles funestus"))
