#################################
# All results plots
#
# modified 25.11.2024 by Jeanne Lemant
#################################

rm(list=ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./1define_scenarios.R")

prevalence_surveys <- read.csv("../data/prevalence_surveys.csv", sep = ";")

PfPRdfU5_BEN = computeAggrTimeSeriesPR(PfPR_dfU5 %>% filter(scenario=="baseline")
                                   , pr_col = "PR"
                                   , level_aggr = "national"
                                   , pop_col = "pop"
                                   , eir_col = "EIR_type"
                                   , EIR_names = c("lower","middle","upper")
                                   , fut_cols = "scenario")[[2]] %>%
  rename(PR_mean = ind_mean, PR_inf = ind_inf, PR_sup = ind_sup)

ggplot(PfPRdfU5_BEN)+
  geom_ribbon(aes(x=year,ymin=PR_inf,ymax=PR_sup))+
  geom_line(aes(x=year,y=PR_mean))+
  xlim(2000,2019)

PfPR_Admin1 = computeAggrTimeSeriesPR(PfPR_dfU5 %>% filter(scenario=="baseline") %>% select(-setting) %>% rename(setting="Admin1")
                                      , pr_col = "PR"
                                      , level_aggr = "setting"
                                      , pop_col = "pop"
                                      , eir_col = "EIR_type"
                                      , EIR_names = c("lower","middle","upper")
                                      , fut_cols = "scenario")[[2]] %>%
  rename(PR_mean = ind_mean, PR_inf = ind_inf, PR_sup = ind_sup) %>%
  rename(Admin1="setting")

model_surveys_Admin1 = prevalence_surveys %>%
  rename(Admin1 = "Admin", year = "year_start") %>%
  select(Admin1,`PfPR_RDT`,year) %>%
  filter(Admin1!="Benin") %>%
  left_join(PfPR_Admin1)

ggplot(model_surveys_Admin1,
       aes(x = `PfPR_RDT`, y = PR_mean, ymin = PR_inf, ymax = PR_sup,
           color = as.factor(year)))+
  geom_pointrange()+
  geom_abline()+
  scale_y_continuous(labels=scales::percent,limits=c(0,.65))+
  scale_x_continuous(labels=scales::percent,limits=c(0,.65))+
  labs(x = "Survey prevalence", y = "Modelled prevalence", color = "Year")

model <- lm(`PfPR_RDT` ~ PR_mean,
            data=model_surveys_Admin1 %>% filter(year != 2015))
summary(model)
