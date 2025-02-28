#################################
# Figure 6 (prioritisation with geographic extension)
#
# modified 28.11.2024 by Jeanne Lemant
#################################

rm(list=ls())

library(cowplot)
library(scales)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./1define_scenarios.R")

figdir = "C:/Users/lemaje/switchdrive/Institution/AIM/7. Internal manuscripts/BEN_SMC/Review/figures/"

targeted_pop_geo_ZS <- futrs %>% 
  filter(year %in% 2024:2026,seed==1,EIR_type=="middle",
         Admin1 %in% c("Borgou","Collines","Donga"),scenario=="Geo",age=="0-5")%>%
  group_by(Admin1,ZS,age,year,scenario) %>%
  summarise(pop=sum(pop)) %>%
  group_by(ZS,Admin1,age,scenario) %>%
  summarise(pop=sum(pop))

avertedplanned_OMcases_ZS <- casesAverted.sumAcrossYears(OMcases_df %>%
                                                           filter(Extension=="Geo") %>%
                                                           select(-setting) %>%
                                                           mutate(setting=ZS),
                                                         cases_col = "OMcases",
                                                         level_aggr = "setting",
                                                         eir_col = "EIR_type",
                                                         EIR_names = c("lower","middle","upper"),
                                                         fut_cols = "scenario",
                                                         counterfactual = "planned",
                                                         year_start = 2024,
                                                         year_end = 2026) %>%
  filter(scenario=="Geo") %>%
  rename(ZS="setting") %>%
  left_join(names_ZS %>% select(-sub) %>% distinct())

avertedplanned_nSevere_ZS <- casesAverted.sumAcrossYears(nSevere_df %>%
                                                           filter(Extension=="Geo") %>%
                                                           select(-setting) %>%
                                                           mutate(setting=ZS),
                                                         cases_col = "nSevere",
                                                         level_aggr = "setting",
                                                         eir_col = "EIR_type",
                                                         EIR_names = c("lower","middle","upper"),
                                                         fut_cols = "scenario",
                                                         counterfactual = "planned",
                                                         year_start = 2024,
                                                         year_end = 2026) %>%
  filter(scenario=="Geo") %>%
  rename(ZS="setting") %>%
  left_join(names_ZS %>% select(-sub) %>% distinct())


color_geo <- "#009ffd"
c_alpha <- .5
b_size <- 50

ggplot(avertedplanned_OMcases_ZS %>%
         mutate(indicator_full="All episodes averted\nper 1000 children under 5") %>%
         rbind(avertedplanned_nSevere_ZS %>%
                 mutate(indicator_full="Severe cases averted\nper 1000 children under 5")) %>%
         ungroup() %>% select(-age) %>%
         left_join(targeted_pop_geo_ZS %>% select(-scenario)) %>%
         mutate(across(starts_with("ca"),~./pop*1000)) ,
       aes(x=ZS,y=ca.tot.mean,ymin=ca.tot.inf,ymax=ca.tot.sup))+
  geom_col(alpha=c_alpha,show.legend = FALSE,fill=color_geo)+
  geom_errorbar(width=.5)+
  labs(x="Sanitary zones",y="")+
  facet_grid(indicator_full~Admin1,scales="free", switch = "y")+
  theme_minimal(base_size = 40)+
  theme(strip.placement = "outside",
        axis.title.y = element_blank())

ggsave(file = paste0(figdir, "Figure7.png"),
       height = 20, width = 18)
ggsave(file = paste0(figdir, "Figure7.svg"),
       height = 20, width = 18)
ggsave(file = paste0(figdir, "Figure7.tiff"),
       height = 12, width = 18, device='tiff')
