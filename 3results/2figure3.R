#################################
# Figure 3 (validation of calibration on prevalence)
#
# modified 28.11.2024 by Jeanne Lemant
#################################

rm(list=ls())

library(cowplot)
library(wesanderson)
library(scales)
library(sf)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./1define_scenarios.R")

figdir = "C:/Users/lemaje/switchdrive/Institution/AIM/7. Internal manuscripts/BEN_SMC/Review/figures/"

# load shapefiles for maps

ZS_shp_3COT <- file.path("../data/BEN_ZS/BEN_ZS_dissolved_Shp_BEN.shp") %>%
  sf::st_read(quiet = TRUE )
#to merge ZS of Cotonou
ZS_shp_1COT_prep <- ZS_shp_3COT %>%
  mutate(ZS_1stcommune=ifelse(grepl("COTONOU",ZS),"COTONOU",ZS),
         Admin1=case_when(NEW_DEPT=="Ou?m?" ~ "Oueme",
                          TRUE ~ NEW_DEPT))

ZS_shp <- aggregate(ZS_shp_1COT_prep, list(ZS_shp_1COT_prep$ZS_1stcommune),head,1) %>%
  select(-ZS)

ZS_shp_simpl <- sf::st_simplify(ZS_shp, preserveTopology = FALSE, dTolerance = .05)

Admin1_shp <- aggregate(ZS_shp, list(ZS_shp$Admin1), head, 1)

#### Load data for validation

prevalence_surveys <- read.csv("../data/prevalence_surveys.csv", sep = ";")

MAP_PfPRU5 <- read.csv("../data/PfPR0to5_CI_Benin_2000-2019_MAP_global.csv")

simul_surveymonths_calib <- readRDS("../1calibration/surveymonths_calibrated_simulations.Rda")

PfPR_BEN = computeAggrTimeSeriesPR(PfPR_dfU5
                                   , pr_col = "PR"
                                   , level_aggr = "national"
                                   , pop_col = "pop"
                                   , eir_col = "EIR_type"
                                   , EIR_names = c("lower","middle","upper")
                                   , fut_cols = "scenario")[[2]] %>%
  rename(PR_mean = ind_mean, PR_inf = ind_inf, PR_sup = ind_sup)

simulated_pop_monthly = unique(simul_surveymonths_calib$pop)

simul_average_surveymonths = simul_surveymonths_calib %>%
  filter(age=="0-5") %>%
  group_by(setting,sub,Admin1,survey_year,seed,EIR_type,age) %>%
  summarise(PR=mean(PR),nHost=mean(nHost)) %>%
  separate(survey_year, into = c("survey", "year"), sep ="_") %>%
  mutate(year=as.integer(year))

simul_average_surveymonths_popw <- simul_average_surveymonths %>%
  left_join(pop_projected_long) %>%
  mutate(pop = pop_All*nHost/simulated_pop_monthly) %>%
  group_by(sub,setting,seed,year,age) %>%
  mutate(pop = mean(pop)) %>%
  select(-pop_All) %>%
  ungroup

PfPR_surveymonths_Admin1 <- computeAggrTimeSeriesPR(simul_average_surveymonths_popw %>%
                                                      select(-setting) %>%
                                                      rename(setting="Admin1") %>%
                                                      mutate(scenario="planned")
                                                    , pr_col = "PR"
                                                    , level_aggr = "setting"
                                                    , pop_col = "pop"
                                                    , eir_col = "EIR_type"
                                                    , EIR_names = c("lower","middle","upper")
                                                    , fut_cols = "scenario")[[2]] %>%
  rename(PR_mean = ind_mean, PR_inf = ind_inf, PR_sup = ind_sup) %>%
  rename(Admin1="setting")

prevalence2017_map <- Admin1_shp %>%
  left_join(model_surveys_Admin1 %>%
              filter(year == 2017) %>%
              select(Admin1,PfPR_RDT,PR_mean,PR_inf,PR_sup) %>%
              pivot_longer(cols = c(PfPR_RDT,PR_mean), names_to = "source", values_to = "PfPR")) %>%
  mutate(lon = st_coordinates(st_centroid(.))[,1],
         lat = st_coordinates(st_centroid(.))[,2]) %>%
  mutate(lat = case_when(Admin1 == "Littoral" ~ lat -0.2,
                         Admin1 == "Zou" ~ lat +0.1,
                         Admin1 == "Atlantique" ~ lat +0.1,
                         Admin1 == "Plateau" ~ lat -0.1,
                         Admin1 == "Mono" ~ lat -0.1,
                         Admin1 == "Oueme" ~ lat -0.2,
                         TRUE ~ lat),
         lon = case_when(Admin1 == "Couffo" & source == "PfPR_RDT"~ lon -0.1,
                         Admin1 == "Couffo" & source == "PR_mean"~ lon -0.5,
                         Admin1 == "Mono" & source == "PfPR_RDT"~ lon -0.2,
                         Admin1 == "Mono" & source == "PR_mean"~ lon -0.55,
                         Admin1 == "Oueme" & source == "PfPR_RDT"~ lon +0.2,
                         Admin1 == "Oueme" & source == "PR_mean"~ lon +0.6,
                         Admin1 == "Plateau" & source == "PfPR_RDT"~ lon +0.2,
                         Admin1 == "Plateau" & source == "PR_mean"~ lon +0.7,
                         TRUE ~ lon)) %>%
  mutate(PfPR_label = ifelse(source == "PfPR_RDT", paste0(round(PfPR*100),"%"),
                             paste0(round(PfPR*100),"% (",round(PR_inf*100), "-", round(PR_sup*100),")")),
         source = ifelse(source == "PfPR_RDT", "Survey prevalence\n(DHS 2017)",
                         "Modelled prevalence\n(matching DHS 2017\ndata collection months)")) %>%
  mutate(source = factor(source, levels = c("Survey prevalence\n(DHS 2017)","Modelled prevalence\n(matching DHS 2017\ndata collection months)")))


##### general settings
c_alpha <- .5
b_size <- 40
color_map <- rgb(78,176,155,maxColorValue = 255)

figure3a <- ggplot(PfPR_BEN %>% filter(scenario=="planned"))+
  geom_ribbon(aes(x=year,ymin=PR_inf,ymax=PR_sup),fill="grey",
              alpha=c_alpha)+
  geom_line(aes(x=year,y=PR_mean,color="Model"),size=2)+
  geom_pointrange(data=MAP_PfPRU5 %>% filter(year>2005),
                  aes(x=year,y=PR_pop_adj/100,ymin=LCI/100,ymax=UCI/100,
                      color="MAP (used for calibration)"),
                  size=1,stroke=2,lwd=2,shape=21,fill="white")+
  xlim(2005,2020)+
  theme_minimal(base_size=b_size)+
  labs(title="Malaria prevalence in children under 5 in Benin",x="Year")+
  scale_y_continuous(labels=scales::percent,breaks=seq(2,7,1)/10,
                     limits=c(.1,.75))+
  scale_color_manual(name="Source",values=c(color_map,"darkgrey"))+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        plot.title = element_text(size=40,hjust=.5))

figure3b <- ggplot(model_surveys_Admin1,
       aes(x = PfPR_RDT, y = PR_mean, ymin = PR_inf, ymax = PR_sup,
           color = as.factor(year)))+
  geom_pointrange(size=1,stroke=2,lwd=2)+
  geom_abline(lwd=2)+
  labs(x = "Survey prevalence", y = "Modelled prevalence", color = "Survey year")+
  scale_color_manual(values = wes_palette("IsleofDogs1")[3:1])+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::percent)+
  theme_minimal(base_size=b_size)+
  theme(legend.position = "bottom")

model <- lm(PfPR_RDT ~ PR_mean,
            data=model_surveys_Admin1
            )
summary(model)

model_wo_2015 <- lm(PfPR_RDT ~ PR_mean,
            data=model_surveys_Admin1 %>% filter(year != 2015)
)
summary(model_wo_2015)

model_surveys_Admin1 <- prevalence_surveys %>%
  filter(Admin!="Benin") %>%
  rename(year = "year_start", Admin1 = "Admin") %>%
  left_join(PfPR_surveymonths_Admin1)

figure3c <- ggplot(data=prevalence2017_map,
                   aes(geometry=geometry,fill=PfPR, label = PfPR_label,
                       x = lon, y = lat))+
  geom_sf(color="gray40",linewidth=.5)+
  geom_label(size=6, fill = "white")+
  scale_fill_distiller(palette = "Purples", direction = 1,
                       labels = scales::label_percent(), name = "Under 5 prevalence")+
  theme_void(base_size = 30)+
  theme(legend.key.size = unit(2, 'lines'),
        strip.text.x = element_text(margin = margin(1.5,0,1.5,0, "cm")))+
  facet_wrap(source~.)


plot_grid(plot_grid(figure3a,figure3b,labels = c('A','B'),nrow=1,label_size = 40),
          plot_grid(figure3c,NULL,labels = c('C',NULL),nrow=1,label_size = 40),nrow=2)

ggsave(file = paste0(figdir, "Figure3.png"),
       height = 20, width = 30)
ggsave(file = paste0(figdir, "Figure3.svg"),
       height = 12, width = 30)

