#################################
# Figure 2 (scenarios maps)
#
# created 13.101.2025 by Jeanne Lemant
#################################

rm(list=ls())

library(cowplot)
library(sf)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./1define_scenarios.R")

figdir = "C:/Users/lemaje/switchdrive/Institution/AIM/7. Internal manuscripts/BEN_SMC/Review/figures/"

# load shapefiles for maps

# ZS_shp_3COT <- file.path("../data/BEN_ZS/BEN_ZS_dissolved_Shp_BEN.shp") %>%
#   sf::st_read(quiet = TRUE )
# #to merge ZS of Cotonou
# ZS_shp_1COT_prep <- ZS_shp_3COT %>%
#   mutate(ZS_1stcommune=ifelse(grepl("COTONOU",ZS),"COTONOU",ZS),
#          Admin1=case_when(NEW_DEPT=="Ou?m?" ~ "Oueme",
#                           TRUE ~ NEW_DEPT))
# 
# ZS_shp <- aggregate(ZS_shp_1COT_prep, list(ZS_shp_1COT_prep$ZS_1stcommune),head,1) %>%
#   select(-ZS)
# 
# ZS_shp_simpl <- sf::st_simplify(ZS_shp, preserveTopology = FALSE, dTolerance = .05)
# 
# Admin1_shp <- aggregate(ZS_shp, list(ZS_shp$Admin1), head, 1)

Admin2_shp <- file.path("../data/BEN_communes/communes.shp") %>%
  sf::st_read(quiet = TRUE) %>%
  mutate(sub=tolower(NAME1_),
         Admin1=case_when(Admn1_N=="Atakora"~"Atacora",
                          Admn1_N=="Kouffo"~"Couffo",
                          Admn1_N=="Ouémé"~"Oueme",
                          TRUE~Admn1_N))

Admin2_shp$sub[!Admin2_shp$sub %in% futrs$sub]
futrs$sub[!futrs$sub %in% Admin2_shp$sub]

# extract variables for scenarios

scenarios <- futrs %>%
  filter(seed==1,EIR_type=="middle",year==2000,age=="0-5") %>%
  select(sub,Admin1,starts_with("fut"),scenario) %>%
  mutate(PMC = futPMCSep2022cov > 0 | futPMCMar2023cov > 0) %>%
  mutate(interventions = case_when(futcovSMC0to10 > 0 ~ "SMC in children under 10",
                                   futcovSMC0to5 > 0 ~ "SMC in children under 5",
                                   PMC ~ "PMC pilot projects",
                                   TRUE ~ NA),
         scenario = case_when(scenario == "planned" ~ "Planned interventions",
                               scenario == "Demo" ~ "Planned interventions\n+ Demographic extension",
                               scenario == "Geo" ~ "Planned interventions\n+ Geographic extension")) %>%
  mutate(interventions = factor(interventions, levels = c("PMC pilot projects",
                                                          "SMC in children under 5",
                                                          "SMC in children under 10")),
         scenario = factor(scenario, levels = c("Planned interventions",
                                                "Planned interventions\n+ Demographic extension",
                                                "Planned interventions\n+ Geographic extension"))) %>%
  mutate(Department_name = ifelse((scenario == "Planned interventions" & sub %in% sub_Demoextension)|
                               (scenario == "Planned interventions\n+ Geographic extension" & sub %in% sub_Geoextension),
                             Admin1,""))

Admin2_shp_scenarios <- Admin2_shp %>%
  left_join(scenarios)

# Admin1_shp_scenarios <- Admin1_shp %>%
#   left_join(scenarios %>% select(Admin1,scenario,Department_name) %>% distinct())

ggplot()+
  geom_sf(data=Admin2_shp_scenarios,
          aes(geometry=geometry,fill=interventions),
          color="gray40",linewidth=.5)+
  #geom_sf(data = Admin1_shp_scenarios, fill = NA, linewidth = 1.05, color = "gray40") +
  #geom_sf_text(data = Admin1_shp_scenarios,aes(label=Department_name),size=12)+
  facet_wrap(scenario~.)+
  theme_void(base_size = 40)+
  theme(legend.key.size = unit(3, 'lines'),
        strip.text.x = element_text(margin = margin(1.5,0,1.5,0, "cm")))  +
  scale_fill_manual(values=c("pink","#009ffd","#ffa400"),
                    na.value="white",name="",na.translate=FALSE)

ggsave(file =paste0(figdir,"Figure2.png")
       ,width = 25, height=15)
ggsave(file = paste0(figdir, "Figure2.svg"),
       height = 12, width = 25)
ggsave(file = paste0(figdir, "Figure2.tiff"),
       height = 15, width = 30, device='tiff')

