#################################
# Figure 5 (cases by age group)
#
# modified 28.11.2024 by Jeanne Lemant
#################################

rm(list=ls())

library(cowplot)
library(scales)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./1define_scenarios.R")

figdir = "C:/Users/lemaje/switchdrive/Institution/AIM/7. Internal manuscripts/BEN_SMC/Review/figures/"

###### OMcases

OMcases_ExtU5 <- computeAggrTimeSeriesCasesInc(allSims=OMcases_dfU5 %>% select(-setting) %>%
                                                 rename(setting="Extension"),
                                               cases_col = "OMcases",
                                               level_aggr = "setting",
                                               pop_col = "pop",eir_col = "EIR_type",
                                               EIR_names = c("lower","middle","upper"),
                                               fut_cols = "scenario",convert_pop = FALSE
) %>%
  rename(Extension="setting")
OMcases_Ext5to10 <- computeAggrTimeSeriesCasesInc(allSims=OMcases_df5to10 %>% select(-setting) %>%
                                                    rename(setting="Extension"),
                                                  cases_col = "OMcases",
                                                  level_aggr = "setting",
                                                  pop_col = "pop",eir_col = "EIR_type",
                                                  EIR_names = c("lower","middle","upper"),
                                                  fut_cols = "scenario",convert_pop = FALSE
) %>%
  rename(Extension="setting")
OMcases_Ext10to100 <- computeAggrTimeSeriesCasesInc(allSims=OMcases_df10to100 %>% select(-setting) %>%
                                                      rename(setting="Extension"),
                                                    cases_col = "OMcases",
                                                    level_aggr = "setting",
                                                    pop_col = "pop",eir_col = "EIR_type",
                                                    EIR_names = c("lower","middle","upper"),
                                                    fut_cols = "scenario",convert_pop = FALSE
) %>%
  rename(Extension="setting")

OMcases_fig3 = bind_rows(OMcases_ExtU5,OMcases_Ext5to10,OMcases_Ext10to100) %>%
  filter(!is.na(Extension)) %>%
  filter(year %in% 2024:2026) %>%
  filter(scenario=="planned"|
           (Extension=="Demo"&scenario=="Demo")|
           (Extension=="Geo"&scenario=="Geo")) %>%
  group_by(Extension,scenario,age) %>%
  summarise(across(starts_with("totCases"),sum)) %>%
  mutate(age=factor(age,levels = c("10-100","5-10","0-5")),
         ExtLabel=ifelse(Extension=="Demo","Alibori+Atacora","Borgou+Collines\n+Donga"),
         scenLabel=case_when(scenario=="planned"~"Planned\ninterventions",
                             scenario=="Demo"~"Planned\ninterventions\n+ Demographic\nextension",
                             TRUE~"Planned\ninterventions\n+ Geographic\nextension"),
         scenLabel=factor(scenLabel,
                          levels=c("Planned\ninterventions",
                                   "Planned\ninterventions\n+ Demographic\nextension",
                                   "Planned\ninterventions\n+ Geographic\nextension")))

####### nSevere

nSevere_ExtU5 <- computeAggrTimeSeriesCasesInc(allSims=nSevere_dfU5 %>% select(-setting) %>%
                                                 rename(setting="Extension"),
                                               cases_col = "nSevere",
                                               level_aggr = "setting",
                                               pop_col = "pop",eir_col = "EIR_type",
                                               EIR_names = c("lower","middle","upper"),
                                               fut_cols = "scenario",convert_pop = FALSE
) %>%
  rename(Extension="setting")

nSevere_Ext5to10 <- computeAggrTimeSeriesCasesInc(allSims=nSevere_df5to10 %>% select(-setting) %>%
                                                    rename(setting="Extension"),
                                                  cases_col = "nSevere",
                                                  level_aggr = "setting",
                                                  pop_col = "pop",eir_col = "EIR_type",
                                                  EIR_names = c("lower","middle","upper"),
                                                  fut_cols = "scenario",convert_pop = FALSE
) %>%
  rename(Extension="setting")
nSevere_Ext10to100 <- computeAggrTimeSeriesCasesInc(allSims=nSevere_df10to100 %>% select(-setting) %>%
                                                      rename(setting="Extension"),
                                                    cases_col = "nSevere",
                                                    level_aggr = "setting",
                                                    pop_col = "pop",eir_col = "EIR_type",
                                                    EIR_names = c("lower","middle","upper"),
                                                    fut_cols = "scenario",convert_pop = FALSE
) %>%
  rename(Extension="setting")

nSevere_fig3 = bind_rows(nSevere_ExtU5,nSevere_Ext5to10,nSevere_Ext10to100) %>%
  filter(!is.na(Extension)) %>%
  filter(year %in% 2024:2026) %>%
  filter(scenario=="planned"|
           (Extension=="Demo"&scenario=="Demo")|
           (Extension=="Geo"&scenario=="Geo")) %>%
  group_by(Extension,scenario,age) %>%
  summarise(across(starts_with("totCases"),sum)) %>%
  mutate(age=factor(age,levels = c("10-100","5-10","0-5")),
         ExtLabel=ifelse(Extension=="Demo","Alibori+Atacora","Borgou+Collines\n+Donga"),
         scenLabel=case_when(scenario=="planned"~"Planned\ninterventions",
                             scenario=="Demo"~"Planned\ninterventions\n+ Demographic\nextension",
                             TRUE~"Planned\ninterventions\n+ Geographic\nextension"),
         scenLabel=factor(scenLabel,
                          levels=c("Planned\ninterventions",
                                   "Planned\ninterventions\n+ Demographic\nextension",
                                   "Planned\ninterventions\n+ Geographic\nextension")))


##### general settings
c_alpha <- .5
b_size <- 40

figure5a <- ggplot(OMcases_fig3,
                     aes(x=scenLabel,y=totCases.mean,fill=age))+
  geom_col(alpha=c_alpha)+
  labs(x="",y="",fill="Age group")+
  facet_grid(.~ExtLabel,scales="free",switch="y")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_fill_manual(values=c("#4f6d7a","#e8dab2","#dd6e42"))+
  labs(title="All episodes")+
  theme_minimal(base_size = b_size)+
  theme(strip.placement = "outside",
        axis.title.y = element_blank(),
        plot.title = element_text(size=40,hjust=.5))

figure5b <- ggplot(nSevere_fig3,
                     aes(x=scenLabel,y=totCases.mean,fill=age))+
  geom_col(alpha=c_alpha)+
  labs(x="",y="",fill="Age group")+
  facet_grid(.~ExtLabel,scales="free",switch="y")+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_fill_manual(values=c("#4f6d7a","#e8dab2","#dd6e42"))+
  labs(title="Severe cases")+
  theme_minimal(base_size = b_size)+
  theme(strip.placement = "outside",
        axis.title.y = element_blank(),
        plot.title = element_text(size=40,hjust=.5))

plot_grid(plot_grid(figure5a + theme(legend.position="none"),
                    figure5b + theme(legend.position="none"),
                    labels = c('A', 'B'),nrow=1,label_size = 40),
          get_legend(
            figure5a),nrow = 1, rel_widths = c(1, .1))

ggsave(file = paste0(figdir, "Figure5.png"),
       height = 12, width = 30)
ggsave(file = paste0(figdir, "Figure5.svg"),
       height = 12, width = 30)
ggsave(file = paste0(figdir, "Figure5.tiff"),
       height = 12, width = 30, device='tiff')

