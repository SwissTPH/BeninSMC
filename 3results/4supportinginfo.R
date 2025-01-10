library(scales)
library(cowplot)
library(grid)
library(gridExtra)

rm(list=ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./1define_scenarios.R")

supportinginfodir <- "C:/Users/lemaje/switchdrive/Institution/AIM/7. Internal manuscripts/BEN_SMC/Review/figures_supportinginfo/"


##### All necessary dataframes

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

effectplanned_OMcases5to10_Ext3years <- percentReductionInc.acrossScenarios(OMcases_df5to10 %>% select(-setting) %>%
                                                                              rename(setting="Extension") %>%
                                                                              group_by(sub,setting,Admin1,ZS,
                                                                                       EIR_type,seed,scenario,age) %>%
                                                                              filter(year %in% 2024:2026) %>%
                                                                              summarise(OMcases=sum(OMcases),
                                                                                        pop=sum(pop)) %>%
                                                                              mutate(year=2024),
                                                                            cases_col = "OMcases",
                                                                            level_aggr = "setting",
                                                                            eir_col = "EIR_type",
                                                                            EIR_names = c("lower","middle","upper"),
                                                                            fut_cols = "scenario",
                                                                            counterfactual = "planned",
                                                                            pop_col = "pop",
                                                                            comparison_year = 2024) %>%
  rename(Extension="setting")

effectplanned_OMcases_Ext3years <- percentReductionInc.acrossScenarios(OMcases_df %>% select(-setting) %>%
                                                                         rename(setting="Extension") %>%
                                                                         group_by(sub,setting,Admin1,ZS,
                                                                                  EIR_type,seed,scenario,age) %>%
                                                                         filter(year %in% 2024:2026) %>%
                                                                         summarise(OMcases=sum(OMcases),
                                                                                   pop=sum(pop)) %>%
                                                                         mutate(year=2024),
                                                                       cases_col = "OMcases",
                                                                       level_aggr = "setting",
                                                                       eir_col = "EIR_type",
                                                                       EIR_names = c("lower","middle","upper"),
                                                                       fut_cols = "scenario",
                                                                       counterfactual = "planned",
                                                                       pop_col = "pop",
                                                                       comparison_year = 2024) %>%
  rename(Extension="setting")

effectplanned_OMcasesU5_Ext3years<- percentReductionInc.acrossScenarios(OMcases_dfU5 %>% select(-setting) %>%
                                                                          rename(setting="Extension") %>%
                                                                          group_by(sub,setting,Admin1,ZS,
                                                                                   EIR_type,seed,scenario,age) %>%
                                                                          filter(year %in% 2024:2026) %>%
                                                                          summarise(OMcases=sum(OMcases),
                                                                                    pop=sum(pop)) %>%
                                                                          mutate(year=2024),
                                                                        cases_col = "OMcases",
                                                                        level_aggr = "setting",
                                                                        eir_col = "EIR_type",
                                                                        EIR_names = c("lower","middle","upper"),
                                                                        fut_cols = "scenario",
                                                                        counterfactual = "planned",
                                                                        pop_col = "pop",
                                                                        comparison_year = 2024) %>%
  rename(Extension="setting")

####### nSevere

effectplanned_nSevere5to10_Ext3years <- percentReductionInc.acrossScenarios(nSevere_df5to10 %>% select(-setting) %>%
                                                                              rename(setting="Extension") %>%
                                                                              group_by(sub,setting,Admin1,ZS,
                                                                                       EIR_type,seed,scenario,age) %>%
                                                                              filter(year %in% 2024:2026) %>%
                                                                              summarise(nSevere=sum(nSevere),
                                                                                        pop=sum(pop)) %>%
                                                                              mutate(year=2024),
                                                                            cases_col = "nSevere",
                                                                            level_aggr = "setting",
                                                                            eir_col = "EIR_type",
                                                                            EIR_names = c("lower","middle","upper"),
                                                                            fut_cols = "scenario",
                                                                            counterfactual = "planned",
                                                                            pop_col = "pop",
                                                                            comparison_year = 2024) %>%
  rename(Extension="setting")

effectplanned_nSevere_Ext3years <- percentReductionInc.acrossScenarios(nSevere_df %>% select(-setting) %>%
                                                                         rename(setting="Extension") %>%
                                                                         group_by(sub,setting,Admin1,ZS,
                                                                                  EIR_type,seed,scenario,age) %>%
                                                                         filter(year %in% 2024:2026) %>%
                                                                         summarise(nSevere=sum(nSevere),
                                                                                   pop=sum(pop)) %>%
                                                                         mutate(year=2024),
                                                                       cases_col = "nSevere",
                                                                       level_aggr = "setting",
                                                                       eir_col = "EIR_type",
                                                                       EIR_names = c("lower","middle","upper"),
                                                                       fut_cols = "scenario",
                                                                       counterfactual = "planned",
                                                                       pop_col = "pop",
                                                                       comparison_year = 2024) %>%
  rename(Extension="setting")

effectplanned_nSevereU5_Ext3years <- percentReductionInc.acrossScenarios(nSevere_dfU5 %>% select(-setting) %>%
                                                                           rename(setting="Extension") %>%
                                                                           group_by(sub,setting,Admin1,ZS,
                                                                                    EIR_type,seed,scenario,age) %>%
                                                                           filter(year %in% 2024:2026) %>%
                                                                           summarise(nSevere=sum(nSevere),
                                                                                     pop=sum(pop)) %>%
                                                                           mutate(year=2024),
                                                                         cases_col = "nSevere",
                                                                         level_aggr = "setting",
                                                                         eir_col = "EIR_type",
                                                                         EIR_names = c("lower","middle","upper"),
                                                                         fut_cols = "scenario",
                                                                         counterfactual = "planned",
                                                                         pop_col = "pop",
                                                                         comparison_year = 2024) %>%
  rename(Extension="setting")

##### general settings
color_demo <- "#ffa400"
color_geo <- "#009ffd"
c_alpha <- .5
b_size <- 50

##### Effect sizes

effectsize_2024_2026 <- rbind(effectplanned_OMcasesU5_Ext3years %>% mutate(indicator="OMcases"),
                              effectplanned_nSevereU5_Ext3years %>% mutate(indicator="nSevere"),
                              
                              effectplanned_OMcases5to10_Ext3years %>% mutate(indicator="OMcases"),
                              effectplanned_nSevere5to10_Ext3years %>% mutate(indicator="nSevere"),
                              
                              effectplanned_OMcases_Ext3years %>% mutate(indicator="OMcases"),
                              effectplanned_nSevere_Ext3years %>% mutate(indicator="nSevere")) %>%
  filter((Extension=="Demo"&scenario=="Demo")|
           (Extension=="Geo"&scenario=="Geo"))

pe_Demo <- ggplot(effectsize_2024_2026 %>% filter(age %in% c("0-5","5-10","0-100"),
                                                  Extension=="Demo") %>%
                    mutate(age=factor(age,levels=c("0-5","5-10","0-100")),
                           indicator_full=ifelse(indicator=="OMcases","All episodes","Severe cases")) %>%
                    mutate(indicator_full=factor(indicator_full,levels=c("Severe cases","All episodes"))),
                  aes(y=indicator_full,x=diffCasesAggr.mean/100,xmin=diffCasesAggr.inf/100,xmax=diffCasesAggr.sup/100,
                      color=indicator_full))+
  geom_pointrange(size=1.5,lwd=2)+
  labs(x="",y="",fill="Age group")+
  scale_x_continuous(labels=scales::percent,breaks = c(0,.1,.2,.3))+
  scale_color_manual(values=c("gray","black"),name="",breaks=c("All episodes","Severe cases"))+
  labs(title="Demographic extension\n(Alibori+Atacora)")+
  facet_grid(age~.,switch = "y")+
  theme_minimal(base_size = 40)+
  theme(axis.text.y = element_blank(),axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank(),panel.grid.major.y= element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        strip.text.y.left = element_text(angle = 0))

pe_Geo <- ggplot(effectsize_2024_2026 %>% filter(age %in% c("0-5","5-10","0-100"),
                                                 Extension=="Geo") %>%
                   mutate(age=factor(age,levels=c("0-5","5-10","0-100")),
                          indicator_full=ifelse(indicator=="OMcases","All episodes","Severe cases")) %>%
                   mutate(indicator_full=factor(indicator_full,levels=c("Severe cases","All episodes"))),
                 aes(y=indicator_full,x=diffCasesAggr.mean/100,xmin=diffCasesAggr.inf/100,xmax=diffCasesAggr.sup/100,
                     color=indicator_full))+
  geom_pointrange(size=1.5,lwd=2)+
  labs(x="",y="",fill="Age group")+
  scale_x_continuous(labels=scales::percent,breaks = c(0,.1,.2,.3))+
  scale_color_manual(values=c("gray","black"),name="",breaks=c("All episodes","Severe cases"))+
  labs(title="Geographic extension\n(Borgou+Collines+Donga)")+
  facet_grid(age~.)+
  theme_minimal(base_size = 40)+
  theme(axis.text.y = element_blank(),axis.ticks.y=element_blank(),
        panel.grid.minor=element_blank(),panel.grid.major.y= element_blank(),
        plot.title = element_text(size=40,hjust = 0.5),
        strip.text.y.left = element_text(angle = 0))

pe_both <- plot_grid(pe_Demo + theme(legend.position="none"),
                     pe_Geo + theme(legend.position="none",
                                    strip.background = element_blank(),
                                    strip.text.y = element_blank()),
                     get_legend(pe_Demo),
                     labels = c('A', 'B'),nrow=1,label_size = 40,rel_widths = c(1,1, .3)
)

pe_xlabel <- textGrob("Reduction in cases compared to scheduled interventions", 
                      gp=gpar(fontsize=40))

pe <- arrangeGrob(pe_both, bottom = pe_xlabel)

grid.arrange(pe)

ggsave(plot = pe,paste0(supportinginfodir,"reduction_ind_age_2024-2026_bw_labels.png"),
       height=12,width=25)

####### Demographic extension

#cases in children from 5 to 10
OMcases_Ext5to10 %>% filter(Extension=="Demo"&scenario%in%c("Demo","planned")) %>%
  filter(year %in% 2024:2026) %>%
  group_by(Extension,scenario,age) %>%
  summarise(across(starts_with("totCases"),sum))

#effect size over three years compared to baseline in children from 5 to 10
effectplanned_OMcases5to10_Ext3years %>% filter(Extension=="Demo"&scenario=="Demo")

effectplanned_nSevere5to10_Ext3years %>% filter(Extension=="Demo"&scenario=="Demo")

####### Geographic extension

#cases in children under 5
OMcases_ExtU5 %>% filter(Extension=="Geo"&scenario%in%c("Geo","planned")) %>%
  filter(year %in% 2024:2026) %>%
  group_by(Extension,scenario,age) %>%
  summarise(across(starts_with("totCases"),sum))

#effect size over three years compared to baseline in children under 5
effectplanned_OMcasesU5_Ext3years %>% filter(Extension=="Geo"&scenario=="Geo")

effectplanned_nSevereU5_Ext3years %>% filter(Extension=="Geo"&scenario=="Geo")

####### Both extensions, whole population

#effect size over three years compared to baseline in whole population
effectplanned_OMcases_Ext3years %>% filter(Extension=="Demo"&scenario=="Demo")

effectplanned_nSevere_Ext3years %>% filter(Extension=="Demo"&scenario=="Demo")

effectplanned_OMcases_Ext3years %>% filter(Extension=="Geo"&scenario=="Geo")

effectplanned_nSevere_Ext3years %>% filter(Extension=="Geo"&scenario=="Geo")

##### Averted cases

avertedplanned_OMcases_Ext <- casesAverted.sumAcrossYears(OMcases_df %>% select(-setting) %>%
                                                            rename(setting="Extension"),
                                                          cases_col = "OMcases",
                                                          level_aggr = "setting",
                                                          eir_col = "EIR_type",
                                                          EIR_names = c("lower","middle","upper"),
                                                          fut_cols = "scenario",
                                                          counterfactual = "planned",
                                                          year_start = 2024,
                                                          year_end = 2026) %>%
  rename(Extension="setting")

avertedplanned_nSevere_Ext <- casesAverted.sumAcrossYears(nSevere_df %>% select(-setting) %>%
                                                            rename(setting="Extension"),
                                                          cases_col = "nSevere",
                                                          level_aggr = "setting",
                                                          eir_col = "EIR_type",
                                                          EIR_names = c("lower","middle","upper"),
                                                          fut_cols = "scenario",
                                                          counterfactual = "planned",
                                                          year_start = 2024,
                                                          year_end = 2026) %>%
  rename(Extension="setting")

avertedplanned_expectedDirectDeaths_Ext <- casesAverted.sumAcrossYears(expectedDirectDeaths_df %>% select(-setting) %>%
                                                                         rename(setting="Extension"),
                                                                       cases_col = "expectedDirectDeaths",
                                                                       level_aggr = "setting",
                                                                       eir_col = "EIR_type",
                                                                       EIR_names = c("lower","middle","upper"),
                                                                       fut_cols = "scenario",
                                                                       counterfactual = "planned",
                                                                       year_start = 2024,
                                                                       year_end = 2026) %>%
  rename(Extension="setting")


figurea <- ggplot(avertedplanned_OMcases_Ext %>%
                    filter(Extension == scenario) %>%
                     mutate(Dpts=ifelse(Extension=="Demo","Demographic (Alibori+Atacora)",
                                        "Geographic (Borgou+Collines+Donga)")),
                   aes(x=Dpts,y=ca.tot.mean,ymin=ca.tot.inf,ymax=ca.tot.sup,fill=Dpts))+
  geom_col(alpha=c_alpha)+
  geom_errorbar(width=.5)+
  labs(title="All averted episodes")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  scale_fill_manual(values=c(color_demo,color_geo),name="Extension")+
  theme_minimal(base_size = b_size)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
        axis.text.x = element_blank(),legend.position = "bottom",
        plot.title = element_text(size=40,hjust = 0.5))

figureb <- ggplot(avertedplanned_nSevere_Ext %>%
                    filter(Extension == scenario) %>%
                     mutate(Dpts=ifelse(Extension=="Demo","Demographic (Alibori+Atacora)",
                                        "Geographic (Borgou+Collines+Donga)")),
                   aes(x=Dpts,y=ca.tot.mean,ymin=ca.tot.inf,ymax=ca.tot.sup,fill=Dpts))+
  geom_col(alpha=c_alpha)+
  geom_errorbar(width=.5)+
  labs(title="Averted severe cases")+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_fill_manual(values=c(color_demo,color_geo),name="Extension")+
  theme_minimal(base_size = b_size)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
        axis.text.x = element_blank(),legend.position = "bottom",
        plot.title = element_text(size=40,hjust = 0.5))

figurec <- ggplot(avertedplanned_expectedDirectDeaths_Ext %>%
                    filter(Extension == scenario) %>%
                     mutate(Dpts=ifelse(Extension=="Demo","Demographic (Alibori+Atacora)",
                                        "Geographic (Borgou+Collines+Donga)")),
                   aes(x=Dpts,y=ca.tot.mean,ymin=ca.tot.inf,ymax=ca.tot.sup,fill=Dpts))+
  geom_col(alpha=c_alpha)+
  geom_errorbar(width=.5)+
  labs(title="Averted malaria deaths")+
  scale_fill_manual(values=c(color_demo,color_geo),name="Extension")+
  theme_minimal(base_size = b_size)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
        axis.text.x = element_blank(),legend.position = "bottom",
        plot.title = element_text(size=40,hjust = 0.5))

plot_grid(plot_grid(figurea + theme(legend.position="none"),
                    figureb + theme(legend.position="none"),
                    figurec + theme(legend.position="none"),
                    labels = c('A', 'B','C'),nrow=1,label_size = 40),
          get_legend(figurea), ncol = 1, rel_heights = c(1, .1))

ggsave(file = paste0(supportinginfodir, "cases_averted_absolute_ext_horizontal.png"),
       height = 12, width = 25)

