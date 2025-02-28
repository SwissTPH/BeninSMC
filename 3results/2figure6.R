#################################
# Figure 6 (demographic versus geographic extension)
#
# modified 28.11.2024 by Jeanne Lemant
#################################

rm(list=ls())

library(cowplot)
library(scales)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./1define_scenarios.R")

figdir = "C:/Users/lemaje/switchdrive/Institution/AIM/7. Internal manuscripts/BEN_SMC/Review/figures/"

targeted_pop_Ext <- futrs %>% filter(year %in% 2024:2026,seed==1,EIR_type=="middle") %>%
  filter((Extension=="Demo"&scenario=="Demo"&age=="5-10")|
           (Extension=="Geo" &scenario=="Geo"&age=="0-5"))%>%
  group_by(Extension,age,year,scenario) %>%
  summarise(pop=sum(pop)) %>%
  group_by(Extension,age,scenario) %>%
  summarise(pop=sum(pop)) %>%
  ungroup() %>%
  rename(target_pop="pop") %>% select(-age)

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

color_demo <- "#ffa400"
color_geo <- "#009ffd"
c_alpha <- .5
b_size <- 50

figure6a <- ggplot(avertedplanned_OMcases_Ext %>%
                       right_join(targeted_pop_Ext) %>%
                       mutate(across(starts_with("ca"),~./target_pop*10^3)) %>%
                       mutate(Dpts=ifelse(Extension=="Demo","Demographic (Alibori+Atacora)",
                                          "Geographic (Borgou+Collines+Donga)")),
                     aes(x=Dpts,y=ca.tot.mean,ymin=ca.tot.inf,ymax=ca.tot.sup,fill=Dpts))+
  geom_col(alpha=c_alpha)+
  geom_errorbar(width=.5)+
  labs(title="All averted episodes\nper thousand")+
  scale_y_continuous(limits=c(-50,900))+
  scale_fill_manual(values=c(color_demo,color_geo),name="Extension")+
  theme_minimal(base_size = b_size)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
        axis.text.x = element_blank(),legend.position = "bottom",
        plot.title = element_text(size=40,hjust = 0.5))

figure6b <- ggplot(avertedplanned_nSevere_Ext %>%
                       right_join(targeted_pop_Ext) %>%
                       mutate(across(starts_with("ca"),~./target_pop*10^3)) %>%
                       mutate(Dpts=ifelse(Extension=="Demo","Demographic (Alibori+Atacora)",
                                          "Geographic (Borgou+Collines+Donga)")),
                     aes(x=Dpts,y=ca.tot.mean,ymin=ca.tot.inf,ymax=ca.tot.sup,fill=Dpts))+
  geom_col(alpha=c_alpha)+
  geom_errorbar(width=.5)+
  labs(title="Averted severe cases\nper thousand")+
  scale_fill_manual(values=c(color_demo,color_geo),name="Extension")+
  theme_minimal(base_size = b_size)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
        axis.text.x = element_blank(),legend.position = "bottom",
        plot.title = element_text(size=40,hjust = 0.5))

figure6c <- ggplot(avertedplanned_expectedDirectDeaths_Ext %>%
                                    right_join(targeted_pop_Ext) %>%
                                    mutate(across(starts_with("ca"),~./target_pop*10^6)) %>%
                                    mutate(Dpts=ifelse(Extension=="Demo","Demographic (Alibori+Atacora)",
                                                       "Geographic (Borgou+Collines+Donga)")),
                                  aes(x=Dpts,y=ca.tot.mean,ymin=ca.tot.inf,ymax=ca.tot.sup,fill=Dpts))+
  geom_col(alpha=c_alpha)+
  geom_errorbar(width=.5)+
  labs(title="Averted malaria deaths\nper million")+
  scale_fill_manual(values=c(color_demo,color_geo),name="Extension")+
  theme_minimal(base_size = b_size)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
        axis.text.x = element_blank(),legend.position = "bottom",
        plot.title = element_text(size=40,hjust = 0.5))

# legend does not show with ggplot version > 3.4
get_legend_35 <- function(plot) {
  # return all legend candidates
  legends <- get_plot_component(plot, "guide-box", return_all = TRUE)
  # find non-zero legends
  nonzero <- vapply(legends, \(x) !inherits(x, "zeroGrob"), TRUE)
  idx <- which(nonzero)
  # return first non-zero legend if exists, and otherwise first element (which will be a zeroGrob) 
  if (length(idx) > 0) {
    return(legends[[idx[1]]])
  } else {
    return(legends[[1]])
  }
}

plot_grid(plot_grid(figure6a + theme(legend.position="none"),
                    figure6b + theme(legend.position="none"),
                    figure6c + theme(legend.position="none"),
                    labels = c('A', 'B','C'),nrow=1,label_size = 40),
          get_legend_35(
            figure6a),ncol = 1, rel_heights = c(1, .1))

ggsave(file = paste0(figdir, "Figure6.png"),
       height = 12, width = 25)
ggsave(file = paste0(figdir, "Figure6.svg"),
       height = 12, width = 25)
ggsave(file = paste0(figdir, "Figure6.tiff"),
       height = 12, width = 25, device='tiff')
