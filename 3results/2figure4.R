#################################
# Figure 4 (validation of calibration against cases)
#
# modified 28.11.2024 by Jeanne Lemant
#################################

rm(list=ls())

library(cowplot)
library(wesanderson)
library(scales)
library(readxl)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./1define_scenarios.R")

WMR2024 = read_xlsx(path = "../data/wmr2024_annex_4f.xlsx",
                    range = "B52:F75",col_names = c("year","WHO_pop",
                                                    "totCases.inf","totCases.mean","totCases.sup"))

BENdir <- "C:/Users/lemaje/switchdrive/Institution/AIM/1 Country Support/Benin/"
DHIS2<-read.csv( paste0( 
  BENdir, "3. Data/2. Processed data/3. Malaria outcomes/",
  "National data/2011-2019_DHIS2_long.csv" ))

figdir = "C:/Users/lemaje/switchdrive/Institution/AIM/7. Internal manuscripts/BEN_SMC/Review/figures/"

OMcases_All = computeAggrTimeSeriesCasesInc(OMcases_df
                                            , cases_col = "OMcases"
                                            , level_aggr = "national"
                                            , pop_col = "pop"
                                            , eir_col = "EIR_type"
                                            , EIR_names = c("lower","middle","upper")
                                            , fut_cols = "scenario"
                                            , convert_pop = FALSE)

OMcases_0to5 = computeAggrTimeSeriesCasesInc(OMcases_dfU5
                                            , cases_col = "OMcases"
                                            , level_aggr = "national"
                                            , pop_col = "pop"
                                            , eir_col = "EIR_type"
                                            , EIR_names = c("lower","middle","upper")
                                            , fut_cols = "scenario"
                                            , convert_pop = FALSE)

nTreat_All = computeAggrTimeSeriesCasesInc(nTreat_df
                                           , cases_col = "nTreat"
                                           , level_aggr = "national"
                                           , pop_col = "pop"
                                           , eir_col = "EIR_type"
                                           , EIR_names = c("lower","middle","upper")
                                           , fut_cols = "scenario"
                                           , convert_pop = FALSE)

nTreat_0to5 = computeAggrTimeSeriesCasesInc(nTreat_dfU5
                                           , cases_col = "nTreat"
                                           , level_aggr = "national"
                                           , pop_col = "pop"
                                           , eir_col = "EIR_type"
                                           , EIR_names = c("lower","middle","upper")
                                           , fut_cols = "scenario"
                                           , convert_pop = FALSE)

combined_indicators = rbind(OMcases_All %>% mutate(indicator = "Modelled episodes"),
                            OMcases_0to5 %>% mutate(indicator = "Modelled episodes"),
                            nTreat_All %>% mutate(indicator = "Modelled treated cases"),
                            nTreat_0to5 %>% mutate(indicator = "Modelled treated cases"))
  
WMR2024_plot <- WMR2024 %>%
  mutate(age = "0-100", .after = "WHO_pop") %>%
  mutate(across(.cols = totCases.inf:totCases.sup, .fns = ~./WHO_pop*1000,
                .names = "inc{.col}")) %>%
  rename_with(.fn = ~ gsub("totCases","",.), .cols = starts_with("inc")) %>%
  mutate(indicator = "WMR 2024")

pop_national <- pop_projected_long %>%
  group_by(year) %>%
  summarise(pop_All = sum(pop_All))

DHIS2_plot <- DHIS2 %>% 
  group_by(year,type) %>% 
  summarise(totCases.mean = sum(value)) %>%
  filter(type == "confirmed_total",year<2020) %>%
  mutate(type="Reported confirmed cases") %>%
  left_join(pop_national) %>%
  mutate(inc.mean = totCases.mean / pop_All * 1000,
         indicator = "Reported cases",
         age = "0-100")

combined_indicators_allsources = rbind(combined_indicators, WMR2024_plot, DHIS2_plot)


##### general settings
c_alpha <- .5
indicator_colours <- c("#264653","#2a9d8f","#e9c46a","#f4a261")
names(indicator_colours) <- c("Modelled episodes", "Modelled treated cases","WMR 2024", "Reported cases")
b_size <- 40

figure4a <- ggplot(combined_indicators_allsources %>% filter(age == "0-100"),
                   aes(x = year, ymin = totCases.inf, y = totCases.mean,
                       ymax = totCases.sup, fill = indicator, colour = indicator))+
  geom_ribbon(alpha = c_alpha)+
  geom_line(size = 2)+
  xlim(2000, 2023)+
  theme_minimal(base_size = b_size)+
  scale_colour_manual(name = "", values = indicator_colours)+
  scale_fill_manual(name = "", values = indicator_colours)+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  labs(x = "Year", title = "Malaria cases in all ages")+
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=40,hjust=.5),
        legend.key.width = unit(1,"cm"))

figure4b <- ggplot(combined_indicators %>% filter(indicator == "Modelled episodes") %>%
                     mutate(age = paste0(age, " ")),
                   aes(x = year, ymin = inc.inf, y = inc.mean,
                       ymax = inc.sup, 
                       linetype = age))+
  geom_ribbon(alpha = c_alpha, fill = indicator_colours[1])+
  geom_line(size = 2, color = indicator_colours[1])+
  xlim(2000, 2023)+
  theme_minimal(base_size = b_size)+
  scale_y_continuous(labels=function(x) format(x, big.mark = " "))+
  scale_colour_manual(values = indicator_colours, guide = "none")+
  scale_fill_manual(values = indicator_colours, guide = "none")+
  labs(x = "Year", title = "Incidence of modelled malaria\nepisodes per 1000")+
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=40,hjust=.5),
        legend.key.width = unit(1,"cm"))+
  guides(linetype = guide_legend(title = "Age group"))

plot_grid(plot_grid(figure4a + theme(legend.position="none"),
          figure4b + theme(legend.position="none"),
          labels = c('A', 'B'), ncol = 1, label_size = 40),
          plot_grid(get_legend(figure4a) ,
                    get_legend(figure4b) , ncol = 1),
          rel_widths = c(1, .4)
)

ggsave(file = paste0(figdir, "Figure4.png"),
       height = 15, width = 20)
ggsave(file = paste0(figdir, "Figure4.svg"),
       height = 15, width = 30)
ggsave(file = paste0(figdir, "Figure4.tiff"),
       height = 15, width = 30, device='tiff')
