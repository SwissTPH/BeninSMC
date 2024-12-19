#################################
# Figure 3 (validation of calibration against cases)
#
# modified 28.11.2024 by Jeanne Lemant
#################################

rm(list=ls())

library(cowplot)
library(wesanderson)
library(ggh4x)
library(readxl)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("./1define_scenarios.R")

WMR2024 = read_xlsx(path = "../data/wmr2024_annex_4f.xlsx",
                    range = "B52:I75",col_names = c("year","WHO_pop",
                                                    "totCases.inf","totCases.mean","totCases.sup",
                                                    "totDeaths.inf","totDeaths.mean","totDeaths.sup"))

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

nSevere_All = computeAggrTimeSeriesCasesInc(nSevere_df
                                           , cases_col = "nSevere"
                                           , level_aggr = "national"
                                           , pop_col = "pop"
                                           , eir_col = "EIR_type"
                                           , EIR_names = c("lower","middle","upper")
                                           , fut_cols = "scenario"
                                           , convert_pop = FALSE)

nSevere_0to5 = computeAggrTimeSeriesCasesInc(nSevere_dfU5
                                           , cases_col = "nSevere"
                                           , level_aggr = "national"
                                           , pop_col = "pop"
                                           , eir_col = "EIR_type"
                                           , EIR_names = c("lower","middle","upper")
                                           , fut_cols = "scenario"
                                           , convert_pop = FALSE)

expectedDirectDeaths_All = computeAggrTimeSeriesCasesInc(expectedDirectDeaths_df
                                            , cases_col = "expectedDirectDeaths"
                                            , level_aggr = "national"
                                            , pop_col = "pop"
                                            , eir_col = "EIR_type"
                                            , EIR_names = c("lower","middle","upper")
                                            , fut_cols = "scenario"
                                            , convert_pop = FALSE)

expectedDirectDeaths_0to5 = computeAggrTimeSeriesCasesInc(expectedDirectDeaths_dfU5
                                                         , cases_col = "expectedDirectDeaths"
                                                         , level_aggr = "national"
                                                         , pop_col = "pop"
                                                         , eir_col = "EIR_type"
                                                         , EIR_names = c("lower","middle","upper")
                                                         , fut_cols = "scenario"
                                                         , convert_pop = FALSE)


combined_indicators = rbind(OMcases_All %>% mutate(indicator = "Episodes"),
                            OMcases_0to5 %>% mutate(indicator = "Episodes"),
                            nTreat_All %>% mutate(indicator = "Treatments"),
                            nTreat_0to5 %>% mutate(indicator = "Treatments"),
                            nSevere_All %>% mutate(indicator = "nSevere"),
                            nSevere_0to5 %>% mutate(indicator = "nSevere"),
                            expectedDirectDeaths_All %>% mutate(indicator = "deaths"),
                            expectedDirectDeaths_0to5 %>% mutate(indicator = "deaths")) %>%
  pivot_longer(cols = totCases.mean:inc.inf, names_to = c("absolute_or_incidence", "bound"),
               values_to = "value", names_sep = "\\.") %>%
  mutate(value = ifelse(absolute_or_incidence == "inc", value, value/1000),
         absolute_or_incidence = ifelse(absolute_or_incidence == "inc", "Incidence per 1000", "Absolute numbers (thousands)"),
         scale_cases = case_when(indicator %in% c("Episodes","Treatments") ~ "All cases",
                                 indicator == "nSevere" ~ "Severe cases only",
                                 indicator == "deaths" ~ "Malaria deaths")) %>%
  pivot_wider(names_from = bound, values_from = value)
  
WMR2024_plot <- WMR2024 %>%
  mutate(age = "0-100", .after = "WHO_pop") %>%
  mutate(across(.cols = totCases.inf:totDeaths.sup, .fns = ~./WHO_pop*1000,
                .names = "{.col}_incidence")) %>%
  pivot_longer(cols = totCases.inf:totDeaths.sup_incidence, names_to = c("indicator", "bound"),
               values_to = "value", names_sep = "\\.") %>%
  mutate(value = ifelse(grepl("incidence", bound), value, value/1000),
         absolute_or_incidence = ifelse(grepl("incidence", bound), "Incidence per 1000", "Absolute numbers (thousands)")) %>%
  mutate(bound = gsub("_incidence","",bound)) %>%
  mutate(value = ifelse(absolute_or_incidence == "totCases", value/1000, value)) %>%
  mutate(scale_cases = ifelse(indicator == "totCases", "All cases", "Malaria deaths"),
         indicator = "WMR 2024")  %>%
  pivot_wider(names_from = bound, values_from = value) 

pop_national <- pop_projected_long %>%
  group_by(year) %>%
  summarise(pop_All = sum(pop_All))

DHIS2_plot <- DHIS2 %>% 
  group_by(year,type) %>% 
  summarise(value=sum(value)) %>%
  filter(type == "confirmed_total",year<2020) %>%
  mutate(type="Reported confirmed cases") %>%
  left_join(pop_national) %>%
  mutate(incidence = value / pop_All * 1000,
         value = value / 1000,
         age = "0-100") %>%
  pivot_longer(cols = c("value", "incidence"),
               names_to = "absolute_or_incidence", values_to = "mean") %>%
  mutate(absolute_or_incidence = ifelse(absolute_or_incidence == "incidence",
                                        "Incidence per 1000", "Absolute numbers (thousands)")) %>%
  mutate(scale_cases = "All cases",
         indicator = "Reported cases")

combined_indicators_WMR2024 = rbind(combined_indicators, WMR2024_plot, DHIS2_plot) %>%
  mutate(scale_cases = factor(scale_cases, levels = c("All cases", "Severe cases only", "Malaria deaths")))


##### general settings
c_alpha <- .5
my_linetypes <- c("solid","solid","solid","dashed","dotted","twodash")
names(my_linetypes) <- c("Episodes", "nSevere","deaths","Treatments","WMR 2024", "Reported cases")
b_size <- 40

ggplot(combined_indicators_WMR2024,
       aes(x = year, y = mean,
           ymin = inf, ymax = sup,
           colour = age, fill = age,
           group = interaction(indicator, age)))+
  geom_ribbon(alpha = c_alpha)+
  geom_line(aes(linetype = indicator),size = 2)+
  xlim(2000, 2023)+
  theme_minimal(base_size = b_size)+
  labs(x = "Year")+
  ggh4x::facet_grid2(absolute_or_incidence~scale_cases, scales = "free_y", independent = "y")+
  scale_color_manual(values = c("darkgrey", "#dd6e42"), name = "Age group")+
  scale_linetype_manual( name = "", values = my_linetypes,
                         breaks = c("Episodes", "Treatments", "WMR 2024", "Reported cases"))+
  scale_fill_manual(values = c("grey", "#dd6e42"), name = "Age group")+
  expand_limits(y = 0)+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.justification.bottom = "left")+
  guides(linetype = guide_legend(order = 1, nrow=2, byrow=TRUE,
                                 override.aes = list(size = 20)))

ggsave(file = paste0(figdir, "Figure3.png"),
       height = 18, width = 22)
ggsave(file = paste0(figdir, "Figure3.svg"),
       height = 15, width = 22)
