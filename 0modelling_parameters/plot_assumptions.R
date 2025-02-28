#################################
# Plots of assumptions (figure 2 and supporting info)
#
# created 05.12.2024 by Jeanne Lemant
#################################

library(tidyverse)
library(scales)
library(ggnewscale)
library(readxl)
library(sf)

rm( list =  ls() )

#### Preparation ####

# where to save the figures

figdir = "C:/Users/lemaje/switchdrive/Institution/AIM/7. Internal manuscripts/BEN_SMC/Review/figures/"
supportinginfodir <- "C:/Users/lemaje/switchdrive/Institution/AIM/7. Internal manuscripts/BEN_SMC/Review/figures_supportinginfo/"

# set working directory to where this file is saved

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

firstup <- function(x) {
  substr(x, 1, 1) <- toupper( substr(x, 1, 1) )
  x
}

# load data

countrydat <- read.csv("../data/BEN_countrydat_oct2022_ITN_MAP_surveys.csv")

source("netdurability.R")

pop_projected <- read.csv("../data/commune_pop_from2013census_3.51growth_rate.csv", sep = ";") %>%
  rename(sub="name")

correspondence_names = read.csv(
  file.path("../data/BEN_admin1_admin2_names.csv"))

correspondence_communes_settings = read.csv(
  file.path("../data/correspondence_communes_to_IRS_SMC_settings.csv"))

correspondence_communes_settings = correspondence_communes_settings%>%
  mutate(sub=tolower(Admin2))

good_names <- rbind(correspondence_communes_settings,
                    correspondence_names %>%
                      filter(!(sub %in% correspondence_communes_settings$sub))) %>%
  select(-Admin2) %>%
  rename(Admin2 = "sub")

PopAgeGender <- readxl::read_xlsx("../data/pop_structure_RGPH4.xlsx")%>%
  setNames(c("Age","Total","M","F"))%>%
  mutate(Age = str_remove_all(Age, " ans"))

# case management
# CM = read.csv("../data/DHS_MICS_eff_cov_CM.csv") %>%
#   select(admin1,year,access,eff_cov) %>%
#   mutate(Admin1=firstup(admin1)) %>%
#   select(-admin1)

# load shapefiles for maps

# ZS_shp_3COT <- file.path("../data/BEN_ZS/BEN_ZS_dissolved_Shp_BEN.shp") %>%
#   sf::st_read(quiet = TRUE)
# #to merge ZS of Cotonou
# ZS_shp_1COT_prep <- ZS_shp_3COT %>%
#   mutate(ZS_1stcommune=ifelse(grepl("COTONOU",ZS),"COTONOU",ZS),
#          Admin1=case_when(NEW_DEPT=="Ouémé" ~ "Oueme",
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
  mutate(Admin2=tolower(NAME1_),
         Admin1=case_when(Admn1_N=="Atakora"~"Atacora",
                          Admn1_N=="Kouffo"~"Couffo",
                          Admn1_N=="Ouémé"~"Oueme",
                          TRUE~Admn1_N))

#### Population increase ####

pop_projected_long <- pop_projected %>%
  pivot_longer(cols=pop2013:pop2000,names_to = "year",values_to = "pop_All",
               names_prefix = "pop") %>%
  mutate(year=as.numeric(year))

pop_map <- left_join(pop_projected_long %>%
                       rename(Admin2="sub"),
                     Admin2_shp,
                     by = "Admin2")

ggplot()+
  geom_sf(data=pop_map %>% filter(year %in% c(2013,2020,2026)),
          aes(geometry=geometry,fill=pop_All/1000),
          color="gray40",linewidth=.5)+
  #geom_sf(data = Admin1_shp, fill = NA, linewidth = 1.05, color = "gray40") +
  facet_wrap(~year)+
  theme_void(base_size = 40)+
  scale_fill_gradient(low = "#ffffb2", high = "#bd0026",name="Population\n(thousands)")

ggsave(filename=paste0(supportinginfodir,"pop_commune.png")
       ,width = 25,height=15)

#### Population structure ####

PopAgeGender$prop=PopAgeGender$Total/PopAgeGender$Total[dim(PopAgeGender)[1]]

ggplot(PopAgeGender %>% filter(Age!="BENIN"),
       aes(x=Age,y=prop))+
  geom_col()+
  scale_y_continuous(labels=scales::percent)+
  labs(x="Age group",y="")+
  theme_minimal(base_size=30)

ggsave(filename=paste0(supportinginfodir,"pop_structure.png")
       ,width = 25,height=10)


#### Seasonality ####

countrydat %>%
  select(Admin1,starts_with("m")) %>%
  distinct() %>%
  pivot_longer(cols=m1:m12,names_to="month",values_to="incidence",names_prefix = "m") %>%
  mutate(month=as.integer(month)) %>%
  ggplot()+
  geom_line(aes(x=month,y=incidence),lwd=4)+
  scale_x_continuous(breaks=seq(3,12,3))+
  facet_wrap(~ Admin1)+
  labs(x="Month",title="Average malaria incidence per 1000 between 2011 and 2017")+
  theme_minimal(base_size=40)+
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=40,hjust=.5))

ggsave(filename=paste0(supportinginfodir,"seasonality.png")
       ,width = 25,height=15)

#### ITN use ####

histITNcovsurveys <- countrydat %>% 
  select(Admin1,starts_with("histITNcovsurveys")) %>%
  distinct() %>%
  mutate(histITNcovsurveys2020=histITNcovsurveys2017,
         histITNcovsurveys2021=smoothcompact_function(L=ITNdecay_P2$L,init=histITNcovsurveys2020,t=1),
         histITNcovsurveys2022=smoothcompact_function(L=ITNdecay_P2$L,init=histITNcovsurveys2020,t=2),
         histITNcovsurveys2023=histITNcovsurveys2017,
         histITNcovsurveys2024=histITNcovsurveys2021,
         histITNcovsurveys2025=histITNcovsurveys2022,
         histITNcovsurveys2023DN=histITNcovsurveys2017,
         histITNcovsurveys2024DN=smoothcompact_function(L=ITNdecay_Duranet$L,init=histITNcovsurveys2023,t=1),
         histITNcovsurveys2025DN=smoothcompact_function(L=ITNdecay_Duranet$L,init=histITNcovsurveys2023,t=2),
         histITNcovsurveys2026=histITNcovsurveys2017,
         histITNcovsurveys2026DN=histITNcovsurveys2017) %>%
  pivot_longer(cols=histITNcovsurveys2011:histITNcovsurveys2026DN,
               names_to="year",values_to="histITNcov",names_prefix = "histITNcovsurveys") %>%
  mutate(DN=grepl("DN",year)) %>%
  filter(!DN|Admin1=="Atacora") %>%
  mutate(year=as.integer(gsub(pattern = "DN","",year))) %>%
  mutate(halflife=case_when(year<2020~"3 years",
                            DN~"1.6 year (polyethylene)",
                            TRUE~"2 years (polyester)")) %>%
  select(-DN) %>%
  mutate(source="from DHS/MIS (dots)")

histITNcovMAP <- countrydat %>%
  select(Admin1,starts_with("histITNcovMAP")) %>%
  distinct() %>%
  pivot_longer(cols=histITNcovMAP2000:histITNcovMAP2019,
               names_to="year",values_to="histITNcov",names_prefix = "histITNcovMAP") %>%
  mutate(year=as.integer(year)) %>%
  mutate(source="MAP",halflife=NA)

ITN_hyp <- rbind(histITNcovsurveys %>% filter(year>=2011),
                 histITNcovsurveys %>%
                   filter(year==2020) %>%
                   mutate(halflife="3 years"),
                 histITNcovMAP %>% filter(year<2011),
                 histITNcovsurveys %>%
                   filter(year==2011) %>%
                   mutate(source="MAP",halflife=NA)) %>%
  mutate(source=factor(source,levels=c("MAP","from DHS/MIS (dots)")))

ggplot(ITN_hyp,aes(x=year,y=histITNcov,color=halflife,linetype=source))+
  geom_line(lwd=2)+
  geom_point(data=histITNcovsurveys %>% filter(year %in% c(2011,2015,2017)),size=8)+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(breaks=seq(2005,2025,10))+
  scale_linetype_manual(name="Source",values=c(3,1))+
  scale_color_manual(name="Halflife",values=c("#00a9a5","#0b5351","#092327"),
                     limits=c("1.6 year (polyethylene)","2 years (polyester)","3 years"))+
  facet_wrap(~ Admin1)+
  labs(x="Year",y="ITN use")+
  theme_minimal(base_size=30)+
  theme(legend.position = "bottom",legend.box = "vertical")+
  guides(linetype = guide_legend(order = 1),
         color = guide_legend(order = 2,
                              override.aes = list(size = 1) ))

ggsave(filename=paste0(supportinginfodir,"histnetuse.png")
       ,width = 18,height=15)

#### ITN type ####

# types of nets distributed in 2020 and 2023 (source: NMCP)
campaigns <- read.csv("../data/ITNtypes_distributions_2020_2023.csv",
                           check.names=FALSE) %>%
  dplyr::select(Admin1,Admin2,ITNtype2020,ITNtype2023) %>%
  mutate(ITNtype2020=ifelse(ITNtype2020=="standard","futITNWeak","futPBO"),
         ITNtype2023=ifelse(ITNtype2023=="standard","futITNWeak","futPBO")) %>%
  mutate(Admin2=ifelse(Admin2=="adjarra","adjara",Admin2),
         Admin2=ifelse(Admin2=="cobly","kobli",Admin2),
         Admin2=ifelse(Admin2=="copargo","kopargo",Admin2),
         Admin2=ifelse(Admin2=="oussa-pehunco","pehonko",Admin2),
         Admin2=ifelse(Admin2=="seme-kpodji","seme",Admin2),
         Admin2=ifelse(Admin2=="toucountouna","toukountouna",Admin2),
         Admin2=ifelse(Admin2=="zogbodomey","zogbodome",Admin2)) %>%
  mutate(ITNtype2023=ifelse(Admin2=="parakou","futPBO",ITNtype2023)) %>%
  mutate(ITNtype2023 = ifelse(Admin2 %in% c("malanville","kandi","banikoara","gogounou"),
                              "next-generation polyethylene", ITNtype2023)) %>%
  mutate(ITNtype2026 = "futPBO") %>%
  pivot_longer(cols = starts_with("ITNtype"),names_to = "year",
               values_to = "ITNtype", names_prefix = "ITNtype") %>%
  mutate(ITNtype = case_when(ITNtype == "futITNWeak" ~ "standard polyester",
                             ITNtype == "futPBO" ~ "next-generation polyester",
                             TRUE ~ ITNtype)) %>%
  mutate(ITNtype = factor(ITNtype, levels = c("standard polyester",
                                              "next-generation polyester",
                                              "next-generation polyethylene")))

Admin2_shp$Admin2[!Admin2_shp$Admin2 %in% campaigns$Admin2]
campaigns$Admin2[!campaigns$Admin2 %in% Admin2_shp$Admin2]

Admin2_shp_nets2020 <- Admin2_shp %>%
  left_join(campaigns)

ggplot()+
  geom_sf(data=Admin2_shp_nets2020,
          aes(geometry=geometry,fill=ITNtype),
          color="gray40",linewidth=.5)+
  #geom_sf(data = Admin1_shp, fill = NA, linewidth = 1.05, color = "gray40") +
  facet_wrap(~year)+
  theme_void(base_size = 60)+
  scale_fill_manual(values=c("palegreen","#40916c","#74c69d"),
                    na.value="white",name="",na.translate=FALSE)+
  theme(legend.position = "bottom")

ggsave(filename=paste0(supportinginfodir,"futITNtypes.png")
       ,width = 25,height=15)


#### IRS ####

countrydat_IRS_long <- countrydat %>%
  select(setting,starts_with("histIRScov")) %>%
  pivot_longer(cols=starts_with("histIRScov"),names_to = c("insecticide","year"),
               values_to = "cov",names_prefix = "histIRScov",
               names_pattern = "([A-Za-z]+)(\\d+)") %>%
  mutate(year=as.integer(year)) %>%
  filter(!is.na(cov)) %>%
  pivot_wider(names_from="insecticide",values_from="cov")

sub_IRS2020 = c("kopargo","djougou","gogounou","kandi","segbana","kouande")

sub_IRS2021 = c("ouake","segbana")

IRSmap.sf <- as.data.frame(lapply(Admin2_shp, rep, 2021-2008+1)) %>%
  mutate(year=rep(2008:2021,each=77)) %>%
  left_join(good_names) %>%
  left_join(countrydat_IRS_long,by=c("year","setting")) %>%
  mutate(ActellicCS=ifelse(year==2020 & Admin2 %in% sub_IRS2020,.9,ActellicCS),
         ActellicCS=ifelse(year==2021 & Admin2 %in% sub_IRS2021,.9,ActellicCS))

ggplot(IRSmap.sf)+
  geom_sf(aes(geometry = geometry, fill = Bendiocarb))+
  scale_fill_gradient(low = "white", high = "#66c2a5",name="     Bendiocarb",
                      labels=percent,na.value="transparent",limits=c(0,1),breaks=c(0,1)) +
  new_scale_fill() +
  geom_sf(aes(geometry = geometry, fill = ActellicCS))+
  scale_fill_gradient(low = "white", high = "#fc8d62",name="Actellic 300CS",
                      labels=percent,na.value="transparent",limits=c(0,1),breaks=c(0,1))+
  new_scale_fill() +
  geom_sf(aes(geometry = geometry, fill = ActellicEC))+
  scale_fill_gradient(low = "white", high = "#8da0cb",name="    Actellic 50EC",
                      labels=percent,na.value="transparent",limits=c(0,1),breaks=c(0,1))+
  facet_wrap(~year,nrow=2)+
  #geom_sf(data = Admin1_shp, fill = NA, linewidth = 1.05, color = "gray40") +
  theme_void(base_size = 40)+
  theme(legend.position = "bottom",legend.spacing.x = unit(1.0, "cm"))

ggsave(filename=paste0(supportinginfodir,"IRS_continuous.png"),
       width = 25,height=15)

#### Case management ####

### cascade is produced with internal code

# dpts = CM %>% 
#   filter(Admin1!="Benin") %>%
#   distinct(Admin1) %>%
#   pull()
# present2001_dpts = CM %>%
#   filter(Admin1!="Benin" , year==2001) %>%
#   select(Admin1) %>%
#   pull()
# missing2001_dpts = setdiff(dpts,present2001_dpts)
# 
# additional2001 = data.frame(Admin1=missing2001_dpts
#                             ,year=2001
#                             ,access= CM %>%
#                               filter(Admin1=="Benin",year==2001) %>%
#                               select(access)
#                             ,eff_cov = CM %>% 
#                               filter(Admin1=="Benin",year==2001) %>% 
#                               select(eff_cov)) %>%
#   mutate(source="national")
# 
# updatedCM = bind_rows(CM %>% mutate(source="departmental"),additional2001) %>%
#   filter(Admin1!="Benin")
# 
# updatedCM_long <- updatedCM %>%
#   pivot_longer(cols = access:eff_cov,)
# 
# ggplot(updatedCM_long,aes(x=year,y=value,color=name))+
#   geom_line(lwd=2)+
#   geom_point(aes(shape=source),size=8)+
#   scale_y_continuous(labels=scales::percent)+
#   labs(x="Year",y="")+
#   scale_shape_manual(values=c(16,17),labels=c("Departmental level","National level"),
#                      name="Survey")+
#   scale_color_manual(name="",values=c(rgb(0,191,196,maxColorValue = 255),
#                                       rgb(67,205,128,maxColorValue = 255)),
#                      labels=c("Children with fever seeking care","Effective treatment coverage"))+
#   facet_wrap(~ Admin1)+
#   theme_minimal(base_size=30)+
#   theme(legend.position = "bottom",legend.box = "vertical")
# 
# ggsave(filename=paste0(supportinginfodir,"access_EffCov.png")
#        ,width = 18,height=15)

