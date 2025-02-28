#' Realistic net attrition for the future net distributions (from 2020)

library(minpack.lm)
library(ggplot2)
library(tidyverse)

### one of the preset functions for decay in OpenMalaria
smoothcompact_function = function(time, k=2.14285714285714, L,init_cov=1){
  return(init_cov*(time<L)*exp( k - k/(1 - (time/L)^2) ))
}

###' function to fit total life L and initial coverage of nets
###' from a vector of coverages
fit_ITN_decay = function( distrib_year = 2011, distrib_month=1, distrib_day = 5
                          , coverages
                          , years
                          , months
                          , days=NULL
                          , k=2.14285714285714
                          , nls_control = list(maxiter = 100)) {
  #' fit half-life and initial coverage from ITN coverage values
  #' @param distrib_year year of initial distribution
  #' @param distrib_month month of initial distribution
  #' @param distrib_day day of initial distribution
  #' @param coverages vector of ITN coverage values
  #' @param years vector of years corresponding to coverage values
  #' @param months vector of months corresponding to coverage values
  #' @param days vector of days corresponding to coverage values
  #' @param nls_control maximal number of iterations for fitting
  #' @param k shape parameter, fixed in ITN decay parameterisations
  #' @export
  #' @importFrom minpack.lm nlsLM
  #' @examples fit_ITN_decay(coverages=c(.6,.3,.2),years=rep(2012,3),months=c(1,9,10))

  if(length(coverages)!=length(years)) stop("Coverages and years should be of the same length")
  if(length(coverages)!=length(months)) stop("Coverages and months should be of the same length")
  if(length(years)!=length(months)) stop("Years and months should be of the same length")

  tab=NULL
  tab$values=coverages
  tab$time=years-distrib_year+(months-distrib_month)/12
  if(!is.null(days)){tab$time=tab$time+(days-distrib_day)/365}
  tab = as.data.frame(tab)

  if(years[1]==distrib_year&months[1]==distrib_month){
    a<-coverages[1]
    #we know the initial coverage
    smoothcompact_fit = nlsLM(values ~ a * (c(1) - (time >= L)) * exp((c(1) - (time >= L)) *k* (c(1) - 1/(1 -(time/L)^2)))
                              , data = tab
                              , start = list(L = 6)
                              , control = nls_control)
    init_cov<-a
    L<-smoothcompact_fit$m$getPars()[1]
  }else{smoothcompact_fit = nlsLM(values ~ a * (c(1) - (time >= L)) * exp((c(1) - (time >= L)) *k* (c(1) - 1/(1 -(time/L)^2)))
                              , data = tab
                              , start = list(a = tab$values[1]
                                             #,k = 1
                                             ,L = 6)
                              , control = nls_control)
    init_cov<-smoothcompact_fit$m$getPars()[1]
    L<-smoothcompact_fit$m$getPars()[2]
  }
  return(list(init_cov=init_cov
              ,L=L))

}

view_fitted_ITN_decay = function(distrib_year = 2011, distrib_month=1, distrib_day = 5
                                 , df_cov) {
  #' fit half-life and initial coverage from ITN coverage values
  #' @param distrib_year year of initial distribution
  #' @param distrib_month month of initial distribution
  #' @param distrib_day day of initial distribution
  #' @param df_cov data.frame with observed ITNcov, year and month
  #' @export
  #' @importFrom ggplot2 ggplot
  #' @examples view_fitted_ITN_decay(df_cov=data.frame(ITNcov=c(1,.9,.8),year=c(2011,2012,2013),month=c(1,5,12)))

  decay = NULL
  last_year = df_cov %>% filter(year==max(year)) %>% distinct(year) %>% pull()
  last_month = df_cov %>% filter(year==last_year) %>% filter(month==max(month)) %>% distinct(month) %>% pull()
  if(last_year-distrib_year>1){
    decay$month = c(distrib_month:12,rep(1:12,last_year-distrib_year-1),1:last_month)
    decay$year = c(rep(distrib_year,length(distrib_month:12))
                   ,rep((distrib_year+1):(last_year-1),each=12)
                   ,rep(last_year,length(1:last_month)))
  }
  if(last_year-distrib_year==1){
    decay$month = c(distrib_month:12,1:last_month)
    decay$year = c(rep(distrib_year,length(distrib_month:12))
                   ,rep(last_year,length(1:last_month)))
  }
  if(last_year==distrib_year){
    decay$month = distrib_month:last_month
    decay$year = rep(distrib_year,length(last_month-distrib_month))
  }

  fit = fit_ITN_decay(distrib_year = distrib_year
                      ,distrib_month = distrib_month
                      ,distrib_day = distrib_day
                      ,coverages = df_cov$ITNcov
                      ,years = df_cov$year
                      ,months = df_cov$month)

  decay = as.data.frame(decay)
  decay$time = decay$year-distrib_year+(decay$month-distrib_month)/12
  decay$L = fit$L
  decay$init_cov = fit$init_cov

  decay$ITNcov = smoothcompact_function(time=decay$time
                                        ,L=fit$L
                                        ,init_cov = fit$init_cov)

  g=ggplot(decay,aes(y = 100 * ITNcov, x = year+(month-1)/12)) +
    geom_line()+
    geom_point(data=df_cov)+
    geom_text(aes(x=(max(year)+min(year))/2,y=max(ITNcov)*100
                  ,label=paste("L=",round(L,2)
                               ,", Initial coverage=",round(init_cov*100,2),"%")))+
    theme_minimal() +
    labs(  x = "Year", y = "Coverage (%)")

  return(list(g,decay))
}

# view_fitted_ITN_decay(distrib_month = 4,df_cov=ITN_cov %>% filter(setting=="15.2"))
#
# view_fitted_ITN_decay(df_cov=data.frame(ITNcov=c(1,.9,.8)
#                                                           ,year=c(2011,2012,2013)
#                                                           ,month=c(1,5,12)))

#' Compute net halflife from a Benin study:
#' Chemical barrier and survivorship: Comparative study of two brands of
#' polyester nets and one brand of polyethylene nets in different conditions of
#' used in Benin
#' Idelphonse B Ahogni, Rock Y Aïkpon, Jean-Fortuné Dagnon, Roseric Azondekon,
#'  Bruno Akinro, Germain G Padonou and Martin C Akogbeto
#'  https://www.dipterajournal.com/pdf/2020/vol7issue5/PartA/7-4-18-884.pdf

#' page 2: "This study was implemented in three randomly selected districts
#' which received mosquito nets during the massive distribution campaign carried
#' out in October 2014"
distrib_year=2014
distrib_month=10

ITN_decay=data.frame(setting=rep(c("all nets","Permanet 2.0","Duranet"),each=5),
                     year=rep(c(2014,2015,2015,2016,2016),3),
                     #follow-up at 6, 12, 18 and 24 months (page 2)
                     month=rep(c(10,4,10,4,10),3),
                     # representative sample of 900 nets (page 2)
                     # LLINs Survivorship page 7
                     # for PermaNet 2.0 (polyester) and Duranet (polyethylene), values read on Figure 10
                     ITNcov=c(c(900,651,511,556,283)/900,
                              1,.96,.84,.68,.52,
                              1,.86,.72,.51,.33)
)

ITNdecay_allnets = fit_ITN_decay(distrib_year = distrib_year
                                 ,distrib_month = distrib_month
                                 ,coverages=ITN_decay %>%
                                   filter(setting=="all nets") %>%
                                   pull(ITNcov)
                                 ,years=ITN_decay %>%
                                   filter(setting=="all nets") %>%
                                   pull(year)
                                 ,months=ITN_decay %>%
                                   filter(setting=="all nets") %>%
                                   pull(month))

ITNdecay_P2 = fit_ITN_decay(distrib_year = distrib_year
                            ,distrib_month = distrib_month
                            ,coverages=ITN_decay %>%
                              filter(setting=="Permanet 2.0") %>%
                              pull(ITNcov)
                            ,years=ITN_decay %>%
                              filter(setting=="Permanet 2.0") %>%
                              pull(year)
                            ,months=ITN_decay %>%
                              filter(setting=="Permanet 2.0") %>%
                              pull(month))

ITNdecay_Duranet = fit_ITN_decay(distrib_year = distrib_year
                                 ,distrib_month = distrib_month
                                 ,coverages=ITN_decay %>%
                                   filter(setting=="Duranet") %>%
                                   pull(ITNcov)
                                 ,years=ITN_decay %>%
                                   filter(setting=="Duranet") %>%
                                   pull(year)
                                 ,months=ITN_decay %>%
                                   filter(setting=="Duranet") %>%
                                   pull(month))

view_fitted_ITN_decay(distrib_year=distrib_year
                      ,distrib_month = distrib_month
                      ,df_cov=ITN_decay %>% filter(setting=="all nets"))
view_fitted_ITN_decay(distrib_year=distrib_year
                      ,distrib_month = distrib_month
                      ,df_cov=ITN_decay %>% filter(setting=="Permanet 2.0"))
view_fitted_ITN_decay(distrib_year=distrib_year
                      ,distrib_month = distrib_month
                      ,df_cov=ITN_decay %>% filter(setting=="Duranet"))


