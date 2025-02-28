##' Convert access to effective treatment coverage
##' @param orig Original values of access to care
##' @param katya Convert from access to effective treatment coverage (boolean)
##' @param country Abbreviation of the country, if NULL default will be used
##' @param scale Scaling factor (integer), if NULL, a default country specific
##'   scaling factor will be used
##' @param reverse Reverse the conversion (default from access to effective
##'   treatment coverage), (boolean)
##' @export
##' @examples # Converting 80% access to care to a 5-day OpenMalaria timestep
##' \dontrun{
## ' convert_cm(orig = .80, scale = 1)
##'
##' # Converting 80% access to care to a 5-day OpenMalaria timestep
##' # in Ghana, where it is assumed that 63.7% are effectively treated
##' convert_cm(orig = .80, country = "GHA")
##'
##' # This is equivalent to the following
##' convert_cm(orig = .80 * .637, scale = 1)
##'
##' # Going from OpenMalaria timesteps to access to care is also possible
##' convert_cm(orig = .249, reverse = TRUE, country = "GHA")
##' }
convert_cm <- function(orig, katya = FALSE, country = NULL, scale = NULL, reverse = FALSE) {
  ## Verify input
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertNumeric(orig, lower = 0, upper = 1, add = assertCol)
  checkmate::reportAssertions(assertCol)
  
  x <- c(
    0, 5, 10, 12, 15, 18, 20, 22, 24, 25, 28, 30, 32, 35, 36, 38, 40, 42, 45,
    48, 49, 50, 53, 55, 59, 60, 62, 65, 68, 70, 73, 75, 78, 80, 82, 85, 88, 90,
    95, 99, 100
  ) / 100
  y <- c(
    0, 0.0182, 0.0356, 0.0418, 0.0516, 0.0635, 0.0725, 0.0821, 0.0921, 0.0972,
    0.1125, 0.1227, 0.1329, 0.1488, 0.1544, 0.1661, 0.1782, 0.1905, 0.2093,
    0.2284, 0.2348, 0.2412, 0.2598, 0.2715, 0.2957, 0.3030, 0.3210, 0.3567,
    0.3949, 0.4165, 0.4449, 0.4646, 0.5010, 0.5319, 0.5644, 0.6057, 0.6466,
    0.6813, 0.7934, 0.9580, 1
  )
  
  country_CM_scale <- as.data.frame(
    rbind(
      TZA = 0.607, BEN = 0.544,
      CMR = 0.548, MOZ = 0.653,
      UGA = 0.799, GHA = 0.637
    )
  )
  
  if (!is.null(country) & is.null(scale)) {
    ## List of pre-defined focus countries, can be extended
    if (!(country %in% rownames(country_CM_scale))) {
      stop(
        paste(
          "country = ", country, "was not in the following list:",
          paste0(rownames(country_CM_scale), collapse = ", ")
        )
      )
    }
    scale <- country_CM_scale[
      tolower(rownames(country_CM_scale)) == tolower(country), "V1"
    ]
  }
  if (is.null(country) & is.null(scale)) scale <- 0.6
  
  if (!is.null(country) & !is.null(scale)) {
    message(paste("Using the following scaling factor:", scale))
    katya <- TRUE
  }
  
  if (!reverse) {
    model <- stats::lm(y ~ stats::poly(x, 5))
    
    orig <- orig * ifelse(katya, scale, 1)
    out <- pmax(0, signif(stats::predict(model, data.frame(x = orig)), 3))
    out <- pmin(1, out)
  } else {
    model <- stats::lm(x ~ stats::poly(y, 5))
    out <- pmax(0, signif(stats::predict(model, data.frame(y = orig)), 3))
    out <- pmin(1, out)
    out <- out * ifelse(katya, 1 / scale, 1)
  }
  return(out)
}

##' Function to scale from care seeking to effective treatment coverage per 14
##' days to 5 days
##' @param dat Country data
##' @param pattern How to identify 'Access' variables
##' @param katya If true, then scales from care seeking to effective treatment
##'   coverage
##' @param scale Scaling factor, if NULL, a default will be used based on
##'   Galactionova et al 2012
##' @param country Abbreviation (alpha-3 code ISO 3166) if using a country
##'   pre-specified scaling factor
##' @export
##' @rdname view_past
##' @examples
##' \dontrun{
##' # Input dataset
##' dat <- data.frame(setting = "alpha", access2005 = .5, access2006 = .1, access2010 = .9)
##' # Converting access values
##' dat <- convert_access(dat = dat, pattern = "access", country = "MOZ")
##' # Visualizing the dataset
##' # view_past(dat = dat, pattern = "access")
##' }
convert_access <- function(dat, pattern = "Access", katya = T,
                           country = NULL, scale = NULL) {
  
  ## Adding an indicator that we have already scaled down the values before
  bads <- which(colnames(dat) == "scaled_down_flag")
  if (length(bads) > 0) {
    stop("No need to run this code again. You have already converted to 5-day probabilities.")
  }
  
  ## List of pre-defined focus countries, can be extended
  country_CM_scale <- as.data.frame(rbind(
    TEST = 0.6,
    TZA = 0.607, BEN = 0.544,
    CMR = 0.548, MOZ = 0.653,
    UGA = 0.799, GHA = 0.637
  ))
  colnames(country_CM_scale) <- "scale"
  
  
  ## Continuing otherwise
  these <- grep(colnames(dat), pattern = pattern)
  message("Converting the following columns to 5-day probabilities:")
  print(paste0(colnames(dat)[these], collapse = ", "))
  
  rows <- nrow(dat[, these])
  cols <- ncol(dat[, these])
  
  if (is.null(country) & is.null(scale)) country <- "TEST"
  if (!is.null(country) & is.null(scale)) {
    scale <- country_CM_scale[tolower(rownames(country_CM_scale)) == tolower(country), "scale"]
    message(paste("Using the following scaling factor:", scale))
  }
  
  tt <- matrix(convert_cm(unlist(dat[, these]),
                          katya = katya, country = country, scale = scale
  ),
  nrow = rows, ncol = cols, byrow = F
  )
  
  ## Now we have scaled down the values
  dat[, these] <- tt
  dat$scaled_down_flag <- TRUE ## flag!
  
  if (sum(is.na(dat)) > 0) {
    stop(paste(
      "NAs in dataset. Country selected:", country, "may not be defined."
    ))
  }
  
  return(dat)
}

##' @title Calculate epidemiological indicators from stacked raw data
##' @param df Input data frame
##' @param indicators Can be a character vector of epidemiological indicators to
##'   calculate or measures from OpenMalaris. The calculated indicators can be
##'   any of:
##'     prevalenceRate
##'     incidenceRate
##'     incidenceRatePerThousand
##'     tUncomp
##'     tSevere
##'     nHosp
##'     edeath
##'     edeathRatePerHundredThousand
##'     edirdeath
##'     edirdeathRatePerHundredThousand
##'     ddeath
##'     ddeathRatePerHundredThousand
##' @param aggregateByAgeGroup Character vector of age groups (e.g. "2-10" for
##'   the right-open interval) to be used for aggregation, if NULL, then age
##'   groups from surveys will be used
##' @param aggregateByDate If NULL, keep original surveys. Otherwise any
##'   combination of:
##'
##'        - "year" aggregate by year (date will be changed to 31-12 of the
##'          corresponding year).
##'
##'        - "month" aggregate by month (date will be changed to the 15th of a
##'          month).
##'
##'        - "survey" keep survey dates.
##' @param timeHorizon Character vector of with start date and end date of time
##'   horizon to be processed, e.g. c("2000-01-01", "2100-01-01")
##' @param use.gc If TRUE, run garbage collection during operation to keep
##'   memory consumption low. Useful if computational ressources are tight.
##' @importFrom data.table ':=' .SD
##' @export
CalcEpiOutputs <- function(df, indicators = c("incidenceRate", "prevalenceRate"),
                           aggregateByAgeGroup = NULL, aggregateByDate = NULL,
                           timeHorizon = NULL, use.gc = FALSE) {
  ## Make sure input is a data.table
  df <- data.table::as.data.table(df)
  
  ## Check if the required measures are available
  reqMeasures <- list(
    prevalenceRate = c("nPatent", "nHost"),
    incidenceRate = c("nUncomp", "nSevere", "nHost"),
    incidenceRatePerThousand = c("nUncomp", "nSevere", "nHost"),
    tUncomp = c("nTreatments1", "nTreatments2"),
    tSevere = c("nTreatments3"),
    nHosp = c("nHospitalDeaths", "nHospitalRecovs", "nHospitalSeqs"),
    edeath = c("expectedDirectDeaths", "expectedIndirectDeaths", "nHost"),
    edeathRatePerHundredThousand = c(
      "expectedDirectDeaths",
      "expectedIndirectDeaths", "nHost"
    ),
    edirdeath = c("expectedDirectDeaths", "nHost"),
    edirdeathRatePerHundredThousand = c("expectedDirectDeaths", "nHost"),
    ddeath = c("nIndDeaths", "nDirDeaths", "nHost"),
    ddeathRatePerHundredThousand = c("nIndDeaths", "nDirDeaths", "nHost")
  )
  
  ## These columns are not measures and should be ignored
  ## TODO Add cohorts, genotypypes, etc.
  fixedCols <- c(
    "scenario_id", "date_aggregation", "date", "age_group",
    "cohort"
  )
  indicators <- indicators[!indicators %in% fixedCols]
  
  ## Select needed measures for calculations and check if they are in the input.
  ## If not, stop.
  missingMeasures <- unlist(
    reqMeasures[indicators]
  )[!unlist(reqMeasures[indicators]) %in% unique(df[["measure"]])]
  if (length(missingMeasures > 0)) {
    stop(paste("The following measures are missing in your OM output:",
               paste0(unique(missingMeasures), collapse = "\n"),
               "Aborting.",
               sep = "\n"
    ))
  }
  
  ## Collect measures for calculation
  neededMeasures <- unique(unlist(reqMeasures[indicators]))
  ## Add other selected measures
  neededMeasures <- union(neededMeasures, indicators)
  ## We only keep the output columns as defined in indicators.
  dropCols <- neededMeasures[!neededMeasures %in% indicators]
  
  ## Narrow table to selected dates and measures
  df <- df[measure %in% neededMeasures]
  df <- df[, survey_date := as.Date(survey_date)]
  if (!is.null(timeHorizon)) {
    df <- df[survey_date >= timeHorizon[1] & survey_date <= timeHorizon[2]]
  }
  
  ## Split and rename third dimension column and aggregate
  ##
  ## TODO Implement cohorts, which requires the creation of an extra column.
  ##      | third_dimension |  ->  | cohort | age_roup |
  ##      |     AB:0-1      |  ->  |   AB   |    0-1   |
  ##
  ## Don't forget the "none" cohort, which should contain individuals not in any
  ## cohort.
  ## Also take special care with the "none"/0 age group!
  data.table::setnames(df, old = "third_dimension", new = "age_group")
  
  ## NOTE This needs to be extended if we also use genotypes, etc.
  
  
  ## NOTE All of the following aggregations are bottlenecks. So make sure that
  ##      these are as fast as possible. Our best bet is to make sure that
  ##      data.table's GForce optimizes the calls. These are fucking fast.
  ##      Verify and benchmark with options(datatable.verbose = TRUE).
  
  ## Age aggregation
  if (!is.null(aggregateByAgeGroup)) {
    groups <- aggregateByAgeGroup
    ## Translate "All" to a huge age range. Nobody should be older than 200,
    ## right?
    groups <- data.table::data.table(x = replace(groups, groups == "All", "0-200"))
    ages <- unique(df[["age_group"]])
    ## Remove "none" group, if present. This suppresses an annoying warning in
    ## the following age group selection.
    ages <- ages[!ages %in% "none"]
    tmp <- data.table::data.table(x = ages)
    
    ## Split the age group string (e.g. "0-1") into lo and hi columns (e.g. 1 0)
    groups <- groups[, c("lo", "hi") := data.table::tstrsplit(
      x, "-",
      fixed = TRUE
    )][, c("lo", "hi") := list(as.numeric(lo), as.numeric(hi))]
    
    tmp <- tmp[, c("lo", "hi") := data.table::tstrsplit(
      x, "-",
      fixed = TRUE
    )][
      ,
      c("lo", "hi") := list(as.numeric(lo), as.numeric(hi))
    ]
    
    ## Now check which monitoring age groups are in the range of the requested
    ## age groups. We will use this information later to sum up the values.
    selAges <- list()
    
    for (i in seq_len(nrow(groups))) {
      nn <- unlist(strsplit(as.character(ages[which(tmp[["lo"]] >= groups[[i, "lo"]] & tmp[["hi"]] <= groups[[i, "hi"]])]), "-", fixed = TRUE))
      nn <- ifelse(length(nn) > 0, paste0(nn[1], "-", nn[length(nn)]), NA)
      selAges[[groups[[i, "x"]]]][["groups"]] <- as.character(ages[which(tmp[["lo"]] >= groups[[i, "lo"]] & tmp[["hi"]] <= groups[[i, "hi"]])])
      selAges[[groups[[i, "x"]]]][["adj_name"]] <- nn
    }
    ## Change 0-200 back to All to avoid confusion
    names(selAges)[names(selAges) == "0-200"] <- "All"
    ## Also add back the "none" group
    selAges <- c(selAges, list(none = list(
      groups = "none",
      adj_name = NA
    )))
    
    ## Now we sum up the values for the desired age groups
    df <- data.table::rbindlist(
      lapply(seq_along(selAges), function(i, list) {
        if (length(list[[i]][["groups"]]) == 0) {
          stop(paste0(
            "Data for age group ", names(list[i]),
            " could not retrieved. Check boundaries.\n"
          ))
        }
        
        if (!is.na(list[[i]][["adj_name"]]) & list[[i]][["adj_name"]] != names(list[i])) {
          warning(
            paste0(
              "Requested age group ", names(list[i]),
              " does not fit monitored boundaries and was adjusted to ",
              list[[i]][["adj_name"]], "\n"
            )
          )
        }
        df[age_group %in% list[[i]][["groups"]],
           .(value = sum(value)),
           by = .(scenario_id, survey_date, measure)
        ][
          , age_group := if (!is.na(list[[i]][["adj_name"]])) {
            list[[i]][["adj_name"]]
          } else {
            names(list[i])
          }
        ]
      }, list = selAges)
    )
    ## Run garbage collector
    if (use.gc) {
      rm(groups, ages, tmp, selAges)
      gc()
    }
  }
  
  ## Date aggregation
  if (!is.null(aggregateByDate)) {
    ## Join the aggregated column from the dictionary so can apply the correct
    ## modifications.
    df <- df[omOutputDict(),
             aggregated := i.aggregated,
             on = c(measure = "measure_name")
    ]
    ## We create the resulting data table by creating the individual sub-data
    ## tables and then using rbindlist.
    df <- data.table::rbindlist(l = list(
      if ("year" %in% aggregateByDate) {
        data.table::rbindlist(list(
          df[aggregated == TRUE, .(value = sum(value)),
             by = .(scenario_id,
                    date = data.table::year(as.Date(survey_date)),
                    measure, age_group
             )
          ],
          df[aggregated == FALSE, .(value = mean(value)),
             by = .(scenario_id,
                    date = data.table::year(as.Date(survey_date)),
                    measure, age_group
             )
          ]
        ))[
          , c("date", "date_aggregation") := .(
            paste0(date, "-12-31"), rep("year", times = nrow(.SD))
          )
        ]
      },
      if ("month" %in% aggregateByDate) {
        data.table::rbindlist(list(
          df[aggregated == TRUE, .(value = sum(value)),
             by = .(scenario_id,
                    year = data.table::year(as.Date(survey_date)),
                    month = data.table::month(as.Date(survey_date)),
                    measure, age_group
             )
          ],
          df[aggregated == FALSE, .(value = mean(value)),
             by = .(scenario_id,
                    year = data.table::year(as.Date(survey_date)),
                    month = data.table::month(as.Date(survey_date)),
                    measure, age_group
             )
          ]
        ))[
          , c(
            "date", "date_aggregation", "year", "month"
          ) := .(
            paste0(year, "-", sprintf("%02d", month), "-15"),
            rep("month", times = nrow(.SD)), NULL, NULL
          )
        ]
      },
      if (is.null(aggregateByDate) || "survey" %in% aggregateByDate) {
        data.table::rbindlist(list(
          df[aggregated == TRUE, .(value = sum(value)),
             by = .(scenario_id, survey_date, measure, age_group)
          ],
          df[aggregated == FALSE, .(value = mean(value)),
             by = .(scenario_id, survey_date, measure, age_group)
          ]
        ))[
          , c(
            "date", "date_aggregation", "survey_date"
          ) := .(
            as.character(survey_date), rep("survey", times = nrow(df)), NULL
          )
        ]
      }
    ), use.names = TRUE)
    ## Run garbage collector
    if (use.gc) {
      gc()
    }
  }
  
  ## Transform to wide format
  ## TODO Cohorts need to be added
  df <- data.table::dcast(
    df,
    scenario_id + date_aggregation + date + age_group ~ measure,
    value.var = "value"
  )
  
  ## Calculate indicators
  if ("incidenceRate" %in% indicators) {
    df[, incidenceRate := (nUncomp + nSevere) / nHost]
  }
  
  if ("incidenceRatePerThousand" %in% indicators) {
    df[, incidenceRatePerThousand := ((nUncomp + nSevere) / nHost) * 1000]
  }
  
  if ("prevalenceRate" %in% indicators) {
    df[, prevalenceRate := nPatent / nHost]
  }
  
  if ("tUncomp" %in% indicators) {
    df[, tUncomp := nTreatments1 + nTreatments2]
  }
  
  if ("tSevere" %in% indicators) {
    df[, tSevere := nTreatments3]
  }
  
  if ("nHosp" %in% indicators) {
    df[, nHosp := nHospitalDeaths + nHospitalRecovs + nHospitalSeqs]
  }
  
  if ("edeath" %in% indicators) {
    df[, edeath := expectedDirectDeaths + expectedIndirectDeaths]
  }
  
  if ("edeathRatePerHundredThousand" %in% indicators) {
    df[, edeathRatePerHundredThousand := (expectedDirectDeaths + expectedIndirectDeaths) / nHost * 1e5]
  }
  
  if ("edirdeath" %in% indicators) {
    df[, edirdeath := expectedDirectDeaths]
  }
  
  if ("edirdeathRatePerHundredThousand" %in% indicators) {
    df[, edirdeathRatePerHundredThousand := expectedDirectDeaths / nHost * 1e5]
  }
  
  if ("ddeath" %in% indicators) {
    df[, ddeath := nIndDeaths + nDirDeaths]
  }
  
  if ("ddeathRatePerHundredThousand" %in% indicators) {
    df[, ddeathRatePerHundredThousand := (nIndDeaths + nDirDeaths) / nHost * 1e5]
  }
  
  ## Drop non-requested columns
  df <- df[, (dropCols) := NULL]
  ## Make sure the date is a string (Needed for SQLite)
  df <- df[, date := as.character(date)]
  
  return(df)
}
