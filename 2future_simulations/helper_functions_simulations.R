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
