#' Clean raw NOAA data
#'
#' @param raw_noaa_df a dataframe containing raw NOAA data on significant
#'     earthquake. Removes dates BCE as they are not handled easily by the date
#'     class that is required by the assignment.
#'     The data used for testing can be found in the object raw_noaa.
#'
#' @importFrom magrittr "%>%"
#'
#' @return A clean dataframe
#' @export
#'
#' @examples
#'
#' test <- eq_clean_data(raw_noaa)
#'
eq_clean_data <- function(raw_noaa_df){

  df <- suppressWarnings(eq_location_clean(raw_noaa_df) %>%
    dplyr::filter(YEAR >= 0) %>%
    dplyr::mutate(
      MONTH = dplyr::case_when(is.na(MONTH)~ 1,
                        TRUE ~ MONTH),
      DAY = dplyr::case_when(is.na(DAY) ~ 1,
                     TRUE ~ DAY),
      DATE = as.Date(paste(YEAR, MONTH, DAY, sep = "/"), format= "%Y/%m/%d"),
      LONGITUDE = as.numeric(LONGITUDE),
      LATITUDE = as.numeric(LATITUDE)
      ))
  return(df)

}

#' Clean location names in raw NOAA data
#'
#' @param raw_noaa_df a dataframe containing raw NOAA data on significant
#'     earthquake.
#'     The data used for testing can be found in the object raw_noaa.
#'
#' @importFrom magrittr "%>%"
#'
#' @return a dataframe with a clean location column called LOCATION_NAME
#' @export
#'
#' @examples
#' clean_location_noaa <- eq_location_clean(raw_noaa)
eq_location_clean <- function(raw_noaa_df){
  df <- raw_noaa %>%
    dplyr::mutate(LOCATION_NAME = stringr::str_to_title(stringr::str_trim(gsub("^.*?:", "", LOCATION_NAME))))
  return(df)
}

#' Filter earthquakes by magnitude
#'
#' Filter the top n_max earthquakes according to their magnitude.
#'
#' @param data Earthquake data
#' @param n_max Max number of earthquakes to pick
#' @param min_date Minimum date
#' @param max_date Maximum date
#'
#' @return A dataframe with the filtered data
#'
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' top_10 <- top_eq( eq_clean_data(raw_data), 10)
#'
top_eq <- function(data, n_max = Inf, min_date = -Inf, max_date = Inf) {
  data %>%
    dplyr::filter(dplyr::between(DATE, min_date, max_date)) %>%
    dplyr::slice_max(EQ_PRIMARY, n=n_max)
}
