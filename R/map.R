
#' Map earthquakes
#'
#' @param data a dataframe containing NOAA earthquake data to be mapped
#' @param annot_col character string of the column to be used for the annotation
#'
#' @importFrom magrittr %>%
#'
#' @return a leaflet map with the earthquakes present in data with circles with
#'     radius proportional to the magnitude of the earthquake, and popups with
#'     the data contained in the annotation column (annot_col).
#'
#' @export
#'
#' @examples
#' eq_clean_data(raw_noaa) %>%
#'     dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'     eq_map(annot_col = "DATE")
eq_map <- function(data, annot_col = NULL){

  popup_text <- data %>%
    dplyr::select(!!annot_col) %>%
    dplyr::mutate(dplyr::across(tidyr::everything(), as.character)) %>%
    unlist(use.names = FALSE)

  map <-leaflet::leaflet(data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircles(lng = ~LONGITUDE,
                        lat = ~LATITUDE,
                        radius = ~EQ_PRIMARY*6000,
                        popup = ~popup_text)

  return(map)
}


#' Create informative labels for map
#'
#' @param data a dataframe containing NOAA earthquake data to be mapped
#'
#' @importFrom magrittr %>%
#'
#' @return a character vector with the nice labels to be used in a map.
#' @export
#'
#' @examples
#' raw_noaa %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")


eq_create_label <- function(data){
  data %>%
    dplyr::select(LOCATION_NAME, EQ_PRIMARY, TOTAL_DEATHS) %>%
    dplyr::mutate(LOCATION_NAME = dplyr::if_else(is.na(LOCATION_NAME), "", as.character(LOCATION_NAME)),
                  EQ_PRIMARY = dplyr::if_else(is.na(EQ_PRIMARY), "", as.character(EQ_PRIMARY)),
                  TOTAL_DEATHS = dplyr::if_else(is.na(TOTAL_DEATHS), 0, TOTAL_DEATHS)) %>%
  summarise(popup_text = paste0(
    "<b>Location: </b>", LOCATION_NAME, "<br>",
    "<b>Magnitude: </b>", EQ_PRIMARY, "<br>",
    "<b>Total deaths: </b>", TOTAL_DEATHS, "<br>"
  )) %>%
    unlist(use.names = FALSE)

}
