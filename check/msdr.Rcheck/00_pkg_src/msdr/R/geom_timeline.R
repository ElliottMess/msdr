#' @rdname geom_timeline
#' @importFrom magrittr "%>%"
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 non_missing_aes = c("size", "shape", "colour"),
                                 default_aes = ggplot2::aes(y = 0.25,
                                                 shape = 21,
                                                 colour = "black",
                                                 fill = "grey",
                                                 size =  1.5,
                                                 alpha = 0.6,
                                                 stroke = 0.5),
                                 extra_params = c("xmin", "xmax", "na.rm"),

                                 setup_data = function(data, params){
                                   data <- data %>%
                                     dplyr::filter(dplyr::between(x, params$xmin, params$xmax))
                                  },

                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_params, coord, na.rm=FALSE){

                                   coords <- coord$transform(data, panel_params)

                                   grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     pch = coords$shape,
                                     gp = grid::gpar(
                                       fill = ggplot2::alpha(coords$fill, coords$alpha),
                                       col = ggplot2::alpha(coords$colour, coords$alpha),
                                       fontsize = coords$size * ggplot2::.pt + coords$stroke * ggplot2::.stroke / 2,
                                       lwd = coords$stroke * ggplot2::.stroke / 2
                                     ))

                                   }
)

#' Timeline geom
#'
#' Timeline geom for earthquake data from NOAA
#'
#' This geom plots earthquakes on a timeline ranging from a minimum (xmin) to a
#' maximum (xmax). Each point is an earthquake.
#'
#' The x aesthetic  is a Date class vector of dates of earthquakes.
#'
#' Size, colour, alpha and y can be provided as optional aesthetic. y must be a
#' factor.
#'
#' @inheritParams ggplot2::geom_point
#' @param xmin Minimum date to represent. Must be in a Date class object
#' @param xmax Maximum date to represent. Must be in a Date class object
#'
#' @return Returns the geom for ggplot2 graphics
#' @export
#'
#' @rdname geom_timeline
#' @examples
#' library(ggplot2)
#' library(lubridate)
#' clean <- eq_clean_data(raw_noaa)
#' ggplot(clean, aes(x = DATE, y = COUNTRY, size=EQ_PRIMARY, fill = DEATHS)) +
#'     geom_timeline(alpha = 0.25, xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31'))
#'
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", show.legend = NA,
                          na.rm = FALSE, inherit.aes = TRUE,
                          xmin = as.Date("0001-01-01"), xmax = as.Date("2021-03-21"),
                          ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(xmin = xmin, xmax = xmax, na.rm = na.rm, ...)
  )
}
