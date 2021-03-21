## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE, message=FALSE-----------------------------------------
library(msdr)

## ----clean--------------------------------------------------------------------
clean_location_noaa <- eq_location_clean(raw_noaa)

test <- eq_clean_data(raw_noaa)

## ----geom_timeline------------------------------------------------------------
library(ggplot2)
clean <- eq_clean_data(raw_noaa) %>% 
  filter(COUNTRY %in% c("AFGHANISTAN", "NEPAL"))

ggplot(clean, aes(x = DATE, y = COUNTRY, size=EQ_PRIMARY, fill = DEATHS)) +
     geom_timeline(alpha = 0.6, xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31'))

## ----geom_timeline_label------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)
data <- eq_clean_data(raw_noaa) %>%
        filter(COUNTRY %in% c('USA', 'CHINA'))

labels <- data %>%
  top_eq(n_max = 10, min_date = ymd('2000-01-01'), max_date = ymd('2015-12-31'))

ggplot(data, aes(x = DATE, y = COUNTRY, fill = DEATHS)) +
  geom_timeline(aes(size = EQ_PRIMARY), alpha = 0.25,
                    xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31')) +
  geom_timeline_label(data = labels,
                      mapping = aes(x = DATE, y = COUNTRY, label = LOCATION_NAME))


## ----eq_mapping---------------------------------------------------------------

 eq_clean_data(raw_noaa) %>%
     dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
     eq_map(annot_col = "DATE")

## ----eq_mapping_label---------------------------------------------------------

raw_noaa %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")

