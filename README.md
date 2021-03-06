
# msdr

<!-- badges: start -->

\[![build](https://travis-ci.org/ElliottMess/msdr.svg?branch=master)\]\[<https://travis-ci.org/github/ElliottMess/msdr>\]
<!-- badges: end -->

The goal of msdr is to provide conveniance functions to analyse [NOAA
Significant Earthquakes
dataset](https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/search).

The package contains three main sets of functions:

1.  Cleaning functions
2.  Plotting functions (in the form of
    [ggplot2](https://ggplot2.tidyverse.org/) geoms)
3.  Mapping functions

The package contains an example dataset that can be access with
msdr::raw\_noaa

## Installation and loading

You can install the released version of msdr from
[github](https://github.com/ElliottMess/msdr) with:

``` r
devtools::install_github("ElliottMess/msdr")
```

It can then be easily loaded with

``` r
library(msdr)
```

## Cleaning functions

There is two cleaning function:

1.  eq\_location\_clean()
2.  eq\_clean\_data()

eq\_location\_clean cleans the location names to provide more
informative labels.

eq\_clean\_data formates the whole dataset to implement better data
management, including calling eq\_location\_clean on the location
column.

``` r
clean_location_noaa <- eq_location_clean(raw_noaa)

test <- eq_clean_data(raw_noaa)
```

## Plotting functions

Two plotting functions exist:

1.  geom\_timeline
2.  geom\_timeline\_label

geom\_timeline creates a timeline representation of the data by date.

It plots earthquakes on a timeline ranging from a minimum (xmin) to a
maximum (xmax). Each point is an earthquake. The x aesthetic is a Date
class vector of dates of earthquakes. Size, colour, alpha and y can be
provided as optional aesthetic. y must be a factor and can be used to
have one timeline per level of the factor. For instance, each line can
be a country.

``` r
library(ggplot2)
clean <- eq_clean_data(raw_noaa) %>% 
  filter(COUNTRY %in% c("AFGHANISTAN", "NEPAL"))

ggplot(clean, aes(x = DATE, y = COUNTRY, size=EQ_PRIMARY, fill = DEATHS)) +
     geom_timeline(alpha = 0.6, xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31'))
```

<img src="man/figures/README-geom_timeline-1.png" width="100%" />
geom\_timeline\_label adds a label to each data point that consists of a
line and a text (for instance, the location of the earthquake) attached
to each line. The x aesthetic provides the date vector for the timeline
and label provides the labelling vector. The top\_eq convienence
function helps find the n\_max optional parameter allows to subset the
data with the n\_max largest earthquakes by magnitude.

``` r
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
```

<img src="man/figures/README-geom_timeline_label-1.png" width="100%" />
\#\# Mapping functions

The mapping functions allows to explore on a map the datasets. There is
two mapping functions:

1.  eq\_map()
2.  eq\_create\_label()

eq\_map() creates maps based on the dataset provided, while
eq\_create\_label() creates more informative label for eq\_map.

``` r

 eq_clean_data(raw_noaa) %>%
     dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
     eq_map(annot_col = "DATE")
```

<img src="man/figures/README-eq_mapping-1.png" width="100%" /> With
labels:

``` r

raw_noaa %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")
```

<img src="man/figures/README-eq_mapping_label-1.png" width="100%" />
