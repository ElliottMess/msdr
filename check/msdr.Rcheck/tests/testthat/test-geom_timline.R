test_that("geom_timeline produces a ggplot",{

  library(ggplot2)
  library(lubridate)

  library(msdr)

  clean <- eq_clean_data(raw_noaa)
  plot <- ggplot(clean, aes(x = DATE, y = COUNTRY, size=EQ_PRIMARY, fill = DEATHS)) +
      geom_timeline(alpha = 0.25, xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31'))

  expect_equal(class(plot)[1], "gg")

})

test_that("geom_timeline_label produces a ggplot",{

  library(msdr)


  data <- eq_clean_data(raw_noaa) %>%
          filter(COUNTRY %in% c('USA', 'CHINA'))

  labels <- data %>%
    top_eq(n_max = 10, min_date = ymd('2000-01-01'), max_date = ymd('2015-12-31'))


  plot <- ggplot(data, aes(x = DATE, y = COUNTRY, fill = DEATHS)) +
    geom_timeline(aes(size = EQ_PRIMARY), alpha = 0.25,
                      xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31')) +
    geom_timeline_label(data = labels,
                        mapping = aes(x = DATE, y = COUNTRY, label = LOCATION_NAME))

  expect_equal(class(plot)[1], "gg")

})
