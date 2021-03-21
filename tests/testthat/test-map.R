test_that("Map gets out of eq_map", {

  # read the raw data
  library(msdr)

  map <- suppressWarnings(eq_map(eq_clean_data(raw_noaa), annot_col = "DATE"))

  expect_equal(class(map)[1], "leaflet")
})


test_that("new labels are string when gets out of eq_create_label", {

  # read the raw data
  library(msdr)

  new_labels <- eq_create_label(eq_clean_data(raw_noaa))

  expect_equal(class(new_labels)[1], "character")
})
