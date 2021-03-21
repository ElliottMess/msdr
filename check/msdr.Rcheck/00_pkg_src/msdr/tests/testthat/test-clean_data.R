test_that("Cleaning process results in an appropriate results", {

    # read the raw data
  library(msdr)

    # Cleaning the raw data.
    data_clean <- eq_clean_data(raw_data )

    # Loading data to see the dimensions.
    dims <- dim(data_clean)

    # Confirming the number of observations.
    expect_equal(dims[1], 6038 )

    # Confirming the number of features.
    expect_equal(dims[2], 48)

    # Checking character to numeric conversions.
    expect_equal(class(data_clean$DATE), "Date")
})


test_that("Cleaning locations removes the text before the first semi-column", {

  # load the raw data
  library(msdr)

  removed_old_locations <- stringr::str_to_title(stringr::str_trim(gsub("^.*?:", "", raw_noaa$LOCATION_NAME)))

  # Cleaning locations
  cleaned_locations <- eq_location_clean(raw_noaa)

  expect_equal(cleaned_locations$LOCATION_NAME, removed_old_locations)


})


test_that("Gets the right top 10 earthquakes",{
  # load the raw data
  library(msdr)
  # get top_eq results
  top_eq_10 <- top_eq(eq_clean_data(raw_noaa), n_max = 10)

  expect_equal(class(top_eq_10)[4], "data.frame")

})

