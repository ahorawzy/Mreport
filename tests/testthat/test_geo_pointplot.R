library(Mreport)
context("geo_pointplot")

load_base()

test_that("This base function can works well",{
  geopp <- geo_pointplot(station_plot,na.rm = T)
  expect_is(geopp,"leaflet")
  expect_is(geopp,"htmlwidget")
})
