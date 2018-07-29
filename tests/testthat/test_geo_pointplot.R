library(Mreport)
context("geo_pointplot")

load_base()
load_sample_base()

test_that("This base function can works well",{
  geopp <- geo_pointplot(station_plot,na.rm = T)
  expect_is(geopp,"leaflet")
  expect_is(geopp,"htmlwidget")
})

test_that("region function can works well",{
  geopp <- geo_pointplot(station_plot,na.rm = T, region = "China")
  expect_is(geopp,"leaflet")
  expect_is(geopp,"htmlwidget")
})

test_that("type function can works well",{
  station_plot_s <- handle_mergeplot(samplebase = sample_base$roadhub,stationplot = station_plot)
  geopp <- geo_pointplot(station_plot_s,na.rm = T, type = T)
  expect_is(geopp,"leaflet")
  expect_is(geopp,"htmlwidget")
})

test_that("popup function can works well",{
  geopp <- geo_pointplot(station_plot,na.rm = T, popup = T)
  expect_is(geopp,"leaflet")
  expect_is(geopp,"htmlwidget")
})
