context("test-clarity_get.R")

## different clarity 'versions'
## v1 -- old, appears deprecated in favor of v2 but will be useful for historical data
v1_url <- "http://results.enr.clarityelections.com/NJ/Cape_May/71890/190686/Web01/en/summary.html"
## v2 -- new, requires request to currentversion for live webscraping
v2_url <- "http://results.enr.clarityelections.com/NJ/Mercer/69894/Web02/#/"
## v2.5 -- request contains epoch time in request format for whatever reason
v2_5_url <- "http://results.enr.clarityelections.com/AL/Madison/72211/Web02/#/"

test_that("integration -- returns valid clarity object", {
    expect_is(clarity_get(v1_url), "clarity_xml")
    expect_is(clarity_get(v2_url), "clarity_xml")
    expect_is(clarity_get(v2_5_url), "clarity_xml")
})



test_that("appends downloadtime node", {
    to <- clarity_get(v1_url)
    expect_true("DownloadTime" %in% xml2::xml_name(xml2::xml_children(to)))
})

test_that("errors work", {
    expect_error(clarity_get("https:///www.google.com"))
})
