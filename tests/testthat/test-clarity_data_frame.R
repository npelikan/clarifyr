context("test-clarity_data_frame.R")

## failing test -- 2018-01-18
test_that("GA Format works", {
  expect_is(as.data.frame(clarity_get("http://results.enr.clarityelections.com/GA/Appling/63993/en/reports.html")), "data.frame")
})

## fails
test_that("NJ/Old format works", {
    expect_is(as.data.frame(clarity_get("http://results.enr.clarityelections.com/NJ/Cape_May/71890/190686/Web01/en/summary.html")), "data.frame")
})

test_that("Madison/New format works", {
    expect_is(as.data.frame(clarity_get("http://results.enr.clarityelections.com/AL/Madison/72211/Web02/#/")), "data.frame")
})

