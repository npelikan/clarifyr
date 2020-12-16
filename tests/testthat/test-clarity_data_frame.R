context("test-clarity_data_frame.R")

test_that("GA Format works", {
    r <- as.data.frame(
        clarity_get("http://results.enr.clarityelections.com/GA/Appling/63993/en/reports.html")
    )
    expect_is(r, "data.frame")
    expect_named(r, c("precinct", "votes", "votetype", "candidate",
                      "party", "race", "ballots_cast", "last_updated"))
    expect_is(r$ballots_cast, "numeric")
    expect_is(r$last_updated, "character")
})

test_that("NJ/Old format works", {
    expect_is(as.data.frame(clarity_get("http://results.enr.clarityelections.com/NJ/Cape_May/71890/190686/Web01/en/summary.html")), "data.frame")
})

test_that("Madison/New format works", {
    expect_is(as.data.frame(clarity_get("http://results.enr.clarityelections.com/AL/Madison/72211/Web02/#/")), "data.frame")
})

