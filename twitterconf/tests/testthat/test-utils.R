test_that("get unique value works", {
  expect_equal(get_unique_value(airquality, Month), length(unique(airquality$Month)))
})

test_that("twitter embed retrieves expected content", {
  some_expected <- '<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Sunsets don&#39;t'

  embedded <- as.character(get_tweet_embed("Interior", "463440424141459456"))

  expect_true(grepl(some_expected, embedded))
})