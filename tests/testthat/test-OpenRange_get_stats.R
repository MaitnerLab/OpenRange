test_that("OpenRange_get_stats returns list", {
  
  skip_on_cran()
  skip_if_offline(host = "r-project.org")
  
  stats <- OpenRange_get_stats()
  
  # OpenRange_get_stats returns NULL if the database is unavailable
  skip_if(is.null(stats),
          message = "The OpenRange database is not available.")
  
  # check for a list
  expect_identical(object = class(stats),expected = "list")
  
  #test below will be skipped if the stats object isn't a list
  
  skip_if_not(class(stats) == "list")
  
  expect_equal(object = length(stats),expected = 3)
  
})
