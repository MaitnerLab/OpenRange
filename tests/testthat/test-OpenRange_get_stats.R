test_that("OpenRange_get_stats returns list", {
  
  skip_if_offline(host = "r-project.org")
  
  stats <- OpenRange_get_stats()
  
  # check for a list
  expect_identical(object = class(stats),expected = "list")
  
  #test below will be skipped if the stats object isn't a list
  
  skip_if_not(class(stats) == "list")
  
  expect_equal(object = length(stats),expected = 3)
  
})
