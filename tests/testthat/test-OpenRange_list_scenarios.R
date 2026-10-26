test_that("scenarios returns df", {
  
  skip_on_cran()
  skip_if_offline(host = "r-project.org")
  
  scenarios <- OpenRange_list_scenarios()
  
  # OpenRange_list_scenarios returns NULL if the database is unavailable
  skip_if(is.null(scenarios),
          message = "The OpenRange database is not available.")

  expect_s3_class(object = scenarios,class = "data.frame") 
    
})
