test_that("scenarios returns df", {
  
  skip_if_offline(host = "r-project.org")
  
    scenarios <- OpenRange_list_scenarios()

  # test below assume a data dictionary and will be skipped if one isn't returned
  expect_s3_class(object = scenarios,class = "data.frame") 
    
})

