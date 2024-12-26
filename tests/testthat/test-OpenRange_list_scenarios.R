test_that("scenarios returns df", {
  
  skip_if_offline(host = "r-project.org")
  
    scenarios <- OpenRange_list_scenarios()

  expect_s3_class(object = scenarios,class = "data.frame") 
    
})

