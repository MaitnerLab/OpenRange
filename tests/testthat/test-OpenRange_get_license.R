test_that("OpenRange_get_license returns df", {
  
  skip_on_cran()
  skip_if_offline(host = "r-project.org")
  
  license <- OpenRange_get_license()
  
  # OpenRange_get_license returns NULL if the database is unavailable
  skip_if(is.null(license),
          message = "The OpenRange database is not available.")
  
  # test below assume a data dictionary and will be skipped if one isn't returned
  expect_s3_class(object = license,
                  class = "data.frame") 
  
})
