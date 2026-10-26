test_that("OpenRange_api_load_species returns sf", {
  
  skip_if_not_installed("vcr")
  
  vcr::use_cassette("OpenRange_api_load_species", {
    
    xs <- OpenRange_api_load_species(species = "Xanthium strumarium")
    
  })
  
  # OpenRange_api_load_species returns NULL if the API is unavailable
  skip_if(is.null(xs),
          message = "The OpenRange API is not available.")
  
  expect_s3_class(object = xs,class = "sf") 
  
  skip_if_not("sf" %in% class(xs))
  
  expect_equal(object = nrow(xs),expected = 1)  
  

})
