test_that("OpenRange_api_load_species returns sf", {
  
  vcr::use_cassette("OpenRange_api_load_species", {
    
    xs <- OpenRange_api_load_species(species = "Xanthium strumarium")
    
  })
  
  expect_s3_class(object = xs,class = "sf") 
  
  skip_if_not("sf" %in% class(xs))
  
  expect_equal(object = nrow(xs),expected = 1)  
  

})

