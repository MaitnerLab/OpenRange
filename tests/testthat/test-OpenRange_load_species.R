test_that("OpenRange_load_species returns sf", {
  
  skip_if_offline(host = "r-project.org")
  
  xs <- OpenRange_load_species(species = "Xanthium strumarium",
                                      default_only = TRUE)
  
  expect_s3_class(object = xs,class = "sf") 
  
  skip_if_not("sf" %in% class(xs))
  
  expect_equal(object = nrow(xs),expected = 1)  
  
})
