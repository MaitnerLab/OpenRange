test_that(".resource_unavailable returns NULL with an informative message", {
  
  expect_message(object = out <- OpenRange:::.resource_unavailable(resource = "test resource"),
                 regexp = "could not be reached")
  
  expect_null(object = out)
  
})

test_that(".api_query fails gracefully when the API cannot be reached", {
  
  skip_on_cran()
  
  # Port 1 on the loopback interface should not be listening, so this stands in
  # for an unavailable Internet resource.  No error should be thrown.
  
  expect_message(object = out <- OpenRange:::.api_query(url = "http://127.0.0.1:1/"),
                 regexp = "could not be reached")
  
  expect_null(object = out)
  
})

test_that("ranges_sql fails gracefully when the database cannot be reached", {
  
  skip_on_cran()
  
  # A database name that does not exist stands in for an unavailable resource.
  # No error should be thrown.
  
  expect_message(object = out <- OpenRange:::ranges_sql(query = "select 1 ;",
                                                        db_name = "no_such_database_xyz"),
                 regexp = "could not be reached")
  
  expect_null(object = out)
  
})
