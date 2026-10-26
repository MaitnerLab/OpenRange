# api urls

# URL for NSR API
#url <- "https://biendata.org/api/range/range-data?species=" # production
# N/A #testing

# Bad URLs for testing

# url <- "www.google.com"
# url <- "www.hisstank.com"

# vcr is used to record API responses so that the API tests do not require an
# internet connection.  It is only Suggested, so configure it only if present.

if (requireNamespace("vcr", quietly = TRUE)) {

  library("vcr") # *Required* as vcr is set up on loading

  invisible(vcr::vcr_configure(
    dir = vcr::vcr_test_path("fixtures")
  ))

}
