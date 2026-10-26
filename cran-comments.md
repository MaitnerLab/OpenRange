## Test environments
* local  "Windows" "10 x64" "build 17763",  R 4.4.1
* win-builder
* macOS builder
* ubuntu-latest on GitHub
* macos-13 on GitHub
* macos-latest on GitHub
* windows-latest on GitHub

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes

This release addresses the CRAN check ERROR on
r-devel-linux-x86_64-fedora-gcc, where re-building the vignette failed because
the remote PostgreSQL database was temporarily refusing connections
("FATAL: remaining connection slots are reserved for non-replication superuser
connections"), and the reminder of the CRAN policy that packages using Internet
resources should fail gracefully with an informative message.

The package now handles unavailable Internet resources as follows:

* The internal `ranges_sql()` function wraps the database connection and query
  in `tryCatch()`.  If the database cannot be reached, is overloaded, or returns
  an error, an informative message is emitted and `NULL` is returned invisibly
  rather than an error being thrown.  Connections are closed via `on.exit()`.
* A new internal helper, `.api_query()`, does the same for the API: connection
  errors, HTTP error statuses, and unparseable responses all result in an
  informative message and an invisible `NULL`.
* Conversion of the returned records into spatial (`sf`) objects is also wrapped
  in `tryCatch()`, so a change in the remote schema produces an informative
  message rather than an error.
* All exported functions check for `NULL` and return `NULL` invisibly, so no
  error is propagated to the user.  They also no longer error when a query
  succeeds but returns no records.
* The vignette checks whether the database is reachable and, if it is not,
  displays the example code without evaluating it, along with a note explaining
  why.  The vignette therefore builds successfully whether or not the resource
  is available.
* Tests that require an Internet resource are skipped on CRAN and are also
  skipped if the resource is unavailable.  New tests confirm that the graceful
  failure paths return `NULL` with a message rather than erroring.
