################################

#'Run an SQL query on the OpenRange database.
#'
#'ranges_sql is an internal function used to submit SQL queries.
#' @param query A PostgreSQL query.
#' @param user The username used to access the BIEN database
#' @param password The password associated with the username
#' @param ranges Alternative value to be substituted for "ranges" in queries when not NULL.
#' @param limit A limit on the number of records to be returned.  Should be a single number or NULL (the default).
#' @param return.query Should  the query used be returned rather than executed?  Default is FALSE
#' @param schema An alternative schema to be accessed.  Used for testing purposes.
#' @param db_name An alternate database to be used rather than the default, vegbien.
#' @param print.query Should  the query used be printed?  Default is FALSE
#' @return A dataframe returned by the query.
#' @keywords internal
#' @import RPostgreSQL
ranges_sql <- function(query,
                    user = 'public_bien',
                    password = 'bien_public',
                    ranges = NULL,
                    limit = NULL,
                    return.query = FALSE,
                    schema = "analytical_db",
                    db_name = 'vegbien',
                    print.query = FALSE){

  # user <- 'public_bien'
  # password <- 'bien_public'
  
  # .is_char(query)
  # .is_char(user)
  # .is_char(password)
  
  if(print.query){
    query <- gsub(pattern = "\n",replacement = "",query)
    query <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  
  if(!is.null(ranges)){
    query<-gsub(pattern = "ranges",replacement = ranges,x = query)}
  
  if(!is.null(limit)){
    query<-gsub(pattern = " ;",replacement = paste(" LIMIT ",limit,";"),x = query)}
  
  if(return.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    return(query)
  }
  
  
  host='vegbiendev.nceas.ucsb.edu'
  dbname= db_name
  user=user
  password=password
  # Name the database type that will be used
  drv <- DBI::dbDriver('PostgreSQL')
  # establish connection with database
  con <- DBI::dbConnect(drv, host=host, dbname=dbname, user=user, password = password)
  
  
  # create query to retrieve
  df <- DBI::dbGetQuery(con, statement = query);
  
  DBI::dbDisconnect(con)
  
  if(print.query){
    query<-gsub(pattern = "\n",replacement = "",query)
    query<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, perl=TRUE)
    print(query)
  }
  
  return(df)
  
}
