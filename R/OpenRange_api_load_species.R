#'Load range maps for given species using the API.
#'
#'OpenRange_api_load_species extracts range maps for the specified species.
#' @param species A single species.
#' @param ... Additional arguments passed to internal functions.
#' @note Details on the construction of BIEN range maps is available at http://bien.nceas.ucsb.edu/bien/biendata/bien-3/
#' @return Range maps for specified species.
#' @examples \donttest{
#' 
#'library(maps) #a convenient source of maps
#'library(sf)
#'
#'temp_dir <- file.path(tempdir(), "BIEN_temp")
#'
#'#Download ranges
#'Abies_poly <- OpenRange_api_load_species(species = "Abies_amabilis")
#'
#'#Plotting files
#'plot(Abies_poly[,1])#plots the range, but doesn't mean much without any reference
#'map('world', fill = TRUE, col = "grey")#plots a world map (WGS84 projection), in grey
#'plot(Abies_poly,col="forest green",add=TRUE) #adds the range to the map
#'
#'#Getting data from the files
#'Abies_poly |>
#'  st_drop_geometry()
#'
#' }
#' @family range functions
#' @importFrom sf st_as_sf st_write
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export

OpenRange_api_load_species <- function(species,
                                  ...){
  
  #projection set (just WGS84 for now)
  
  projection <- 4326
  
  #check input formats
  .is_char(species)
  
  #make sure there are no spaces in the species names
  species <- gsub(" ","_",species)
  

  #make the URL
  url <- paste0("https://biendata.org/api/range/range-data?species=",species)
  
  # Download the data from the API
  req <- GET(url = url)
  
  # convert to raw - this throws an error
  raw_content <- rawToChar(req$content)
  
  # convert raw content from JSON to R object
  df <- fromJSON(raw_content)
  
  # check for messages
  
  if("message" %in% names(df)){
    
    message(df$message)
    return(invisible(NULL))
    
  }
  
  
  if(length(df)==0){
    message("No species matched")
  }else{
    
    
    poly <- st_as_sf(x = df,
                     wkt = "st_astext",
                     crs = "epsg:4326")
    
    
  }#else
  
  return(poly) 
  
}#end fx



