#'Download range maps for given species using the API.
#'
#'OpenRange_api_species extracts range maps for the specified species.
#' @param species A single species.
#' @param include_id Logical. Should the range_id be appended to the file name?  Needed to save multiple maps per species.
#' @param directory Directory that range maps should be saved in.  If none is specified, range maps will be saved in the current working directory.
#' @param ... Additional arguments passed to internal functions.
#' @note Details on the construction of BIEN range maps is available at http://bien.nceas.ucsb.edu/bien/biendata/bien-3/
#' @return Range maps for specified species.
#' @examples \donttest{
#'
#' library(OpenRange)
#' library(maps) #a convenient source of maps
#' library(sf)
#' library(ggplot2)
#' 
#' 
#' # Create temp directory
#' 
#' temp_dir <- file.path(tempdir(), "BIEN_temp")
#' 
#' #Download ranges
#' 
#' OpenRange_api_species(species = "Abies_amabilis",
#'                       directory = temp_dir,
#'                       include_id = TRUE)#saves ranges to a temporary directory
#' 
#' #Reading files
#' 
#' Abies_poly <- st_read(dsn = temp_dir,
#'                       layer = "Abies_amabilis_117684")
#' 
#' # Get a map to plot on
#' 
#' world <- map("world", plot = FALSE, fill = TRUE)|>
#'   st_as_sf()
#' 
#' #Plotting files
#' 
#' ggplot(data = world)+
#'   geom_sf()+
#'   geom_sf(data = Abies_poly,
#'           fill="green")+
#'   coord_sf(xlim = st_bbox(Abies_poly)[c(1,3)],
#'            ylim = st_bbox(Abies_poly)[c(2,4)]) +
#'   theme_bw()
#' 
#' #Getting data from the files
#' 
#' Abies_poly |>
#'   st_drop_geometry()
#' 
#' }
#' @family range functions
#' @importFrom sf st_as_sf st_write
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
OpenRange_api_species <- function(species,
                              directory = NULL,
                              include_id = TRUE,
                              ...){
  
  #projection set (just WGS84 for now)
  
    projection <- 4326
  
  #check input formats
    .is_char(species)

  #make sure there are no spaces in the species names
    species <- gsub(" ","_",species)
    
  #record original working directory,change to specified directory if given
    
    if(is.null(directory)){
      directory <- getwd()
    }
    
  
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
  
      for(l in 1:length(df$species)){
        
        Species <- df$species[l]
        
        if(projection==4326){
          
          sp_range <- st_as_sf(x = df[l,, drop = FALSE],
                               wkt = "st_astext",
                               crs = "epsg:4326")
          
          
        }
        
        if(projection==3857){
          
          sp_range <- st_as_sf(x = df[l,, drop = FALSE],
                               wkt = "st_astext",
                               crs = "epsg:3857")
          
          
        }
        
        
        
        #save output
        if(include_id){
          
          suppressWarnings(st_write(obj = sp_range,
                                    dsn = file.path(directory),
                                    layer = paste(df$species[l],"_",df$range_id[l],sep=""),
                                    driver = "ESRI Shapefile",
                                    append = FALSE,
                                    quiet=TRUE))
          
        }else{
          
          suppressWarnings(st_write(obj = sp_range,
                                    dsn = file.path(directory),
                                    layer = paste(df$species[l]),
                                    driver = "ESRI Shapefile",
                                    append = FALSE,
                                    quiet=TRUE))
          
        }
        
        
      }#for species in df loop
  
    return(invisible(NULL))
  
  
    }#end fx



