#'Download range maps for given species using the API.
#'
#'OpenRange_api_species downloads range maps for the specified species.
#' @param species A single species.
#' @param include_id Logical. Should the range_id be appended to the file name?  Needed to save multiple maps per species.
#' @param directory Directory that range maps should be saved in.
#' @param ... Additional arguments passed to internal functions.
#' @note Details on the construction of BIEN range maps is available at http://bien.nceas.ucsb.edu/bien/biendata/bien-3/
#' @note This function relies on an Internet resource.  If that resource is
#'   unavailable, NULL will be returned along with an informative message.
#' @return NULL. Called for its side effect of downloading range maps.
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
#'                       include_id = TRUE) # saves ranges to a temporary directory
#'
#' # Nothing is saved if the data source is unavailable, so check that the
#' # file exists before using it.
#'
#' if(file.exists(file.path(temp_dir, "Abies_amabilis_117684.shp"))){
#'
#'   #Reading files
#'
#'   Abies_poly <- st_read(dsn = temp_dir,
#'                         layer = "Abies_amabilis_117684")
#'
#'   # Get a map to plot on
#'
#'   world <- map("world", plot = FALSE, fill = TRUE)|>
#'     st_as_sf()
#'
#'   #Plotting files
#'
#'   ggplot(data = world)+
#'     geom_sf()+
#'     geom_sf(data = Abies_poly,
#'             fill="green")+
#'     coord_sf(xlim = st_bbox(Abies_poly)[c(1,3)],
#'              ylim = st_bbox(Abies_poly)[c(2,4)]) +
#'     theme_bw()
#'
#'   #Getting data from the files
#'
#'   Abies_poly |>
#'     st_drop_geometry()
#'
#' }
#'
#' }
#' @family range functions
#' @importFrom sf st_as_sf st_write
#' @export
OpenRange_api_species <- function(species,
                              directory,
                              include_id = TRUE,
                              ...){

  #projection set (just WGS84 for now)

    projection <- 4326

  #check input formats
    .is_char(species)

  #make sure there are no spaces in the species names
    species <- gsub(" ","_",species)

  #make the URL
    url <- paste0("https://biendata.org/api/range/range-data?species=",species)

  # Query the API.  Returns NULL (with a message) if the API is unavailable.
    df <- .api_query(url = url)

    if(is.null(df)){return(invisible(NULL))}

  # check for messages

    if("message" %in% names(df)){

      message(df$message)
      return(invisible(NULL))

    }

    if(length(df) == 0 || length(df$species) == 0){

      message("No species matched")
      return(invisible(NULL))

    }

      for(l in 1:length(df$species)){

        Species <- df$species[l]

        if(projection==4326){record_crs <- "epsg:4326"}
        if(projection==3857){record_crs <- "epsg:3857"}

        # Returns NULL (with a message) if the records cannot be interpreted
        # as spatial data.
        sp_range <- .as_sf(df = df[l,, drop = FALSE],
                           crs = record_crs,
                           resource = "OpenRange API")

        if(is.null(sp_range)){return(invisible(NULL))}

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


