#'Load Open Range maps for specified species.
#'
#'OpenRange_load_species returns spatial data for the specified species.
#' @param species A single species or a vector of species.
#' @param projection Numeric. What projection should maps be returned in?  4326 (default) or 3857 
#' @param scenario Which climate scenario(s) should be represented by maps?  See BIEN_ranges_list_scenarios for options. Set to NULL to download all.
#' @param default_only Logical. Should only default ranges be included? Default is TRUE.
#' @param ... Additional arguments passed to internal functions.
#' @return A SpatialPolygonsDataFrame containing range maps for the specified species.
#' @examples \donttest{
#' #' 
#' library(maps)
#' library(ggplot2)
#' library(sf)
#' 
#' species_vector <- c("Abies_lasiocarpa","Abies_amabilis")
#' abies_maps <- OpenRange_load_species(species = species_vector)
#' 
#' # To get all maps for a species, use "default = FALSE".
#' # Here, this returns maps with different thresholds from the same model
#' std_all <- OpenRange_load_species(species = "Stellaria debilis",
#'                                   default_only = FALSE)
#' 
#' #To just get the default map, use "default = TRUE"
#' std_default <- OpenRange_load_species(species = "Stellaria debilis",
#'                                       default_only = TRUE)
#' 
#' #get world map
#' 
#' world <- map("world", plot = FALSE, fill = TRUE)|>
#'   st_as_sf()
#' 
#' #Plotting ranges
#' 
#' ggplot(data = world)+
#'   geom_sf()+
#'   geom_sf(data = abies_maps,
#'           mapping = aes(fill = species),
#'           alpha=0.5)+
#'   coord_sf(xlim = st_bbox(abies_maps)[c(1,3)],
#'            ylim = st_bbox(abies_maps)[c(2,4)]) +
#'   theme_bw()
#' 
#'}
#' @family range functions
#' @export
OpenRange_load_species <- function(species,
                                   default_only = TRUE,
                                   projection = 4326,
                                   scenario = "present",
                                   ...){
  
  .is_char(species)
  .is_char(scenario)
  
  
  if(length(scenario)==1){
    if(scenario != "present"){default_only <- FALSE}
  }else{default_only <- FALSE}
  
  
  
  
  #make sure there are no spaces in the species names
  species <- gsub(" ","_",species)
  
  if(projection==4326){st_select<-"ST_AsText(geom2)"}
  if(projection==3857){st_select<-"ST_AsText(geom)"}
  
  if(default_only){
    default_map_select <- " and is_default = 1 "    
  }else{
    default_map_select <- ""
  }
  
  #If scenario is NULL, download ignore it and grab all
  
    if(!is.null(scenario)){
      
      scenario_select <- paste("AND scenario in (", paste(shQuote(scenario, type = "sh"),collapse = ', '), ")")
      
    }else{
      
      scenario_select <- ""
    }
  
  
  
  
  # set the query
  query <- paste("SELECT range_id,range_name,species_id,species,rangetype_id,source_id,source_name,run_id,run,batch_id,model_id,
                       model, statistics_unique_id, base_model, mod_type, model_moment, sampling, scenario_id, scenario, 
                       scenario_filecode, time_period, climate_model, rcp, threshold_id, threshold, background, is_default, 
                       basename, rel_path, ",st_select," 
                    FROM ranges.range 
                    WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ")",
                  scenario_select, default_map_select, 
                 "ORDER BY species ;")
  
  
  df <- ranges_sql(query)
  
  if(length(df)==0){
    message("No species matched")
  }else{
    
    
    poly <- st_as_sf(x = df,
                     wkt = "st_astext",
                     crs = "epsg:4326")
    
    
  }#else
  
  return(poly) 
  
}
