#'Download range maps for given species.
#'
#'OpenRange_species extracts range maps for the specified species.
#' @param species A single species or a vector of species.
#' @param include_id Logical. Should the range_id be appended to the file name?  Needed to save multiple maps per species.
#' @param projection Numeric. What projection should maps be returned in?  4326 (default) or 3857 
#' @param scenario Which climate scenario should be represented by maps?  See BIEN_ranges_list_scenarios. Set to NULL to download all.
#' @param default_only Logical. Should only default ranges be included? Default is TRUE.
#' @param directory Directory that range maps should be saved in.
#' @param matched Return a list of taxa that were downloaded. Default is TRUE.
#' @param match_names_only Check for range maps for the taxa specified without downloading range maps. Default is FALSE.
#' @param ... Additional arguments passed to internal functions.
#' @note Details on the construction of BIEN range maps is available at http://bien.nceas.ucsb.edu/bien/biendata/bien-3/
#' @return Range maps for specified species.
#' @examples \donttest{
#' 
#'library(maps) #a convenient source of maps
#'library(sf)
#'
#'species_vector <- c("Abies_lasiocarpa","Abies_amabilis")
#'
#'#check whether the species are available
#'OpenRange_species(species_vector,match_names_only = TRUE)
#'
#'#Create a temp directory
#'temp_dir <- file.path(tempdir(), "BIEN_temp")
#'
#'#Download ranges
#'OpenRange_species(species = species_vector,
#'                  directory = temp_dir)#saves ranges to a temporary directory
#'
# OpenRange_species(species = "Abies_lasiocarpa",
#                   directory = temp_dir)
# 
#'#Reading files
#'
#'Abies_poly <- st_read(dsn = temp_dir,
#'                      layer = "Abies_amabilis_117684")
#'
#'#Plotting files
#'plot(Abies_poly[,1])#plots the range, but doesn't mean much without any reference
#'map('world', fill = TRUE, col = "grey")#plots a world map (WGS84 projection), in grey
#'plot(Abies_poly,col="forest green",add=TRUE) #adds the range of Abies lasiocarpa to the map
#'
#'#Getting data from the files
#'Abies_poly |>
#'  st_drop_geometry()
#'
#' }
#' @family range functions
#' @importFrom sf st_as_sf st_write
#' @export
OpenRange_species <- function(species,
                              directory,
                              default_only = TRUE,
                              matched = TRUE,
                              match_names_only = FALSE,
                              include_id = TRUE,
                              projection = 4326,
                              scenario = 'present', ...){
  
  .is_char(species)
  .is_log(matched)
  .is_log(match_names_only)
  .is_char(scenario)
  
  if(length(scenario)==1){
    if(scenario != "present"){default_only <- FALSE}
  }else{default_only <- FALSE}
  
  #make sure there are no spaces in the species names
  species <- gsub(" ","_",species)
  
  if(match_names_only==FALSE){
    
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
    }#else
    
    #list matched species
    
    if(matched==TRUE){
      found <- as.data.frame(cbind(species,matrix(nrow=length(species),ncol=1,data="No")))
      colnames(found) <- c("Species","Range_map_downloaded?")
      found$`Range_map_downloaded?` <- as.character(found$`Range_map_downloaded?`)
      found$`Range_map_downloaded?`[which(species%in%df$species)] <- "Yes"
      return(found)
    }#matched = true
  }#match names only if statement
  
  if(match_names_only == TRUE){
    
    rangeQuery <- paste("SELECT species FROM ranges.range WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") ORDER BY species ;")
    # create query to retrieve
    df <- ranges_sql(rangeQuery,...)
    
    
    if(length(df)==0){
      message("No species matched")
    }else{
      found<-as.data.frame(cbind(species,matrix(nrow=length(species),ncol=1,data="No")))
      colnames(found)<-c("Species","Range_map_available?")
      found$`Range_map_available?`<-as.character(found$`Range_map_available?`)
      found$`Range_map_available?`[which(species%in%df$species)]<-"Yes"
      return(found)
      
    }
    
  } #matched_names_only ==TRUE
}
