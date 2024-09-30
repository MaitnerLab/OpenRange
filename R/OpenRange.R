###########################################

#'Download information on available climate change scenarios
#'
#'OpenRange_list_scenarios provides information on climate scenarios with range projections available.
#' @param ... Additional arguments passed to internal functions.
#' @note Details on the construction of BIEN range maps is available at http://bien.nceas.ucsb.edu/bien/biendata/bien-3/
#' @return Data.frame containing climate change scenarios available in BIEN.
#' @examples \dontrun{
#'
#' cc_scenarios <- OpenRange_list_scenarios()
#'
#'}
#' @family range functions
#' @family metadata functions
#' @export
OpenRange_list_scenarios <- function(...){
  

  # set the query
  query <- paste("SELECT DISTINCT  scenario_id, scenario, scenario_filecode, time_period, climate_model, rcp  
                    FROM ranges.range ;")
  
  
  return(ranges_sql(query = query))
  

}


###########################################

#'Download range maps for given species.
#'
#'OpenRange_species_new extracts range maps for the specified species.
#' @param species A single species or a vector of species.
#' @param include_id Logical. Should the range_id be appended to the file name?  Needed to save multiple maps per species.
#' @param projection Numeric. What projection should maps be returned in?  4326 (default) or 3857 
#' @param scenario Which climate scenario should be represented by maps?  See BIEN_ranges_list_scenarios.
#' @param default_only Logical. Should only default ranges be included? Default is TRUE.
#' @param directory Directory that range maps should be saved in.  If none is specified, range maps will be saved in the current working directory.
#' @param matched Return a list of taxa that were downloaded. Default is TRUE.
#' @param match_names_only Check for range maps for the taxa specified without downloading range maps. Default is FALSE.
#' @param ... Additional arguments passed to internal functions.
#' @note Details on the construction of BIEN range maps is available at http://bien.nceas.ucsb.edu/bien/biendata/bien-3/
#' @return Range maps for specified species.
#' @examples \dontrun{
#' 
#'library(maps) #a convenient source of maps
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
#'OpenRanges_species("Abies_lasiocarpa",temp_dir)
#'
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
#'Abies_poly %>%
#'  st_drop_geometry()
#'
#' }
#' @family range functions
#' @importFrom sf st_as_sf st_write
#' @export
OpenRange_species <- function(species,
                                    default_only = TRUE,
                                    directory = NULL,
                                    matched = TRUE,
                                    match_names_only = FALSE,
                                    include_id = TRUE,
                                    projection = 4326,
                                    scenario = 'present', ...){

  .is_char(species)
  .is_log(matched)
  .is_log(match_names_only)
  .is_char(scenario)
  
  
  if(scenario != "present"){default_only = F}
  
  #make sure there are no spaces in the species names
  species <- gsub(" ","_",species)
  
  if(match_names_only==FALSE){
    
    #record original working directory,change to specified directory if given
    if(is.null(directory)){
      directory <- getwd()
    }
    
    if(projection==4326){st_select<-"ST_AsText(geom2)"}
    if(projection==3857){st_select<-"ST_AsText(geom)"}
    
    if(default_only){
      default_map_select <- " and is_default = 1 "    
      }else{
        default_map_select <- ""
      }
    
    
    # set the query
    query <- paste("SELECT range_id,range_name,species_id,species,rangetype_id,source_id,source_name,run_id,run,batch_id,model_id,
                       model, statistics_unique_id, base_model, mod_type, model_moment, sampling, scenario_id, scenario, 
                       scenario_filecode, time_period, climate_model, rcp, threshold_id, threshold, background, is_default, 
                       basename, rel_path, ",st_select," 
                    FROM ranges.range 
                    WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") 
                          AND scenario in (", paste(shQuote(scenario, type = "sh"),collapse = ', '), ")", default_map_select, 
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




#######################################
#'Load Open Range maps for specified species.
#'
#'OpenRange_load_species returns spatial data for the specified species.
#' @param species A single species or a vector of species.
#' @param projection Numeric. What projection should maps be returned in?  4326 (default) or 3857 
#' @param scenario Which climate scenario(s) should be represented by maps?  See BIEN_ranges_list_scenarios for options.
#' @param default_only Logical. Should only default ranges be included? Default is TRUE.
#' @param ... Additional arguments passed to internal functions.
#' @return A SpatialPolygonsDataFrame containing range maps for the specified species.
#' @examples \dontrun{
#'library(maps)
#'species_vector <- c("Abies_lasiocarpa","Abies_amabilis")
#'abies_maps <- OpenRange_load_species(species = species_vector)
#'
#'#New functionality
#'std_all <- OpenRange_load_species(species = "Stellaria debilis", default_only = FALSE) #Includes multiple thresholds
#'std_default <- OpenRange_load_species(species = "Stellaria debilis", default_only = TRUE) #Only includes one default threshold
#'
#'#Plotting files
#'plot(abies_maps)#plots the spatialpolygons, but doesn't mean much without any reference
#'map('world', fill = TRUE, col = "grey")#plots a world map (WGS84 projection), in grey
#'plot(std_default,col="forest green",add=TRUE) #adds the range of X. strumarium
#'plot(abies_maps[1,], add = T, col ="light green")
#'}
#' @family range functions
#' @export
OpenRange_load_species <- function(species, default_only = T, projection = 4326, scenario = "present", ...){
  
  .is_char(species)
  .is_char(scenario)
  
  if(scenario != "present"){default_only = F}
  
  #make sure there are no spaces in the species names
  species <- gsub(" ","_",species)
  
  if(projection==4326){st_select<-"ST_AsText(geom2)"}
  if(projection==3857){st_select<-"ST_AsText(geom)"}
  
  if(default_only){
    default_map_select <- " and is_default = 1 "    
  }else{
    default_map_select <- ""
  }
  
  
  # set the query
  query <- paste("SELECT range_id,range_name,species_id,species,rangetype_id,source_id,source_name,run_id,run,batch_id,model_id,
                       model, statistics_unique_id, base_model, mod_type, model_moment, sampling, scenario_id, scenario, 
                       scenario_filecode, time_period, climate_model, rcp, threshold_id, threshold, background, is_default, 
                       basename, rel_path, ",st_select," 
                    FROM ranges.range 
                    WHERE species in (", paste(shQuote(species, type = "sh"),collapse = ', '), ") 
                          AND scenario in (", paste(shQuote(scenario, type = "sh"),collapse = ', '), ")", default_map_select, 
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


#########################################################################################

#'Download range maps that intersect a user-supplied sf object.
#'
#'OpenRange_sf extracts range maps that intersect a specified SpatialPolygons or SpatialPolygonsDataFrame object.
#' @param sf An object of class sf
#' @param crop.ranges Should the ranges be cropped to the focal area? Default is FALSE.
#' @param projection Numeric. What projection should maps be returned in?  4326 (default) or 3857 
#' @param scenario Which climate scenario should be represented by maps?  See BIEN_ranges_list_scenarios.
#' @param default_only Logical. Should only default ranges be included? Default is TRUE.
#' @template ranges_spatial
#' @return All range maps that intersect the user-supplied shapefile.
#' @examples \dontrun{
#'
#'saguaro_poly <- OpenRange_load_species("Carnegiea gigantea")
#'
#'#Create a temp directory
#'temp_dir <- file.path(tempdir(), "BIEN_temp")
#'
#'#Get range maps for all species with ranges that overlap the saguaro
# OpenRange_sf(sf = saguaro_poly,
#             directory = temp_dir)
#'
#'#Note that this will save many sfs to the directory (or working directory)
#' }
#' @family range functions
#' @import sf
#' @export
OpenRange_sf <- function(sf,
                         directory = NULL,
                         species.names.only = FALSE,
                         return.species.list = TRUE,
                         crop.ranges = FALSE,
                         include.gid = FALSE,
                         projection = 4326,
                         scenario = 'present',
                         default_only = TRUE,
                         ...){
  
  .is_log(return.species.list)
  .is_log(species.names.only)
  .is_log(crop.ranges)
  .is_log(include.gid)
  
  wkt <- sf |>
    st_geometry() |>
    st_as_text()
  
  
  if(species.names.only == FALSE){
    
    #set directory for saving
    if(is.null(directory)){
      directory <- getwd()
    }  
    
    if(scenario != "present"){
      default_only = F
      }

    if(default_only){
      
      default_map_select <- " and is_default = 1 "
      
    }else{
      
      default_map_select <- ""
      
    }
    
    
    # set the query
    if(crop.ranges){
    if(projection==4326){
        query <- paste("SELECT range_id,range_name,species_id,species,rangetype_id,source_id,source_name,run_id,run,batch_id,model_id,
                           model, statistics_unique_id, base_model, mod_type, model_moment, sampling, scenario_id, scenario, 
                           scenario_filecode, time_period, climate_model, rcp, threshold_id, threshold, background, is_default, 
                           basename, rel_path, ST_AsText(ST_intersection(geom2,ST_GeographyFromText('SRID=4326;",paste(wkt),"'))),      
                        FROM ranges.range 
                        WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),geom2)
                        AND scenario in (", paste(shQuote(scenario, type = "sh"),collapse = ', '), ")", default_map_select, 
                       "ORDER BY species ;")
        }
    
    
    if(projection==3857){
      query <- paste("SELECT range_id,range_name,species_id,species,rangetype_id,source_id,source_name,run_id,run,batch_id,model_id,
                           model, statistics_unique_id, base_model, mod_type, model_moment, sampling, scenario_id, scenario, 
                           scenario_filecode, time_period, climate_model, rcp, threshold_id, threshold, background, is_default, 
                           basename, rel_path, ST_AsText(ST_intersection(geom,ST_GeographyFromText('SRID=3857;",paste(wkt),"'))),      
                        FROM ranges.range 
                        WHERE st_intersects(ST_GeographyFromText('SRID=3857;",paste(wkt),"'),geom)
                        AND scenario in (", paste(shQuote(scenario, type = "sh"),collapse = ', '), ")", default_map_select, 
                     "ORDER BY species ;")}  
    
    
    }else{
      
      if(projection==4326){
        
        query <- paste("SELECT range_id,range_name,species_id,species,rangetype_id,source_id,source_name,run_id,run,batch_id,model_id,
                           model, statistics_unique_id, base_model, mod_type, model_moment, sampling, scenario_id, scenario, 
                           scenario_filecode, time_period, climate_model, rcp, threshold_id, threshold, background, is_default, 
                           basename, rel_path, ST_AsText(geom2)       
                        FROM ranges.range 
                        WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),geom2)
                        AND scenario in (", paste(shQuote(scenario, type = "sh"),collapse = ', '), ")", default_map_select, 
                       "ORDER BY species ;")
      }
      
      
      if(projection==3857){
        query <- paste("SELECT range_id,range_name,species_id,species,rangetype_id,source_id,source_name,run_id,run,batch_id,model_id,
                           model, statistics_unique_id, base_model, mod_type, model_moment, sampling, scenario_id, scenario, 
                           scenario_filecode, time_period, climate_model, rcp, threshold_id, threshold, background, is_default, 
                           basename, rel_path, ST_AsText(geom)       
                        FROM ranges.range 
                        WHERE st_intersects(ST_GeographyFromText('SRID=3857;",paste(wkt),"'),geom)
                        AND scenario in (", paste(shQuote(scenario, type = "sh"),collapse = ', '), ")", default_map_select, 
                       "ORDER BY species ;")}   
      
      
    }
    
    # execute the query
    df <- ranges_sql(query = query, limit =10)
    
    if(length(df)==0){
      
      message("No species matched")
      
    }else{
      
      for(l in 1:length(df$species)){
        
        Species <- df$species[l]
        
        if(projection==3857){
          
          sp_range <- st_as_sf(x = df[l,, drop = FALSE],
                               wkt = "st_astext",
                               crs = "epsg:3857")
          
        }
        
        if(projection==4326){
          
          sp_range <- st_as_sf(x = df[l,, drop = FALSE],
                               wkt = "st_astext",
                               crs = "epsg:4326")
          
          
        }
        

          if(include.gid==T){

            st_write(obj = sp_range,
                     dsn = file.path(directory),
                     layer = paste(df$species[l],"_",df$range_id[l],sep=""),
                     driver = "ESRI Shapefile",
                     append = FALSE,
                     quiet=TRUE)
            
            
          }else{

            st_write(obj = sp_range,
                     dsn = file.path(directory),
                     layer = paste(df$species[l],sep=""),
                     driver = "ESRI Shapefile",
                     append = FALSE,
                     quiet=TRUE)
            
          }
          
          #save output
        }#if sp_range is not null  
      
      if(return.species.list){
        
        return(df[,2])
        
      }#if return.species.list  
      
    }#else
    
  }#species names only if statement
  
  if(species.names.only == TRUE){
    
    query <- paste("SELECT species
                   FROM ranges.range
                   WHERE st_intersects(ST_GeographyFromText('SRID=4326;",paste(wkt),"'),geom2)")
    
    # create query to retrieve
    
    df <- ranges_sql(query)
    
    if(length(df)==0){
      message("No species found")
    }else{
      return(df)
      
    }
    
  } #species.names.only ==TRUE


}
