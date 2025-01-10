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
#' @examples \donttest{
#'
#'saguaro_poly <- OpenRange_load_species("Carnegiea gigantea")
#'
#'#Create a temp directory
#'temp_dir <- file.path(tempdir(), "BIEN_temp")
#'
#'#Get range maps for all species with ranges that overlap the range range of saguaros
#'OpenRange_sf(sf = saguaro_poly,
#'            directory = temp_dir)
#'
#'#Note that this will save many sfs to the directory (or working directory)
#' }
#' @family range functions
#' @import sf
#' @export
OpenRange_sf <- function(sf,
                         directory,
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
