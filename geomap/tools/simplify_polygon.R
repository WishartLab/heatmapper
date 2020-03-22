library(rmapshaper)
library(stringi)
#FUNCTION to load map from GADM database
#Input variables
#*******************
# cntry_code - 3 chahracter country code which is on https://gadm.org/download_country_v3.html
# lvl - which level for political districts (0 - country level
#                                            1 - large countries state/province smaller countries counties
#                                            2 - large countries counties, smaller countries municipalities/parish/town)
load_map <- function(cntry_code,
                     lvl){
  library(raster)
  map <- getData('GADM', country = cntry_code, level = lvl)
  detach("package:raster", unload = TRUE)
  return(map)
}

#FUNCTION to simplify polygons using rmapshaper library
#Input variables
#*******************
# map - sp filetype map
# threshold - percentage to which level decrease the complexity of polygon borders, default 1%

simplify_polygons <- function(map,
                              threshold = 0.01){
  map_simplified <- ms_simplify(map, keep = threshold, keep_shapes = T)
  return(map_simplified)
}

#FUNCTION to prepare map for heatmapper
#Input variables
#*******************
# country_code - 3 chahracter country code which is on https://gadm.org/download_country_v3.html
# level - which level for political districts (0 - country level
#                                            1 - large countries state/province smaller countries counties
#                                            2 - large countries counties, smaller countries municipalities/parish/town)
# threshold - percentage to which level decrease the complexity of polygon borders, default 1%

prepare_map <- function(country_code,
                        threshold,
                        level){
  print("loading map")
  print("*************************")
  #load map
  map <- load_map(cntry_code = country_code,
                  lvl = level)
  library(dplyr)
  library(stringr)
  #Extract data from map object
  map_data <- map@data 
  #extract row names
  row_names <- map_data %>% row.names()
  #extract number of polygons
  nr_polygons <- map_data %>% dim() %>% first()
  #Extract polygon ids
  polygon_ids <- NULL
  for (i in 1:nr_polygons){
    id <- polygons[[i]]@ID
    polygon_ids <- c(polygon_ids,id)
  }
 
  print("Checking polygon IDs")
  print("*************************")
  #Check if polygon ids and data rownames match
  if (!identical(polygon_ids, row_names)){
    print("Polygon IDs do not match with dataframe")
    print("Updating polygon IDs")
    #Update polygon ids
    for (i in 1:(map_data %>% dim() %>% first())){
      map@polygons[[i]]@ID <- row_names[i]
    }
  }
  #Simplify polygons
  print("Simplifying polygons")
  print("*************************")
  map_simple <- simplify_polygons(map = map,
                                  threshold = threshold)
  
  #Clean the data on map object
  print("Cleaning dataframe")
  print("*************************")
  if (level == 2){
    map_data <- map_data %>% 
      mutate(row_nr = row_number()) %>%
      group_by(row_nr) %>% 
      mutate(state = stri_trans_general(NAME_1, id = "Latin-ASCII"),
             county = stri_trans_general(NAME_2, id = "Latin-ASCII"),
             state_abbreviation = (HASC_2 %>% str_split(pattern = "\\.") %>% unlist())[2]) %>% 
      ungroup()
    
    #Check NA-s in state abbreviation
    state_abb_na <- map_data$state_abbreviation %>% is.na() %>% sum()
    
    map_data_names <- map_data %>% 
      mutate(NAME = if_else(
        state_abb_na > nr_polygons/2,
        NAME = paste(county, state, sep = ", "),
        NAME = paste(county, state_abbreviation, sep = ", ")
      )) %>% 
      select(NAME)
    
  } else if (level == 1){
    map_data_names <- map_data %>% 
      mutate(row_nr = row_number()) %>%
      group_by(row_nr) %>% 
      mutate(NAME = stri_trans_general(NAME_1, id = "Latin-ASCII")) %>% 
      ungroup() %>% 
      select(NAME)
  }
  map_simple@data <- map_data_names
  return(map_simple)
}

# australia <- prepare_map(country_code = "AUS",
#                          threshold = 0.01,
#                          level = 2)
