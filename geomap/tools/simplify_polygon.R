if (!requireNamespace(c("dplyr",
                        "stringr",
                        "stringi",
                        "RCurl",
                        "rmapshaper",
                        "raster"),
                      quietly = TRUE)){
  install.packages(c("dplyr",
                   "stringr",
                   "stringi",
                   "RCurl",
                   "rmapshaper",
                   "raster"))
}
library(rmapshaper)
library(stringi)
library(RCurl)
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
  map <- NULL
  map <- tryCatch({
    getData('GADM', country = cntry_code, level = lvl)
  }, error = function(err) {
    cat("ERROR: ", conditionMessage(err), "\n")
  })
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
  #Validating country code
  url_temp <- paste("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_",
                       country_code,
                       "_0_sp.rds",
                       sep = "")
  request <- url.exists(url = url_temp)
  if(!request){
    print("No map available. Please check the country code")
    return(NULL)
  }
  #Validating level
  if (!(level %in% c(0,1,2,3))){
    print("Level should be integer between 0 and 3")
    return(NULL)
  }
  print("loading map")
  print("*************************")
  #load map
  map <- load_map(cntry_code = country_code,
                  lvl = level)
  if (is.null(map)){
    print("No map found. Check level.")
    return(NULL)
  }
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
    print("*************************")
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
  map_data <- map_data %>% 
    mutate(row_nr = row_number())
  if (level == 2){
    map_data <- map_data %>% 
      group_by(row_nr) %>% 
      mutate(state = stri_trans_general(NAME_1, id = "Latin-ASCII"),
             county = stri_trans_general(NAME_2, id = "Latin-ASCII"),
             state_abbreviation = (HASC_2 %>% str_split(pattern = "\\.") %>% unlist())[2]) %>% 
      ungroup()
    
    #Check NA-s in state abbreviation
    state_abb_na <- map_data$state_abbreviation %>% is.na() %>% sum()
    
    map_data_names <- map_data %>% 
      group_by(row_nr) %>% 
      mutate(NAME = if_else(
        state_abb_na > nr_polygons/2,
        paste(county, state, sep = ", "),
        paste(county, state_abbreviation, sep = ", ")
      )) %>% 
      ungroup() %>% 
      select(NAME)
    
  } else if (level == 1){
    map_data_names <- map_data %>% 
      group_by(row_nr) %>% 
      mutate(NAME = stri_trans_general(NAME_1, id = "Latin-ASCII")) %>% 
      ungroup() %>% 
      select(NAME)
  } else if (level == 3){
    map_data_names <- map_data %>% 
      group_by(row_nr) %>% 
      mutate(
        state = stri_trans_general(NAME_1, id = "Latin-ASCII"),
        county = stri_trans_general(NAME_2, id = "Latin-ASCII"),
        parish = stri_trans_general(NAME_3, id = "Latin-ASCII"),
        NAME = paste(parish, county, state, sep = ", ")) %>% 
      ungroup() %>% 
      select(NAME)
  } else {
    map_data_names <- map_data %>% 
      rename(NAME = NAME_0) %>% 
      select(NAME)
  }
  map_simple@data <- map_data_names
  return(map_simple)
}

# canada <- prepare_map(country_code = "CAN",
#                          threshold = 0.01,
#                          level = 2)
