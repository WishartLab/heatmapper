library(raster)
library(rgeos)
# source: http://gis.stackexchange.com/questions/62292/how-to-speed-up-the-plotting-of-polygons-in-r
# Get the main polygons, will determine by area.
getSmallPolys <- function(poly, minarea) {
  # Get the areas
  areas <- lapply(poly@polygons, function(x) sapply(x@Polygons, function(y) y@area))
	
  # Quick summary of the areas
  print(quantile(unlist(areas)))

  # Which are the big polygons?
  bigpolys <- lapply(areas, function(x) which(x > minarea))
	print(bigpolys[[1]][4])
  length(unlist(bigpolys))

  # Get only the big polygons
  for(i in 1:length(bigpolys)){
    if(length(bigpolys[[i]]) >= 1 && !is.na(bigpolys[[i]][4]) && bigpolys[[i]][4] >= 1){
      poly@polygons[[i]]@Polygons <- poly@polygons[[i]]@Polygons[bigpolys[[i]]]
      poly@polygons[[i]]@plotOrder <- 1:length(poly@polygons[[i]]@Polygons)
    }
  }
  return(poly)
}


country <- "CAN"
level <- 1
gSimplifyTol <- 0.05
minarea <- 0.001
fileName <- paste0(country, "_", level, ".rds")
filePath <- paste0("heatmapper/choropleth/data/", fileName)

#x <- readRDS("file.rds") 
#x <- readOGR("folder/file.shp", layer = "file", verbose = FALSE)
x <- getData('GADM', country=country, level=level)
print(names(x))
x <- getSmallPolys(x, minarea)
origNames <- x[[paste0("NAME_", level)]]
print(origNames)

x <- gSimplify(x, tol=gSimplifyTol, topologyPreserve=TRUE)
x2 <- as(x, "SpatialPolygonsDataFrame")
x$dummy <- origNames
names(x) <- "NAME"

#leaflet(x) %>% addTiles() %>% addPolygons(weight = 1, color = "black")

saveRDS(x, filePath)

#test <- readRDS(filePath)
#leaflet(test) %>% addTiles() %>% addPolygons(weight = 1, color = "black")

