library(raster)
library(rgeos)
library(leaflet)
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
minarea <- 0.005
fileName <- paste0(country, "_", level, ".rds")
filePath <- paste0("heatmapper/geomap/data/", fileName)

# source: http://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding
unwanted_chars = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
	'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
	'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
	'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
	'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

#x <- readRDS("file.rds") 
#x <- readOGR("folder/file.shp", layer = "file", verbose = FALSE)
x <- getData('GADM', country=country, level=level)
print(names(x))
x <- getSmallPolys(x, minarea)
origNames <- x[[paste0("NAME_", level)]]

origNames <- chartr(paste(names(unwanted_chars), collapse=''), 
	paste(unwanted_chars, collapse=''), origNames)

print(origNames)

x <- gSimplify(x, tol=gSimplifyTol, topologyPreserve=TRUE)
x2 <- as(x, "SpatialPolygonsDataFrame")
x$dummy <- origNames
names(x) <- "NAME"

leaflet(x) %>% addTiles() %>% addPolygons(weight = 1, color = "black")

saveRDS(x, filePath)

#test <- readRDS(filePath)
#leaflet(test) %>% addTiles() %>% addPolygons(weight = 1, color = "black")

