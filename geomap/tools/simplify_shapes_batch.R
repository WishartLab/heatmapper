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


# Load file with list of maps we want.
maps = read.csv(file='missing3.csv', stringsAsFactors = FALSE, header = FALSE);
count <- 0
count.all <- 0
code.strings <- array() # string we can paste into ui.R
code.strings.all <- array() # string we can paste into ui.R
cases.warn <- array() # to keep track of which maps produced warnings
cases.err <-array() # to keep track of which maps produced errors
had.warn <- FALSE
had.err <- FALSE
for (i in 1:dim(maps)[1]) {
	country <- maps[i,1]
	name <- maps[i,2]
	level <- maps[i,3]
	
	print(paste(country, level, sep=""));
	
	gSimplifyTol <- 0.005
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
	
	result = tryCatch({
			x <- getData('GADM', country=country, level=level)
			# print(names(x))
			
			count.all <<- count.all + 1
			level.string <- ""
			if (level >= 2) {
				level.string <- paste(" (Level ", level, ")", sep="")
			}
			code.strings.all[count.all + 1] <- paste("\"", name, level.string, "\" = \'data/", basename(filePath), "\',", sep="");
			
			# Remove comments - added by David Arndt
			#slot(x, "polygons") <- lapply(slot(x, "polygons"), "comment<-", NULL)
			#print(slot(x, "polygons"))
			
			x <- getSmallPolys(x, minarea)
			origNames <- x[[paste0("NAME_", level)]]

			origNames <- chartr(paste(names(unwanted_chars), collapse=''), 
				paste(unwanted_chars, collapse=''), origNames)

			#print(origNames)

			x <- gSimplify(x, tol=gSimplifyTol, topologyPreserve=TRUE)
			x2 <- as(x, "SpatialPolygonsDataFrame")
			x$dummy <- origNames
			names(x) <- "NAME"

			leaflet(x) %>% addTiles() %>% addPolygons(weight = 1, color = "black")

			saveRDS(x, filePath)

			count <<- count + 1
			level.string <- ""
			if (level >= 2) {
				level.string <- paste(" (Level ", level, ")", sep="")
			}
			code.strings[count + 1] <- paste("\"", name, level.string, "\" = \'data/", basename(filePath), "\',", sep="");

			#test <- readRDS(filePath)
			#leaflet(test) %>% addTiles() %>% addPolygons(weight = 1, color = "black")
		
# 		}, warning = function(war) {
# 			print (paste("Warning retrieving", country, level, ":", sep=" "));
# 			print(war)
# 			
# 			if (had.warn) {
# 				cases.warn[length(cases.warn)+1] <<- paste(country, name, level, sep=",")
# 			} else {
# 				cases.warn[1] <- paste(country, name, level, sep=",")
# 				had.warn <<- TRUE
# 			}
# 			
		}, error = function(err) {
			print (paste("Error retrieving", country, level, ":", sep=" "));
			print(err)
			
			if (had.err) {
				cases.err[length(cases.err)+1] <<- paste(country, name, level, sep=",")
			} else {
				cases.err[1] <- paste(country, name, level, sep=",")
				had.err <<- TRUE
			}
			
		}, finally = {
			
		});
	
	
	#print ("result = ", result);
}

write(code.strings, "code_strings.txt")
write(code.strings.all, "code_strings_all.txt")
write(cases.warn, "warnings.txt")
write(cases.err, "errors.txt")
print (paste("Number of ok maps:", toString(count)));

