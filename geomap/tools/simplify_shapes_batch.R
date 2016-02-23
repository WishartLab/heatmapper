library(raster)
library(rgeos)
library(leaflet)

###############################################################################
#
# Script to download a list of maps from gadm.org and simplify their shapes for
# faster loading.
#
# To run, put your input file into the parent directory of the heatmapper
# project, and then change to that same directory for running the script (not
# sure why, but it works). E.g.
# 
#   $ Rscript heatmapper/geomap/tools/simplify_shapes_batch.R
#
# Input:
# - A .csv file as given by the input.file variable. The first column should
#   consist of the 3-letter country code, the second column the human-readable
#   name of the country, and the third column the GADM map level. An example is
#   present in geomap/tools/.
#
# Output:
# - Updates map files in the geomap/data directory.
# - Writes cases that were not successfully processed to errors.txt
# - Writes R code for cases that were successful to code_strings.txt. This code
#   can be pasted into geomap/ui.R.
# - Writes code_strings_all.txt, which also includes cases that were
#   successfully downloaded but not successfully processed.
#
# Adapted from Sasha Babicki's original script by David Arndt, Feb 2016.
#
###############################################################################

input.file <- 'maps_to_get.csv'

# Tolerance level when simplifying shapes. Lower number = more detail.
# Note: These values were arrived at through a fair amount of testing. A balance must be
# sought between map loading speed and map resolution, and the values below gave a pretty
# good compromise, taking into account that some users may want to create maps of
# particular regions within larger countries (e.g. the Eastern U.S. coast, the Caucasus,
# or the region around Beijing).
gSimplifyTolMax <- 0.03 # Highest (coarsest) tolerance level for large/complex countries.
gSimplifyTolMed <- 0.006 # Tolerance level for mid-complexity countries.
gSimplifyTolMin <- 0.0003 # Tolerance level for small/simple countries.
largeCountryAreas <- 20000 # Countries with an areas count at this level or above are considered large/complex.
medCountryAreas <- 800 # Countries with an areas count at this level or above are considered at least mid-complexity.
smallCountryAreas <- 100 # Countries with an areas count below this level are considered small/simple.

minarea <- 0.005

# source: http://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding
unwanted_chars = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
	'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
	'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
	'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
	'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

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

# Main map processing routine.
# remove.comments indicates whether we should remove comments. Sometime we should, sometimes we shouldn't.
processMap <- function(country, level, remove.comments) {
	fileName <- paste0(country, "_", level, ".rds")
	filePath <- paste0("heatmapper/geomap/data/", fileName)
	
	#x <- readRDS("file.rds") 
	#x <- readOGR("folder/file.shp", layer = "file", verbose = FALSE)
	
	x <- getData('GADM', country=country, level=level)
	# print(names(x))
	
	# obtain map size by areas count, and assign simplification tolerance level based on this
	areas <- lapply(x@polygons, function(x) sapply(x@Polygons, function(y) y@area))
	areas.count <- length(unlist(areas))
	if (areas.count > largeCountryAreas) {
		gSimplifyTol <- gSimplifyTolMax
	} else if (areas.count > medCountryAreas) {
		gSimplifyTol <- ((areas.count - medCountryAreas)/(largeCountryAreas - medCountryAreas))*(gSimplifyTolMax - gSimplifyTolMed) + gSimplifyTolMed
	} else if (areas.count > smallCountryAreas) {
		gSimplifyTol <- ((areas.count - smallCountryAreas)/(medCountryAreas - smallCountryAreas))*(gSimplifyTolMed - gSimplifyTolMin) + gSimplifyTolMin
	} else {
		gSimplifyTol <- gSimplifyTolMin
	}
	print(paste(country, level, length(unlist(areas)), gSimplifyTol))
	
	if (!remove.comments) { # only do this the first time through this routine for a given country
		count.all <<- count.all + 1
		level.string <- ""
		if (level >= 2) {
			level.string <- paste(" (Level ", level, ")", sep="")
		}
		code.strings.all[count.all + 1] <<- paste("\"", name, level.string, "\" = \'data/", basename(filePath), "\',", sep="");
	}
	
	# Remove comments - added by David Arndt
	if (remove.comments) {
		slot(x, "polygons") <- lapply(slot(x, "polygons"), "comment<-", NULL)
		#print(slot(x, "polygons"))
	}
	
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
	code.strings[count + 1] <<- paste("\"", name, level.string, "\" = \'data/", basename(filePath), "\',", sep="");
}

# Load file with list of maps we want.
maps = read.csv(file=input.file, stringsAsFactors = FALSE, header = FALSE);
count <<- 0
count.all <<- 0
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
	
	# Try first to process the map for the given country and level without removing
	# comments. The comments are, for some countries, used for keeping track of something
	# important, but leaving them in causes shape simplification for some countries to fail.
	# Removing comments from all countries causes others to fail. So first we try to process
	# the map while retaining comments, then if that fails we try processing the map without
	# comments, and then if even that fails we give up and write the country info to a file.
	# At GADM level 1, this results in only 31 tiny countries being not successfully
	# processed, probably because they do not have level 1 but only have level 0 data (i.e.
	# only the overall borders of the country but no internal administrative divisions).
	result = tryCatch({
			processMap(country, level, FALSE)
		}, error = function(err) {
			
			tryCatch({
				processMap(country, level, TRUE)
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
			
		}, finally = {
			
		});
	
	
	#print ("result = ", result);
}

write(code.strings, "code_strings.txt")
write(code.strings.all, "code_strings_all.txt")
#write(cases.warn, "warnings.txt")
write(cases.err, "errors.txt")
print (paste("Number of ok maps:", toString(count)));

