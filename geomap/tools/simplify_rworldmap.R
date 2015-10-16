library(rworldmap)
#library(leaflet)

continents <- c("Africa", "Antarctica", "Asia", "Australia", "Europe", "North America", "South America")
spdf <- getMap()
for(selected_continent in continents){
	x <- spdf[spdf$REGION %in% selected_continent,]
	fileName <- paste0(sub(" ", "_", x = selected_continent, fixed = TRUE), ".rds")
	filePath <- paste0("heatmapper/geomap/data/", fileName)
	#saveRDS(x, filePath)
}

saveRDS(spdf, "heatmapper/geomap/data/World_Countries.rds")
#leaflet(spdf) %>% addTiles() %>% addPolygons()




