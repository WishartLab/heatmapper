get_path <- function(inFile){
	if(is.null(inFile)){
		return(NULL)
	}
	else{
    path <- inFile$datapath
	}
}

#### Heatmap Helpers ####
get_heatmap <- function(x, rowv = NA, colv = NA) {
	if(is.null(x)){
		return(NULL)
	}
	else{
		heatmap(x, Rowv = rowv, Colv = colv)
	}
}

get_row_dendrogram <- function(x){
	if(is.null(x)){
		return(NULL)
	}
	else{
		as.dendrogram(hclust(dist(x)))
	}
}

get_col_dendrogram <- function(x){
	if(is.null(x)){
		return(NULL)
	}
	else{
		as.dendrogram(hclust(dist(t(x))))
	}
}

remove_strings <- function(x){
	nums <- sapply(x, is.numeric)
	y <- x[,nums]
	
	# try to find a column with title name
	name = 'NAME'
	tryCatch({
		nameRow <- x[,name]
		rownames(y) <- make.names(nameRow, unique=TRUE)
		},
		finally = {return(data.matrix(y))})
}

# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties
percent_map <- function(area, var, lowColour, highColour, legend.title, min = 0, max = 100) {

  # generate vector of fill colors for map
  shades <- colorRampPalette(c(lowColour, highColour))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
    include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]

	if(area == 'county'){
		get_county_dmap(fills)
	}
	else if(area == 'state'){
		get_state_dmap(fills)
	}
	else if(area == 'province'){
		get_canada_dmap(fills)
	}
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % or less"),
    paste0(min + inc, " %"),
    paste0(min + 2 * inc, " %"),
    paste0(min + 3 * inc, " %"),
    paste0(max, " % or more"))
  
  legend("bottomleft", 
    legend = legend.text, 
    fill = shades[c(1, 25, 50, 75, 100)], 
    title = legend.title)
}

get_state_dmap <- function(fills) {
	map("state", fill = TRUE, col = fills, 
    resolution = 0, lty = 0, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0), bg="black")
}

get_canada_dmap <- function(fills) {
	map("state", fill = TRUE, col = fills, 
    resolution = 0, lty = 0, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
}

get_county_dmap <- function(fills) {
	# plot choropleth map
  map("county", fill = TRUE, col = fills, 
    resolution = 0, lty = 0, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
}

get_body_map <- function(bodypartInput) {
		#function to change the rgb color of the xml paths
		changeColor<-function(bodypart,color){
		        node<-xpathSApply(doc, paste("//path[@id='",bodypart,"']/context/rgb",sep=""))[[1]]
		        rgbCol<-col2rgb(color)
		        xmlAttrs(node)["r"]=rgbCol[1]/255
		        xmlAttrs(node)["g"]=rgbCol[2]/255
		        xmlAttrs(node)["b"]=rgbCol[3]/255
		}
		
		#read the xml image
		doc<-xmlParse("data/Human_body_front_and_side.ps.xml")
		
		#these are the different parts you can change
		bodyparts<-c("head","hand-right","hand-left","foot-left","foot-right","lowerleg-left","lowerleg-right",
		            "upperleg-left","upperleg-right","torso","forearm-right","forearm-left","upperarm-right","upperarm-left")
		shades <- colorRampPalette(c("red", "green"))(100)
	
		#color the bodyparts with random color
		mapply(function(x,y){changeColor(x,y)},bodyparts,shades)
		
		
		#load the XML as a picture
		body<-readPicture(saveXML(doc))
		
		#plot it
		grid.arrange(pictureGrob(body), ncol=1)
}