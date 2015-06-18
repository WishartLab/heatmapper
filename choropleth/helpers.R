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

jscolourInput <- function (inputId, label, value = "#000000"){
	tagList(
		singleton(tags$head(tags$script(src="js/jscolor/jscolor.js"))),
		tags$strong(label),
		tags$br(),
		tags$input(
			id = inputId,
			value = value, 
			class = "color {hash:true}", 
			onchange = paste0("$('#", inputId, "').trigger('newVal')")), 
		tags$br(), 
		tags$br())
}