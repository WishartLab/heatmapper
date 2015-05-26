library(shiny)
library(maps)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output){
	
	# source: http://www.r-bloggers.com/make-your-own-electoral-map/
	output$discreteMap <- renderPlot({
		new_theme_empty <- theme_bw()  # Create our own, mostly blank, theme
		new_theme_empty$line <- element_blank()
		new_theme_empty$rect <- element_blank()
		new_theme_empty$strip.text <- element_blank()
		new_theme_empty$axis.text <- element_blank()
		new_theme_empty$axis.title <- element_blank()
		new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")

		stateShapes <- map("state", plot = FALSE, fill = TRUE)
		stateShapes <- fortify(stateShapes)  # Load state shapefiles and convert to a data.frame
		
		uniqueStates <- sort(unique(stateShapes$region))
		stateResults <- runif(length(uniqueStates), 0, 100)
		names(stateResults) <- uniqueStates  # Romney vote / Obama vote:
		stateResults["alabama"] <- 60.7/38.4; stateResults["arizona"] <- 55/43.4; stateResults["arkansas"] <- 60.4/37.0
		stateResults["california"] <- 38.5/59.2; stateResults["colorado"] <- 47/50.7; stateResults["connecticut"] <- 40.6/58.3
		stateResults["delaware"] <- 40/58.6; stateResults["district of columbia"] <- NA; stateResults["florida"] <- 49.3/49.8  # 7.1/91.4
		stateResults["georgia"] <- 53.4/45.4; stateResults["idaho"] <- 65.8/31.5; stateResults["illinois"] <- 41.1/57.3
		stateResults["indiana"] <- 54.3/43.8; stateResults["iowa"] <- 46.5/52.1; stateResults["kansas"] <- 59.9/37.9
		stateResults["kentucky"] <- 60.5/37.8; stateResults["louisiana"] <- 58.5/39.8; stateResults["maine"] <- 40.3/56.3
		stateResults["maryland"] <- 37/61.2; stateResults["massachusetts"] <- 37.5/60.9; stateResults["michigan"] <- 45.5/53.6
		stateResults["minnesota"] <- 45.1/52.8; stateResults["mississippi"] <- 55.4/43.6; stateResults["missouri"] <- 53.9/44.3
		stateResults["montana"] <- 56.6/41.5; stateResults["nebraska"] <- 60.5/37.8; stateResults["nevada"] <- 45.7/52.3
		stateResults["new hampshire"] <- 46.7/52; stateResults["new jersey"] <- 51.0/57.9; stateResults["new mexico"] <- 43/52.9
		stateResults["new york"] <- 35.9/62.7; stateResults["north carolina"] <- 50.6/48.4; stateResults["north dakota"] <- 58.7/38.9
		stateResults["ohio"] <- 48.2/50.1; stateResults["oklahoma"] <- 66.8/33.2; stateResults["oregon"] <- 44.1/53.3
		stateResults["pennsylvania"] <- 46.8/51.9; stateResults["rhode island"] <- 35.2/63.1; stateResults["south carolina"] <- 55/43.7
		stateResults["south dakota"] <- 57.9/39.9; stateResults["tennessee"] <- 59.5/39; stateResults["texas"] <- 57.2/41.4
		stateResults["utah"] <- 72.7/24.9; stateResults["vermont"] <- 31.2/67.1; stateResults["virginia"] <- 47.8/50.8
		stateResults["washington"] <- 42.8/55.2; stateResults["west virginia"] <- 42.8/55.2; stateResults["wisconsin"] <- 62.3/35.5
		stateResults["wyoming"] <- 69.3/28
		
		stateResults <- 100 * stateResults / (stateResults + 1)  # Convert to proportion of the two-party vote
		
		stateShapes$repVote <- by(stateResults, uniqueStates, mean)[stateShapes$region]  # Add results to shapefile frame
		
		myPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))  # Make a custom color palette
		normalize <- function(x){  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))  }
		myPalette <- colorRampPalette(myPalette(1000)[round(1000 * normalize(qbeta(1:999 /1000, 1/3, 1/3)))])
		myPalette <- myPalette(1000)#[0:700]  # Not needed, if you leave out DC, which really skews the palette
		
		mapPlot <- ggplot(stateShapes,
											aes(x = long, y = lat, group = group,
													fill = repVote))
		mapPlot <- mapPlot + geom_polygon(colour = "BLACK")
		mapPlot <- mapPlot + coord_map(project="conic", lat0 = 30)
		mapPlot <- mapPlot + new_theme_empty
		mapPlot <- mapPlot + scale_fill_gradientn("Romney Prop.\nof the 2PV",
																							colours = myPalette)
		mapPlot <- mapPlot + ggtitle("2012 Election Returns by State")
		return(mapPlot)
		})
})