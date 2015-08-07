FILE_UPLOAD <- "Select Data File"
SELECT_EXAMPLE <- "Select Example File"
LAYERS <- "Show/Hide Layers"
BIN_NUMBER <- "Number of Shades"
FILL_OPACITY <- "Heatmap Opacity"
BANDWIDTH <- "Gaussian Radius Multiplier"
GRID_POINTS <- "Contour Smoothness"
DOWNLOAD_PLOT <- "Download Plot"
DOWNLOAD_TABLE <- "Download Table"
WIDTH <- "Plot Width (in px)"
HEIGHT <- "Plot Height (in px)"
CONTOUR_WIDTH <- "Contour Line Width (in px)"

HEAD_TASKS <- function(activeTab){
	list(
		includeHTML("../www/navbar.html"),
		tags$script(paste0("$('", activeTab, "').addClass('active');")), 
		
		tags$head(
			tags$style("
				.toggleButton {width:100%;} 
				.fa-angle-down:before, .fa-angle-up:before {float:right;}
				#lowColour, #highColour, #missingColour {width:100%}
				#file_progress {height:0;}
				#sidebarPanel {width:23.45em;}
				#mainPanel {left:24.45em; position:absolute;}
				#tableDownload {float:right;}")
		)
	)
}

FILE_UPLOAD_PANEL <- function(){
  list(  
		radioButtons('chooseInput', label = FILE_UPLOAD, 
    	inline=TRUE, 
	  	choices = c(
	  		"Upload File" = 'fileUpload',
				"Example File" = 'example'), 
    	selected = 'fileUpload'),
    	conditionalPanel(condition = "input.chooseInput == 'fileUpload'",
        fluidRow(
            column(8, fileInput('file', label = NULL)), 
            column(4, HTML("<button id='clearFile' class='action-button' style='display:inline;float:right;'>Clear File</button>"))  
        )
    )
	)
}

BIN_SLIDER <- function(min, max, value){
	fluidRow(
		column(3, tags$label(BIN_NUMBER)),
		column(9, 
			sliderInput("binNumber", 
				label = NULL, 
				min = min, 
				max = max, 
				value = value)
	    )
    )
}

COLOUR_SCHEME_SELECT <- function(){
	fluidRow(
    		column(3, tags$label("Colour Scheme")),
				column(9,					
			selectInput('colourScheme', label = NULL, 
	  		choices = c(
	  			'Custom' = "custom",
	  			'Rainbow' = "rainbow", 
	  			'Topo' = "topo"
	  		), 
	  		selected = 'custom'
	  	)))
}

FILL_OPACITY_SLIDER <- function(){
		fluidRow(
	  	column(3, tags$label(FILL_OPACITY)), 
	  	column(9, sliderInput('fillOpacity', label = NULL, min = 0, max = 1, value = 0.5, step = 0.05))
	  )
}

BANDWIDTH_SLIDER <- function(min, max, value, step){
	fluidRow(
		column(3, tags$label(BANDWIDTH)), 
		column(9, sliderInput('gaussianRadius', label = NULL, min = min, max = max, value = value, step = step))
	)
}

GRID_POINTS_SLIDER <- function(min, max, value, step){
	fluidRow(
		column(4, tags$label(GRID_POINTS)),
		column(8, sliderInput('contourSmoothness', label = NULL, min = min, max = max, value = value, step = step))
	)
}

DOWNLOAD_BUTTONS <- function(){
	list(
		downloadButton('plotDownload', DOWNLOAD_PLOT, class = "btn-info"),
		downloadButton('tableDownload', DOWNLOAD_TABLE, class = "btn-info"),
		tags$br(), tags$br()
	)
}

JSCOLOUR_ROW <- function(low, high){
	fluidRow(
		column(6, jscolourInput("lowColour", label = "Low Colour", value = low)),
		column(6, jscolourInput("highColour", label = "High Colour", value = high))
	)
}

ADVANCED_OPTIONS_BUTTON <- function(){
	actionButton('advancedOptionsButton', label = "Show Advanced Options", class = "toggleButton fa fa-angle-down")
}
