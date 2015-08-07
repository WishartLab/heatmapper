# FILE_UPLOAD_PANEL()
FILE_UPLOAD <- "Select Data File" 

# EXAMPLE_FILE_SELECT()
SELECT_EXAMPLE <- "Select Example File"

# LAYERS_SELECT()
LAYERS <- "Show/Hide Layers"

# BIN_SLIDER()
BIN_NUMBER <- "Number of Shades"

# FILL_OPACITY_SLIDER()
FILL_OPACITY <- "Heatmap Opacity"

# BANDWIDTH_SLIDER()
BANDWIDTH <- "Gaussian Radius Multiplier"

# GRID_POINTS_SLIDER()
GRID_POINTS <- "Contour Smoothness"

# DOWNLOAD_BUTTONS()
DOWNLOAD_PLOT <- "Download Plot"
DOWNLOAD_TABLE <- "Download Table"

WIDTH <- "Plot Width (in px)"
HEIGHT <- "Plot Height (in px)"
CONTOUR_WIDTH <- "Contour Line Width (in px)"
INCLUDE_JS <- singleton(includeScript("../www/js/active.js"))

# imports navbar, sets active tab, adds CSS
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

# file upload vs example selection, file upload button when fileUpload is selected
FILE_UPLOAD_PANEL <- function(selected){
  list(  
		radioButtons('chooseInput', label = FILE_UPLOAD, 
    	inline=TRUE, 
	  	choices = c(
	  		"Upload File" = 'fileUpload',
				"Example File" = 'example'), 
    	selected = 'example'),
    	conditionalPanel(condition = "input.chooseInput == 'fileUpload'",
        fluidRow(
            column(8, fileInput('file', label = NULL)), 
            column(4, HTML("<button id='clearFile' class='action-button' style='display:inline;float:right;'>Clear File</button>"))  
        )
    )
	)
}

# example file dropdown selection
EXAMPLE_FILE_SELECT <- function(choices){
	
}

# multiple selection for hide/show layers
LAYERS_SELECT <- function(choices, selected){
	selectInput('layers', label = LAYERS, 
		multiple = TRUE,
		choices = choices, 
		selected = selected)
}

# number of shades slider
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

# colour scheme dropdown selection
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

# heatmap opacity slider
FILL_OPACITY_SLIDER <- function(){
		fluidRow(
	  	column(3, tags$label(FILL_OPACITY)), 
	  	column(9, sliderInput('fillOpacity', label = NULL, min = 0, max = 1, value = 0.5, step = 0.05))
	  )
}

# gaussian radius multiplier slider
BANDWIDTH_SLIDER <- function(min, max, value, step){
	fluidRow(
		column(3, tags$label(BANDWIDTH)), 
		column(9, sliderInput('gaussianRadius', label = NULL, min = min, max = max, value = value, step = step))
	)
}

# contour smoothness slider
GRID_POINTS_SLIDER <- function(min, max, value, step){
	fluidRow(
		column(4, tags$label(GRID_POINTS)),
		column(8, sliderInput('contourSmoothness', label = NULL, min = min, max = max, value = value, step = step))
	)
}

# plotdownload and table download buttons
DOWNLOAD_BUTTONS <- function(){
	list(
		downloadButton('plotDownload', DOWNLOAD_PLOT, class = "btn-info"),
		downloadButton('tableDownload', DOWNLOAD_TABLE, class = "btn-info"),
		tags$br(), tags$br()
	)
}

# low and high colour selections
JSCOLOUR_ROW <- function(low, high){
	fluidRow(
		column(6, jscolourInput("lowColour", label = "Low Colour", value = low)),
		column(6, jscolourInput("highColour", label = "High Colour", value = high))
	)
}

# advanced options panel
ADVANCED_OPTIONS_PANEL <- function(options_list){
	list(
		actionButton('advancedOptionsButton', label = "Show Advanced Options", class = "toggleButton fa fa-angle-down"), 
		conditionalPanel(condition = "input.advancedOptionsButton%2",
			wellPanel(options_list)
		)
	)
}

