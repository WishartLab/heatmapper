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

FILE_UPLOAD_PANEL <- function(){
  tags$span(  
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
	tags$span(
		downloadButton('plotDownload', DOWNLOAD_PLOT, class = "btn-info"),
		downloadButton('tableDownload', DOWNLOAD_TABLE, class = "btn-info"),
		tags$br(), tags$br()
	)
}
