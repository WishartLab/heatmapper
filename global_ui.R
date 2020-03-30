library(jscolourR)
library(spin)
library(shinyBS)

# error messages
ERR_file_upload <- "Please upload a file."
ERR_file_multi_upload <- "Please upload a set of files."
ERR_file_read <- "File could not be read. Please ensure that the file you uploaded is valid."
ERR_plot_display <- "Heatmap could not be displayed. Please ensure that the file you uploaded is valid."

# FILE_UPLOAD_PANEL()
FILE_UPLOAD <- "Select Data File" 

# EXAMPLE_FILE_SELECT()
SELECT_EXAMPLE <- "Select Example File"
EXAMPLE_FILES <- c(
					"Example 1" = 'example_input/example1.txt',
					"Example 2" = 'example_input/example2.txt',
					"Example 3" = 'example_input/example3.txt')

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

RESOLUTION <- "Downloaded plot resolution (pixels per inch)"
WIDTH <- "Plot Width (pixels)"
HEIGHT <- "Plot Height (pixels)"
	# Note: On Retina/HiDPI displays, "pixels" here refers to virtual pixels, though a
	# single virtual pixel may be rendered by more than one physical pixel. Shiny/R should
	# automatically scale up the number of actual pixels used to render the image.
CONTOUR_WIDTH <- "Contour Line Width (pixels)"
BRIGHTNESS <- "Colour Brightness"

# imports navbar, sets active tab, adds CSS
HEAD_TASKS <- function(activeTab, left = "65%", top = "45%"){
	list(
		includeHTML("../www/navbar.html"),
		includeHTML("../www/notice.html"),
		tags$script(paste0("$('", activeTab, "').addClass('active');")), 
		tags$head(includeScript("../www/js/google-analytics.js")),
		
		tags$head(
			# HTML("<link rel=\"stylesheet\" href=\"//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css\">"),
			tags$style("
                input[type='file'] {width:15em;}
				.toggleButton {width:100%;} 
                .clearButton {float:right; font-size:12px;}
				.fa-angle-down:before, .fa-angle-up:before {float:right;}
				#lowColour, #highColour, #midColour, #missingColour {width:100%}
				#file_progress, #colClusterFile_progress, #rowClusterFile_progress  {height:0;}
				#sidebarPanel {width:23.45em;}
				#mainPanel {left:24.45em; position:absolute; min-width:25em;}
				#exampleButton {float:right;}")
		),
		div(class = "busy", 
			absolutePanel(width = "50px", height = "100px",
				fixed = TRUE, left = left, top = top, 
				h5("Loading"), tags$br(), spin()
			)
		)
	)
}

# file upload vs example selection, file upload button when fileUpload is selected
FILE_UPLOAD_PANEL <- function(selected = 'fileUpload'){
    list(  
		tipify(radioButtons('chooseInput', label = FILE_UPLOAD, 
    	    inline=TRUE, 
	  	    choices = c(
	  		    "Upload File" = 'fileUpload',
				"Example File" = 'example'), 
            selected = selected), "Upload file or select example file", placement = "right"),
        conditionalPanel(condition = "input.chooseInput == 'fileUpload'",
            HTML("<button id='clearFile' class='action-button clearButton'>Clear File</button>"), 
            bsTooltip("clearFile", "Clear uploaded file", "right"),
            fileInput('file', label = NULL)
        )
	)
}

# example file dropdown selection
EXAMPLE_FILE_SELECT <- function(){
	list(
		conditionalPanel(condition = "input.chooseInput == 'example'",
			tags$label(SELECT_EXAMPLE), 
            tags$br(),
            
            tipify(actionButton('exampleButton', label = NULL, class = "btn-info", icon = icon("fa fa-info-circle")), 
            title = "View Example File Details", placement = "right"), 
             
            selectInput('exampleFiles', width="83%",
				label = NULL,
				choices = EXAMPLE_FILES,
				selected = 1), 
            
	  		conditionalPanel(condition = "input.exampleButton>0",
	  			wellPanel(id = "exampleInfo",
	  				tags$label("Example File Information"),
	  				HTML("<button id='closeExampleButton' class='action-button' style='float:right;'><i class='fa fa-times'></i></button>"),
						conditionalPanel(condition = "input.exampleFiles == 'example_input/example1.txt'", includeHTML("www/example1info.html")),
						conditionalPanel(condition = "input.exampleFiles == 'example_input/example2.txt'", includeHTML("www/example2info.html")),
						conditionalPanel(condition = "input.exampleFiles == 'example_input/example3.txt'", includeHTML("www/example3info.html"))
				))
		)
	)
}

# multiple selection for hide/show layers
LAYERS_SELECT <- function(choices, selected){
	tipify(selectInput('layers', label = LAYERS, 
		multiple = TRUE,
		choices = choices, 
		selected = selected), "Add or remove heatmap layers", placement = "right")
}

# number of shades slider
BIN_SLIDER <- function(min, max, value){
	tipify(fluidRow(
		column(3, tags$label(BIN_NUMBER)),
		column(9, 
			sliderInput("binNumber", 
				label = NULL, 
				min = min, 
				max = max, 
				value = value)
	    )
    ), "Adjust the number of colours", placement = "right")
}

BRIGHTNESS_SLIDER <- function(){
	tipify(fluidRow(
		column(3, tags$label(BRIGHTNESS)),
		column(9, 
            sliderInput('plotBrightness', label = NULL, 
                min = -50,
                max = 50, 
                value = 0)
	    )
    ), "Adjust the brightness of colours", placement = "right")
}

# colour scheme dropdown selection
COLOUR_SCHEME_SELECT <- function(selected = 'custom'){
	tipify(fluidRow(
    column(3, tags$label("Colour Scheme")),
		column(9,					
			selectInput('colourScheme', label = NULL, 
	  		choices = c(
	  		  'Custom' = "custom",
	  		  'Blue/Yellow' = "blue/yellow",
	  			'Red/Green' = "red/green",
	  			'Pink/White/Green' = "piyg",
	  			'Blue/Green/Yellow' = "topo",
	  			'Grayscale' = "grayscale",
	  			'Rainbow' = "rainbow"
	  		), 
	  		selected = selected)
		)
	), "Select custom or preset colour scheme", placement = "right")
}

COLOUR_SCHEME_SELECT_LIMITED <- function(selected = 'custom'){
  tipify(fluidRow(
    column(3, tags$label("Colour Scheme")),
    column(9,					
           selectInput('colourScheme', label = NULL, 
                       choices = c(
                         'Custom' = "custom",
                         'Blue/Yellow' = "blue/yellow",
                         'Red/Green' = "red/green",
                         'Pink/White/Green' = "piyg",
                         'Grayscale' = "grayscale"
                       ), 
                       selected = selected)
    )
  ), "Select custom or preset colour scheme", placement = "right")
}

# heatmap opacity slider
FILL_OPACITY_SLIDER <- function(value = 0.5){
	tipify(
        fluidRow(
	  	    column(3, tags$label(FILL_OPACITY)), 
	  	    column(9, sliderInput('fillOpacity', label = NULL, min = 0, max = 1, value = value, step = 0.05))
        ),
    "Adjust the heatmap colour opacity", placement = "right")
}

# gaussian radius multiplier slider
BANDWIDTH_SLIDER <- function(min, max, value, step){
	tipify(fluidRow(
		column(3, tags$label(BANDWIDTH)), 
		column(9, sliderInput('gaussianRadius', label = NULL, min = min, max = max, value = value, step = step))),
        "Adjust the bandwidth for kernel density estimation", placement = "right"
	)
}

# contour smoothness slider
GRID_POINTS_SLIDER <- function(min, max, value, step){
	tipify(fluidRow(
		column(4, tags$label(GRID_POINTS)),
		column(8, sliderInput('contourSmoothness', label = NULL, min = min, max = max, value = value, step = step))),
        title = "Adjust the number of grid points in each direction for kernel density estimation", placement = "right"
	)
}

# plotdownload and table download buttons
DOWNLOAD_BUTTONS <- function(){
	list(
        tags$style("#tableDownload {float:right;}"),
		tipify(downloadButton('plotDownload', DOWNLOAD_PLOT, class = "btn-info"), "Download the heatmap plot", placement = "top"),
		tipify(downloadButton('tableDownload', DOWNLOAD_TABLE, class = "btn-info"), "Download the raw data", placement = "top"),
		tags$br(), tags$br()
	)
}

# plotdownload and table download buttons with file format selection options
DOWNLOAD_BUTTONS_WITH_SELECTION <- function(plotChoices = c("JPEG" = 'jpg',"PDF" = 'pdf',"PNG" = 'png',"TIFF" = 'tiff'),
																						plotSelected = "png", tableChoices=c("TXT"='txt', "CSV" = 'csv'), tableSelected = "txt"){
	list(
        fluidRow(
            column(4, tags$label("Download")), 
            column(8, tags$label("File Format"))
        ),
        fluidRow(
            column(4,
		        tipify(downloadButton('plotDownload', "Plot ", class = "btn-info"), "Download the heatmap plot", placement = "top")
            ), 
            column(8, 
                tipify(
                    selectInput('downloadPlotFormat', label = NULL, 
                        choices = plotChoices, 
                        selected = plotSelected), 
                    "Select plot download format", placement = "top")
            )
        ),
        fluidRow(
            column(4,
                tipify(downloadButton('tableDownload', "Table", class = "btn-info"), "Download the raw data", placement = "top")
            ), 
            column(8, 
                tipify(
                    selectInput('downloadTableFormat', label = NULL, 
                        choices = tableChoices, 
                        selected = tableSelected), 
                        "Select table download format", placement = "top")
            )
        )
	)
}

# low and high colour selections
JSCOLOUR_ROW <- function(low, high){
	fluidRow(
		tipify(column(6, jscolourInput("lowColour", label = "Low Colour", value = low)), "Select colour for low values", placement = "top"),
		tipify(column(6, jscolourInput("highColour", label = "High Colour", value = high)), "Select colour for high values", placement = "top")
	)
}

JSCOLOUR_3_ROW <- function(low = "#33FF00", mid = "#000000", high = "#FF0000"){
    fluidRow(
        tipify(column(4, jscolourInput("lowColour", label = "Low Colour", value = low)), "Select colour for low values", placement = "top"),
        tipify(column(4, jscolourInput("midColour", label = "Middle Colour", value = mid)), "Select colour for middle values", placement = "top"),
    	tipify(column(4, jscolourInput("highColour", label = "High Colour", value = high)), "Select colour for low values", placement = "top")
    )
}

# advanced options panel
ADVANCED_OPTIONS_PANEL <- function(options_list){
	list(
		tipify(actionButton('advancedOptionsButton', label = "Show Advanced Options", class = "toggleButton fa fa-angle-down"), 
            "View more options", placement = "right"), 
		conditionalPanel(condition = "input.advancedOptionsButton%2",
		                 wellPanel(options_list))
	)
}

INCLUDE_JS <- function(){
	singleton(includeScript("../www/js/active.js"))
}
