library(spin)
library(shinyBS)
library(jscolourR)

shinyUI(fluidPage(
	includeHTML("www/navbar.html"),

	tags$head(
		HTML("<link rel=\"stylesheet\" href=\"//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css\">"),
		tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}
			input[type=file]{display:inline;}
			#selectedX, #selectedY, #selectedValue {width:100%;}
			#lowColour, #highColour {width:100%;}
			#imageFile_progress, #gridFile_progress {height:0;}")),
	
	div(class = "busy", absolutePanel(width = "50px", height = "100px",
		fixed = TRUE, left = "50%", top = "40%", 
		h5("Loading"), tags$br(), spin())),
	
	sidebarLayout(
	  sidebarPanel(
	  	
	  	radioButtons('imageSelect', label = "Select Image File", 
	  		inline=TRUE, 
	  		choices = c(
	  		"Upload Image" = 'imageUpload', 
	  		"Example Image" = 'imageExample'), 
	  		selected = 'imageUpload'
	  	),
	  	conditionalPanel(condition = "input.imageSelect == 'imageUpload'",
	  		fileInput('imageFile', label = NULL)
	  	),
	  	
	  	radioButtons('gridSelect', label = "Select Grid File", 
	  		inline=TRUE, 
	  		choices = c(
	  		"Upload Grid" = 'gridUpload', 
	  		"Example Grid" = 'gridExample'), 
	  		selected = 'gridExample'
	  	),
	  	
	  	conditionalPanel(condition = "input.gridSelect == 'gridUpload'",
	  		fluidRow(
	  			column(8, fileInput('gridFile', label = NULL)), 
	  			column(4, HTML("<button id='clearGrid' class='action-button' style='display:inline;float:right;'>Clear File</button>"))
	  		)
	  	),
	  	
	  	# or select grid ____ x ____,  grid radius ___
	  	tags$label("Selected point"),
	  	textOutput('xyCoordsError'),
			HTML("
				<table class = 'data table table-bordered table-condensed'>
					<tbody>
						<tr>
							<th>x</th>
							<th>y</th>
							<th>Select</th>
						</tr>
						<tr>
							<td><input type=number id=selectedX min=1 /></td>
							<td><input type=number id=selectedY min=1 /></td>
							<td><button id=submitCoords type=button class='action-button'>Submit</button></td>
						</tr>
						<tr>
							<th colspan='2'>value</th>
							<th>Update</th>
						</tr>
						<tr>			
							<td colspan='2'><input type=number id=selectedValue min=0 /></td>
							<td><button id=submitValue type=button class='action-button'>Submit</button></td>
							
						</tr>
					</tbody>
				</table>
			"),

	  	fluidRow(
	  		column(3, tags$label("Display")), 
	  		column(9, 
	  			radioButtons('displayType', label =NULL, 
	  				choices = c(
	  					"square" = 'square', 
	  					"gaussian" = 'gaussian'
	  				), 
	  				selected = 'gaussian', 
	  				inline = TRUE))
	  	), 
	  	
	  	conditionalPanel(condition = "input.displayType == 'gaussian'",
		  	# contours
		  	fluidRow(
		  		column(3, tags$label("Gaussian Radius Multiplier")), 
		  		column(9, sliderInput('gaussianRadius', label = NULL, min = 0.05, max = 2, value = 0.4, step=0.05))
		  	), 
		  	
		  	fluidRow(
		  		column(4, tags$label("Contour Smoothness")), 
		  		column(8, sliderInput('contourSmoothness', label = NULL, min = 10, max = 400, value = 200, step = 10),
		  		bsTooltip(id = "contourSmoothness", 
						title = "This feature sets the number of grid points in each direction for kernel density estimation",
						placement = "top"))
		  	)
	  	),
	  	
	  	fluidRow(
	  		column(3, tags$label("Heatmap Opacity")), 
	  		column(9, sliderInput('fillOpacity', label = NULL, min = 0, max = 1, value = 0.5, step = 0.05))
	  	), 
	  	fluidRow(
	  		column(3, tags$label("Number of Shades")), 
	  		column(9, sliderInput('numShades', label = NULL, min = 2, max = 50, value = 8))
	  	),
	  	 
	  	selectInput('colourScheme', label = "Colour Scheme", 
	  		choices = c(
	  			'custom' = "custom",
	  			'rainbow' = "rainbow", 
	  			'topo' = "topo"
	  		), 
	  		selected = 'rainbow'
	  	),
	  	
	  	conditionalPanel(condition = "input.colourScheme == 'custom'", 
	  		fluidRow(
	  			column(6, jscolourInput('lowColour', label = "Low Colour", "#FFFF00")), 
	  			column(6, jscolourInput('highColour', label = "High Colour", value = "#EE00FF"))
	  		)
	  	), 
	  	
	  	selectInput('layers', label = "Show/Hide Layers", multiple = TRUE,# width = "50%", 
	  		choices = c(
	  			"image" = 'showImage', 
	  			"grid lines" = 'showGrid', 
	  			"heatmap" = 'showHeatmap', 
	  			"contour lines" = 'showContour'
	  		), 
	  		selected = c('showImage', 'showHeatmap')
	  	),
	  	
	  	# code for checkboxes instead of select
	  	#checkboxGroupInput('layers', label = "Show/Hide Layers",
	  	#	choices = c(
	  	#		"show image" = 'showImage', 
	  	#		"show grid" = 'showGrid', 
	  	#		"show heatmap" = 'showHeatmap', 
	  	#		"show contour lines" = 'showContour'
 	  	#	),
	  	#	selected = c('showImage', 'showGrid', 'showHeatmap')
	  	#),
	  	
	  	downloadButton('plotDownload', label = "Download Plot"), 
	  	downloadButton('tableDownload', label = "Download Table"),
	  	
	  	tags$br(), 
	  	tags$br(),
	  	
	  	actionButton('advancedOptionsButton', label = "Show Advanced Options", class = "toggleButton fa fa-angle-down"),
	  	conditionalPanel(condition = "input.advancedOptionsButton%2",
	  	wellPanel(id = "advancedPanel", 
	  		
	  		span(id = "fullStretchImage", 
					checkboxInput('stretchImage', label = strong("Stretch image to fit grid"), value = TRUE)), 
	  		bsTooltip(id = "fullStretchImage", 
					title = "Warning: changing this feature may cause misalignment of the heatmap layer",
					placement = "top"), 
	  	
	  		sliderInput('numGridRows', label = "Number of rows", min = 3, max = 200, step = 1, value = 50),
		  	bsTooltip(id = "numGridRows", 
					title = "Warning: any changes to values will be lost after changing the number of rows",
					placement = "top"),
	  		
	  		checkboxInput('showSelectedPoint', label = strong("Highlight selected point"), value = FALSE),
	  		
	  		selectInput('downloadPlotFormat', label = "Plot download file type", 
					choices = c(
						"JPEG" = 'jpg',
						"PDF" = 'pdf',
						"PNG" = 'png'
					), 
					selected = 'png')	,
	  			
	  		sliderInput('plotWidth', label = "Plot width (in px)", min = 400, max = 2000, value = 600),
				sliderInput('plotHeight', label = "Plot height (in px)", min = 400, max = 2000, value = 500) 
	  	
	  	))
		),
		mainPanel(
			tabsetPanel(
				tabPanel("Plot",
						plotOutput("ggplotMap", click = "plot_click")
				),
				tabPanel("Table", 
					tags$br(),
					dataTableOutput("table")
				)
			)
		)
	),
	singleton(includeScript("www/js/active.js"))
))
