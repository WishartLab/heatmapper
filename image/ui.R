library(spin)
library(shinyBS)
library(jscolourR)

shinyUI(fluidPage(
	includeHTML("www/navbar.html"),

	tags$head(
		HTML("<link rel=\"stylesheet\" href=\"//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css\">"),
		tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}
			input[type=file]{display:inline;}
			#lowColour2, #highColour2 {width:100px;}")),
	
	div(class = "busy", absolutePanel(width = "50px", height = "100px",
		fixed = TRUE, left = "50%", top = "40%", 
		h5("Loading"), tags$br(), spin())),
	
	sidebarLayout(
	  sidebarPanel(

	  	## design changes as of July 17 ##
	  	
	  	radioButtons('imageSelect', label = "Select Image File", 
	  		inline=TRUE, 
	  		choices = c(
	  		"Upload Image" = 'imageUpload', 
	  		"Example Image" = 'imageExample'), 
	  		selected = 'imageExample'
	  	),
	  	conditionalPanel(condition = "input.imageSelect == 'imageUpload'",
	  		fileInput('imageFile2', label = NULL)),
	  		
	  	radioButtons('gridSelect', label = "Select Grid File", 
	  		inline=TRUE, 
	  		choices = c(
	  		"Upload Grid" = 'gridUpload', 
	  		"Example Grid" = 'gridExample'), 
	  		selected = 'gridExample'
	  	),
	  	conditionalPanel(condition = "input.gridSelect == 'gridUpload'",
	  		fileInput('gridFile', label = NULL)),
	  	
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
							<td><input type=number id=selectedX min=1 style='width:100%;'/></td>
							<td><input type=number id=selectedY min=1 style='width:100%;'/></td>
							<td><button id=submitCoords type=button class='action-button'>Submit</button></td>
						</tr>
						<tr>
							<th colspan='2'>value</th>
							<th>Update</th>
						</tr>
						<tr>			
							<td colspan='2'><input type=number id=selectedValue min=0 style='width:100%;'/></td>
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
	  		column(3, tags$label("Gaussian Radius")), 
	  		column(9, sliderInput('gaussianRadius', label = NULL, min = 2, max = 40, value = 10))
	  	), 
	  	
	  	fluidRow(
	  		column(4, tags$label("Contour Smoothness")), 
	  		column(8, sliderInput('contourSmoothness', label = NULL, min = 1, max = 100, value = 50))
	  	)
	  	),
	  	
	  	fluidRow(
	  		column(3, tags$br(), tags$label("Opacity")), 
	  		column(9, sliderInput('opacity', label = NULL, min = 0, max = 1, value = 0.25))
	  	), 
	  	fluidRow(
	  		column(3, tags$label("Colour Intensity")), 
	  		column(9, sliderInput('colourIntensity', label = NULL, min = 1, max = 100, value = 50))
	  	),
	  	 
	  	selectInput('colourScheme', label = "Colour Scheme", 
	  		choices = c(
	  			'rainbow' = "rainbow", 
	  			'custom' = "custom"
	  		), 
	  		selected = 'custom'
	  	),
	  	
	  	conditionalPanel(condition = "input.colourScheme == 'custom'", 
	  		fluidRow(
	  			column(6, jscolourInput('lowColour2', label = "Low Colour")), 
	  			column(6, jscolourInput('highColour2', label = "High Colour"))
	  		)
	  	), 
	  	
	  	# show/hide checkboxes
	  	checkboxGroupInput('layers', label = "Show/Hide Layers",
	  		choices = c(
	  			"show image" = 'showImage2', 
	  			"show grid" = 'showGrid2', 
	  			"show heatmap" = 'showHeatmap2', 
	  			"show contour lines" = 'showContours2'
 	  		),
	  		selected = c('showImage2', 'showGrid2', 'showHeatmap2')
	  	),
	  	downloadButton('downloadPlot2', label = "Download Plot"), 
	  	downloadButton('downloadTable2', label = "Download Table"),
	  	
	  	
	  	
	  	tags$br(), 
	  	tags$br(),
	  	tags$br(),
	  	tags$br(),
	  	
	  	
	  	#### old design ####
	  	
	  	
	  	actionButton('fileInputOptionsButton', label = "Hide File Options", class = "toggleButton fa fa-angle-up"),
			
	  	wellPanel(id = "fileInputPanel",
				radioButtons('chooseInput',
    			label = "Choose Input Type",
    			choices = c(
    				"Upload Image" = 'fileUpload',
    				"Example Image" = 'example'),
    			selected = 'fileUpload'),
				
				conditionalPanel(condition = "input.chooseInput == \'fileUpload\'",
	  			fileInput('imageFile', label = "Upload image here")
				),
				span(id = "fullStretchImage", 
					checkboxInput('stretchImage', label = strong("Stretch image to fit grid"), value = TRUE)), 
	  		bsTooltip(id = "fullStretchImage", 
					title = "Warning: changing this feature may cause misalignment of the heatmap layer",
					placement = "top"),
				
				sliderInput('plotWidth', label = "Plot width (in px)", min = 400, max = 2000, value = 600),
				sliderInput('plotHeight', label = "Plot height (in px)", min = 400, max = 2000, value = 500)
			),
	  	
	  	actionButton('editOptionsButton', label = "Hide Editing Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "editPanel"
				
				
			),
	  	actionButton('plotOptionsButton', label = "Hide Plot Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "plotPanel", 
				sliderInput('numGridRows', label = "Number of rows", min = 3, max = 200, step = 1, value = 50),
		  	bsTooltip(id = "numGridRows", 
					title = "Warning: any changes to values will be lost after changing the number of rows",
					placement = "top"),
				
				sliderInput('nKde2d', label = "Contour detail", 
					min = 10, max = 400, value = 200, step = 10),
				bsTooltip(id = "nKde2d", 
					title = "This feature sets the number of grid points in each direction for kernel density estimation",
					placement = "top"),
				
		  	checkboxInput('showContour', label = strong("Show contour lines"), value = TRUE),
		  	checkboxInput('showFill', label = strong("Show contour fill"), value = TRUE),
				
				checkboxInput('showImage', label = strong("Show background image"), value = TRUE),
				
		  	checkboxInput('showPoints', label = strong("Show points"), value = TRUE),
				checkboxInput('showSelectedPoint', label = strong("Highlight selected point"), value = FALSE),
		  	
		  	selectInput('pointType', label = "Select point type", 
		  		choices = c(
		  			"circle - hollow" = 1, 
		  			"circle - filled" = 16, 
		  			"square - hollow" = 0, 
		  			"square - filled" = 15
		  			), 
		  		selected = 0)
				),
	  	
	  	actionButton('colourOptionsButton', label = "Hide Colour Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "colourPanel", 
				
				sliderInput('contourBins', label = "Number of bins for contours", min = 2, max = 50, value = 10), 
				
				sliderInput('fillOpacity', label = "Fill opacity", min = 0, max = 1, value = 0.5, step = 0.05),
				selectInput('colour', label = "Colour Scheme", selectize = FALSE,
						choices = c(
							"Rainbow" = 'rainbow', 
							"Topo" = 'topo', 
							"Custom" = 'custom'), 
						selected = 'custom'), 
				conditionalPanel(condition = "input.colour == 'custom'",
					tags$div(id = 'colourSection', 
						jscolourInput("lowColour", label = "Colour for low numbers", value = "#FFFF00"),
						jscolourInput("highColour", label = "Colour for high numbers", value = "#EE00FF")))
			),
	  	
	  	actionButton('downloadOptionsButton', label = "Hide Download Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "downloadPanel", 
				selectInput('downloadPlotFormat', label = "Plot download file type", 
					choices = c(
						"JPEG" = 'jpg', 
						"PNG" = 'png', 
						"PDF" = 'pdf'
					), 
					selected = 'jpg'),
				downloadButton('plotDownload', label = "Download plot"),
				tags$br(), 
				tags$br(),
				
				downloadButton('tableDownload', label = "Download table")
			)
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
