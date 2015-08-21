source("../global_ui.R")

shinyUI(list(HEAD_TASKS("#imageTab", "65%", "50%"), fluidPage(title = "Image Overlay", 
	
	tags$head(
		tags$style("
			#xyCoordsError {display:inline;}
			#selectedX, #selectedY, #selectedValue, #pointsTable {width:100%;}
			#imageFile_progress, #gridFile_progress {height:0;}")
	),
	
	sidebarLayout(position = "right",
	  sidebarPanel(id = "sidebarPanel", width = 1,
	  	
	  	radioButtons('imageSelect', label = "Select Image File", 
	  		inline=TRUE, 
	  		choices = c(
	  		"Upload Image" = 'imageUpload', 
	  		"Example Image" = 'imageExample'), 
	  		selected = 'imageUpload'
	  	),
	  	conditionalPanel(condition = "input.imageSelect == 'imageUpload'",
	  		HTML("<button id='clearImage' class='action-button clearButton'>Clear File</button>"),
	  		fileInput('imageFile', label = NULL)
	  	),
	  	
	  	radioButtons('gridSelect', label = "Select Grid File", 
	  		inline=TRUE, 
	  		choices = c(
	  		"Upload Grid" = 'gridUpload', 
	  		"Example Grid" = 'gridExample'), 
	  		selected = 'gridUpload'
	  	),
	  	
	  	conditionalPanel(condition = "input.gridSelect == 'gridUpload'",
	  		HTML("<button id='clearGrid' class='action-button clearButton'>Clear File</button>"), 
	  		fileInput('gridFile', label = NULL)
	  	),
	  	
	  	LAYERS_SELECT(
	  		c(
	  			"Image" = 'showImage', 
	  			"Grid Lines" = 'showGrid', 
	  			"Heatmap" = 'showHeatmap', 
	  			"Contour Lines" = 'showContour', 
	  			"Axis Labels" = 'showAxisLabels'
	  		), 
	  		c('showImage', 'showHeatmap', 'showContour')
	  	),

	  	fluidRow(
	  		column(3, tags$label("Display")), 
	  		column(9, 
	  			radioButtons('displayType', label =NULL, 
	  				choices = c(
	  					"Square" = 'square', 
	  					"Gaussian" = 'gaussian'
	  				), 
	  				selected = 'gaussian', 
	  				inline = TRUE))
	  	), 
	  	
	  	conditionalPanel(condition = "input.displayType == 'gaussian'",
		  	BANDWIDTH_SLIDER(0.05, 2, 0.4, 0.05),
		  	GRID_POINTS_SLIDER(10, 400, 200, 10)
	  	),
	  	
	  	FILL_OPACITY_SLIDER(),
	  	BIN_SLIDER(2, 50, 8),
	  	
	  	COLOUR_SCHEME_SELECT('rainbow'),
	  	
	  	conditionalPanel(condition = "input.colourScheme == 'custom'", 
	  		JSCOLOUR_ROW("#FFFF00", "#EE00FF")
	  	), 
	  	
	  	DOWNLOAD_BUTTONS(),
	  	
	  	ADVANCED_OPTIONS_PANEL(
	  		list(
		  		span(id = "fullStretchImage", 
						checkboxInput('stretchImage', label = strong("Stretch image to fit grid"), value = TRUE)), 
		  		bsTooltip(id = "fullStretchImage", 
						title = "Warning: changing this feature may cause misalignment of the heatmap layer",
						placement = "top"), 
		  	
		  		sliderInput('numGridRows', label = "Number of rows", min = 3, max = 200, step = 1, value = 50),
			  	bsTooltip(id = "numGridRows", 
						title = "Warning: any changes to values will be lost after changing the number of rows",
						placement = "top"),
		  			
		  		sliderInput('plotWidth', label = WIDTH, min = 400, max = 2000, value = 600),
					sliderInput('plotHeight', label = HEIGHT, min = 400, max = 2000, value = 520),
		  		
		  		selectInput('downloadPlotFormat', label = "Plot download file type", 
						choices = c(
							"JPEG" = 'jpg',
							"PDF" = 'pdf',
							"PNG" = 'png'
						), 
						selected = 'png')	
	  		)
	  	)
		),
		
		mainPanel(id = "mainPanel",
			tabsetPanel(
				tabPanel("Plot",
					wellPanel(
				  	tags$label("Selected point"), textOutput('xyCoordsError'),
						HTML("
							<table id = 'pointsTable' class = 'data table table-bordered table-condensed'>
								<tbody>
									<tr>
										<th>X</th>
										<th>Y</th>
										<th>Show</th>
			
										<th colspan='2'>Value</th>
										<th>Update</th>
									</tr>
									<tr>
										<td><input type=number id=selectedX min=1 /></td>
										<td><input type=number id=selectedY min=1 /></td>
										<td style='text-align:center;'><input type=checkbox id=showSelectedPoint /></td>
			
										<td colspan='2'><input type=number id=selectedValue min=0 /></td>
										<td><button id=submitValue type=button class='action-button'>Submit</button></td>
									</tr>
								</tbody>
							</table>
						")
				  	
					),
					plotOutput("ggplotMap", click = "plot_click")
				),
				tabPanel("Table", 
					tags$br(),
					dataTableOutput("table")
				)
			)
		)
	),
	INCLUDE_JS()
)))
