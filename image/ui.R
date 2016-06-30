source("../global_ui.R")

shinyUI(list(HEAD_TASKS("#imageTab", "65%", "50%"), fluidPage(title = "Image Overlay", 
	
	tags$head(
		tags$style("
			#xyCoordsError {display:inline;}
			#selectedX, #selectedY, #selectedValue, #pointsTable {width:100%;}
			#imageFile_progress, #file_progress {height:0;}")
	),
	
	sidebarLayout(position = "right",
	  sidebarPanel(id = "sidebarPanel", width = 1,
	  	
	  	# Radio buttons to select image to upload or choose example image, plus button for example info.
	  	fluidRow(
	  		column(9,
	  		radioButtons('imageSelect', label = "Select Image File",
	  													 inline=TRUE,
	  													 choices = c(
	  													 	"Upload Image" = 'imageUpload',
	  													 	"Example" = 'imageExample'),
	  													 selected = 'imageUpload'),
	  		inline=TRUE
	  		),
	  		column(3,
	  					 conditionalPanel(condition = "input.imageSelect == 'imageExample'",
	  					 								 actionButton('exampleButton', label = NULL, class = "btn-info", icon = icon("fa fa-info-circle")),
	  					 								 bsTooltip(id = "exampleButton", title = "View Example Details", placement = "right")
	  					 )
	  		)
	  	),
	  	
	  	# Example info box
	  	conditionalPanel(condition = "input.exampleButton>0",
	  									 wellPanel(id = "exampleInfo",
	  									 					tags$label("Example File Information"),
	  									 					HTML("<button id='closeExampleButton' class='action-button' style='float:right;'><i class='fa fa-times'></i></button>"),
	  									 					includeHTML("www/example1info.html")
	  									 )),
	  	
	  	# UI for uploading custom image
	  	conditionalPanel(condition = "input.imageSelect == 'imageUpload'",
	  		HTML("<button id='clearImage' class='action-button clearButton'>Clear File</button>"),
	  		fileInput('imageFile', label = NULL)
	  	),
	  	
	  	# Select to upload custom grid or use example grid
	  	radioButtons('gridSelect', label = "Select Grid File", 
	  		inline=TRUE, 
	  		choices = c(
	  		"Upload Grid" = 'fileUpload', 
	  		"Example Grid" = 'fileExample'), 
	  		selected = 'fileUpload'
	  	),
	  	# When select "Example Grid", user can choose either the example grid or an empty grid
	  	conditionalPanel(condition = "input.gridSelect == 'fileExample'", 
  			selectInput('exampleSelect', label = "Select Type of File", 
	  			choices = c(
	  				"Example Grid" = 'example', 
	  				"Empty Grid" = 'empty'), 
	  			selected = 'example'
	  		), 
	  		# When select "Empty Grid", show slider for choosing the number of rows.
	  		conditionalPanel(condition = "input.exampleSelect == 'empty'", 
	  			sliderInput('numGridRows', label = "Number of Rows", min = 3, max = 200, step = 1, value = 50),
			  	bsTooltip(id = "numGridRows", 
						title = "Warning: any changes to values will be lost after changing the number of rows",
						placement = "right")
	  		)
	  	),
	  	
	  	conditionalPanel(condition = "input.gridSelect == 'fileUpload'",
	  		HTML("<button id='clearFile' class='action-button clearButton'>Clear File</button>"), 
	  		fileInput('file', label = NULL)
	  	),
	  	
	  	LAYERS_SELECT(
	  		c(
	  			"Image" = 'showImage', 
	  			"Grid Lines" = 'showGrid', 
	  			"Heatmap" = 'showHeatmap', 
	  			"Contour Lines" = 'showContour', 
	  			"Axis Labels" = 'showAxisLabels'
	  		), 
	  		c('showImage', 'showHeatmap')
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
	  	
	  	FILL_OPACITY_SLIDER("0.2"),
	  	BIN_SLIDER(2, 50, 7),
	  	
	  	COLOUR_SCHEME_SELECT(),
	  	#COLOUR_SCHEME_SELECT_LIMITED(),
	  	
	  	conditionalPanel(condition = "input.colourScheme == 'custom'", 
	  		JSCOLOUR_ROW("#0000FF", "#FF0000")
	  	), 
	  	
	  	DOWNLOAD_BUTTONS_WITH_SELECTION(),
	  	
	  	ADVANCED_OPTIONS_PANEL(
	  		list(
	  			
	  			selectInput('downloadPlotResolution', label = RESOLUTION,
	  									choices = c("72" = '72',"144" = '144',"300" = '300'),
	  									selected = '144'),
	  			
		  		span(id = "fullStretchImage", 
						checkboxInput('stretchImage', label = strong("Stretch image to fit grid"), value = FALSE)), 
	  			bsTooltip(id = "fullStretchImage", 
						title = "Warning: changing this feature may cause misalignment of the heatmap layer",
						placement = "top"),
		  		
		  		sliderInput('plotWidth', label = WIDTH, min = 400, max = 2000, value = 600),
					sliderInput('plotHeight', label = HEIGHT, min = 400, max = 2000, value = 520)
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
	INCLUDE_JS(), 
	tags$script("
    var fileControl = $('#imageFile');

    $('#clearImage').on('click', function () {
        
        fileControl.replaceWith( fileControl = fileControl.clone( true ) );
        
        $('#imageFile_progress').hide();
    });
    $( '#imageFile' ).change(function() {
      document.getElementById('imageFile_progress').setAttribute('style', 'height:20px; margin-top:5px;');
    });    
	")
)))
