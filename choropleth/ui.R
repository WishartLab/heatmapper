library(leaflet)
library(jscolourR)
shinyUI(fluidPage(
  includeHTML("www/navbar.html"),
  # Add a little CSS to make the map background pure white
  tags$head(
  	HTML("<link rel=\"stylesheet\" href=\"//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css\">"),
		tags$style("
		.toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}
		#showcase-code-position-toggle, #showcase-sxs-code { display: none; }
    .floater { background-color: white; padding: 8px; opacity: 0.7; border-radius: 6px; 
			box-shadow: 0 0 15px rgba(0,0,0,0.2); }")),
sidebarLayout(
	sidebarPanel(
		actionButton('fileInputOptionsButton', label = "Hide File Options", class = "toggleButton fa fa-angle-up"),
		wellPanel(id = "fileInputPanel", 
		radioButtons('chooseInput',
				label = "Choose Input Type",
    		choices = c(
    			"Upload File" = 'fileUpload',
					"Example File" = 'example'),
    		selected = 'example'),
    		
    	conditionalPanel(condition = "input.chooseInput == 'fileUpload'", 
    		fileInput("file", label = strong("Choropleth File input"))), 
    		
    	conditionalPanel(condition = "input.chooseInput == 'example'", 
    		wellPanel(
    			HTML("This example file is from the <a href=\"http://shiny.rstudio.com/tutorial/lesson5/\">RStudio Shiny tutorial</a>"))), 
    		
    	selectInput("area", label = "Area to use", 
    		choices = c(
    			"USA: By County" = 'county', 
    			"USA: By State" = 'state',
    			"France" = 'france'), 
   			selected = 'state'),
    		
   		selectInput("colSelect", label = "Column to use", choices = c("loading..."="temp")),
		
   		sliderInput("range", 
   			label = "Range of interest", 
   			min = 0, 
   			max = 100, 
   			value = c(0, 100))
			), 
			
		actionButton('colourOptionsButton', label = "Hide Colour Options", class = "toggleButton fa fa-angle-up"),
		wellPanel(id = "colourPanel", 		
    	jscolourInput("lowColour", label = "Colour for low numbers", value = "#FFEDA0"),
    		
    	jscolourInput("highColour", label = "Colour for high numbers", value = "#800026"),
		
			sliderInput("binNumber", 
				label = "Number of bins", 
				min = 2, 
				max = 8, 
				value = 8),
		
			sliderInput("fillOpacity", 
				label = "Fill Opacity", 
				min = 0, 
				max = 1, 
				value = 0.8)
			
		), 
		
		actionButton('mapOptionsButton', label = "Hide Map Options", class = "toggleButton fa fa-angle-up"),
		wellPanel(id = "mapPanel", 	
		strong("Map background"), 
			checkboxInput("showTiles", label = "Show Tiles", value = TRUE), 
		
			sliderInput("lineSize", 
				label = "Line Width", 
				min = 0,
				max = 5,
				value = 1)
			),
    		

		
	actionButton('labelOptionsButton', label = "Hide Label Options", class = "toggleButton fa fa-angle-up"),
	wellPanel(id = "labelPanel", 	
    	textInput('legend',
    		label = "Custom legend title", 
   			value = "Legend")
	), 
	actionButton('downloadOptionsButton', label = "Hide Download Options", class = "toggleButton fa fa-angle-up"),
	wellPanel(id = "downloadPanel")
	),
	mainPanel(
		tabsetPanel(id = "tabSelections", type = "tabs",
			#tabPanel(title= "sadf"),
			tabPanel(title = "Interactive", 
			  leafletOutput(
			    "map", "100%", 500
			  ),
			  absolutePanel(
			    right = 30, top = 60, width = 200, class = "floater",
			    
			    h4("Region Information"),
			    uiOutput("stateInfo")
			  ),
			 
				# Legend
			  absolutePanel(
			    right = 30, top = 320, style = "", class = "floater",
			    uiOutput("legend")
			  )
			),
			tabPanel(title = "Table", 
				tags$br(),
				dataTableOutput("table"))))),
	singleton(includeScript("www/js/active.js"))
))