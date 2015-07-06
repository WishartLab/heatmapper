library(jscolourR)
library(leaflet)

shinyUI(fluidPage(
	includeHTML("navbar.html"),
	sidebarLayout(
		sidebarPanel(
			radioButtons('chooseInput',
				label = "Choose Input Type",
    		choices = c(
    			"Upload File" = 'fileUpload',
					"Example File" = 'example'),
    		selected = 'example'),
    		
    	conditionalPanel(condition = "input.chooseInput == 'fileUpload'", 
    		fileInput("file", label = h3("Choropleth File input"))), 
    		
    	conditionalPanel(condition = "input.chooseInput == 'example'", 
    		wellPanel(
    			HTML("This example file is from the <a href=\"http://shiny.rstudio.com/tutorial/lesson5/\">RStudio Shiny tutorial</a>"))),
    		
    	selectInput("area", label = "Area to use", 
    		choices = c(
    			"USA: By County" = 'county', 
    			"USA: By State" = 'state', 
   				"Canada: By Province" = 'province'), 
   			selected = 'county'),
    		
   		selectInput("colSelect", label = "Column to use", choices = c()),
    		
   		sliderInput("range", 
   			label = "Range of interest:", 
   			min = 0, 
   			max = 100, 
   			value = c(0, 100)), 
    		
    	jscolourInput("lowColour", label = "Colour for low numbers", value = "#F2F2FF"),
    		
    	jscolourInput("highColour", label = "Colour for high numbers", value = "#23B000"),
    		
    	textInput('legend',
    		label = "Custom legend title", 
   			value = "")
   		),
		mainPanel(
			tabsetPanel(type = "tabs", 
				tabPanel(title = "Plot", plotOutput("plot")),
				tabPanel(title = "Interactive",
					
					
					
					# Add a little CSS to make the map background pure white
  tags$head(tags$style("
    #showcase-code-position-toggle, #showcase-sxs-code { display: none; }
    .floater { background-color: white; padding: 8px; opacity: 0.7; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }
  ")),

  leafletMap(
    "map", "100%", 500,
    # By default OpenStreetMap tiles are used; we want nothing in this case
    #initialTileLayer = NULL,
    #initialTileLayerAttribution = NULL,
    options=list(
      center = c(40, -98.85),
      zoom = 4,
      maxBounds = list(list(17, -180), list(59, 180))
    )
  ),
  
  absolutePanel(
    right = 30, top = 10, width = 200, class = "floater",
    
    h4("US Population Density"),
    uiOutput("stateInfo")
  ),
  
	# Legend
  absolutePanel(
    right = 30, top = 280, style = "", class = "floater",
    tags$table(
      mapply(function(from, to, color) {
        tags$tr(
          tags$td(tags$div(
            style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
          )),
          tags$td(from, "-", to)
        )
      }, densityRanges$from, densityRanges$to, palette, SIMPLIFY=FALSE)
    )
  )
					),
				tabPanel(title = "Table", dataTableOutput("table"))
				)))))