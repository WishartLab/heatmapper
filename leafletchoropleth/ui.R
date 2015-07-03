library(leaflet)
library(jscolourR)
shinyUI(fluidPage(
  includeHTML("navbar.html"),
	
  # Add a little CSS to make the map background pure white
  tags$head(tags$style("
    #showcase-code-position-toggle, #showcase-sxs-code { display: none; }
    .floater { background-color: white; padding: 8px; opacity: 0.7; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }
  ")),
sidebarLayout(
	sidebarPanel(
		radioButtons('chooseInput',
				label = "Choose Input Type",
    		choices = c(
    			"Upload File" = 'fileUpload',
					"Example File" = 'example'),
    		selected = 'fileUpload'),
    		
    	conditionalPanel(condition = "input.chooseInput == 'fileUpload'", 
    		fileInput("file", label = h3("Choropleth File input"))), 
    		
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
   			label = "Range of interest:", 
   			min = 0, 
   			max = 100, 
   			value = c(0, 100)), 
    		
    	jscolourInput("lowColour", label = "Colour for low numbers", value = "#FFEDA0"),
    		
    	jscolourInput("highColour", label = "Colour for high numbers", value = "#800026"),
		
			checkboxInput("showTiles", label = "Show Tiles", value = TRUE),
		
			sliderInput("lineSize", 
				label = "Line Width", 
				min = 0,
				max = 5,
				value = 1),
		
			sliderInput("fillOpacity", 
				label = "Fill Opacity", 
				min = 0, 
				max = 1, 
				value = 0.8),
    		
			sliderInput("binNumber", 
				label = "Number of bins", 
				min = 2, 
				max = 8, 
				value = 8),
		
    	textInput('legend',
    		label = "Custom legend title", 
   			value = "")
	),
	mainPanel(
		tabsetPanel(type = "tabs",
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
			    right = 30, top = 340, style = "", class = "floater",
			    uiOutput("legend")
			  )
			),
			tabPanel(title = "Table", 
				tags$br(),
				dataTableOutput("table")))))
))