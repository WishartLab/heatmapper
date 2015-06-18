library(jscolourR)

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
				tabPanel(title = "Plot", plotOutput("map")),
				tabPanel(title = "Table", dataTableOutput("table"))
				)))))