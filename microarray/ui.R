library(jscolourR)
shinyUI(fluidPage(
	includeHTML("www/navbar.html"),
	tags$head(
		HTML("<link rel=\"stylesheet\" href=\"//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css\">"),
		tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}")),

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
    			fileInput("file", label = strong("File input")))
					), 
    		
  		actionButton('colourOptionsButton', label = "Hide Colour Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "colourPanel", 
    		jscolourInput("lowColour", label = "Colour for low numbers", value = "#FFFA00"),
    		
    		jscolourInput("highColour", label = "Colour for high numbers", value = "#FF0000")
			)),
		mainPanel(
			plotOutput("map")
			)), 
	singleton(includeScript("www/js/active.js"))
	))