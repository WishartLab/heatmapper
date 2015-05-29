shinyUI(navbarPage(
	title = "Heatmapper", 
	
	#### Main Panel ####
	tabPanel(title = "Introduction", 
		tabsetPanel(type = "tabs", 
			tabPanel(title = "Overview"),
			tabPanel(title = "Gallery"),
			tabPanel(title = "Instructions"),
			tabPanel(title = "Contact"))),
	
	#### Microarray Panel ####
	tabPanel(title = "Microarray", 
		tabsetPanel(type = "tabs", 
			tabPanel(title = "Options", 
				selectInput('fileType', 
					label = h3("Select File Input Type"), 
					choices = c(
						"Clustered Data (.cdt)" = 'cdt', 
						"Preclustered Data (.txt, .csv, .pcl)" = 'txt'
						), 
					selected = 'cdt'),
				conditionalPanel(condition = "input.fileType == 'cdt'", 
					fileInput("cdtFile", multiple = TRUE, label = h3(".cdt File input")), 
					fileInput("gtrFile", label = h3(".gtr File input")), 
					fileInput("atrFile", label = h3(".atr File input")))),
			tabPanel(title = "Plot", 
				plotOutput("heatmap")),
			tabPanel(title = "Dendrogram", 
				h2("Row Dendrogram"), 
				plotOutput("rowDendrogram"), 
				h2("Column Dendrogram"), 
				plotOutput("colDendrogram")),
			tabPanel(title = "Table", dataTableOutput("heatmapTable")))), 
	
	#### Continuous Maps Panel ####
	tabPanel(title = "Continuous Maps", 
		sidebarLayout(
    	sidebarPanel(fileInput("cmFile", label = h3("File input"))),
			mainPanel(
				tabsetPanel(type = "tabs", 
					tabPanel(title = "Plot", plotOutput("continuousMap")), 
					tabPanel(title = "Table", dataTableOutput("continuousTable")))))), 

	#### Discrete Maps Panel ####
	tabPanel(title = "Discrete Maps", 
		sidebarLayout(
    	sidebarPanel(
    	
    		radioButtons('dmChooseInput',
    			label = "Choose Input Type",
    			choices = c(
    				"Upload File" = 'dmFileUpload',
						"Example File" = 'dmExample'),
    			selected = 'dmExample'),
    		
    		conditionalPanel(condition = "input.dmChooseInput == 'dmFileUpload'", 
    			fileInput("dmFile", label = h3("Choropleth File input"))), 
    		
    		conditionalPanel(condition = "input.dmChooseInput == 'dmExample'", 
    			wellPanel(
    				HTML("This example file is from the <a href=\"http://shiny.rstudio.com/tutorial/lesson5/\">RStudio Shiny tutorial</a>"))),
    		
    		selectInput("dmArea", label = "Area to use", 
    			choices = c(
    				"USA: By County" = 'county', 
    				"USA: By State" = 'state', 
    				"Canada: By Province" = 'province'), 
    			selected = 'county'),
    		
    		selectInput("dmColSelect", label = "Column to use", choices = c()),
    		
    		sliderInput("dmRange", 
    			label = "Range of interest:", 
    			min = 0, 
    			max = 100, 
    			value = c(0, 100)), 
    		
    		selectInput("dmLowColour", 
    			label = "Colour for low values", 
    			choices = c( 
    				"red" = 'red',
    				"orange" = 'orange',
    				"yellow" = 'yellow',
    				"green" = 'darkgreen',
    				"blue" = 'blue',
    				"purple" = 'purple', 
    				"white" = 'white',
    				"grey" = 'grey', 
    				"black" = 'black'),
    			selected = 'white'),
    		
    		selectInput("dmHighColour", 
    			label = "Colour for high values", 
    			choices = c( 
    				"red" = 'red',
    				"orange" = 'orange',
    				"yellow" = 'yellow',
    				"green" = 'darkgreen',
    				"blue" = 'blue',
    				"purple" = 'purple', 
    				"white" = 'white',
    				"grey" = 'grey', 
    				"black" = 'black'),
    			selected = 'darkgreen'), 
    		
    		textInput('dmLegend',
    			label = "Custom legend title", 
    			value = "")
    		),
			mainPanel(
				tabsetPanel(type = "tabs", 
					tabPanel(title = "Plot", plotOutput("discreteMap")), 
					tabPanel(title = "Table", dataTableOutput("discreteTable"))))))
	
))