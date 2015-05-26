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
		sidebarLayout(
    		sidebarPanel(fileInput("heatmapFile", label = h3("File input"))),
			mainPanel(
				tabsetPanel(type = "tabs", 
					tabPanel(title = "Plot", plotOutput("heatmap")),
					tabPanel(title = "Dendrogram", plotOutput("heatmapDendrogram")),
					tabPanel(title = "Table", dataTableOutput("heatmapTable")))))), 
	
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
    		sidebarPanel(fileInput("dmFile", label = h3("File input"))),
			mainPanel(
				tabsetPanel(type = "tabs", 
					tabPanel(title = "Plot", plotOutput("discreteMap")), 
					tabPanel(title = "Table", dataTableOutput("discreteTable"))))))
	
))