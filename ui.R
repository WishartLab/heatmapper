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
						"cdt" = 'cdt', 
						"pcl" = 'pcl',
						"txt" = 'txt'), 
					selected = 'cdt'),
				conditionalPanel(condition = "input.fileType == 'cdt'", 
					fileInput("cdtFile", label = h3(".cdt File input")), 
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
    	sidebarPanel(fileInput("dmFile", label = h3("File input"))),
			mainPanel(
				tabsetPanel(type = "tabs", 
					tabPanel(title = "Plot", plotOutput("discreteMap")), 
					tabPanel(title = "Table", dataTableOutput("discreteTable"))))))
	
))