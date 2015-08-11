source("../global_ui.R")

shinyUI(fluidPage(
	HEAD_TASKS("#introductionTab"),
	
	sidebarLayout(
		sidebarPanel(id = "sidebarPanel",
			navlistPanel(id = "navlistPanel", widths = c(12,12), well = FALSE,
				tabPanel("Introduction", value = "introduction"),
				tabPanel("Microarray", value = "microarray"),
				tabPanel("Latitude/Longitude", value = "latitude_longitude"),
				tabPanel("Choropleth", value = "choropleth"),
				tabPanel("Distance Matirx", value = "distance_matrix"),
				tabPanel("Image Overlay", value = "image")
			)
		),
		mainPanel(id = "mainPanel",
			tabsetPanel(
				tabPanel("Instructions", 
					tags$br(),
					uiOutput("instructions")), 
				tabPanel("Gallery", 
					tags$br(),
					uiOutput("gallery")), 
				tabPanel("Contact", 
					tags$br(),
					uiOutput("contact"))
			)
		)
	)
))