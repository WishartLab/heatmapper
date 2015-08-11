source("../global_ui.R")

shinyUI(fluidPage(
	tags$head(
		tags$style(".row {margin-left:0;}")), 
	HEAD_TASKS("#introductionTab"),
	
	navlistPanel(id = "navlistPanel",
		tabPanel("Introduction"), 
		tabPanel("Microarray"),
		tabPanel("Latitude/Longitude"),
		tabPanel("Choropleth"),
		tabPanel("Distance Matirx"),
		tabPanel("Image Overlay"),
	tabsetPanel(
				tabPanel("Instructions"), 
				tabPanel("Gallery"), 
				tabPanel("Contact")
			))
))