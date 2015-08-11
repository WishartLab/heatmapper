source("../global_ui.R")

shinyUI(fluidPage(
	
	HEAD_TASKS("#introductionTab"),
	
	sidebarLayout(
	  sidebarPanel(id = "sidebarPanel"), 
		mainPanel(id = "mainPanel", 
			tabsetPanel(
				tabPanel("Instructions"), 
				tabPanel("Gallery"), 
				tabPanel("Contact")
			)
		)
	)
))