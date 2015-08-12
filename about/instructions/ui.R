source("../global_about.R")
shinyUI(fluidPage(
	NAVBAR("#aboutTab"),

	sidebarLayout(
		sidebarPanel(id = "sidebarPanel",
			NAVLIST_PANEL()
		),
		mainPanel(id = "mainPanel",
			titlePanel("Instructions"),
			tags$br(),
			uiOutput("instructions")
		)
	)
))