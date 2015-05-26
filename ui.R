shinyUI(navbarPage(
	title = "Heatmapper", 
	
	#### Main Panel ####
	tabPanel(title = "Introduction"),
	
	#### Microarray Panel ####
	tabPanel(title = "Microarray"), 
	
	#### Continuous Maps Panel ####
	tabPanel(title = "Continuous Maps"), 
	
	#### Discrete Maps Panel ####
	tabPanel(title = "Discrete Maps", 
		plotOutput(outputId='discreteMap')
		)
	
))