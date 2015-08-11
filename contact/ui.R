source("../global_ui.R")

shinyUI(fluidPage(
	HEAD_TASKS("#contactTab"), 
    includeHTML("www/contact.html"),
	INCLUDE_JS()
))