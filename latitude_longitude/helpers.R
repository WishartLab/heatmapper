jscolourInput <- function (inputId, label, value = "#000000"){
	tagList(
		singleton(tags$head(tags$script(src="js/jscolor/jscolor.js"))),
		tags$strong(label),
		tags$br(),
		tags$input(
			id = inputId,
			value = value, 
			class = "color {hash:true}", 
			onchange = paste0("$('#", inputId, "').trigger('newVal')")), 
		tags$br(), 
		tags$br())
}