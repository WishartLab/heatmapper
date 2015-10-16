NAVLIST_PANEL <- function(){
	navlistPanel(id = "navlistPanel", widths = c(12,12), well = FALSE,
		tabPanel("Introduction", value = "introduction"),
		tabPanel("Expression Heat Map", value = "expression"),
		tabPanel("Distance Matrix", value = "distance_matrix"),
		tabPanel("Image Overlay", value = "image"),
		tabPanel("Geomap", value = "geomap"),
		tabPanel("Geodensity", value = "latitude_longitude")
	)
}

NAVBAR <- function(activeTab){
    list(
        includeHTML("../../www/navbar.html"),
        tags$script(
            paste0("$('", activeTab, "').addClass('active');
            $('#heatmapper-logo').on('click', function(){ window.location.href = '/'; });")
        )
        
    )
}