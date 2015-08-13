NAVLIST_PANEL <- function(){
	navlistPanel(id = "navlistPanel", widths = c(12,12), well = FALSE,
		tabPanel("Introduction", value = "introduction"),
		tabPanel("Microarray", value = "microarray"),
		tabPanel("Geodensity", value = "latitude_longitude"),
		tabPanel("Geomap", value = "choropleth"),
		tabPanel("Distance Matirx", value = "distance_matrix"),
		tabPanel("Image Overlay", value = "image")	
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