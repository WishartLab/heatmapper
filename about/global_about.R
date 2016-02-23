NAVLIST_PANEL <- function(){
	navlistPanel(id = "navlistPanel", widths = c(12,12), well = FALSE,
		tabPanel("Introduction", value = "introduction"),
		tabPanel("Expression", value = "expression"),
		tabPanel("Pairwise", value = "pairwise"),
		tabPanel("Image Overlay", value = "image"),
		tabPanel("Geomap", value = "geomap"),
		tabPanel("Geocoordinate", value = "geocoordinate")
	)
}

NAVLIST_PANEL_FOR_GALLERY <- function(){
	navlistPanel(id = "navlistPanelForGallery", widths = c(12,12), well = FALSE,
		tabPanel("Expression", value = "expression"),
		tabPanel("Pairwise", value = "pairwise"),
		tabPanel("Image Overlay", value = "image"),
		tabPanel("Geomap", value = "geomap"),
		tabPanel("Geocoordinate", value = "geocoordinate")
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