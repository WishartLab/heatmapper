## COVID-19 updates

In response to COVID-19, Heatmapper has been adapted to
map cold- and flu-like symptoms in Colombia, South America, to enable officials
to better respond to the crisis. People in Colombia can self-report symptoms at
[cov19col.herokuapp.com](https://cov19col.herokuapp.com).
Please help get the word out!

The project is open source, for both the
[self-reporting app](https://github.com/Marco8301/QT-Cov19-OS)
and the heatmapping component (in the `symptom_mapping` branch of this repository).


## Heatmapper is hosted at www.heatmapper.ca
* Expression Heat Map: www.heatmapper.ca/expression
* Pairwise Comparison: www.heatmapper.ca/pairwise
* Image Overlay: www.heatmapper.ca/image
* Geomap: www.heatmapper.ca/geomap
* Geocoordinate: www.heatmapper.ca/geocoordinate

## Required R Packages
<pre><code>
install.packages(c("shiny", "shinyBS", "d3heatmap", "gplots", "ggdendro", "jpeg", "png", "tiff", "ggplot2", "ggtern", "MASS", "reshape2", "leaflet", "RColorBrewer", "raster", "devtools", "Cairo", "ape"))
install.packages('DT', repos = 'http://cran.rstudio.com')
library(devtools)
devtools::install_github('sbabicki/jscolourR')
devtools::install_github('ramnathv/htmlwidgets')
devtools::install_github('timelyportfolio/htmlwidgets_spin')
</code></pre>


