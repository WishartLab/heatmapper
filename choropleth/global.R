#data_file <- readRDS("data/counties.rds")
data_file <- read.delim("data/statetest2.txt")
name_col <- data_file[[1]]
nums_col <- data_file[[2]]
names(nums_col) <- name_col
density = c(nums_col)

# Breaks we'll use for coloring
densityBreaks <- round(seq(min(density, na.rm = TRUE), max(density, na.rm = TRUE), length.out = 9), 0)

# Construct break ranges for displaying in the legend
densityRanges <- data.frame(
  from = head(densityBreaks, length(densityBreaks)-1),
  to = tail(densityBreaks, length(densityBreaks)-1)
)

# Eight colors for eight buckets
palette <- colorRampPalette(c("#FFEDA0", "#800026"))(8)
	#c("#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C",
   #          "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
# Assign colors to states
colors <- structure(
  palette[cut(density, densityBreaks)],
  names = tolower(names(density))
)

# The state names that come back from the maps package's state database has
# state:qualifier format. This function strips off the qualifier.
getStateName <- function(id) {
  strsplit(id, ":")[[1]][1]
}