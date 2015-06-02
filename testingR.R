library(maps)

x <- read.table("heatmapper/data/statetest2.txt", header = TRUE, sep="\t")	
x <- x[,-3]
x [,-1] <- as.numeric(sub("%","",x[,-1]))
x [,1] <- tolower(x[,1])
#var <- x[,2]
y <- match.map("state", x[,1])
var <- x[y,2]

# generate vector of fill colors for map
shades <- colorRampPalette(c("yellow", "red"))(100)

# constrain gradient to percents that occur between min and max
var <- pmax(var, 0)
var <- pmin(var, 100)
percents <- as.integer(cut(var, 100, include.lowest = TRUE, ordered = TRUE))
fills <- shades[percents]

map(database = "state", fill = TRUE, col = fills,
   	projection = "polyconic", 
    bg="black")

