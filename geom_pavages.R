library(RColorBrewer) #to use brewer.pal
library(fields) #to use designer.colors


# code de Nadieh Bremer
# http://www.visualcinnamon.com/2013/11/how-to-create-hexagonal-heatmap-in-r.html

#Function to create the polygon for each hexagon
Hexagon <- function (x, y, unitcell = 1, col = col) {
  polygon(c(x, x, x + unitcell/2, x + unitcell, x + unitcell, 
            x + unitcell/2), c(y + unitcell * 0.125, 
                               y + unitcell * 0.875, 
                               y + unitcell * 1.125, 
                               y + unitcell * 0.875, 
                               y + unitcell * 0.125, 
                               y - unitcell * 0.125), 
          col = col, border=NA)
}#function

dimmat <- 25

mat<- matrix(runif(2500), nrow =dimmat , ncol = dimmat)
x <- as.vector(mat)

#Number of rows and columns of your SOM
SOM_Rows <- dim(mat)[1]
SOM_Columns <- dim(mat)[2]

#To make room for the legend
par(mar = c(0.4, 2, 2, 7))

#Initiate the plot window but do show any axes or points on the plot
plot(0, 0, type = "n", axes = FALSE, xlim=c(0, SOM_Columns), 
     ylim=c(0, SOM_Rows), xlab="", ylab= "", asp=1)


ColRamp <- rev(designer.colors(n=50, col=brewer.pal(9, "Spectral")))



#Make a vector with length(ColRamp) number of bins between the minimum and 
#maximum value of x. 
#Next match each point from x with one of the colors in ColorRamp
ColorCode <- rep("#FFFFFF", length(x)) #default is all white
Bins <- seq(min(x, na.rm=T), max(x, na.rm=T), length=length(ColRamp))
for (i in 1:length(x))
  if (!is.na(x[i])) ColorCode[i] <- ColRamp[which.min(abs(Bins-x[i]))] 

#Actual plotting of hexagonal polygons on map
offset <- 0.5 #offset for the hexagons when moving up a row
for (row in 1:SOM_Rows) {
  for (column in 0:(SOM_Columns - 1)) 
    Hexagon(column + offset, row - 1, col = ColorCode[row + SOM_Rows * column])
  offset <- ifelse(offset, 0, 0.5)
}






