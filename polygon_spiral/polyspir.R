
library(ggplot2)
library(maptools)
library(sp)
library(dplyr)
library(rgeos)
library(vec2dtransf)

ids <- factor(c("1", "2", "3", "4", "5", "6"))

values <- data.frame(
  #  id = ids,
  #  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
  
  id=factor(c("1","2","3","4")),
  value=c(1,2,3,4)
)




positions <- data.frame(
  # id = rep(ids, each = 4),
  # x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
  #     0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  #y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
  #    2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
  id=rep("1", each=4),
  x=c(0,10,10,0),
  y=c(0,0,10,10)
)



# Currently we need to manually merge the two together
datapoly <- merge(values, positions, by = c("id"))

drawthat <- function(datapoly){
  p <- ggplot(datapoly, aes(x = x, y = y)) +
    geom_polygon(aes( group = id), fill=NA, colour="green", show.legend = FALSE)+
    geom_point(colour="black")+
    theme_void()
  p
}


#=============================
#  version avec sp 
#===============

mycoords <- matrix( nrow=4, ncol=2)
mycoords[,1] <-positions$x
mycoords[,2] <-positions$y
mycoords

poly <- Polygon(coords = mycoords )
polys <- Polygons(list(poly), ID = 1)
spolys <- SpatialPolygons(list(polys))
plot (spolys)


#tool functions
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

spatialPolygonFactory<- function(spts,id){
  p<- Polygon(spts)
  ps <- Polygons(list(p), ID = id)
  sps <- SpatialPolygons(list(ps))
  return(sps)
}

transformationFactory <- function (angle , homothetic , translaX, translaY){
  return(AffineTransformation(parameters = c(homothetic*cos(angle), 
                                             -homothetic*sin(angle),
                                             translaX,
                                             homothetic*sin(angle),
                                             homothetic*cos(angle),
                                             translaY)))
}



listps <- spolys@polygons[[1]]
pps <- listps@Polygons[[1]]
coordsP <- coordinates(pps)
linesP  <- as(spolys, "SpatialLines")

baseP <- spatialPolygonFactory(coordsP, id=1)

#calcul du ratio homothetie
#triangle rectancgle de petit côté translaX et de grand côté taille du carré originel - translaX
translaX <- 1
arete <- 10
hyp <- sqrt(  (arete-translaX)^2  + (translaX^2))
lambda <- hyp / arete
alpha <- atan(translaX/arete) 

transfo1 <- transformationFactory(angle = (alpha),
                                  homothetic = lambda,
                                  translaX ,translaY = 0)




dev.off()
plot(baseP)
splpts <- spsample(baseP,30, type = "stratified")
plot(splpts, add=T)
triangles <- gDelaunayTriangulation(splpts)

dev.off()
plot(baseP)
plot(triangles, add=T)

plot(currentP)



for (i in 1:length(triangles@polygons)){
#  plot(triangles[i,], add=T)
  currentP <- triangles[i,]
  
      while(currentP@polygons[[1]]@area >= 1){
  

      shrinkP <- applyTransformation(transfo1, currentP)
      Sys.sleep(1)
      plot(shrinkP, add=T, col=i)
      currentP <- shrinkP
    }
    
}




bboxPolyFactory <- function (currentP) {
  bb <- data.frame(currentP@bbox)
  bbcoordsX <- c(bb$min[1], bb$max[1], bb$max[1], bb$min[1])
  bbcoordsY <- c(bb$min[2], bb$min[2], bb$max[2], bb$max[2])
  bbcoords <- matrix(nrow = 4, ncol = 2)
  bbcoords[, 1] <- bbcoordsX
  bbcoords[, 2] <- bbcoordsY
  spbb <- spatialPolygonFactory(bbcoords, id = 2)
  return(spbb)
}

currentP <- triangles[15,]

dev.off()
plot(baseP)
plot(currentP, add=T)
spbb <- bboxPolyFactory(currentP = currentP )
plot(spbb,add=T)


Xmin  <- min (spbb@polygons[[1]]@Polygons[[1]]@coords[,1] )
Ymin <- min (spbb@polygons[[1]]@Polygons[[1]]@coords[,2] )
Xmax <- max (spbb@polygons[[1]]@Polygons[[1]]@coords[,1] )
Ymax <- max (spbb@polygons[[1]]@Polygons[[1]]@coords[,2] )

coteH <- abs(Xmax - Xmin)
coteV <- abs(Ymax-Ymin)

ecart <- 0.2 * coteH
translaX <-  ecart
translaY <- 0

hyp <- sqrt(  (ecart)^2  + ((coteV-ecart )^2))
lambda <- hyp / coteV
alpha <- atan(ecart/(coteV-ecart )) 


transfo1 <- transformationFactory(angle = rad2deg(alpha),
                                  homothetic = lambda,
                                  translaX ,translaY )  

shrinkP <- applyTransformation(transfo1, currentP)

plot(shrinkP, add=T)
bbshrink <- bboxPolyFactory(shrinkP)
plot(bbshrink, add=T)

dev.off()
plot(baseP)
plot(currentP, add=T)

currentP <- baseP

while(currentP@polygons[[1]]@area >= 1){
  
  plot(spbb, add=T)
  
  bbXlong <- max(abs (spbb@polygons[[1]]@Polygons[[1]]@coords[,1]-
                        spbb@polygons[[1]]@Polygons[[1]]@coords[,2]))
  arete <- bbXlong
  translaX <- 0.1 * arete
  
  hyp <- sqrt(  (arete-translaX)^2  + (translaX^2))
  lambda <- hyp / arete
  alpha <- atan(translaX/arete) 
  
  transfo1 <- transformationFactory(angle = (alpha),
                                    homothetic = lambda,
                                    translaX ,translaY = 0)  
  
  shrinkP <- applyTransformation(transfo1, currentP)
  Sys.sleep(0.3)
  plot(shrinkP, add=T)
  
    
  currentP <- shrinkP
}



plot(baseP)


