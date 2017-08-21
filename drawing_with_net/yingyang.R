library(igraph)
library(maptools)
library(sp)
library(dplyr)
library(rgeos)
library(scales)

g <-erdos.renyi.game(n=100, p.or.m = 150 , type = "gnm")
V(g)$deg <- centr_degree(g,mode = "total")$res
coords <- layout_with_kk(g)
dev.off()
par(mar = rep(0, 4))
par(bg="grey10")
plot.igraph(g,  
            vertex.frame.color = "grey85",
            vertex.color="black",
            vertex.size=rescale(V(g)$deg,to=c(0.8,10)), 
            vertex.label=NA,
            edge.color = "grey85" ,
            edge.arrow.mode="-",
            edge.width=  0.3,   
            layout=coords ,
            frame=T) 


dev.off()

spCircle<- function (center, radius, nbpts, anglemin, anglemax ) {
  spangles <-   seq(from=anglemin, to=anglemax, length.out = nbpts)

  x <- center[1] +  cos(spangles)*radius
  y <- center[2] +  sin(spangles)*radius
  return(data.frame(cbind(x,y)))
}

DemiccExterneDroite <- spCircle(c(0,0), 10, 1000, -pi/2, pi/2)
DemiccExterneGauche <- spCircle(c(0,0), 10, 1000, pi/2, 3*pi/2)
demiCDroite <- spCircle(c(0,5),5,500,-pi/2, pi/2)
demiCGauche <- spCircle(c(0,-5),5,500,pi/2, 3*pi/2)

petitCercleHaut <- spCircle(c(0,5), 2, 200, 2*pi,0)
petitCercleBas <- spCircle(c(0,-5), 2, 200, 2*pi,0)




cchaut <- rbind(petitCercleHaut, petitCercleHaut[1,])
ccbas <- rbind(petitCercleBas, petitCercleBas[1,])





DemiccExterneGauche <- arrange(DemiccExterneGauche, desc(y))
demiCGauche <- arrange(demiCGauche, y)
demiCDroite <-arrange(demiCDroite,y)
yin <- rbind(DemiccExterneGauche, demiCGauche, demiCDroite)
yin <- rbind(yin, yin[1, ])  # join
PsCbas <- Polygons(list(Polygon(ccbas, hole = FALSE )), ID="yin inclus")
PsYin <- Polygons(list(Polygon(yin, hole = F),Polygon(cchaut, hole = TRUE )), ID = "yin")
spYin <- SpatialPolygons(list(PsYin, PsCbas))


DemiccExterneDroite <- arrange(DemiccExterneDroite, desc(y))
yang <- rbind(DemiccExterneDroite, demiCGauche, demiCDroite)
yang <- rbind(yang, yang[1,])
PsChaut <- Polygons(list(Polygon(cchaut, hole = FALSE)), ID="yang inclus") 
PsYang <- Polygons(list(Polygon(yang, hole = FALSE),Polygon(ccbas, hole = TRUE )), ID = "yang")
spYang <- SpatialPolygons(list(PsYang, PsChaut))



plot(spYin, col="white")
plot(spYang, col="black", add=T)



#=============================
#sampling spatial pour remplir avec des trucs



ptsYang <- spsample(spYang, type = "random", n=8000)
ptsYin <- spsample(spYin, type = "random", n=8000)

dev.off()
par(bg="grey50")
plot(ptsYang, pch=21, col="black",cex=0.1)
plot(ptsYin, pch=21, col="white",cex=0.1, add=T)








#Création de carrés

square <- t(replicate(50, {
  o <- runif(2)
  c(o, o + c(0, 0.1), o + 0.1, o + c(0.1, 0), o)
}))
ID <- paste0('sq', seq_len(nrow(square)))

# Create SP
polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(square, row(square)), ID))

# Create SPDF
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

plot(polys.df, col=rainbow(50, alpha=0.5))

