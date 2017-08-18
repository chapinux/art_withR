library(igraph)
library(maptools)
library(sp)
library(dplyr)
library(rgeos)

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
  spangles <-   order(runif(nbpts) * (anglemax-anglemin) + anglemin )

  x <- center[1] +  cos(spangles)*radius
  y <- center[2] +  sin(spangles)*radius
  return(data.frame(cbind(x,y)))
}

DemiccExterneDroite <- spCircle(c(0,0), 10, 1000, -pi/2, pi/2)
DemiccExterneGauche <- spCircle(c(0,0), 10, 1000, pi/2, 3*pi/2)
demiCDroite <- spCircle(c(0,5),5,500,-pi/2, pi/2)
demiCGauche <- spCircle(c(0,-5),5,500,pi/2, 3*pi/2)

petitCercleHaut <- spCircle(c(0,5), 2, 400, 2*pi,0)
petitCercleBas <- spCircle(c(0,-5), 2, 400, 2*pi,0)


DemiccExterneGauche <- arrange(DemiccExterneGauche, desc(y))
demiCGauche <- arrange(demiCGauche, y)
demiCDroite <-arrange(demiCDroite,y)
yin <- rbind(DemiccExterneGauche, demiCGauche, demiCDroite)
yin <- rbind(yin, yin[1, ])  # join
PsYin <- Polygons(list(Polygon(yin, hole = T)), ID = "yin")


ccbas <- rbind(petitCercleBas, petitCercleBas[1,])
PsCbas <- Polygons(list(Pccbas <- Polygon(ccbas, hole = FALSE )), ID="yin inclus")

plot(SpatialPolygons(list(PsCbas)))
spYin <- SpatialPolygons(list(PsYin, PsCbas))



plot(spYin, col="blue")



cchaut <- rbind(petitCercleHaut, petitCercleHaut[1,])
Pcchaut <- Polygon(cchaut, hole = F)


plot(spYin)



DemiccExterneDroite <- arrange(DemiccExterneDroite, desc(y))
yang <- rbind(DemiccExterneDroite, demiCGauche, demiCDroite)
yang <- rbind(yang, yang[1,])
Pyang <- Polygon(yang, hole = FALSE)
ccbas <- rbind(petitCercleBas, petitCercleBas[1,])
Pccbas <- Polygon(ccbas, hole = TRUE )
Pcchaut <- Polygon(cchaut, hole = FALSE)
holeBas <- Polygons(list(Pccbas), ID="hole bas")

holeBas@Polygons[[1]]@hole <- TRUE
PsYang <- Polygons(list(Pyang, Pcchaut), ID="yang")

spYang <- SpatialPolygons(list(PsYang, holeBas))




dev.off()


plot(spYang)
plot(spYang, col="green")





#=============================
#sampling spatial pour remplir avec des trucs


maptools::checkPolygonsHoles(PsYang)
spYang <- SpatialPolygons(list(PsYang, holeBas))


plot(spYang)

ptsYang <- spsample(spYang, type = "random", n=8000)


dev.off()
plot(ptsYang, pch=21, col="blue",cex=0.1)

plot(spYang)

ptsYang %over% SpatialPolygons(list(Pccbas))



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

