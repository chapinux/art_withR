library(spatstat)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(maptools)
library(sp)

W23 <- as.owin(list(xrange=c(0,3), yrange=c(0,3)))

set.seed(16)



################################@
#spirale logarithmique
###############################

nbpoints <-300

a=2
b=1.2
tt <-seq(from=0, to=8*pi, length.out = nbpoints )
yy <- a*b^tt*sin(tt)
xx<- a*b^tt*cos(tt)
xx <-rescale(xx, to=c(0.2,2.8))
yy <- rescale(yy, to=c(0.2,2.8))


main.lines <- psp(head(xx,nbpoints-1),head(yy,nbpoints-1), tail(xx,nbpoints-1), tail(yy,nbpoints-1), window=W23)

mycol <- topo.colors(100)
Lambda <- 7
sublinesqty <- 10
nblvl <- 12 

affiche(main.lines, W23)

##############################@@@@
# triskel
#################################


nbpoints <-500

k=4
tt <-seq(from=0, to=2*pi, length.out = nbpoints )
yy <- 1.5*sin(tt) - sin(k*tt)
xx<- 1.5*cos(tt) - cos(k*tt)
xx <-rescale(xx, to=c(0.2,2.8))
yy <- rescale(yy, to=c(0.2,2.8))

main.lines <- psp(head(xx,nbpoints-1),head(yy,nbpoints-1), tail(xx,nbpoints-1), tail(yy,nbpoints-1), window=W23)


Lambda <- 4
sublinesqty <- 10
nblvl <- 18 

mycol <-brewer.pal(100, 'PuBu')
affiche(main.lines, W23)


##############################@@@@
# astroide
#################################


nbpoints <-300

k=4
tt <-seq(from=0, to=2*pi, length.out = nbpoints )
yy <- k*(sin(tt))^3
xx<-  k*(cos(tt))^3
xx <-rescale(xx, to=c(0.2,2.8))
yy <- rescale(yy, to=c(0.2,2.8))

main.lines <- psp(head(xx,nbpoints-1),head(yy,nbpoints-1), tail(xx,nbpoints-1), tail(yy,nbpoints-1), window=W23)


Lambda <- 5
sublinesqty <- 7
nblvl <- 18 

mycol <-brewer.pal(nblvl, 'YlGnBu')
affiche(main.lines, W23)



##############################@@@@
# pattern 1 :  droites perpendiculaires à 45degres
#################################


nl1 <- 12
# diagonal montante
xx <- sample(0:30, nl1)
yy <-sample(0:30, nl1)
ll <- sample(2:20, nl1)
xx2 <- xx
yy2 <- yy
for (j in 1:nl1) {
  xx2[j] <-  xx[j] + min(ll[j], (30-xx[j]),(30-yy[j]))
  yy2[j] <-  yy[j] + min(ll[j], (30-xx[j]),(30-yy[j]))
}



#diagonale descendante
xx3 <- sample(0:30, nl1)
yy3 <- sample(0:30, nl1)
ll2 <- sample(2:20, nl1)
xx4 <- xx3
yy4 <- yy3 
for (j in 1:nl1) {
  xx4[j] <-  xx3[j] + min(ll2[j], xx3[j],yy3[j],(30-xx3[j]),(30-yy3[j]))
  yy4[j] <-  yy3[j] - min(ll2[j], xx3[j],yy3[j],(30-xx3[j]),(30-yy3[j]))
}

xx <- append(xx,xx3)
yy <- append(yy, yy3)
xx2 <-append(xx2, xx4)
yy2 <- append(yy2,yy4)

xx <- scales::rescale(xx, to=c(0,3), from = c(0,30))
yy <- scales::rescale(yy, to=c(0,3), from = c(0,30))
xx2 <-scales::rescale(xx2, to=c(0,3), from = c(0,30))
yy2 <- scales::rescale(yy2, to=c(0,3), from = c(0,30))


main.lines <- psp(xx,yy,xx2,yy2, window=W23,check = F)

Lambda <- 9
sublinesqty <- 12
nblvl <- 8 
mycol <-brewer.pal(nblvl, 'GnBu')
affiche(main.lines,W23)






##############################@@@@
# pattern 2 carré + cercle 
#################################

Lambda <- 9
sublinesqty <- 12
nblvl <- 8 

mycol <-brewer.pal(nblvl, 'GnBu')


# carre

x1<- c(1,2,2,1)
x2<- c(2,2,1,1)
y1 <- c(1,1,2,2)
y2 <- c(1,2,2,1)
carre <- psp(x1,y1,x2,y2, window=W23,check = F)
affiche(carre)



#rond
nbpoints = 50 
tt <-seq(from=0, to= 2*pi, length.out = nbpoints )
a <- sqrt(2)/2
x <- (cos(tt) * a )+1.5
y <- (sin(tt) * a ) +1.5
yy1 <-head(y, nbpoints-1)
yy2 <-tail(y, nbpoints-1)
xx1<- head(x, nbpoints-1)
xx2<- tail(x, nbpoints-1)
rond <- psp(xx1,yy1,xx2,yy2, window=W23)

ml <- psp(append(x1,xx1), append(y1,yy1), append(x2,xx2), append(y2,yy2),window = W23)


affiche(ml)
affiche_ext(ml)


#################################
# pattern 3 nbXnblines
###################################

nblines <-2

Vcombx1 <-runif(nblines, 0, 3)
Vcomby1 <- rep(0,nblines) 
Vcombx2 <- runif(nblines, 0, 3)
Vcomby2 <- rep(3,nblines)

listcordVcom <- c(Vcombx1, Vcomby1,Vcomby1, Vcomby2)

Hcombx1 <- rep(0,nblines)
Hcomby1 <- runif(nblines, 0, 3)
Hcombx2 <- rep(3,nblines)
Hcomby2 <- runif(nblines, 0, 3)


ml <- psp(append(Vcombx1,Hcombx1), append(Vcomby1, Hcomby1), 
          append(Vcombx2, Hcombx2), append(Vcomby2, Hcomby2)  ,window = W23)

Lambda <- 9
sublinesqty <-12
nblvl <- 12
mycol <-brewer.pal(nblvl, 'RdYlBu')
affiche(ml, ml$window)

dev.off()


##################################"
# pattern 4 shp file
###############################

map <- readShapeSpatial("Dropbox/stats/art_with_R/contour_france_2/contour_france_2.shp")
# adjust the window to have some margin 
bbox <- owin( xrange = c(map@bbox[1,1]*0.5, map@bbox[1,2]*1.1) , yrange =  c(map@bbox[2,1]*0.98, map@bbox[2,2]*1.02))

p1 <- as.psp.SpatialLinesDataFrame(map,window = bbox, marks = NULL)


plot(p1)

# in case of something else than SpatialLinesDataframe
bbox <- as.owin(map)
bbox <- owin(xrange = bbox$xrange, yrange = bbox$yrange, mask = NULL)

polys <- as(map, "SpatialPolygons")

convertpolyLines_to_psp <- function(p,w){
  ll <- as(p, "SpatialLines")
  s <- as.psp.SpatialLines(ll, window = w, marks = NULL )
  s$marks <-NULL
  return(s)
}

p1 <-convertpolyLines_to_psp(polys[1],bbox)
plot(p1)
nbpolymax <- length(polys) / 4
nbpolymax <- 50
for (i in 2:nbpolymax){
  s <- convertpolyLines_to_psp(polys[i], bbox)
    p1 <- append.psp(p1,s)
}


distmap()

#masking ??
map2 <- as.SpatialPolygons(map)
m1 <- owin(p1$window$xrange, p1$window$yrange, poly=data.frame(x=rev(map$x), y=rev(map$y)))

p1 <- rescale.psp(p1, 10000)


x<- as.list(c(p1$ends$x0, p1$ends$x1))
y<- as.list(c(p1$ends$y0, p1$ends$y1))
xy <-list(x=x,y=y)


m1 <- owin(p1$window$xrange, p1$window$yrange, poly=data.frame(x=x,y=y))

plot(distmap(m1))

Lambda <- 0.50
sublinesqty <- 0.8
nblvl <- 8
mycol <-rev(brewer.pal(nblvl, 'PuBu'))

# distances are toobig so resacle it
p1 <- as.psp.SpatialLinesDataFrame(map,window = bbox, marks = NULL)
p1 <- rescale.psp(p1, 10000)

is.polygonal(as.owin(p1))


p1$marks<- NULL
affiche(p1,p1$window)

plot(distmap(p1))


##################################"
# pattern 5 graphs 
###############################
library(igraph)


make_seg_from_graph <- function(gcube, lay){
  # init graph and layout
  ll<- lay

  ll[,1] <- scales::rescale(ll[,1], to=c(0,10))
  ll[,2] <- scales::rescale(ll[,2], to=c(0,10))
  
  
  xmin <- min(ll[,1]) -1
  xmax <- max(ll[,1]) +1
  ymin <- min(ll[,2]) -1
  ymax <- max(ll[,2]) +1
  # broaden the window
  wd <- (xmax - xmin) / 8
  ht <- (ymax - ymin) / 8
  
  ww <- owin(xrange = c(xmin,xmax), yrange = c(ymin, ymax))
  
  #put coords of layout into vertices argument 
  V(gcube)$xcor <- ll[,1]
  V(gcube)$ycor <- ll[,2]

  
  # iterate on edges of the graphe
  #data frame of X0 x and y coords  and X1 x and y coords
  tseg <-NULL
  for (e in E(gcube)){
    e1 <- V(gcube)[ends(gcube, e)[1]]
    e2 <- V(gcube)[ends(gcube, e)[2]]
    tseg <- rbind(tseg,data.frame(e1$xcor,e1$ycor,e2$xcor, e2$ycor))
  }  
  # convert into psp 
  seg <- psp(tseg$e1.xcor, tseg$e1.ycor, tseg$e2.xcor, tseg$e2.ycor, window = ww)
  return(seg)
}

Lambda <- 3
sublinesqty <-2
nblvl <- 8
mycol <-rev(brewer.pal(nblvl, 'BrBG'))


#liste des noms de graphes cools
#Cubical , Chvatal, 

g <- make_graph("Zachary")
#layout plusieurs fois car pas toujours  determenisite
lay <- layout_nicely(g)
plot(g, layout=lay)

seg <- make_seg_from_graph(g, lay)
affiche(seg,seg$window)


# Powwer law deg PL 
g<- NULL
nb <- 80
degs <- sample(1:nb, nb, replace=TRUE, prob=(1:nb)^-2)
if (sum(degs) %% 2 != 0) { degs[1] <- degs[1] + 1 }
g <- sample_degseq(degs, method="vl")

g <- sample_gnp(n = 24, p=0.1)


lay <- layout_nicely(g)
plot(g, layout=lay)
seg <- make_seg_from_graph(g, lay)

Lambda <- 3
sublinesqty <-6
nblvl <- 8
mycol <-rev(brewer.pal(nblvl, 'YlGnBu'))

affiche(seg,seg$window)









affiche_ext <-function(ml){
  main.lines<- ml
  # Sample points proportionally to the distance to the lines
  dist.from.line <- distmap(main.lines)
  # exponential transformation of the distance
  dist.from.line.exp <- Lambda*exp(-Lambda*dist.from.line)
  samp.pois <- rpoispp(dist.from.line.exp*sublinesqty)
  mask <- owin(c(1,2),c(1,2))
  samp.pois <- subset(samp.pois, !(x>1 & x<2 & y>1 & y<2 ), drop = T)
  #samp.pois <- runifpoint(dist.from.line.exp*sublinesqty)
  # Project to the lines using project2segment(b, a)
  Xproj <- project2segment(samp.pois, main.lines)$Xproj
  sub.lines <- psp(samp.pois$x, samp.pois$y, Xproj$x, Xproj$y, window=W23)
  # Connect with the lines
  all.lines <- append.psp(main.lines, sub.lines)
  dist.from.all.lines <- distmap(all.lines, dimyx=c(1200, 800))
    #png(filename = "Figure_1B.png", width=6000, height=4000, res=400)
  par(mai=c(0,0,0,0))
  dist.from.all.lines.exp <- exp(-Lambda*dist.from.all.lines)
  plot(dist.from.all.lines.exp, col=mycol, 
       legend=FALSE, main="", box=FALSE, ribbon=FALSE)
  contour(dist.from.all.lines.exp, add=TRUE, lwd=1, drawlabels=F, nlevels=nblvl)
  contour(dist.from.all.lines, add=TRUE, lwd=1, drawlabels=F, levels = 0.3)
  plot(sub.lines, add=T,  col="white", lwd=1.3)
  plot(main.lines, add=T, col="white", lwd=3)

  }



affiche <-function(ml,w){
  main.lines<- ml
  # Sample points proportionally to the distance to the lines
  dist.from.line <- distmap(main.lines)
  # exponential transformation of the distance
  dist.from.line.exp <- Lambda*exp(-Lambda*dist.from.line)
  samp.pois <- rpoispp(dist.from.line.exp*sublinesqty)
  #samp.pois <- runifpoint(dist.from.line.exp*sublinesqty)
  # Project to the lines using project2segment(b, a)
  Xproj <- project2segment(samp.pois, main.lines)$Xproj
  sub.lines <- psp(samp.pois$x, samp.pois$y, Xproj$x, Xproj$y, window=w)
  
  # Connect with the lines
 all.lines <- append.psp(main.lines, sub.lines)
  dist.from.all.lines <- distmap(all.lines, dimyx=c(1200, 800))
  
  #png(filename = "Figure_1B.png", width=6000, height=4000, res=400)
  par(mai=c(0,0,0,0))
  dist.from.all.lines.exp <- exp(-Lambda*dist.from.all.lines)
  plot(dist.from.all.lines.exp, col=mycol, 
       legend=FALSE, main="", box=FALSE, ribbon=FALSE)
  contour(dist.from.all.lines.exp, add=TRUE, lwd=1, drawlabels=F, nlevels=nblvl)
  contour(dist.from.all.lines, add=TRUE, lwd=1, drawlabels=F, levels = 0.3)
  plot(sub.lines, add=T,  col="white", lwd=1)
  plot(main.lines, add=T, col="white", lwd=2)
}






main.lines <- psp(seq(from=0, to=3 , length.out = 5),seq(from=0, to=2, length.out = 5),seq(from=0, to=3, length.out = 5),seq(from=0, to=2, length.out = 5), window=W23)


affiche(main.lines,W23)

main.lines <- psp(1.25,1,1.75,1, window=W23)


############# code pour siplifier shpp en virant les petites surfaces
area <- lapply(map@polygons, function(x) sapply(x@Polygons, function(y) y@area))
quantile(unlist(area))


mainPolys <- lapply(area, function(x) which(x > 150))
#map chargée avec readShapePoly
CCBound <- map
CCBound@data <- CCBound@data[-c(1:2),]
CCBound@polygons <- CCBound@polygons[-c(1:2)]
CCBound@plotOrder <- 1:length(CCBound@polygons)
mainPolys <- mainPolys[-c(1:2)] 
for(i in 1:length(mainPolys)){
  if(length(mainPolys[[i]]) >= 1 && mainPolys[[i]][1] >= 1){
    CCBound@polygons[[i]]@Polygons <- CCBound@polygons[[i]]@Polygons[mainPolys[[i]]]
    CCBound@polygons[[i]]@plotOrder <- 1:length(CCBound@polygons[[i]]@Polygons)
  }
}

plot(CCBound)
############# code pour siplifier shpp en virant les petites surfaces


###############################@@
# piscine
####################################@@


set.seed(16211)
#rc <- rpoispp(function(x,y){50 *log(x)*  exp(-3*(max(y)-y))}, 10, win=W23)
rc <- runifpoint(6, win=W23)
rcdist <- (distmap(rc, dimyx=c(2000, 2000)))

plot(rc)
plot(rcdist)

Lambda <- 0.2
rcdist.exp <- Lambda*exp(-Lambda*rcdist)
plot(rcdist.exp, legend=FALSE, main="", frame=FALSE, box=FALSE, ribbon=FALSE, col=mycol)

plot(rcdist.exp)
rc2 <- rpoispp(1/rcdist.exp*6)
rc3 <- rpoispp(1/rcdist.exp*2)

#rc2 <- rpoispp(rcdist.exp)
rcd <- dirichlet(rc2)
rcd2 <- dirichlet(rc3)

plot(rcd)
plot(rcd2)

nblvl <- 120
mycol  <- colorRampPalette(brewer.pal(8,"Blues"))(70)
wdfond = 3
wdtop = 2
wdtop2= 3

colfond= rgb(t(col2rgb("#ffffdd")/255), alpha = 0.15)
coltop= rgb(t(col2rgb("white")/255), alpha = 0.2)
coltop2= rgb(t(col2rgb("white")/255), alpha = 0.1)
colcontour = rgb(t(col2rgb("#ccccff")/255), alpha = 0.01)
#png(filename = "piscine2.png", width=8000, height=8000, res=800)
par(mai=c(0,0,0,0))
plot(rcdist.exp, legend=FALSE, main="", frame=FALSE, box=FALSE, ribbon=FALSE, col=mycol)
plot(rcd, add=T, col=colfond, lwd=wdfond)
contour(rcdist.exp, add=T, col=colcontour,lwd=wdfond, drawlabels=F, nlevels=nblvl)
plot(rcd, add=T, col=colfond, lwd=wdtop )
plot(rcd2, add=T, col=coltop2, lwd=wdtop2 )


dev.off()



#########
#spyrographe ou presque 
#########
par(mar = c(1, 1, 1, 1), bg="violetred4")
circlize::chordDiagram(matrix(1,15,15)-diag(15),
                       row.col="white",
                       column.col="yellow",
                       symmetric = F,
                       transparency = 0.5,
                       annotationTrack = NULL)

str(matrix(1,20,20))
str(diag(20))
