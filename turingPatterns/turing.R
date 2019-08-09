library(dplyr)
library(icosa)

hexG <-  hexagrid(12)
plot3d(hexG, guides = F, arcs = T)

#objet sp à plat

#spHexG <-  newsp(hexG)
#plot(spHexG)

#library(sf)
#sf_hexgrid <-  st_as_sf(spHexG@sp)
#plot(sf_hexgrid)
#dev.off()

# library(rgdal)
# file <-
#   system.file("extdata", "land_polygons_z3.shx", package = "icosa")
# worldBorders <- readOGR(file, "land_polygons_z3")
# 
# plot(sf_hexgrid)
# 
# #projection dans le système de la grille
# crsSfHex <-  st_crs(sf_hexgrid)
# world <-  spTransform(worldBorders, spHexG@proj4string)
# plot(spHexG)
# plot(world,col="orange", add=T)
# 
# plot(spHexG)
# plot(world,col="orange", add=T)
# 
# facesOccupied <-  occupied(hexG, world)
# 
# plot3d(hexG,
#        guides = F,
#        lwd = 0.1,
#        col = "grey")
# faces3d(facesOccupied, col = "orange")



# ###########""
# 
# 
# 
# v55 <- hexG@faces["F55", ]
# oldVertics <- hexG@vertices[v55, ]
# 
# extruz <- CarToPol(hexG@vertices[v55, ])
# extruz
# newradius <-  extruz[1, "rho"] * 1.1
# extruz[, "rho"] <- extruz[, "rho"] * 1.1
# extruz
# extruz <-  PolToCar(extruz, radius = newradius)
# oldVertics
# extruz
# 
# hexG@vertices[hexG@faces["F55", ], ] == oldVertics
# 
# hexG@vertices[v55, ] <- extruz
# 
# plot3d(hexG, guides = F)
# points3d(extruz,
#          col = "orange",
#          add = T,
#          cex = 5)
# 
# 
# # remplir une liste alternativement
# a <- paste0("a", seq(1:10))
# b <- paste0("b", seq(1:10))
# c(rbind(a, b))
# 
# 
# interleavePointsCoord <- function(lettreCoord, P1, P2) {
#   return(c(rbind(P1[, lettreCoord], P2[, lettreCoord])))
# }
# 
# 
# xx <- interleavePointsCoord("x", oldVertics, extruz)
# yy <- interleavePointsCoord("y", oldVertics, extruz)
# zz <- interleavePointsCoord("z", oldVertics, extruz)








displayExtrudedFace <- function(extrudedFace, centerFace, faceValue, mypalette = "green") {
    #hexagonal face = 3 quads between centerpoints and circoncference points
    
    xxxx <- c(extrudedFace[1:3, "x"], centerFace[, "x"])
    yyyy <- c(extrudedFace[1:3, "y"], centerFace[, "y"])
    zzzz <-  c(extrudedFace[1:3, "z"], centerFace[, "z"])
    
    xxxx2 <- c(extrudedFace[3:5, "x"], centerFace[, "x"])
    yyyy2 <- c(extrudedFace[3:5, "y"], centerFace[, "y"])
    zzzz2 <-  c(extrudedFace[3:5, "z"], centerFace[, "z"])
    
    xxxx3 <- c(extrudedFace[c(5, 6, 1), "x"], centerFace[, "x"])
    yyyy3 <- c(extrudedFace[c(5, 6, 1), "y"], centerFace[, "y"])
    zzzz3 <-  c(extrudedFace[c(5, 6, 1), "z"], centerFace[, "z"])
    
    quads3d(c(xxxx, xxxx2, xxxx3),
            c(yyyy, yyyy2, yyyy3),
            c(zzzz, zzzz2, zzzz3),
            col = c(mypalette))
}


#inverser les points sinon ça croise le polygione

#quads3d(face[,"x"], face[,"y"], face[,"z"], col="green")

displayExtrudedColonne <-
  function(oldVertics, extruz, mycolor = "green") {
    for (faceIterator in 1:6) {
      idx <- c(faceIterator, (faceIterator %% 6) + 1)
      faface <- rbind(oldVertics[idx, ], extruz[rev(idx), ])
      quads3d(faface[, "x"], faface[, "y"], faface[, "z"], col = mycolor, skipRedraw=T  )
    }
  }

refreshDisplay <- function(hg) {
   plot3d(hg,
          sphere=T,
          guides = F,
         lwd = 0.1)
}

extrude <- function(faceName, grid, extrusionPct) {
  faPts <- grid@faces[faceName, ]
  oldVertics <- grid@vertices[faPts, ]
  extrudedFace <- CarToPol(grid@vertices[faPts, ])
  newradius <-  extrudedFace[1, "rho"] * (1+ extrusionPct)
  extrudedFace[, "rho"] <- newradius
  extrudedFace <-  PolToCar(extrudedFace, radius = newradius)
  return(extrudedFace)
  }

extrudedFaceCenter <- function(faceName, hexGrid,pctExtrusion)
{
  centerOld <- hexGrid@faceCenters[faceName, ]
  centerNew <-  CarToPol(matrix(centerOld, nrow = 1))
  newRadius <- centerNew[, "rho"] * (1 +pctExtrusion)
  centerNew[, "rho"] <- newRadius
  centerNew <-  PolToCar(centerNew, radius = newRadius)
  return(centerNew)  
} 


verticesOfFace <- function(faceName, grid){
  faPts <- grid@faces[faceName, ]
  return(grid@vertices[faPts, ])
}


extrudeOneFace <-  function(faceName, extrudPct, verbose=FALSE){
  if(verbose){cat("extrusion de ", faceName, " de ",(extrudPct), "%\n")}
  if(!anyNA(grid@faces[faceName,]) && extrudPct){
  extFaCenter <-  extrudedFaceCenter(faceName, grid, extrudPct)
  extrudedFace <-  extrude(faceName, grid, extrudPct)
  displayExtrudedFace(extrudedFace , extFaCenter,extrudPct, mypalette)
  oldFace <-  verticesOfFace(faceName, grid)
  displayExtrudedColonne(oldFace,extrudedFace,mypalette )
  }
}


grid <-  hexagrid(18)


faces <-  facelayer(grid)
nouvellesVal <- rnorm(length(faces), 0, 1)
values(faces) <- nouvellesVal + abs(min(nouvellesVal))
library(RColorBrewer)
mypalette <-  brewer.pal(8, "Blues")




rgl.close()
par3d(windowRect=c(658,52,1920,1022))
plot3d(grid, guides=F)
rgl.clear(type = "lights")
light3d( theta = 40, phi=30)

#refreshDisplay(hexG)
#toutes les faces sauf la première qui n'est pas un hexagone
nbMax <- nrow(grid@faces)
listfaces <-  sample(c(rownames(grid@faces)), 800 , replace = F)



mapply(extrudeOneFace, listfaces , 
       extrudPct = faces@values * 0.15)






varyingTheta <- 0 
varyingPhi <- 0
fov = 180
for (f in listfaces){
  #cat(f ,"tutu\n")
  valeurExtrud <-  faces@values[which(faces@names == f )]
  #valeurExtrud <-  0.2
  extrudeOneFace(f, valeurExtrud*0.1)
  #varyingTheta <- varyingTheta + 0.2
  # varyingPhi <- varyingPhi + (90 / (length(listfaces) ))
  # cat(varyingPhi, " ")
  # fov <-  fov - 0.1
  # 
  # rgl.viewpoint(theta = varyingTheta, phi = varyingPhi)
}




