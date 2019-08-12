library(icosa)
library(dplyr)


hexG <-  hexagrid(2)
#objet sp à plat

spHexG <-  newsp(hexG)
plot3d(hexG)
plot(spHexG)
gridlabs(spHexG)
faces <-  spHexG@faces 

voisins <-  vicinity(spHexG, "F2")


#exemple : affectation de valeurs à certaines cases
fl <-  facelayer(spHexG)
fl@values[voisins] <-  1:5

plot(spHexG, col=c("red","blue"))







voisins
plot(spHexG)
plot(spHexG@sp[voisins], col="red", add=T)
gridlabs(spHexG)


## equation des reactifs U et V 

sizeGrid <- nrow(faces) 
dx <-  2.0 / sizeGrid
T <-  10
dt <-  0.001
nbiter <-  floor(T/dt)



#discrete laplacian on hexGrid 
#laplacian2D_hex <-  function(h,gridf){

  #cas général 
  

fl@values <-  rnorm(length(fl@values))


  h <- "F162"
  gridf <-  spHexG
  
  voisins_et_moi <- vicinity(gridf,h)
  voisins <-  voisins_et_moi[-which(voisins_et_moi==h)]
  #les 6 ou 5 voisins de la case courrante 
  neighVal <-  fl@values[voisins]
  selfVal <-  fl@values[h]
  res <- -6 * selfVal  + sum(neighVal)
  
  
  
res <-  res / dx**2    
 # return(res)
  
#}

dUdt <- function(U, V, a=2.5e-4, k=0.005){
  res <-  a * laplacian2D_hex(U) + U - U^3 -V + k
  return(res)
}


dVdt <- function(U,V, tau=0.1, a=2.5e-4, b=5e-3){
  res <-  (U + a -b * V) /tau
  return(res)
}


