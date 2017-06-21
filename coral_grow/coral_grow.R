library(igraph)
library(network)
library(ggnet)
library(readr)
library(intergraph)
library(ggplot2)
library(sna)
library(scales)
library(dplyr)




ll<- read_delim("lgc.csv", delim=" ", col_types = list(col_character(), col_character()))
names(ll)<- c("O","D")
n <- 80000
ll <- head(ll,n)

g <- graph.data.frame(ll,directed = T)
V(g)$deg <- centr_degree(g,mode = "total")$res
#layout is fixed once and for all (very costly for large n)
coords <- layout_with_kk(g)

par(mar = rep(0, 4))
par(bg="grey10")
plot.igraph(g,  
            vertex.frame.color = "grey75",
            vertex.color=NA,
            vertex.size=rescale(V(g)$deg,to=c(0.8,20)), 
            vertex.label=NA,
            edge.color = "grey55" ,
            edge.arrow.mode="-",
            edge.width=  0.2,   
            layout=coords ,
            frame=T) 

cat("titi")



make_png_igraph(g,coords)







lay <- gplot.layout.fruchtermanreingold(g,layout.par = list(niter=500))



g %v% "x" <- lay[,1]
g %v% "y" <- lay[,2]



png("coral%d.png", width = 1000, height = 1000, res=92)
for ( i in 1:20){
  
  #compute degree
  cat(i, "/20\n")
  degs <- degree(g,gmode = "total")
  g <- g %s% which(degs>1)
  coords <- cbind(g %v% "x" ,  g %v% "y")
  make_png_sna(g,coords)
}

dev.off()



make_png_sna <- function(g,layout){
  p<- ggnet2(g, node.size = 2, 
             node.color = "grey75",
             size="deg", 
             edge.color = "grey55" ,  
             edge.size=  0.1,  shape=21, 
             mode=layout , layout.par = list(niter=1000)) 
  p <- p + ggplot2::theme(panel.background = element_rect(fill = "grey10"))
  p <- p +guides(size=FALSE)
  plot(p)
}


make_png_igraph <- function(g,layout){
  plot.igraph(g,  
             vertex.frame.color = "grey75",
             vertex.color=NA,
             vertex.size=rescale(V(g)$deg,c(2,20)), 
             vertex.shape="circle",
             vertex.label=NA,
             edge.color = "grey55" ,
             edge.arrow.size=NA,
             edge.width=  0.1,   
             layout=layout ,
             frame=T) 
 
}


#====================================================================
#smaller network
#====================================================================

lll<- read_delim("lgc.csv", delim=" ", col_types = list(col_character(), col_character()), col_names = c("O","D"))


ggg <- graph.data.frame(lll)
ggg <-g
delete()
# affect degree as attributes of vertices
V(ggg)$deg <- centr_degree(ggg,mode = "total")$res

#layout fixe
lay2 <- layout_with_fr(ggg)
V(ggg)$x <- lay2[,1]
V(ggg)$y <- lay2[,2]


png("coral%d.png", width = 1000, height = 1000, res=92)

for ( i in 1:20){
  #remove nodes of degree 1 and edges
  ggg2 <- delete_vertices(ggg , V(ggg)[V(ggg)$deg==1])
  coords <- cbind(ggg2$x, ggg2$y)
   make_png_igraph(ggg2,coords)
}

dev.off()


coords <- cbind(V(ggg)$x, V(ggg)$y)

plot.igraph(ggg,  
            vertex.frame.color = "grey75",
            vertex.color=NA,
            vertex.size=rescale(V(ggg)$deg,to=c(2,20)), 
            vertex.label=NA,
            edge.color = "grey55" ,
            edge.arrow.mode="-",
            #edge.width=  0.1,   
            layout=coords ,
            frame=T) 
