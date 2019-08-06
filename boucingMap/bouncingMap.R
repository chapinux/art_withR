library(dplyr)
library(rayshader)
library(png)
library(raster)
library(sf)
library(viridis)
library(tuneR)
library(signal)
library(rgl)
library(cartography)

#shp file with geometries
neufTroisSHP <- st_read("~/art_withR/boucingMap/neuftrois.shp")


neufTroisSHP <-  st_simplify(neufTroisSHP, preserveTopology = T,dTolerance = 50)
#st_write(neufTroisSHP,"~/art_withR/boucingMap/neufTrois_simple.shp")

#version rapide du plot
# par(mar=c(0,0,0,0))
 plot(st_geometry(neufTroisSHP), border="white", bgc="black")


#elevation avec l'intensité d'un sample 
# nbIris <- length(neufTroisSHP$OBJECTID)
#614 iris 


sampleLength <-  68
soundfile <-   file.path("~/art_withR/boucingMap/introlongue_niceCut.wav")
intro <-  readWave(soundfile, from = 0, to = sampleLength , units="seconds")
# writeWave(intro, "introlongue.wav")


#on veut une visu toutes les 1/10 secondes et que 1 s c'est 44100 samples, 
#ça fait des fenètres de 4410 samples

frames_per_s <-12 
sampRate <-  intro@samp.rate
windowsSize <-  sampRate / frames_per_s

#spectre de l'intro
fftIntro <-  specgram(intro@left, n=windowsSize, Fs = intro@samp.rate)
#retire la partie complexe / phase
fftIntro$S <- abs(fftIntro$S)


#nb frequences  doit être >= au nb IRIS pour que tout le monde ait qqch 
#length(fftIntro$f)




#get rid of useless info in shp 
neufTroisSHP$M2_IP <-  NULL
neufTroisSHP$M2_EMP <-  NULL
neufTroisSHP$M2_POP <-  NULL
neufTroisSHP$C_CAINSEE <-  NULL
neufTroisSHP$N_QU <-  NULL
neufTroisSHP$N_SQ_IR <-  NULL
neufTroisSHP$C_IR <-  NULL
neufTroisSHP$C_TYPEIR <-  NULL
neufTroisSHP$L_IR <-  NULL


# sort geometries (IRIS)  by size
neuftroisTri <- neufTroisSHP %>% arrange(desc(SHAPE_Area))


# number of bands of frequencies
nb_bandes <-  31
#each geometry recieve a band number
neuftroisTri$bande <-  (neuftroisTri$SHAPE_Area %>% ntile(nb_bandes) )



#seems to be traditional values of EQ freq : double each band 
 limites_de_bandesEQ<-c(0, 20,25,31.5,40,50,63,80,100,125,160,200,250,315,400,500,630,800,1000,1250,1600,2000,2500,3150,4000,5000,6300,8000,10000,12500,16000)
 
# 
 pEQ <-  limites_de_bandesEQ / max(fftIntro$f)
 qq <-  quantile(fftIntro$f, probs = pEQ)


#split the frequencies of the FFT by the desired number of frequency bands 
bandeslabels <-  (nb_bandes-1):1
 fftIntro$bande <-  cut(fftIntro$f, qq, include.lowest=TRUE, labels = bandeslabels  )


library(reshape2)
longfft <-  melt(fftIntro$S, varnames = c("f_index","t_index") , value.name = "intensite")

longfft$bande <-(cut(longfft$f, nb_bandes, include.lowest=TRUE, labels=nb_bandes:1 ))
#modify type of factor cf ?factor pour le caster en numeric 
longfft$bande <- as.numeric(as.character(longfft$bande))


#normalize by 4 parts quart so the bass isnt the only high values 
longfft$quart <-  cut(longfft$intensite,4, labels=FALSE)
# 4 maximum intensities 
maximumIntensityByQuarts <-  longfft %>% group_by(quart) %>% summarise(quartMax=max(intensite))

#each intensity gets its quart maximal value
longfft <-   inner_join(longfft, maximumIntensityByQuarts, by= "quart")

#normalize each intensity by it quart maximum value
longfft <- longfft %>%  mutate(intensite=intensite/quartMax )



#compute intensity mean value in each freq band, to affect to each geometry
mean_ByBand_By_t <-   longfft %>% group_by(bande, t_index) %>% summarise(valAgg = mean(intensite))
cube93 <-  left_join(neuftroisTri, mean_ByBand_By_t)


 sampleID <-  1400
 display_by_Sample(sampleID)


display_by_Sample <- function(sampleID){
 
  par(mar=c(0,0,0,0))
  
  tutu <-cube93[cube93$t_index==sampleID,"valAgg"]
  plot(tutu["valAgg"],border=NA,  bgc="black", pal=viridis, key.pos=NULL, main = NULL )
}
library(tiff)

nb_frames <-  sampleLength * frames_per_s
#  display_by_Sample(400, neuftroisTri,fftIntro)
#  display_by_Sample(600, neuftroisTri,fftIntro)
#  display_by_Sample(800, neuftroisTri,fftIntro)

imagefifi <- readPNG("art_withR/boucingMap/neufTroisOSM.png")


#remove  borders
imagefifi <- imagefifi[21:(nrow(imagefifi)-20),21:(ncol(imagefifi)-20),]

neufTroisOSMraster <- imagefifi




for (sampleID in 2 : nb_frames){
  cat(sampleID,"\n")	
  if (sampleID %% 5 == 0 ){
    gc()
  }
  
  fifi <- tempfile()
  png(fifi, width = 800, height = 800)
  
  display_by_Sample(sampleID)
  dev.off()
  
  #layer elevation
  localtif <-  raster::raster(fifi)
  
  # #layer couleur
  # fifi <- tempfile()
  # png(fifi)
  # display_by_Sample_Othecolor(sampleID)
  # dev.off()
  # imagefifi <-  readPNG(fifi)
  # dim(imagefifi)
  # imagefifi <- readPNG("art_withR/boucingMap/neufTroisOSM.png") 
  # #imagefifiOSM <-  readTIFF("art_withR/boucingMap/fondCarteOSM/fondCarteOSM.tif")
  # 
  # 
  # #remove  borders  
  # imagefifi <- imagefifi[21:(nrow(imagefifi)-20),21:(ncol(imagefifi)-20),]
  # 
  # #add alpha value
  # imagefifi.alpah <- array(NA, dim=c(nrow(imagefifi), ncol(imagefifi), 4))
  # imagefifi.alpah[,,-4] <- imagefifi    
  # #imagefifi.alpah[,,4] <- 0.9
  # imagefifi[,,4] <- 0.9
  # grid::grid.raster(imagefifi)
  # 
  #create elevation matrix from the localtif created from fifi
  elmat <- matrix(raster::extract(localtif,raster::extent(localtif),buffer=1000),
                  nrow=ncol(localtif),ncol=nrow(localtif))
  
  #remove borders from the elevation matrix because they are white so they are interpreted as high elevation values
  elmat <- elmat[21:(nrow(elmat)-20),21:(ncol(elmat)-20)]

  elmat %>%
    sphere_shade(progbar = FALSE,texture = "bw") %>%
    add_overlay(neufTroisOSMraster) %>% 
    add_shadow(ray_shade(elmat,zscale=3000,maxsearch = 300,progbar = FALSE),0.7) %>%
    plot_3d(elmat, zscale = 1, fov=70, theta=70+sampleID*0.5, phi=35, windowsize=c(100,100), zoom=0.65) 
  # Render Viewport to File
  rgl.snapshot(paste0("~/art_withR/boucingMap/frames/frame",sprintf("%05d",sampleID),".png"), fmt = "png", top = F)
  rgl.clear()
}
rgl.close()




#creer la vidéo r le rate
#ffmpeg -r 78 -i frame%05d.png -c:v libx264 out.mp4

  
#ajouter le son sur le flux video 
#-r rate 
# -i input files and sound 
#ffmpeg -r 60 -i out.mp4 -i intro.wav -c:v copy -c:a aac out_et_son.mp4


