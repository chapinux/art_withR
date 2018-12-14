
library(viridis)
library(RColorBrewer)
library(ggplot2)
library(dplyr)


df.data <- tibble(x = runif(20000) 
                  ,y = rnorm(20000) * 0.2
)



ggplot(df.data, aes(x = x, y = y)) +
  geom_point()+
  ylim(c(-4,4))



gbleu <- ggplot(df.data, aes(x = x, y = y)) +
  stat_density2d(aes(fill = ..density..), geom = 'tile', contour = F, show.legend = F)+
  theme_void()
gbleu

display.brewer.all()


ggplot(df.data, aes(x = x, y = y)) +
  stat_density2d(aes(fill = ..density.. ), geom = 'tile', contour = F) +
  scale_fill_distiller(palette = 'YlOrBr')

ggplot(df.data, aes(x = x, y = y)) +
  stat_density2d(aes(fill = ..density..), geom = 'tile', contour = F) +
  scale_fill_distiller(palette = 'RdYlBu')+
  ylim(c(-4,4))

ggplot(df.data, aes(x = x, y = y)) +
  stat_density2d(aes(fill = ..density..), geom = 'tile', contour = F) +
  scale_fill_viridis()








