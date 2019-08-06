library(wordcloud)
library(tidyverse)

d <- tibble(word=character(), freq= numeric())

d <- d %>%  add_row (word = "Modeling", freq= 120)
d <- d %>%  add_row (word = "Simulation", freq= 120)
d <- d %>%  add_row (word = "Statistics", freq= 13)
d <- d %>%  add_row (word = "Data\nanalysis", freq= 11)
d <- d %>%  add_row (word = "Urban\nsprawl", freq= 11)
d <- d %>%  add_row (word = "Urban heat\nislands ", freq= 11)
d <- d %>%  add_row (word = "GIS", freq= 5)
d <- d %>%  add_row (word = "PLU", freq= 60)
d <- d %>%  add_row (word = "Geographic\nInformation", freq= 60)
d <- d %>%  add_row (word = "Energetic\nperformance", freq= 30)
d <- d %>%  add_row (word = "Buildings", freq= 40)
d <- d %>%  add_row (word = "ABM", freq= 80)
d <- d %>%  add_row (word = "networks", freq= 20)
d <- d %>%  add_row (word = "Simulation Model\n Analysis", freq= 100)
d <- d %>%  add_row (word = "Sociology of\nOrganizations", freq= 20)
d <- d %>%  add_row (word = "Spatial\nanalysis", freq= 50)
d <- d %>%  add_row (word = "Social\nnetworks", freq= 10)
d <- d %>%  add_row (word = "Model\nvalidation", freq= 50)
d <- d %>%  add_row (word = "Sensitivity\nAnalysis", freq= 20)
d <- d %>%  add_row (word = "Netlogo", freq= 10)
d <- d %>%  add_row (word = "OpenMOLE", freq= 60)
d <- d %>%  add_row (word = "art with R ", freq= 5)
d <- d %>%  add_row (word = "Dataviz", freq= 10)
d <- d %>%  add_row (word = "Systems of \n Cities", freq= 10)
d <- d %>%  add_row (word = "Geography", freq= 44)



d$freq <-  1 / d$freq 


names(d) <- c("word", "freq")
set.seed(1234)
wordcloud(words = d$word, freq =  d$freq,  
          max.words=200, random.order=TRUE, rot.per=0.15, random.color = T,
          colors=brewer.pal(8, "Dark2"),scale = c(0.1,2.5))

dev.off()

