# devtools::install_github("rstudio/leaflet")
# devtools::install_github("hadley/ggplot2")
# devtools::install_github("hadley/stringr")
# devtools::install_github("dkahle/ggmap")


# 
library(leaflet)
library(foreign)
library(ggmap)
library(stringr)
library(ggplot2)

Data <- read.spss("CZ.sav",to.data.frame = TRUE)

Data <- Data[-1, ]
names(Data)

Data %>% 
  select(is.factor)
