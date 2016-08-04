require(foreign)
require(ggmap)
require(stringr)

Data.Blank <- read.spss("Data/CZ.sav",to.data.frame=TRUE)
head(Data.Blank)

Data.Geo <- unique(Data.Blank[,c("Inst","Place")])
Data.Geo$Inst  <- str_trim(as.character(Data.Geo$Inst))
Data.Geo$Place <- str_trim(as.character(Data.Geo$Place))

Data.Geo$Loc <- paste(gsub(","," ",Data.Geo$Inst),gsub("Y","IJ",Data.Geo$Place),sep=", ")

Data.Geo$Long <- NA
Data.Geo$Lat  <- NA

# for(i in 1:nrow(Data.Geo)){
#   Data.Geo[i,c("Long","Lat")] <- geocode(Data.Geo[i,"Loc"])
# }
# 
# write.dta(Data.Geo,file = "Data/Data.Geo.dta")

Data.Geo <- read.spss("Data/Geo.Fill.sav",to.data.frame=TRUE)

Data.Geo$Inst  <- str_trim(as.character(Data.Geo$Inst))
Data.Geo$Place <- str_trim(as.character(Data.Geo$Place))

Data.Blank$Inst  <- str_trim(as.character(Data.Blank$Inst))
Data.Blank$Place <- str_trim(as.character(Data.Blank$Place))

Data.Comp <- merge(Data.Blank,Data.Geo,by="Inst",all.x=TRUE)

Data.Comp2 <- Data.Comp[,c("Inst","Place.x","Code","Product","Desc","Price","Long","Lat")]
names(Data.Comp2) <- c("Instelling","Plaats","Code","Product","Beschrijving","Prijs","Long","Lat")
save(Data.Comp2,file = "Data/Data.Comp2.Rdata")
