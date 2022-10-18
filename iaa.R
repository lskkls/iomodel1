install.packages("reshape2")
install.packages("irr")
library("reshape2")
library("irr")

setwd( "R" )
annotation              <- read.csv("bla.csv", header=FALSE)
annotation              <- subset(annotation, V3!="DEBUG")
winter_annotation       <- subset(annotation, V2 %in% c('winterWonderland','frisky','escapeTheCold'))
summer_annotation       <- subset(annotation, V2 %in% c('escapeTheHeat','mildSummer','scorchingHeat', 'hotAndHumid'))

#winter
dfwinter                <- winter_annotation
dfwinter$V1             <- NULL
dfwinter                <- reshape(dfwinter, idvar = "V4", timevar = "V3", direction = "wide")
names                   <- c(1,2,3,4)
dfwinter[,names]        <- lapply(dfwinter[,names] , factor)
both                    <- union(levels(dfwinter$V2.Boaz), levels(dfwinter$V2.Halmar))
dfwinter$V2.Boaz        <- factor(dfwinter$V2.Boaz, levels=both)
dfwinter$V2.Nathan      <- factor(dfwinter$V2.Nathan, levels=both)
dfwinter$V2.Halmar      <- factor(dfwinter$V2.Halmar, levels=both)
dfwinter$V2.Jan         <- factor(dfwinter$V2.Jan, levels=both)

#summer
dfsummer                <- summer_annotation
dfsummer$V1             <- NULL
dfsummer                <- reshape(dfsummer, idvar = "V4", timevar = "V3", direction = "wide")
names                   <- c(1,2,3,4)
dfsummer[,names]        <- lapply(dfsummer[,names] , factor)
both                    <- union(levels(dfsummer$V2.Boaz), levels(dfsummer$V2.Halmar))
dfsummer$V2.Boaz        <- factor(dfsummer$V2.Boaz, levels=both)
dfsummer$V2.Nathan      <- factor(dfsummer$V2.Nathan, levels=both)
dfsummer$V2.Halmar      <- factor(dfsummer$V2.Halmar, levels=both)
dfsummer$V2.Jan         <- factor(dfsummer$V2.Jan, levels=both)

#Fleisch kappa winter
dfwinter$V4 <- NULL
kappam.fleiss(dfwinter, detail = TRUE)

#Fleisch kappa winter without Boaz
dfwinter$V4 <- NULL
dfwinter$V2.Boaz <- NULL
kappam.fleiss(dfwinter, detail = TRUE)

#Fleisch kappa summer
dfsummer$V4 <- NULL
kappam.fleiss(dfsummer, detail = TRUE)

#Fleisch kappa summer without Boaz
dfsummer$V4 <- NULL
dfsummer$V2.Boaz <- NULL
kappam.fleiss(dfsummer, detail = TRUE)



