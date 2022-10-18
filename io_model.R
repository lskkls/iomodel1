install.packages("dplyr")           # for data prep
install.packages("data.table")      # for data prep
install.packages("caTools")         # for sampling
install.packages("caret")           # for model
library("dplyr")
library("data.table")
library("caTools")
library("caret")

#load data files
setwd( "R" )
tagged_data     <- read.csv("bla.csv", header=FALSE)
untagged_data   <- read.csv("bla1.csv", header=FALSE)

#preparing untagged data.
names(untagged_data)[1] <- "localityid"
names(untagged_data)[2] <- "nutsid"
names(untagged_data)[3] <- "nutslevel"
names(untagged_data)[4] <- "name"
names(untagged_data)[5] <- "area"
names(untagged_data)[6] <- "populationDensity"
names(untagged_data)[7] <- "latitude"
names(untagged_data)[8] <- "longitude"
names(untagged_data)[9] <- "acommodations"
names(untagged_data)[10] <- "coastalCategory"
names(untagged_data)[11] <- "borderCategory"
names(untagged_data)[12] <- "isIsland"
names(untagged_data)[13] <- "islandCountryCode"
names(untagged_data)[14] <- "metroCategory"
names(untagged_data)[15] <- "metroCode"
names(untagged_data)[16] <- "metroLabel"
names(untagged_data)[17] <- "mountainCategory"
names(untagged_data)[18] <- "urbanRuralRemoteCategory"
names(untagged_data)[19] <- "districtId"

names <- c(10,11,12,14,17,18)
untagged_data[,names] <- lapply(untagged_data[,names] , factor)
untagged_data[ ,c(2,3,4,13,15,16,19)] <- list(NULL)

#preparing tagged data.
names(tagged_data)[1] <- "id"
names(tagged_data)[2] <- "tag"
names(tagged_data)[3] <- "createdBy"
names(tagged_data)[4] <- "localityid"

tagged_data <- tagged_data[!(tagged_data$createdBy=="DEBUG"),]
#tagged_data <- tagged_data[!(tagged_data$createdBy=="Boaz"),]        Enable this line to remove outlier annotation
tagged_data$id <- NULL

#making a df of the most common winter tags per locality.
winter_tags <- subset(tagged_data, tag %in% c('winterWonderland','frisky','escapeTheCold'))
winter_tags$tag <- as.factor(winter_tags$tag)
winter_tags$createdBy <- as.factor(winter_tags$createdBy)

winter_tags  <- winter_tags %>%
          group_by(localityid, tag) %>%
            mutate(count = n_distinct(createdBy))
winter_tags <- winter_tags[,c(3,1,4)]

winter_tags <- as.data.table(winter_tags)
winter_tags <- winter_tags[winter_tags[, .I[which.max(count)], by=localityid]$V1]
winter_tags$count <- NULL
winter_tags$tag <- as.character(winter_tags$tag)
winter_tags$tag <- as.factor(winter_tags$tag)

#making a df of the most common summer tags per locality.
summer_tags <- subset(tagged_data, tag %in% c('escapeTheHeat','mildSummer','scorchingHeat', 'hotAndHumid'))

summer_tags$tag <- as.factor(summer_tags$tag)
summer_tags$createdBy <- as.factor(summer_tags$createdBy)

summer_tags  <- summer_tags %>%
  group_by(localityid, tag) %>%
  mutate(count = n_distinct(createdBy))
summer_tags <- summer_tags[,c(3,1,4)]

summer_tags <- as.data.table(summer_tags)
summer_tags <- summer_tags[summer_tags[, .I[which.max(count)], by=localityid]$V1]
summer_tags$count <- NULL
summer_tags$tag <- as.character(summer_tags$tag)
summer_tags$tag <- as.factor(summer_tags$tag)

#merge tags with locality data
summer_data <- merge(summer_tags, untagged_data, by="localityid")
winter_data <- merge(winter_tags, untagged_data, by="localityid")

#splitting summer and winter into test and training set.
set.seed(101) 
spl_s = sample.split(summer_data$tag, SplitRatio = 0.7)
train_s = subset(summer_data, spl_s==TRUE)
test_s = subset(summer_data, spl_s==FALSE)

spl_w = sample.split(winter_data$tag, SplitRatio = 0.7)
train_w = subset(winter_data, spl_w==TRUE)
test_w = subset(winter_data, spl_w==FALSE)

#regression model summer
train_s <- train_s[, -c(3,4,7,13)] # delete columns 5 through 7

train_control = trainControl(method = "cv", number = 5, search = "grid")

multi_classification_Tree_Grid_s =  expand.grid(maxdepth = c(1,3,5,7,9))

set.seed(50)
model_s = train(tag~., data = train_s, method = "rpart2", trControl = train_control, tuneGrid = multi_classification_Tree_Grid_s)

pred_s = predict(model_s, test_s)
confusionMatrix(data = pred_s, test_s$tag)

#regression model winter
train_w <- train_w[, -c(3,4,7,13)] # delete columns 5 through 7

train_control_w = trainControl(method = "cv", number = 5, search = "grid")

multi_classification_Tree_Grid_w =  expand.grid(maxdepth = c(1,3,5,7,9))

set.seed(50)
model_w = train(tag~., data = train_w, method = "rpart2", trControl = train_control, tuneGrid = multi_classification_Tree_Grid_w)

pred_w = predict(model_w, test_w)
confusionMatrix(data = pred_w, test_w$tag)

#create tags for dataset by applying model
untagged_data <- untagged_data[, -c(3,6,12)]
final_tags <- untagged_data
final_tags$tag_s <- predict(model_s, newdata = untagged_data)
final_tags$tag_w <- predict(model_w, newdata = untagged_data)

#creating database-friendly format csv
export_tags <- final_tags[,c(10,1)]
names(export_tags)[names(export_tags) == 'tag_s'] <- 'tag'

export_tags_w <- final_tags[,c(11,1)]
names(export_tags_w)[names(export_tags_w) == 'tag_w'] <- 'tag'

export <- rbind(export_tags, export_tags_w)
export <- export %>% mutate(id = row_number())
export$createdBy <- "placeholder"
export <- export[,c(3,1,4,2)]
export <- export[sample(nrow(export)),]

write.table(export, "tagged1.csv", row.names=F, col.names=F, sep=",")
