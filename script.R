## download the file
# URL <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
# download.file(URL, destfile = "repdata-data-StormData.csv.bz2", method = "curl")

### this takes some time
########################
# library(reshape)
# library(plyr)
# library(ggplot2)
### data processing
dat <- read.table('repdata-data-StormData.csv.bz2', header = T, sep = ",", stringsAsFactors=FALSE)
dim(dat)
str(dat)
length(unique(dat$EVTYPE))
### clean the data

# Convert all names to lowercase
names(dat) <- tolower(names(dat))
# Convert evtype to lowercase
dat$evtype <- tolower(dat$evtype)
# Recode major categories of events into fewer groups
dat$evtype <- gsub("^.*(fire).*$", "fire", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(flood).*$", "flood", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(rain).*$", "rain", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(precip).*$", "precipitation", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(shower).*$", "rain", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(hurricane).*$", "hurricane", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(heat).*$", "heat", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(hot).*$", "heat", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(warm).*$", "heat", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(wind).*$", "wind", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(snow).*$", "snow", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(blizzard).*$", "blizzard", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(hail).*$", "hail", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(ice).*$", "ice", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(icy).*$", "ice", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(sleet).*$", "sleet", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(freez).*$", "freeze", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(frost).*$", "frost", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(thunderstorm).*$", "thunderstorm", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(torn).*$", "tornado", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(volcan).*$", "volcanic", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(wint).*$", "winter weather", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(cold).*$", "cold", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(cool).*$", "cold", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(dry).*$", "dry", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(mud).*$", "mudslide", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(spout).*$", "waterspout", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(surf).*$", "surf", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(waves).*$", "surf", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(swell).*$", "surf", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(lightning).*$", "lightning", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(fog).*$", "fog", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(dust).*$", "dust", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(wet).*$", "wet", dat$evtype, ignore.case=TRUE)
dat$evtype <- gsub("^.*(summary).*$", NA, dat$evtype, ignore.case=TRUE)
### after cleaning the same event
length(unique(dat$evtype))


### Top ten health threaten events ranked by total fataliteis and injuries
#fatalities <- aggregate(dat$FATALITIES, list(dat$EVTYPE), sum)
#injuries <- aggregate(dat$INJURIES, list(dat$EVTYPE), sum)
library(plyr)
fatalities <- ddply(dat, "evtype", summarise, fatalilities.No. = sum(fatalities) )
dim(fatalities)
injuries <- ddply(dat, "evtype", summarise, injuries.No. = sum(injuries) )
dim(injuries)
health <- merge(fatalities, injuries, by="evtype")
### create the total of fatalities and health
health[,4] <- health[,2] + health[,3]
colnames(health) <- c("type", "fatalities", "injuries", "total")
### take the top 10 health threat event
Top10 <- health[order(health$total, decreasing=TRUE),][1:10,]
Top10

### start plot
library(reshape2)
Top10 <- melt(Top10, id.vars="type")
library(ggplot2)
ggplot(Top10, aes(x = reorder(type, -value), y = value)) + 
  geom_bar(stat = "identity", aes(fill = variable), position = "dodge") + 
  scale_y_sqrt("Number of Cases") + xlab("Event Type") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ggtitle("Top 10 Health Threat Events")



### The most harmful events for economy, first make the unit same
unique(dat$propdmgexp)
unique(dat$cropdmgexp)
economy <- dat[ , c("evtype", "propdmg", "propdmgexp", "cropdmg", "cropdmgexp")]
### make the property unit the same
index1 <- which(economy$propdmgexp %in% c("K","k"))
index2 <- which(economy$propdmgexp %in% c("M","m"))
index3 <- which(economy$propdmgexp %in% c("B","b"))
economy[,"propdmg"][index1] <- economy[,"propdmg"][index1]*(10^3)
economy[,"propdmg"][index2] <- economy[,"propdmg"][index2]*(10^6)
economy[,"propdmg"][index3] <- economy[,"propdmg"][index3]*(10^9)
#### same for the crop
index4 <- which(economy$cropdmgexp %in% c("K","k"))
index5 <- which(economy$cropdmgexp %in% c("M","m"))
index6 <- which(economy$cropdmgexp %in% c("B","b"))
economy[,"cropdmg"][index4] <- economy[,"cropdmg"][index4]*(10^3)
economy[,"cropdmg"][index5] <- economy[,"cropdmg"][index5]*(10^6)
economy[,"cropdmg"][index6] <- economy[,"cropdmg"][index6]*(10^9)

### Then we find top 10 threat for property and crop
property <- ddply(economy, "evtype", summarise, property = sum(propdmg) )
crop <- ddply(economy, "evtype", summarise, crop = sum(cropdmg) )
### take the top 10 health threat event
property.Top10 <- property[order(property$property, decreasing = TRUE),   ][1:10, ]
crop.Top10 <- crop[order(crop$crop, decreasing = TRUE),   ][1:10, ]

library(gridExtra)
plot1 <- ggplot(property.Top10, aes(x = reorder(evtype, -property), y = property)) + 
  geom_bar(stat = "identity", position = "dodge", color = "indianred1", fill = "lightblue") + 
  scale_y_sqrt("Property Damage(US Dollar)") + xlab("Event Type") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ggtitle("Top 10 Property Threat Events")

plot2 <- ggplot(crop.Top10, aes(x = reorder(evtype, -crop), y = crop)) + 
  geom_bar(stat = "identity", position = "dodge", color = "indianred1", fill = "lightgreen") + 
  scale_y_sqrt("Property Damage(US Dollar)") + xlab("Event Type") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  ggtitle("Top 10 Crop Threat Events")

grid.arrange(plot1, plot2, ncol = 2)



#### 3.3 Which part of US suffer most from the weather disaster?
US.state <- dat[,c("state", "fatalities", "injuries")]
### columnwise addition for sub-total 
US.state$subtotal <- US.state$fatalities + US.state$injuries
### find the total event for eah state
US.state_total <- ddply(US.state, 'state', summarise, total = sum(subtotal))
head(US.state_total)
#### start plot
ggplot(US.state_total, aes(x = reorder(state, total), y = total)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_sqrt("Number of health cases") + xlab("State") +
  theme(axis.text.x = element_text(hjust=1)) + 
  ggtitle("Number of Health Case in State") +
  coord_flip() 

