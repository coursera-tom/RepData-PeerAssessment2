library(dplyr)
library(ggplot2)

# acquire data set
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipfile <- "data/StormData.csv.bz2"

if(!file.exists("data")) {
  dir.create("data")
}
if(!file.exists(zipfile)) {
  download.file(url, destfile=zipfile, method="curl")
}

raw_data <- read.csv(zipfile,  stringsAsFactors=T) 


## Clean up and prepare data
data <- raw_data[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP",
                    "CROPDMG", "CROPDMGEXP")]
summary(data)

## Handle PROPDMGEXP and CROPDMGEXP
# B=Billion (1e9), M=Million (1e6), K=Thousand (1e3), 0,1,2--8 = 1,10,100,...1e8
# "+","-","?" = unknown (NA)
#
data <- mutate(data, PD = ifelse(PROPDMGEXP=="B"|PROPDMGEXP=="b"|PROPDMGEXP=="9", 1e9*PROPDMG, 
                          ifelse(PROPDMGEXP=="M"|PROPDMGEXP=="m"|PROPDMGEXP=="6", 1e6*PROPDMG,
                          ifelse(PROPDMGEXP=="K"|PROPDMGEXP=="k"|PROPDMGEXP=="3", 1e3*PROPDMG, 
                          ifelse(PROPDMGEXP=="H"|PROPDMGEXP=="h"|PROPDMGEXP=="2", 1e2*PROPDMG,
                          ifelse(PROPDMGEXP=="0", PROPDMG, 
                          ifelse(PROPDMGEXP=="1", 10*PROPDMG, 
                          ifelse(PROPDMGEXP=="4", 1e4*PROPDMG, 
                          ifelse(PROPDMGEXP=="5", 1e5*PROPDMG, 
                          ifelse(PROPDMGEXP=="7", 1e7*PROPDMG,
                          ifelse(PROPDMGEXP=="8", 1e5*PROPDMG, 
                          ifelse(PROPDMGEXP %in% c("-","+","?"), NA, PROPDMG ))))))))))))

data <- mutate(data,CD = ifelse(CROPDMGEXP=="B"|CROPDMGEXP=="b"|CROPDMGEXP=="9", 1e9*CROPDMG, 
                         ifelse(CROPDMGEXP=="M"|CROPDMGEXP=="m"|CROPDMGEXP=="6", 1e6*CROPDMG,
                         ifelse(CROPDMGEXP=="K"|CROPDMGEXP=="k"|CROPDMGEXP=="3", 1e3*CROPDMG, 
                         ifelse(CROPDMGEXP=="H"|CROPDMGEXP=="h"|CROPDMGEXP=="2", 1e2*CROPDMG,
                         ifelse(CROPDMGEXP=="0", CROPDMG, 
                         ifelse(CROPDMGEXP=="1", 10*CROPDMG, 
                         ifelse(CROPDMGEXP=="4", 1e4*CROPDMG, 
                         ifelse(CROPDMGEXP=="5", 1e5*CROPDMG, 
                         ifelse(CROPDMGEXP=="7", 1e7*CROPDMG,
                         ifelse(CROPDMGEXP=="8", 1e5*CROPDMG, 
                         ifelse(CROPDMGEXP %in% c("-","+","?"), NA, CROPDMG ))))))))))))
 

## Find which types of events have most fatalities, injuries & property
totals <- summarise(group_by(data, EVTYPE), Total_Fatalities=sum(FATALITIES), 
                   Total_Injuries=sum(INJURIES), Total_Property_Damage=sum(PD),Total_Crop_Damage=sum(CD))

#
top_fatals <- arrange(select(totals,EVTYPE, Total_Fatalities) , desc(Total_Fatalities))

top_injuries <- arrange(select(totals,EVTYPE, Total_Injuries) , desc(Total_Injuries))

top_property_damages <- arrange(select(totals,EVTYPE, Total_Property_Damage) , desc(Total_Property_Damage))
top_crop_damages <- arrange(select(totals,EVTYPE, Total_Crop_Damage) , desc(Total_Crop_Damage))

# Plotting top 10 events

top10_fatals <- top_n(top_fatals, 10, Total_Fatalities)
qplot(data=top10_fatals, x=EVTYPE, y=Total_Fatalities, fill=EVTYPE,
      main="U.S. Top 10 Fatality Events", xlab="Event", ylab="Total Number of Fatalities") +
      geom_bar(stat="identity") + coord_flip() +
      theme(legend.title=element_blank())

top10_injuries <- top_n(top_injuries, 10, Total_Injuries)
qplot(data=top10_injuries, x=EVTYPE, y=Total_Injuries, fill=EVTYPE,
      main="U.S. Top 10 Injury Events", xlab="Event", ylab="Total Number of Injuries") +
      geom_bar(stat="identity") + coord_flip() +
      theme(legend.title=element_blank())

top10_property_damages <- top_n(top_property_damages, 10, Total_Property_Damage)
qplot(data=top10_property_damages, x=EVTYPE, y=Total_Property_Damage, fill=EVTYPE,
      main="U.S. Top 10 Property Damange Events", xlab="Event", ylab="Total $ of Damages") +
  geom_bar(stat="identity") + coord_flip() +
  theme(legend.title=element_blank())

top10_crop_damages <- top_n(top_crop_damages, 10, Total_Crop_Damage)
qplot(data=top10_crop_damages, x=EVTYPE, y=Total_Crop_Damage, fill=EVTYPE,
      main="U.S. Top 10 Crop Damange Events", xlab="Event", ylab="Total $ of Damages") +
  geom_bar(stat="identity") + coord_flip() +
  theme(legend.title=element_blank())
