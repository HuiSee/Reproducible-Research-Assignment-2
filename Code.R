## Read Data ##
storm <- read.csv(bzfile("F:/Coursea/05. Reproducible Research/Quiz Week 4/repdata-data-StormData.csv.bz2"))

## Derived Data ##
impactdata<-storm [, c("EVTYPE", "PROPDMG", "PROPDMGEXP","FATALITIES", "INJURIES", "CROPDMG", "CROPDMGEXP")]
impactdata<-impactdata[!grepl("Summary", impactdata$EVTYPE),]
impactdata$EVTYPE<-as.factor(toupper(impactdata$EVTYPE))
impactdata$PROPDMGEXP<-as.factor(toupper(impactdata$PROPDMGEXP))
impactdata$CROPDMGEXP<-as.factor(toupper(impactdata$CROPDMGEXP))

## Data Analysis - Summarize ##
summary(impactdata$PROPDMGEXP)
summary(impactdata$CROPDMGEXP)
sum(impactdata$PROPDMGEXP %in% c("", "0", "B", "H", "M", "K")) / nrow(impactdata)

prop<-as.character(impactdata$PROPDMGEXP)
prop[prop %in% c("?","-","+","1","2","3","4","5","6","7","8")]<-NA
prop[prop %in% c("","0")]<-0
prop<-gsub("B", 10^9, prop)
prop<-gsub("K", 1000, prop)
prop<-gsub("H", 100, prop)
prop<-gsub("M", 10^6, prop)
impactdata$PROPDMGEXP<-as.numeric(prop)
impactdata$TOTALPROPDMG<-impactdata$PROPDMGEXP*impactdata$PROPDMG

crop<-as.character(impactdata$CROPDMGEXP)
crop[crop %in% c("?","2")]<-NA
crop[crop %in% c("","0")]<-0
crop<-gsub("B", 10^9, crop)
crop<-gsub("K", 1000, crop)
crop<-gsub("M", 10^6, crop)
impactdata$CROPDMGEXP<-as.numeric(crop)
impactdata$TOTALCROPDMG<-impactdata$CROPDMGEXP*impactdata$CROPDMG

## Get Total ##
library(plyr)
library(ggplot2)
library(gridExtra)

totalImpactdata<-ddply(impactdata, .(EVTYPE), summarize, totalFatal=sum(FATALITIES),totalInjured=sum(INJURIES),totalPropDmg=sum(TOTALPROPDMG),totalCropDmg=sum(TOTALCROPDMG),totalDamages=sum(totalPropDmg,totalCropDmg))

fataldata<-totalImpactdata[ order(-totalImpactdata[, 2]), ][1:10, ]
injurdata<-totalImpactdata[ order(-totalImpactdata[,3]), ][1:10, ]

propdmgdata<-totalImpactdata[ order(-totalImpactdata[, 4]), ][1:10, ]
cropdmgdata<-totalImpactdata[ order(-totalImpactdata[,5]), ][1:10, ]
totalEconImpact<-totalImpactdata[ order(-totalImpactdata[,6]), ][1:10, ]


## Plot Graph ##
g1<-ggplot(data = fataldata, aes(y=totalFatal, x=reorder(EVTYPE, totalFatal))) + geom_bar(stat="identity", fill="orange") +coord_flip()+ ylab("Total Fatalities")+ xlab("Event Type")+ ggtitle("Top Event Types by Total Fatalities")
print (g1)

g2<-ggplot(data = injurdata, aes(y=totalInjured, x=reorder(EVTYPE, totalInjured))) + geom_bar(stat="identity", fill="red")+ coord_flip()+ ylab("Total Injuries")+ xlab("Event Type") + ggtitle("Top Event Types by Total Injuries")
grid.arrange(g1,g2, ncol=2)

g3<-ggplot(data = propdmgdata, aes(y=totalPropDmg/10^9, x=reorder(EVTYPE, totalPropDmg))) + geom_bar(stat="identity", fill="orange") +coord_flip()+ ylab("Total Property Damages (US$ Billions)")+ xlab("Event Type")+ ggtitle("Top Event Types by Total Property Damages")
g4<-ggplot(data = cropdmgdata, aes(y=totalCropDmg/10^9, x=reorder(EVTYPE, totalCropDmg))) + geom_bar(stat="identity", fill="orange") +coord_flip()+ ylab("Total Crop Damages (US$ Billions)")+ xlab("Event Type")+ ggtitle("Top Event Types by Total Crop Damages")
grid.arrange(g3, g4, ncol=2)

ggplot(data = totalEconImpact, aes(y=totalDamages/10^9, x=reorder(EVTYPE, totalDamages))) + geom_bar(stat="identity", fill="orange") +coord_flip()+ ylab("Total Damages (US$ Billions)")+ xlab("Event Type")+ ggtitle("Top Event Types by Total Damages")


