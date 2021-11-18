library(gridExtra)
library(ggplot2)
download.file(url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",destfile = "StormData.csv.bz2")


Storm_Data <- read.table("StormData.csv.bz2",header=T, quote="\"", sep=",")


library(gridExtra)
library(ggplot2)
#(A)# ** Health Damage **  ## 

#1# Fatalities 
Event_Fatalities <- tapply(Storm_Data$FATALITIES, Storm_Data$EVTYPE, sum)
##Top 5 Fatal
Top_5_Fatal <- Event_Fatalities[order(Event_Fatalities,decreasing = TRUE)][1:5]
Df_Top_5_Fatal <- data.frame(names(Top_5_Fatal),Top_5_Fatal)
names(Df_Top_5_Fatal) <- c("Event","Fatalities")
## Max Fatalities 
Max_Fatalities <- Event_Fatalities[Event_Fatalities == max(Event_Fatalities)]

## Plotting the Data
Plot1 <- qplot(x=Df_Top_5_Fatal$Fatalities,y=Df_Top_5_Fatal$Event,geom = "col",xlab = "Fatalities" ,ylab ="Event")

#2# Injuries 
Event_Injuries <- tapply(Storm_Data$INJURIES, Storm_Data$EVTYPE, sum)
##TOP 5 Injuries
Top_5_Injuries <- Event_Injuries[order(Event_Injuries,decreasing = TRUE)][1:5]
Df_Top_5_Injuries <- data.frame(names(Top_5_Injuries),Top_5_Injuries)
names(Df_Top_5_Injuries) <- c("Event","Injuries")
## Max Injuries 
Max_Injuries <- Event_Injuries[Event_Injuries == max(Event_Injuries)] 

## Plotting the Data
Plot2 <- qplot(x=Df_Top_5_Injuries$Injuries,y=Df_Top_5_Injuries$Event,geom = "col",xlab = "Injuries" ,ylab ="Event")

grid.arrange(Plot1,Plot2 ,ncol=2)

#(B)#  ** Economic Damage ** ##  

#1# Property Damage 
Prop_Damage <- tapply(Storm_Data$PROPDMG, Storm_Data$EVTYPE, sum)
##Top 5 property Damage
Top_5_Property_DM <- Prop_Damage[order(Prop_Damage,decreasing = TRUE)][1:5]
Df_Top_5_Property_DM <- data.frame(names(Top_5_Property_DM),Top_5_Property_DM)
names(Df_Top_5_Property_DM) <- c("Event","Property Damage")
## Max Property Damage 
Max_Prop_Damage <- Prop_Damage[Prop_Damage == max(Prop_Damage)]
## Plotting the Data
Plot3 <- qplot(x=Df_Top_5_Property_DM$`Property Damage`,y=Df_Top_5_Property_DM$Event,geom = "col",xlab = "Propety Damage (USD)" ,ylab ="Event")



#2# Crop Damage
Crop_Damage <- tapply(Storm_Data$CROPDMG, Storm_Data$EVTYPE, sum)
## Top 5 crop Damage
Top_5_Crop_DM <- Crop_Damage[order(Crop_Damage,decreasing = TRUE)][1:5]
Df_Top_5_Crop_DM <- data.frame(names(Top_5_Crop_DM),Top_5_Crop_DM)
names(Df_Top_5_Crop_DM) <- c("Event","Crop Damage")
## Max Crop_Damage 
Max_Crop_Damage <- Crop_Damage[Crop_Damage == max(Crop_Damage)]

## Plotting the Data
Plot4 <- qplot(x=Df_Top_5_Crop_DM$`Crop Damage`,y=Df_Top_5_Crop_DM$Event,geom = "col",xlab = "Crop Damage (USD)" ,ylab ="Event")

grid.arrange(Plot3,Plot4 ,ncol=2)




