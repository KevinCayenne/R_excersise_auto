setwd("c:/Users/acer/Desktop/self_learning _package/")

library(readxl)
library(ggplot2)
library(ggpubr)
library(gtools)
library(gridExtra)

File.list = mixedsort(list.files("CYtry"),  decreasing=TRUE)
combined = paste("./CYtry/", File.list, sep="")
file.leng = length(combined)

## load files for each sheet
Day.1.Oximetry <- read_excel(combined[1], sheet = 1)
Day.1.Activity <- read_excel(combined[1], sheet = 2)
Day.1.Intake <- read_excel(combined[1], sheet = 3)

Day.2.Oximetry <- read_excel(combined[2], sheet = 1)
Day.2.Activity <- read_excel(combined[2], sheet = 2)
Day.2.Intake <- read_excel(combined[2], sheet = 3)

OximetryA <- list()
OximetryA[[1]] <- Day.1.Oximetry
OximetryA[[2]] <- Day.2.Oximetry

ActivityA <- list()
ActivityA[[1]] <- Day.1.Activity
ActivityA[[2]] <- Day.2.Activity

IntakeA <- list()
IntakeA[[1]] <- Day.1.Intake
IntakeA[[2]] <- Day.2.Intake

OximetryCom <- list()

for (i in 1:length(OximetryA)){
  
  Oximetry <- OximetryA[[i]]
  ## delete rows, cols, and revised the dataframe
  OximetryR <- as.data.frame(Oximetry[-c(1:20),-c(1:14)])
  
  colnames(OximetryR) <- c(OximetryR[1,])
  OximetryR <- OximetryR[-1,]
  OximetryR <- OximetryR[c(-148),]
  
  ## change time to numeric
  OximetryR$`Absolute time` <- as.numeric(OximetryR$`Absolute time`)
  OximetryR$`Relative time` <- as.numeric(OximetryR$`Relative time`)
  
  ## make the time correct!
  tt <- OximetryR$`Relative time` + as.numeric(Oximetry[4,2]) + as.numeric(Oximetry[5,2])
  ht <- OximetryR$`Relative time` + as.numeric(Oximetry[5,2])
  
  ## change to the normalise time 
  tt <- as.POSIXct(tt * (60*60*24)
             , origin="1899-12-30"
             , tz="GMT")
  
  ht <- as.POSIXct(ht * (60*60*24)
             , origin="2017-11-25"
             , tz="GMT")
  
  ## query out the time value for determination data type
  h.str <- as.numeric(format(tt, "%H")) +
           as.numeric(format(tt, "%M"))/60
  
  ## check point
  head(tt)
  head(h.str)
  
  ## decide which value to begin 
  which.first <- function(timeH){
    i <- 1
    k <- 1
    Tpoint <- 13 # set the begin time
    while(i < Tpoint){
      if (timeH[k] >= Tpoint){
        i <- timeH[k]
      } else {
        k <- k+1
      }
    }
    return(list(k,i))
  }  
  
  ## get the first value
  beginv <- which.first(h.str)
  begi <- beginv[[1]]
  
  ## set dark label: 1, light label: 2
  initset <- 72
  lenR <- length(OximetryR)
  
  OximetryR[begi:(begi+initset),lenR+1] <- 1 
  OximetryR[(begi+initset+1):(begi+initset+1+initset),lenR+1] <- 2
  colnames(OximetryR)[lenR+1] <- c("DayTag")
  
  ## add normalised time column and day tag
  OximetryR$RealTime <- tt
  OximetryR$Days <- i
  OximetryR$HoursT <- h.str

  #total_Oximetry$HoursT[total_Oximetry$HoursT>=13] <- -total_Oximetry$HoursT
  
  ## remove NA rows
  OximetryR <- OximetryR[!is.na(OximetryR$DayTag),]

  OximetryCom[[i]] <- OximetryR
}

total_Oximetry <- rbind(OximetryCom[[1]], OximetryCom[[2]])
for (i in 3:14){
  total_Oximetry[,i] <- as.numeric(total_Oximetry[,i])
}
total_Oximetry$DayTag <- as.factor(total_Oximetry$DayTag)
total_Oximetry$Days <- as.factor(total_Oximetry$Days)

total_Oximetry$sameTime <- as.POSIXct(c(as.character(total_Oximetry$RealTime[1:146]), as.character(total_Oximetry$RealTime[1:146])))
  
for(i in 3:9){
  print(
    ggplot(total_Oximetry, aes(sameTime, total_Oximetry[,i], group = Days, color = Days)) + 
    geom_point() +
    geom_line() +
    geom_vline(xintercept=13) +
    ylab(colnames(total_Oximetry[i])) +
    labs(title = sprintf("Oximetry_%s", colnames(total_Oximetry[i]))) +
    theme(plot.title = element_text(hjust = 0.5))
    )
}
