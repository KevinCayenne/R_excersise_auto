setwd("c:/Users/acer/Desktop/self_learning _package/")

## library packages ----
library(readxl)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(gtools)
library(gridExtra)
library(plyr)

## load files in directory ----
File.list = mixedsort(list.files("CYtry"),  decreasing=TRUE)
combined = paste("./CYtry/", File.list, sep="")
file.leng = length(combined)

## define variables ----
dateA <- "2017-11-25"

start.value <- 3
O.value.tochoose <- 73
A.value.tochoose <- 220
I.value.tochoose <- 220
initset <- O.value.tochoose - 1
initseta <- A.value.tochoose - 1
initseti <- I.value.tochoose - 1

## load files for each sheet ----
Day.1.Oximetry <- read_excel(combined[start.value], sheet = 1)
Day.1.Activity <- read_excel(combined[start.value], sheet = 2)
Day.1.Intake <- read_excel(combined[start.value], sheet = 3)

Day.2.Oximetry <- read_excel(combined[start.value+1], sheet = 1)
Day.2.Activity <- read_excel(combined[start.value+1], sheet = 2)
Day.2.Intake <- read_excel(combined[start.value+1], sheet = 3)

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
ActivityCom <- list()
IntakeCom <- list()


## functions ----
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
## decide which value to begin ----
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
             , origin=dateA
             , tz="GMT")
  
  ## query out the time value for determination data type
  h.str <- as.numeric(format(tt, "%H")) +
           as.numeric(format(tt, "%M"))/60
  
  ## check point
  head(tt)
  head(h.str)

  ## get the first value
  beginv <- which.first(h.str)
  begi <- beginv[[1]]
  
  ## set dark label: 1, light label: 2
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
for (i in 1:length(ActivityA)){
  
  Activity <- ActivityA[[i]]
  ## delete rows, cols, and revised the dataframe
  ActivityR <- as.data.frame(Activity[-c(1:20),-c(1:4)])
  colnames(ActivityR) <- c(ActivityR[1,])
  ActivityR <- ActivityR[-1,]
  
  ## change time to numeric
  ActivityR$`Absolute time` <- as.numeric(ActivityR$`Absolute time`)
  ActivityR$`Relative time` <- as.numeric(ActivityR$`Relative time`)
  
  ## make the time correct!
  tta <- ActivityR$`Relative time` + as.numeric(Activity[4,2]) + as.numeric(Activity[5,2])
  hta <- ActivityR$`Relative time` + as.numeric(Activity[5,2])
  
  ## change to the normalise time 
  tta <- as.POSIXct(tta * (60*60*24)
                   , origin="1899-12-30"
                   , tz="GMT")
  
  hta <- as.POSIXct(hta * (60*60*24)
                   , origin=dateA
                   , tz="GMT")
  
  ## query out the time value for determination data type
  h.stra <- as.numeric(format(tta, "%H")) +
    as.numeric(format(tta, "%M"))/60
  
  ## check point
  head(tta)
  head(h.stra)

  ## get the first value
  beginva <- which.first(h.stra)
  begia <- beginva[[1]]
  
  ## set dark label: 1, light label: 2
  lenRa <- length(ActivityR)
  
  ActivityR[begia:(begia+initseta),lenRa+1] <- 1 
  ActivityR[(begia+initseta+1):(begia+initseta+1+initseta),lenRa+1] <- 2
  colnames(ActivityR)[lenRa+1] <- c("DayTag")
  
  ## add normalised time column and day tag
  ActivityR$RealTime <- tta
  ActivityR$Days <- i
  ActivityR$HoursT <- h.stra
  
  #total_Oximetry$HoursT[total_Oximetry$HoursT>=13] <- -total_Oximetry$HoursT
  
  ## remove NA rows
  ActivityR <- ActivityR[!is.na(ActivityR$DayTag),]
  
  ActivityCom[[i]] <- ActivityR
}
for (i in 1:length(IntakeA)){
  
  Intake <- IntakeA[[i]]
  ## delete rows, cols, and revised the dataframe
  IntakeR <- as.data.frame(Intake[-c(1:20),-c(1:8)])
  
  colnames(IntakeR) <- c(IntakeR[1,])
  IntakeR <- IntakeR[-1,]
  
  ## change time to numeric
  IntakeR$`Absolute time` <- as.numeric(IntakeR$`Absolute time`)
  IntakeR$`Relative time` <- as.numeric(IntakeR$`Relative time`)
  
  ## make the time correct!
  tti <- IntakeR$`Relative time` + as.numeric(Intake[4,2]) + as.numeric(Intake[5,2])
  hti <- IntakeR$`Relative time` + as.numeric(Intake[5,2])
  
  ## change to the normalise time 
  tti <- as.POSIXct(tti * (60*60*24)
                   , origin="1899-12-30"
                   , tz="GMT")
  
  hti <- as.POSIXct(hti * (60*60*24)
                   , origin = dateA
                   , tz="GMT")
  
  ## query out the time value for determination data type
  h.stri <- as.numeric(format(tti, "%H")) +
    as.numeric(format(tti, "%M"))/60
  
  ## check point
  head(tti)
  head(h.stri)

  ## get the first value
  beginvi <- which.first(h.stri)
  begii <- beginvi[[1]]
  
  ## set dark label: 1, light label: 2
  lenRi <- length(IntakeR)
  
  IntakeR[begii:(begii+initseti),lenRi+1] <- 1 
  IntakeR[(begii+initseti+1):(begii+initseti+1+initseti),lenRi+1] <- 2
  colnames(IntakeR)[lenRi+1] <- c("DayTag")
  
  ## add normalised time column and day tag
  IntakeR$RealTime <- tti
  IntakeR$Days <- i
  IntakeR$HoursT <- h.stri
  
  #total_Oximetry$HoursT[total_Oximetry$HoursT>=13] <- -total_Oximetry$HoursT
  
  ## remove NA rows
  IntakeR <- IntakeR[!is.na(IntakeR$DayTag),]
  
  IntakeCom[[i]] <- IntakeR
}

## combined days ----
total_Oximetry <- rbind(OximetryCom[[1]], OximetryCom[[2]])
total_Activity <- rbind(ActivityCom[[1]], ActivityCom[[2]])
total_Intake <- rbind(IntakeCom[[1]], IntakeCom[[2]])

## change data type ----
for (i in 3:14){
  total_Oximetry[,i] <- as.numeric(total_Oximetry[,i])
}
for (i in 3:4){
  total_Activity[,i] <- as.numeric(total_Activity[,i])
}
for (i in 3:8){
  total_Intake[,i] <- as.numeric(total_Intake[,i])
}

## create columns and change to factor ----
total_Oximetry$DayTag <- as.factor(total_Oximetry$DayTag)
total_Oximetry$Days <- as.factor(total_Oximetry$Days)
total_Oximetry$sameTime <- as.POSIXct(c(as.character(total_Oximetry$RealTime[1:(O.value.tochoose*2)]), as.character(total_Oximetry$RealTime[1:(O.value.tochoose*2)])))

total_Activity$DayTag <- as.factor(total_Activity$DayTag)
total_Activity$Days <- as.factor(total_Activity$Days)
total_Activity$sameTime <- as.POSIXct(c(as.character(total_Activity$RealTime[1:(A.value.tochoose*2)]), as.character(total_Activity$RealTime[1:(A.value.tochoose*2)])))

total_Intake$DayTag <- as.factor(total_Intake$DayTag)
total_Intake$Days <- as.factor(total_Intake$Days)
total_Intake$sameTime <- as.POSIXct(c(as.character(total_Intake$RealTime[1:(I.value.tochoose*2)]), as.character(total_Intake$RealTime[1:(I.value.tochoose*2)])))

colnames(total_Oximetry)[3:8] <- c("VO2(ml_min_kg075)", "VCO2(ml_min_kg075)", "EE(Kcal_day_kg075)", "VO2(ml_min)", "VCO2(ml_min)", "EE(Kcal_day)")
colnames(total_Intake)[5:8] <- c("Food_Cons_rate(g_g)", "Drink(ml)", "Consumption(ml)", "Drink_Cons_rate(ml_g)")

## Oximetry plot ----
for(i in 3:9){
  
  DO <- O.value.tochoose*2

  sameTime <- as.POSIXct(c(as.character(total_Oximetry$RealTime[1:(O.value.tochoose*2)]), as.character(total_Oximetry$RealTime[1:(O.value.tochoose*2)]), as.character(total_Oximetry$RealTime[1:(O.value.tochoose*2)])))
  Days <- as.factor(rep(c("First", "Second","Mean"), c(DO, DO, DO)))
  levels(Days) <- list(First = "First", Second = "Second", Mean = "Mean")
  
  Ap <- as.numeric(tapply(total_Oximetry[,i], total_Oximetry$sameTime, mean))
  Ap <- c(total_Oximetry[,i], Ap)
  tempdf <- data.frame(Ap, sameTime, Days)
  
  bardf <- aggregate(total_Oximetry[,i], list(total_Oximetry$DayTag, total_Oximetry$Days), sum)
  bardf.2 <- aggregate(total_Oximetry[,i], list(total_Oximetry$Days), sum)
  namedf <- c("L/D", "Day", "Sum")
  colnames(bardf) <- namedf
  
  bardf.2 <- cbind(c(3,3), bardf.2)
  colnames(bardf.2) <- namedf
  bardf.2$`L/D` <- as.factor(bardf.2$`L/D`)
  bardft <- rbind(bardf, bardf.2)
           
  p1 <- ggplot(total_Oximetry, aes(sameTime, total_Oximetry[,i], group = Days, color = Days)) + 
        geom_point() +
        geom_line() +
        geom_vline(xintercept=13) +
        ylab(colnames(total_Oximetry[i])) +
        labs(title = sprintf("Oximetry_dot_%s", colnames(total_Oximetry[i]))) +
        theme(plot.title = element_text(hjust = 0.5))

  p2 <- ggplot(tempdf, aes(sameTime, Ap, group = Days, color = Days)) + 
        geom_point() +
        geom_line() +
        geom_vline(xintercept=13) +
        theme_bw() +
        ylab(colnames(total_Oximetry[i])) +
        labs(title = sprintf("Oximetry_dot_%s", colnames(total_Oximetry[i]))) +
        theme(plot.title = element_text(hjust = 0.5))
  
  p3 <- ggplot(tempdf, aes(sameTime, Ap, group = Days, color = Days)) + 
        geom_point() +
        geom_line() +
        geom_vline(xintercept=13) +
        theme_bw() +
        ylab(colnames(total_Oximetry[i])) +
        labs(title = sprintf("Oximetry_dot_%s", colnames(total_Oximetry[i]))) +
        theme(plot.title = element_text(hjust = 0.5))
  
  
  # print(
  #   totalp <- ggarrange(p1, p2, ncol = 2, nrow = 1)
  #   )

  bar1 <- ggplot(bardft, aes(Day, Sum, group = bardft$`L/D`, fill = bardft$`L/D`)) +
                geom_bar(stat = "identity",  position = position_dodge(.9)) +
                scale_fill_manual(values=c("#F0E442", "#999999", "#0072B2"), labels =c("Light", "Dark", "Total")) +
                labs(title = sprintf("Oximetry_Sum_%s", colnames(total_Oximetry[i]))) +
                theme_bw() + 
                theme(plot.title = element_text(hjust = 0.5)) +
                guides(fill=guide_legend(title="LorD\n")) 
  
  bardf2 <- aggregate(total_Oximetry[,i], list(total_Oximetry$DayTag, total_Oximetry$Days), mean)
  bardf2.2 <- aggregate(total_Oximetry[,i], list(total_Oximetry$Days), mean)
  namedf2 <- c("L/D", "Day", "Sum")
  colnames(bardf2) <- namedf2
  
  bardf2.2 <- cbind(c(3,3), bardf2.2)
  colnames(bardf2.2) <- namedf2
  bardf2.2$`L/D` <- as.factor(bardf2.2$`L/D`)
  bardft2 <- rbind(bardf2, bardf2.2)
  
  bar2 <- ggplot(bardft2, aes(Day, Sum, group = bardft$`L/D`, fill = bardft$`L/D`)) +
                geom_bar(stat = "identity",  position = position_dodge(.9)) +
                scale_fill_manual(values=c("#F0E442", "#999999", "#0072B2"), labels =c("Light", "Dark", "Mean")) +
                labs(title = sprintf("Oximetry_Mean_%s", colnames(total_Oximetry[i]))) +
                theme_bw() + 
                theme(plot.title = element_text(hjust = 0.5)) +
                guides(fill=guide_legend(title="LorD\n")) 
  
  jpeg(filename = sprintf("c:/Users/acer/Desktop/self_learning _package/CYtry/image/Oximetry/%s.jpg", colnames(total_Oximetry[i])), width = 1200, height = 1000)
  print(
    totalp2 <- ggarrange(p1, p2, bar1, bar2, ncol = 2, nrow = 2)
  )
  dev.off()
}
## Activity plot ----
for(i in 3:3){
  
  DA <- A.value.tochoose*2
  
  sameTime <- as.POSIXct(c(as.character(total_Activity$RealTime[1:(A.value.tochoose*2)]), as.character(total_Activity$RealTime[1:(A.value.tochoose*2)]), as.character(total_Activity$RealTime[1:(A.value.tochoose*2)])))
  Days <- as.factor(rep(c("First", "Second","Mean"), c(DA, DA, DA)))
  levels(Days) <- list(First = "First", Second = "Second", Mean = "Mean")
  
  Ap <- as.numeric(tapply(total_Activity[,i], total_Activity$sameTime, mean))
  Ap <- c(total_Activity[,i], Ap)
  tempdf <- data.frame(Ap, sameTime, Days)
  
  bardf <- aggregate(total_Activity[,i], list(total_Activity$DayTag, total_Activity$Days), sum)
  bardf.2 <- aggregate(total_Activity[,i], list(total_Activity$Days), sum)
  namedf <- c("L/D", "Day", "Sum")
  colnames(bardf) <- namedf
  
  bardf.2 <- cbind(c(3,3), bardf.2)
  colnames(bardf.2) <- namedf
  bardf.2$`L/D` <- as.factor(bardf.2$`L/D`)
  bardft <- rbind(bardf, bardf.2)
  
  p1 <- ggplot(total_Activity, aes(sameTime, total_Activity[,i], group = Days, color = Days)) + 
    geom_line() +
    ylab(colnames(total_Activity[i])) +
    labs(title = sprintf("Activity_dot_%s", colnames(total_Activity[i]))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  p2 <- ggplot(tempdf, aes(sameTime, Ap, group = Days, color = Days)) + 
    geom_line() +
    theme_bw() +
    ylab(colnames(total_Activity[i])) +
    labs(title = sprintf("Activity_dot_%s", colnames(total_Activity[i]))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  p3 <- ggplot(tempdf, aes(sameTime, Ap, group = Days, color = Days)) + 
    geom_line() +
    theme_bw() +
    ylab(colnames(total_Activity[i])) +
    labs(title = sprintf("Activity_dot_%s", colnames(total_Activity[i]))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # print(
  #   totalp <- ggarrange(p1, p2, ncol = 2, nrow = 1)
  #   )
  
  bar1 <- ggplot(bardft, aes(Day, Sum, group = bardft$`L/D`, fill = bardft$`L/D`)) +
    geom_bar(stat = "identity",  position = position_dodge(.9)) +
    scale_fill_manual(values=c("#F0E442", "#999999", "#0072B2"), labels =c("Light", "Dark", "Total")) +
    labs(title = sprintf("Activity_Sum_%s", colnames(total_Activity[i]))) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill=guide_legend(title="LorD\n")) 
  
  bardf2 <- aggregate(total_Activity[,i], list(total_Activity$DayTag, total_Activity$Days), mean)
  bardf2.2 <- aggregate(total_Activity[,i], list(total_Activity$Days), mean)
  namedf2 <- c("L/D", "Day", "Sum")
  colnames(bardf2) <- namedf2
  
  bardf2.2 <- cbind(c(3,3), bardf2.2)
  colnames(bardf2.2) <- namedf2
  bardf2.2$`L/D` <- as.factor(bardf2.2$`L/D`)
  bardft2 <- rbind(bardf2, bardf2.2)
  
  bar2 <- ggplot(bardft2, aes(Day, Sum, group = bardft$`L/D`, fill = bardft$`L/D`)) +
    geom_bar(stat = "identity",  position = position_dodge(.9)) +
    scale_fill_manual(values=c("#F0E442", "#999999", "#0072B2"), labels =c("Light", "Dark", "Mean")) +
    labs(title = sprintf("Activity_Mean_%s", colnames(total_Activity[i]))) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill=guide_legend(title="LorD\n")) 
  
  jpeg(filename = sprintf("c:/Users/acer/Desktop/self_learning _package/CYtry/image/Activity/Activity.jpg"), width = 1200, height = 1000)
  print(
    totalp2 <- ggarrange(p1, p2, bar1, bar2, ncol = 2, nrow = 2)
  )
  dev.off()
  
}
## Intake plot ----
for(i in 3:8){
  
  DI <- I.value.tochoose*2
  
  sameTime <- as.POSIXct(c(as.character(total_Intake$RealTime[1:(I.value.tochoose*2)]), as.character(total_Intake$RealTime[1:(I.value.tochoose*2)]), as.character(total_Intake$RealTime[1:(I.value.tochoose*2)])))
  Days <- as.factor(rep(c("First", "Second","Mean"), c(DI, DI, DI)))
  levels(Days) <- list(First = "First", Second = "Second", Mean = "Mean")
  
  Ap <- as.numeric(tapply(total_Intake[,i], total_Intake$sameTime, mean))
  Ap <- c(total_Intake[,i], Ap)
  tempdf <- data.frame(Ap, sameTime, Days)
  
  bardf <- aggregate(total_Intake[,i], list(total_Intake$DayTag, total_Intake$Days), sum)
  bardf.2 <- aggregate(total_Intake[,i], list(total_Intake$Days), sum)
  namedf <- c("L/D", "Day", "Sum")
  colnames(bardf) <- namedf
  
  bardf.2 <- cbind(c(3,3), bardf.2)
  colnames(bardf.2) <- namedf
  bardf.2$`L/D` <- as.factor(bardf.2$`L/D`)
  bardft <- rbind(bardf, bardf.2)
  
  p1 <- ggplot(total_Intake, aes(sameTime, total_Intake[,i], group = Days, color = Days)) + 
    geom_point() +
    geom_line() +
    ylab(colnames(total_Intake[i])) +
    labs(title = sprintf("Intake_dot_%s", colnames(total_Intake[i]))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  p2 <- ggplot(tempdf, aes(sameTime, Ap, group = Days, color = Days)) + 
    geom_point() +
    geom_line() +
    geom_vline(xintercept=13) +
    theme_bw() +
    ylab(colnames(total_Intake[i])) +
    labs(title = sprintf("Intake_dot_%s", colnames(total_Intake[i]))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  p3 <- ggplot(tempdf, aes(sameTime, Ap, group = Days, color = Days)) + 
    geom_point() +
    geom_line() +
    geom_vline(xintercept=13) +
    theme_bw() +
    ylab(colnames(total_Intake[i])) +
    labs(title = sprintf("Intake_dot_%s", colnames(total_Intake[i]))) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # print(
  #   totalp <- ggarrange(p1, p2, ncol = 2, nrow = 1)
  #   )
  
  bar1 <- ggplot(bardft, aes(Day, Sum, group = bardft$`L/D`, fill = bardft$`L/D`)) +
    geom_bar(stat = "identity",  position = position_dodge(.9)) +
    scale_fill_manual(values=c("#F0E442", "#999999", "#0072B2"), labels =c("Light", "Dark", "Total")) +
    labs(title = sprintf("Intake_Sum_%s", colnames(total_Intake[i]))) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill=guide_legend(title="LorD\n")) 
  
  bardf2 <- aggregate(total_Intake[,i], list(total_Intake$DayTag, total_Intake$Days), mean)
  bardf2.2 <- aggregate(total_Intake[,i], list(total_Intake$Days), mean)
  namedf2 <- c("L/D", "Day", "Sum")
  colnames(bardf2) <- namedf2
  
  bardf2.2 <- cbind(c(3,3), bardf2.2)
  colnames(bardf2.2) <- namedf2
  bardf2.2$`L/D` <- as.factor(bardf2.2$`L/D`)
  bardft2 <- rbind(bardf2, bardf2.2)
  
  bar2 <- ggplot(bardft2, aes(Day, Sum, group = bardft$`L/D`, fill = bardft$`L/D`)) +
    geom_bar(stat = "identity",  position = position_dodge(.9)) +
    scale_fill_manual(values=c("#F0E442", "#999999", "#0072B2"), labels =c("Light", "Dark", "Mean")) +
    labs(title = sprintf("Intake_Mean_%s", colnames(total_Intake[i]))) +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill=guide_legend(title="LorD\n")) 
  
  jpeg(filename = sprintf("c:/Users/acer/Desktop/self_learning _package/CYtry/image/Intake/%s.jpg", colnames(total_Intake[i])), width = 1200, height = 1000)
  print(
    totalp2 <- ggarrange(p1, p2, bar1, bar2, ncol = 2, nrow = 2)
  )
  dev.off()
  
}