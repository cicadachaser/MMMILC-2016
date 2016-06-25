#This is an R script to analyze the MMMILC Project data from 2016

start<-proc.time()

# header info -------------------------------------------------------------

#clear all variables
rm(list=ls())
graphics.off()
pardefault <- par(no.readonly = T)

#load packages, 
library(ggplot2)
library(rmarkdown)
library(knitr)
library(plyr)
library(gridExtra)
library(ggmap)

#se function that removes NA's
std.err <- function(x) sd(x[!is.na(x)])/sqrt(length(x[!is.na(x)]))

# read data ---------------------------------------------------------------

#setwd and load data
setwd("C:\\Users\\louie\\Documents\\GitHub\\MMMILC-2016") #LHY desktop

#load data
setwd("2016 data") 
mw.locs<-read.csv("milkweed coordinates 2015-05-23.csv")
data.first<-read.csv("2016 MMMILC Project Data Weeks 1-3.csv",header=T,strip.white=T,na.strings= c(" ", "")) #observations
data.second<-read.csv("2016 MMMILC Project Data Weeks 4-6.csv",header=T,strip.white=T,na.strings= c(" ", "")) #observations
data.third<-read.csv("2016 MMMILC Project Data Weeks 7-9.csv",header=T,strip.white=T,na.strings= c(" ", "")) #observations
data.fourth<-read.csv("2016 MMMILC Project Data 2016-06-17.csv",header=T,strip.white=T,na.strings= c(" ", "")) #observations
setwd("..")

data<-rbind(data.third,data.first,data.second,data.fourth)

# #remove all observations of plants that were replanted in 2016; it might be okay to use these, but they were planted in early April 2016
#replanted.2016<-data[grep("replanted",data$notes),"milkweed.ID"]
# data<-data[!data$milkweed.ID %in% replanted.2016,]

#alternatively, just remove the 50 replanting obs themselves 
data<-data[-grep("replanted",data$notes),]

#remove duplicate data from Week 6
data<-data[!(data$date=="5/6/2016" & data$name.1=="Scott Soderquist" & data$name.2=="Alex Preisler"),]

#milkweed status to character and monarch stage columns to character
data$milkweed.status<-as.character(data$milkweed.status)
data[,"monarch1.stage"] <- as.character(data[,"monarch1.stage"] )
data[,"monarch2.stage"] <- as.character(data[,"monarch2.stage"] )
data[,"monarch3.stage"] <- as.character(data[,"monarch3.stage"] )
data[,"monarch4.stage"] <- as.character(data[,"monarch4.stage"] )
data[,"monarch5.stage"] <- as.character(data[,"monarch5.stage"] )

# the two % columns to numerics
data$percent.green <- as.numeric(gsub("%", "", data$percent.green))
data$leaf.damage <- as.numeric(gsub("%", "", data$leaf.damage))

#date and time to date.time object
data$date.time <- strptime(paste(data$date, data$time), format = "%m/%d/%Y %H:%M")
data$julianDate <- as.numeric(strftime(data$date.time, format = "%j"))
data$date<-as.Date(data$date, "%m/%d/%Y")

#calculate the project.day and week
data$project.day<-julian(data$date,origin=as.Date("2016-03-28"))+1
data$week<-as.integer((data$project.day-1) %/% 7+1)
count(data,vars="week")

#remove data from the current week
data<-data[data$week<13,]

#####s
#need to create a proabable time spent on each milkweed
#order by paste(name.1, name.2, name.3)
#order by date.time
#calculate the diff between given milkweed and following milkweed (if observers equal, if not then 4 min)
#if the difference is more than 15 minutes or less than 0 minutes, make the elapsed time 4 minutes
#this accounts for lunch breaks, different trips, final plant observed by that team (all elapsed times we cannot know)
#re-order dataset in chronological order within teams within weeks

data$team<-paste(data$name.1, data$name.2, data$name.3)
#count(data,vars="team")

data <- data[order(data$week,data$team,data$date.time),]
data$rawTimes <- difftime(c(data$date.time[-1], min(data$date.time)), data$date.time, units = "mins")
data$elapsedTime <- ifelse(data$rawTimes>=0 & data$rawTimes<15, data$rawTimes, median(data$rawTimes))

#order by date, then by milkweed.ID - is this necessary?
#data<-data[order(data$date, data$milkweed.ID),]

#cleaning object data
#any observations 0 percent.green, but ALIVE status. replace with DEAD
data[ which(data$percent.green==0 & data$milkweed.status=="ALIVE") , "milkweed.status"] <- "DEAD"

#any observations NA percent.green, but ALIVE status. action TBD, but real data here, so leaving intact.
data[ which(is.na(data$percent.green) & data$milkweed.status=="ALIVE") , ]

#any observation with a non-zero percent green and NA for status, replace with ALIVE
data[ which(data$percent.green > 0 & is.na(data$milkweed.status)) , "milkweed.status"] <- "ALIVE"

#exclude training data
###############
#DATES TBD
#data<-data[data$date>"2015-04-26",]
#trip<-trip[trip$date>"2015-04-26",]

#add number of larvae and egg columns
monarchStages <- cbind(data$monarch1.stage, data$monarch2.stage, data$monarch3.stage, data$monarch4.stage, data$monarch5.stage)
data$nL1<-apply(monarchStages , 1 , function(x) sum(grepl("L1", x)))
data$nL2<-apply(monarchStages , 1 , function(x) sum(grepl("L2", x)))
data$nL3<-apply(monarchStages , 1 , function(x) sum(grepl("L3", x)))
data$nL4<-apply(monarchStages , 1 , function(x) sum(grepl("L4", x)))
data$nL5<-apply(monarchStages , 1 , function(x) sum(grepl("L5", x)))
data$nEggs<-apply(monarchStages , 1 , function(x) sum(grepl("E", x)))

#total larvae on each plant
data$nLTotal <- rowSums(cbind(data$nL1, data$nL2, data$nL3, data$nL4, data$nL5), na.rm=TRUE)

#add monarch load column
data$monarchLoad <- data$nLTotal + data$nEggs

#check if sum of larval class counts is equal to total larvae.
which(data$nLTotal!=rowSums(cbind(data$nL1, data$nL2, data$nL3, data$nL4, data$nL5)))

monarchLengths <- data[,c("monarch1.length", "monarch2.length", "monarch3.length", "monarch4.length", "monarch5.length")]


data$L1lengths <- sapply(1:nrow(data), function(x) c(monarchLengths[ x , which(grepl("L1", monarchStages[x , ]))]))
data$L2lengths <- sapply(1:nrow(data), function(x) c(monarchLengths[ x , which(grepl("L2", monarchStages[x , ]))]))
data$L3lengths <- sapply(1:nrow(data), function(x) c(monarchLengths[ x , which(grepl("L3", monarchStages[x , ]))]))
data$L4lengths <- sapply(1:nrow(data), function(x) c(monarchLengths[ x , which(grepl("L4", monarchStages[x , ]))]))
data$L5lengths <- sapply(1:nrow(data), function(x) c(monarchLengths[ x , which(grepl("L5", monarchStages[x , ]))]))

data$catLengths <- sapply(1:nrow(data), function(x) c(monarchLengths[ x , which(grepl("[L1-5]", monarchStages[x , ]))]))

# #how many of the plants are NE?
# prop.ne.fun<-function (x) {sum(x=="N.E.")/length(x)}
# prop.ne.summ<-aggregate(milkweed.status~milkweed.ID, data=data, prop.ne.fun)
# colnames(prop.ne.summ)<-c("milkweed.ID","prop.ne")
# p1<- ggplot(prop.ne.summ, aes (x=milkweed.ID, y=prop.ne))
# p1+geom_point(aes(color=prop.ne,size=prop.ne))+scale_color_gradientn(colors=c("green","red"))



# calculate participant metrics -------------------------------------------

#function to call all observations from a student and summarize
#add elements within this function to add rows to student summary table
###################################################
#these are empty objects to store: summary statistics, plots, and a lists of suspected missed plants.
#student.summary function outputs to these objects by default
student.plots <- list()
compare.plots <- list()
missedPlants <- list()

student.summary <- function(student.name){
  student <- list()
  #list trips student.name was on
  student.obs <- sort(which(student.name==data$name.1 | student.name==data$name.2 | student.name==data$name.3))
  
  #get observations student.name was a part of
  dStudent <- data[student.obs,]
  
  #monarchs found
  student$larvaeFound <- sum(dStudent$nLTotal)
  
  #live plants surveyed
  student$livePlants <- sum(dStudent$milkweed.status== "ALIVE")
  
  #total plants surveyed
  student$totalPlants <- nrow(dStudent)
  
  #total time in field and average time spent per milkweed
  student$timeInField <- sum(dStudent$elapsedTime, na.rm = TRUE)
  student$timePerMilkweed <- mean(dStudent$elapsedTime, na.rm = TRUE)
  
  #list plants censused in the most recent trip, determine if that sequence is a complete one
  #print any plants missing from the sequence
  plantsRecentWeek <- sort(dStudent[dStudent$week==max(dStudent$week),"milkweed.ID"])
  completeSeries <- min(plantsRecentWeek):max(plantsRecentWeek)
  missedPlants[[student.name]] <<- c(completeSeries[which(is.na(match(completeSeries , plantsRecentWeek)))])
  
  
  #make some plots, put them into a list to save for student reports
  #student monarch discovery rate compared to average
  monarchProb <- sapply(unique(data$week),function(x) sum (data[data$week == x , "nLTotal"]) / nrow(data[data$week == x ,]))
  
  studentMonarchProb <- sapply(unique(data$week),function(x) sum (dStudent[dStudent$week == x , "nLTotal"]) / nrow(dStudent[dStudent$week == x ,]))
  
  monarchDetection <- data.frame(monarchProb = monarchProb, studentMonarchProb = studentMonarchProb)
  
  p1 <- qplot(data = monarchDetection, y = monarchProb) + geom_line() + ylab("p(monarch larva)") + xlab("week")+ ggtitle("Student comparison to average")
  p1 <- p1 + geom_point(size = 3,data = monarchDetection, aes(y = studentMonarchProb), colour = "red" )
  
  #student egg discovery rate compared to average
  eggProb <- sapply(unique(data$week), 
                    function(x) sum (data[data$week == x , "nEggs"]) / nrow(data[data$week == x ,]))
  
  studentEggProb <- sapply(unique(data$week), 
                           function(x) sum (dStudent[dStudent$week == x , "nEggs"]) / nrow(dStudent[dStudent$week == x ,]))
  eggDetection <- data.frame(eggProb = eggProb, studentEggProb = studentEggProb)
  
  p2 <- qplot(data = eggDetection, y = eggProb) + geom_line() + ylab("p(egg)") + xlab("week")
  p2 <- p2 + geom_point(size = 3,data = eggDetection, aes(y = studentEggProb), colour = "red" )
  
  #time per plant by week, average vs student
  dStudentExp <- merge(all.y = TRUE , dStudent , data.frame("week" = unique(data$week)))
  weeklyStudentAvg <- with(dStudentExp, tapply(elapsedTime, INDEX = week, FUN = mean))
  weeklyTimeAvg <- with(data, tapply(elapsedTime, INDEX = week, FUN = mean))
  
  timeDevotion <- data.frame(weeklyTimeAvg, weeklyStudentAvg)
  
  p3 <- qplot(data = timeDevotion, y = weeklyTimeAvg) + geom_line() + ylab("minutes / plant") + xlab("week")
  p3 <- p3 + geom_point(size = 3,data = timeDevotion, aes(y = weeklyStudentAvg), colour = "red" )
  
  #student number of plants censused per week
  weekCount <- data.frame(week = 1:max(data$week), count = NA, cumMonarch = 0)
  weekCount[match(names(table(dStudent$week)) , weekCount$week) , "count"] <- table(dStudent$week)
  
  p4 <- qplot(data = weekCount, x = weekCount$week , y = as.numeric(weekCount$count)) 
  p4  <- p4 + geom_line() + xlab("week") + ylab("plants per week") + ggtitle("Student data")
  p4 <- p4 + coord_cartesian(xlim = c(0, max(data$week))) + coord_cartesian(ylim = c(0, 1.2*max(weekCount$count, na.rm = TRUE))) 
  
  
  #cumulative monarch eggs and larvae found on plants
  monarchs <- with(dStudent, tapply(dStudent$monarchLoad, dStudent$week, sum))
  weekCount[match(names(monarchs), weekCount$week) , "cumMonarch"] <- monarchs
  weekCount$cumMonarch <- cumsum(weekCount$cumMonarch)
  
  p5 <- qplot(data = weekCount, x = weekCount$week , y = as.numeric(weekCount$cumMonarch)) 
  p5  <- p5 + geom_line() + xlab("week") + ylab("cumulat. monarchs") 
  p5 <- p5 + coord_cartesian(xlim = c(0, max(data$week))) + coord_cartesian(ylim = c(0, 1.2*max(weekCount$cumMonarch, na.rm = TRUE))) 
  
  
  #average percent green in each week compared to average
  weekGreen <- with(data, tapply(percent.green, week, function(x) mean(x, na.rm = TRUE)))
  studentGreen <- with(dStudent, tapply(percent.green, week, function(x) mean(x, na.rm = TRUE)))
  
  greenPlot <- data.frame(week = 1:max(data$week) , weekGreen = NA, studentGreen = NA)
  greenPlot[match(names(studentGreen), greenPlot$week) , "studentGreen"] <- studentGreen
  greenPlot[match(names(weekGreen), greenPlot$week) , "weekGreen"] <- weekGreen
  
  p6 <- qplot(data = greenPlot, y = weekGreen) + geom_line() 
  p6 <- p6 + ylab("avg % green") + xlab("week") + coord_cartesian(ylim = c(min(c(weekGreen, studentGreen)*.6, na.rm = TRUE), 100))
  p6 <- p6 + geom_point(size = 3,data = greenPlot, aes(y = studentGreen), colour = "red" )
  
  #create a list of all plots for this student, append that list of plots for later plotting, and plot them now
  compare.plots[[student.name]] <<- list(p1, p2, p3, p6)
  student.plots[[student.name]] <<- list( p4, p5)
  #convert summary list to data.frame and print
  data.frame(student)
  #plot individual student plots (commented out to silence output when all are run together)
  # do.call(grid.arrange, c(compare.plots[[student.name]], student.plots[[student.name]]))
}

student.summary(as.character(data$name.1[1]))

#apply the function of all students, and create student reports
#make a list of names, remove NA value
name.list <- unique(c(as.character(data$name.1), as.character(data$name.2), as.character(data$name.3)))
name.list <- sort(name.list[!is.na(name.list)])

#apply student.summary function to all students, create a data frame form the result
student.df <- data.frame(sapply( name.list, function(x) student.summary(x)))

#add a column of averages using a mean function that can deal with NaN's and NA's
student.df$average <- sapply(1:nrow(student.df), function(x) mean(unlist( student.df[x,]), na.rm = TRUE))

#remove spaces for pdf file names, add current date for report and pdf extension
file.names.list <- paste(format(Sys.time(), '%m-%d-%Y'),"_",
                         gsub(" ", "_", name.list), 
                         ".pdf", sep = "")

#this is the loop to run the report generating script
#for each student name in name.list, it generates a report including summary statistics from
#student.df and plots several things

#get the last week interval
last.week <- sort(seq( min(data$julianDate), min(data$julianDate) + max(data$week)*7, by = 7), decreasing = TRUE)[c(2,1)]
last.week <- format(as.Date(last.week, origin=as.Date("2015-01-01")) ,  '%d %b')
last.week <- paste("week", " ", max(data$week), ": ", last.week[1], " - ",last.week[2], sep = "" )

##################################################  

# create student reports --------------------------------------------------


#go to reports folder
setwd("reports")

for(i in 1:length(name.list)){
  student.name <- name.list[i]
  render(input = "student_report.Rmd", output_format = "pdf_document",
         output_file = file.names.list[i])
}

setwd("..")

# calculate overall metrics -----------------------------------------------


#overall summary report
#calculate summary statistics, turn them into a data.frames for a table at top of report
#summary statistics from a given subset of the data
summary.table <- function(dataToSumm = data){
  #number of observations last week 
  nObs <- nrow(dataToSumm)
  #percent green last week
  avgGreen <- mean(dataToSumm$percent.green, na.rm = TRUE) 
  #larvae per plant last week
  larvaePerPlant <- sum(dataToSumm$nLTotal, na.rm = TRUE) / nObs
  #eggs per plant last week
  eggPerPlant <- sum(dataToSumm$nEggs, na.rm = TRUE) / nObs
  
  summ.df <- data.frame(nObs, avgGreen, larvaePerPlant, eggPerPlant)   
  summ.df
}


#apply the summary function to whole dataset and last weeks data, rbind for report      
overall <- summary.table(dataToSumm = data)
lastWeek <- summary.table(dataToSumm = data[data$week == max(data$week, na.rm = TRUE) , ] )
#this object goes to report
summStats <- rbind("overall" = overall, "lastWeek" = lastWeek)

#status counts
weekStatus <-table( data$week, data$milkweed.status)
weekStatus <- cbind("week"=unique(data$week),weekStatus , "total" = rowSums(weekStatus))
status.table <- tail(weekStatus, 10) #last x weeks

#overall student rankings
#this object goes to report
rankTable <- as.data.frame( t( student.df[, -ncol(student.df) ]) )
rankTable <- rankTable[ order(unlist(rankTable$totalPlants) , decreasing = TRUE), ]
rankTable <- head(rankTable, 50) #top x students in # plants obs

#plots
#milkweed count by week
milkweed.obs.by.week <- aggregate(milkweed.ID~week,length,data=data)
p1 <- ggplot(milkweed.obs.by.week, aes(x=week, y=milkweed.ID))
p1 <- p1 + geom_point(col="red")+geom_line()+geom_hline(yintercept=length(unique(data$milkweed.ID)),lty='dashed')+coord_cartesian(ylim = c(0, 400))+scale_x_continuous(breaks=c(1:max(data$week)))+ylab("milkweed count")


#How long did it take to measure each milkweed?
speed.by.week<-aggregate(elapsedTime~week,data=data,mean) 
p2<-ggplot(speed.by.week,aes(x=week,y=elapsedTime))
p2 <- p2+geom_point(col="blue")+geom_line()+coord_cartesian(ylim = c(0, 8))+scale_x_continuous(breaks=c(1:max(data$week)))+ylab("minutes per plant")

#average total stem length per plant
#total stem len and area
data$stem.count<-as.integer(data$stem.count)
data$mean.len<-rowMeans(data[,which(colnames(data)=="len.1"):which(colnames(data)=="len.10")],na.rm=T)
data$mean.dia<-rowMeans(data[,which(colnames(data)=="dia.1"):which(colnames(data)=="dia.10")],na.rm=T)
data$total.stem.len<-data$stem.count*data$mean.len
data$total.stem.area<-data$stem.count*data$mean.dia^2*pi

# in week 6, plants 474-488 were measured twice. This gave us the opportunity see how comparable the two sets of obs were!
# dups<-data[which(data$week==6 & data$milkweed.ID>=474 & data$milkweed.ID<=488),]
# dups<-dups[order(dups$milkweed.ID),]
# 
# lm.dups.len<-lm(dups[dups$julianDate==127,"total.stem.len"]~dups[dups$julianDate==129,"total.stem.len"])
# summary(lm.dups.len)
# plot(dups[dups$julianDate==129,"total.stem.len"],dups[dups$julianDate==127,"total.stem.len"])
# abline(lm.dups.len)
# title("Two observer teams on the same plants had pretty similar length measurements")
# text(30,60,"Adj R-squared:  0.9115, F-statistic: 93.67 on 1 and 8 DF,  p-value: 1.083e-05",cex=0.75)
# 
# lm.dups.area<-lm(dups[dups$julianDate==127,"total.stem.area"]~dups[dups$julianDate==129,"total.stem.area"])
# summary(lm.dups.area)
# plot(dups[dups$julianDate==129,"total.stem.area"],dups[dups$julianDate==127,"total.stem.area"])
# abline(lm.dups.area)
# title("Two observer teams on the same plants had pretty similar area measurements")
# text(60,10,"Adj R-squared:  0.7246, F-statistic: 24.68 on 1 and 8 DF,  p-value: 0.001096",cex=0.75)

#plant size by week
total.stem.len.by.week<-aggregate(total.stem.len~week,function(x) c(mean=mean(x),se=std.err(x)),data=data)
total.stem.area.by.week<-aggregate(total.stem.area~week,function(x) c(mean=mean(x),se=std.err(x)),data=data);total.stem.area.by.week

#plant growth
p3.limits <- aes(ymax = total.stem.len[,"mean"] + total.stem.len[,"se"], ymin=total.stem.len[,"mean"] - total.stem.len[,"se"])
p3<-ggplot(total.stem.len.by.week,aes(x=week,y=total.stem.len[,"mean"]))
p3 <- p3+geom_point(col="forestgreen")+geom_line()+scale_x_continuous(breaks=c(1:max(data$week)))+ylab("total stem per plant (cm)")+geom_errorbar(p3.limits, width=0.2)

#catepillar growth
catLength.by.week<- as.data.frame(with( data, tapply(catLengths , week ,function(x) mean(as.numeric(unlist(x)), na.rm=TRUE))))
catLength.by.week$week <- 1:nrow(catLength.by.week)
colnames(catLength.by.week)[1]<-"mean"
p4 <- ggplot(data=catLength.by.week)
p4 <- p4+geom_point(aes(x=week, y=catLength.by.week[,"mean"]), col="purple")
p4 <- p4+scale_x_continuous(breaks=c(1:max(data$week)))+ylab("avg catepillar (mm)") + geom_line(aes(x=catLength.by.week[,"week"], y=catLength.by.week[,"mean"]))

#plot egg counts by week
eggs.by.week<-aggregate(nEggs~week,sum,data=data)
p5 <- ggplot(eggs.by.week, aes(x=week, y=nEggs))
p5 <- p5+geom_point(col="red")+geom_line()+scale_x_continuous(breaks=c(1:max(data$week)))+ylab("Eggs per week")

#plot larvae counts by week
larvae.by.week<-aggregate(nLTotal~week,sum,data=data)
p6 <- ggplot(larvae.by.week, aes(x=week, y=nLTotal))
p6 <- p6+geom_point(col="red")+geom_line()+geom_hline(yintercept=318,lty='dashed')+coord_cartesian(ylim = c(0, 50))+
  scale_x_continuous(breaks=c(1:max(data$week)))+ylab("Larvae per week")

#put weekly plots together for overall report
weekSummPlots <- list(p1,p2,p3,p5,p6,p4) 


#plot phenology-ontogeny landscape plotting

cat.length <- as.numeric(tapply(data$catLengths ,  cut(data$date.time, "2 weeks") ,    
                                function(x) mean(as.numeric(unlist(x)), na.rm=TRUE)))

cat.se <-  as.numeric(tapply(data$catLengths ,  cut(data$date.time, "2 weeks") ,    
                             function(x) std.err(as.numeric(unlist(x)))))

plant.area <- as.numeric(tapply(data$total.stem.area ,  cut(data$date.time, "2 weeks") ,    
                                function(x) mean(as.numeric(x), na.rm=TRUE)))

plant.se <-  as.numeric(tapply(data$total.stem.area ,  cut(data$date.time, "2 weeks") ,    
                               function(x) std.err(as.numeric(x))))

dates <- as.character(format( as.Date(sort(unique(cut(data$date.time, "2 weeks")))) , format = "%m-%d"))

phenOntD <- as.data.frame(cbind.data.frame(cat.length, cat.se, plant.area, plant.se, dates))
#this object is plotted directly in overall report script
p7 <- ggplot(data = phenOntD, aes(x = cat.length, y = plant.area)) + geom_path() + geom_point(col = "forestgreen") +ggtitle("Phenology Ontogeny landscape")
p7 <- p7+geom_label(label = dates)


#mapping and spatial analysis
#go up a directory for spatial data

mw.locs$transect<-as.factor(mw.locs$transect)

lowerleftlong=-121.765
lowerleftlat= 38.566483
upperrightlong=-121.745
upperrightlat= 38.576428

#NDC<-get_map(location=c(lon=-121.755162,lat=38.570702),zoom=15, maptype="terrain",source="google")

NDC<-get_map(location=c(lowerleftlong, lowerleftlat, upperrightlong, upperrightlat), maptype="terrain",source="stamen")
NDCmap<-ggmap(NDC, extent = "panel")

#just the ones seen last week

nw.locs.previous.week <- mw.locs[match(unique(data[data$week==max(data$week)-1 ,"milkweed.ID" ]) , mw.locs$name) , ]
nw.summ.by.plant.previous.week<-data[data$week==max(data$week)-1,c("total.stem.len","total.stem.area","milkweed.ID","nEggs","nLTotal","monarchLoad")]

nw.locs.max.week <- mw.locs[match(unique(data[data$week==max(data$week) ,"milkweed.ID" ]) , mw.locs$name) , ]
nw.summ.by.plant.max.week<-data[data$week==max(data$week),c("total.stem.len","total.stem.area","milkweed.ID","nEggs","nLTotal","monarchLoad")]


#this object is plotted directly in overall report script

p8.1<-NDCmap+geom_point(aes(x=longitude,y=latitude,color=nw.summ.by.plant.previous.week$monarchLoad, size=nw.summ.by.plant.previous.week$total.stem.len, alpha=0.5),data=nw.locs.previous.week)+theme(legend.position="bottom")+ggtitle("Milkweeds surveyed in the previous week")+scale_color_continuous(name = "number of monarchs obs per plant",low="green",high="red")+scale_size_continuous(name = "total stem length")+guides(alpha=F)

p8.2<-NDCmap+geom_point(aes(x=longitude,y=latitude,color=nw.summ.by.plant.max.week$monarchLoad, size=nw.summ.by.plant.max.week$total.stem.len, alpha=0.5),data=nw.locs.max.week)+theme(legend.position="bottom")+ggtitle("Milkweeds surveyed last week")+scale_color_continuous(name = "number of monarchs obs per plant",low="green",high="red")+scale_size_continuous(name = "total stem length")+guides(alpha=F)

#time of observations: day of week, time of day
p9 <- qplot(factor(weekdays(data$date.time),levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),xlab="day of the week")
p10 <- qplot(data$date.time$hour, xlab="hour of the day")
timePlots <- list(p9, p10)

# create overall report ---------------------------------------------------


#print report
setwd("reports")
render(input = "overall_report.Rmd", output_format = "pdf_document",
       output_file = paste(format(Sys.time(), "%m-%d-%Y"), " overall report.pdf", sep = "" ))
setwd("..")

proc.time()-start
