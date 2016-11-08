#load required packages
library(RMySQL)
library(plyr)
library(lubridate)
library(tidyr)
library(gplots)

#connecto to db
db <- dbConnect(MySQL(), user='root', password='root', dbname='bike', host='localhost')

#read required tables
weather <- dbReadTable(db, "weather")
station <- dbReadTable(db, "state")


########################################### WEATHER CLUSTER #####################################################
# clean and relabel factors of variable Events
weather$Events<-as.factor(weather$Events)
weather$Events <- revalue(weather$Events, c("rain" = "Rain"))
weather$Events <- revalue(weather$Events, c("Fog" = "2"))
weather$Events <- revalue(weather$Events, c("Fog-Rain" = "3"))
weather$Events <- revalue(weather$Events, c("Rain" = "4"))
weather$Events <- revalue(weather$Events, c("Rain-Thund" = "5"))
weather$Events <- factor(as.character(weather$Events), levels=c("1","2","3", "4", "5")) 
weather$Events[is.na(weather$Events)] <- 1
weather$Events <- as.numeric(weather$Events) 

#create weather clusters based on variable Events
target.v <- weather$Events

#compute distance between target variables 
distances <- dist(target.v, method="euclidean")
clusterweather1 <- hclust(distances, method="ward.D")
plot(clusterweather1)

#set k=number of clusters and label
clustergroups1 <- cutree(clusterweather1, k=3) 

#append cluster groups to dataset
cluster.group<-as.data.frame(clustergroups1)
complete.weather.df <- cbind(weather, cluster.group)

#bookmark
write.table(df.weather, file="FINAL.weather.csv", row.names=FALSE, col.names=TRUE, sep=",")








########################################### STATION CLUSTER #####################################################
#separate date and time
station2<-station %>%
  separate(time, into =c("Dates", "Times"), sep="\\ ")

#subset 2015~2015 data
station2$Dates<-ymd(station2$Dates)
station2<- with(station2, subset(station2, station2$Dates > "2014/09/01")) 

#create new variables: day and month
station2$day<-wday(station2$Dates) #sunday=1
station2$month<-month(station2$Dates)

#remove seconds from Times
station2$Times = substr(station2$Times,1,nchar(station2$Times)-3)

#compute bike availability
station2$Pct.avail <- station2$bikes_available / (station2$bikes_available + station2$docks_available)
station2$Pct.avail <- round(station2$Pct.avail, digits=2)

#subset weekdays 
station2.dt <- as.data.table(station2)
station2.weekday<- station2.dt[day %in% c("2","3","4","5", "6")]

#extract station_id vector
stationid_vector<-unique(station2.dt$station_id)

#group by station id and compute avg pct.avail per time
##weekday subset
weekday.sub<-tapply(station2.weekday$Pct.avail, list(station2.weekday$station_id, station2.weekday$Time), mean)

#remove columns with NA
#select every 5th column
weekday.sub<-weekday.sub[,colMeans(is.na(weekday.sub)) == 0]  
weekday.sub<-weekday.sub[,seq(1, ncol(weekday.sub), by = 5)]

#add stationid vector
weekday.sub<-as.data.frame(weekday.sub)
weekday.sub$stationid<-stationid_vector

#svd decomposition, excluding station id
weekday.sub2<-as.matrix(weekday.sub[,-270])

#svd
weekday.svd<-svd(weekday.sub2)

#plot variance as per eigenvalues
plot(cumsum(weekday.svd$d^2/sum(weekday.svd$d^2)))

#reconstruct using X PCA's
pc.use <- 2
weekday.recon <- weekday.svd$u[,pc.use] %*% diag(weekday.svd$d[pc.use], length(pc.use), length(pc.use)) %*% t(weekday.svd$v[,pc.use])

#cluster
weekday.PCA <- weekday.recon[,1:pc.use]
weekday.distances <- dist(weekday.PCA, method="euclidean")
#### ward method factors in distance between centroids and variance within centroid
weekday.cluster <- hclust(weekday.distances, method="ward.D")
plot(weekday.cluster)
#### label data points according to their cluster group
weekday.cluster.vector <- cutree(weekday.cluster, k=4)

#append cluster groups to dataset
weekday.cluster.vector<-as.data.frame(weekday.cluster.vector)
df.weekday<- cbind(weekday.sub, weekday.cluster.vector)

#order data by cluster number 4,2,1,3
df.weekday$weekday.cluster.vector <- factor(df.weekday$weekday.cluster.vector, levels=c("4", "2", "1", "3")) 
df.weekday<-df.weekday[order(df.weekday$weekday.cluster.vector),]

#move stationid column to beginning and delete cluster column
df.weekday<-df.weekday[c(270, 1:269, 271)]

#bookmark
#write.table(df.weekday, file="FINALweekday.csv", row.names=FALSE, col.names=TRUE, sep=",")

#for heatmap visualiation purposes
#show only hourly column names
time<-as.character(seq(ISOdatetime(2001,2,3,0,0,0), ISOdatetime(2001,2,4,0,0,0), by=(3600)))
time2 <- substr(time,12,nchar(time)-3)
time3 <- time2[-length(time2)]
time3[8] <- "06:55"
DFfiltered <- df.weekday[,(names(df.weekday) %in% time3)]

names(DFfiltered)[1:7] <- ""
names(DFfiltered)[8] <- "07:00"
names(DFfiltered)[9:16] <- ""
names(DFfiltered)[18:23] <- ""

#generate heatmap
matrix.weekday<-data.matrix(DFfiltered)
bk<-seq(0,1, length=1000)
heatmap.2(matrix.weekday[,-271], keysize= 1.5, col=colorRampPalette(c("red","white", "green")), Rowv=FALSE, Colv=FALSE, breaks=bk, trace = "none", density.info = "density", denscol="black", xlab = "Weekday Time (24 Hour Period)", ylab = "Bike Station ID", main = "2015 Bike Availability during Weekdays (% Fullness)")

#display stationid's cluster group
df.weekday$stationid[which(df.weekday$weekday.cluster.vector==1)]
df.weekday$stationid[which(df.weekday$weekday.cluster.vector==2)]
df.weekday$stationid[which(df.weekday$weekday.cluster.vector==3)]
df.weekday$stationid[which(df.weekday$weekday.cluster.vector==4)]

#> df.weekday$stationid[which(df.weekday$weekday.cluster.vector==1)]
#[1]  2  3  7  8  9 16 21 22 23 24 26 29 30 33 34 35 36 49 58 59 61 66 67 71 83
#> df.weekday$stationid[which(df.weekday$weekday.cluster.vector==2)]
#[1]  4 27 41 42 45 47 48 51 63 68 75 76 82
#> df.weekday$stationid[which(df.weekday$weekday.cluster.vector==3)]
#[1]  5  6 10 11 12 13 14 25 31 32 37 38 56 57 60 62 64 65 77 80
#> df.weekday$stationid[which(df.weekday$weekday.cluster.vector==4)]
#[1] 28 39 46 50 54 55 69 70 72 73 74 84






########################################### PREPARE FOR LOGIT REGRESSION #####################################################
#prepare dataset
station3<-station2.weekday

#remove minutes from Times
station3$Times = substr(station3$Times,1,nchar(station3$Times)-3)

#take every 15th minute
station3<-station3[seq(1, nrow(station3), by = 15),]

#create DV
station3$available <- with(station3, bikes_available < 6)
station3$available <- as.character(as.integer(station3$available)) #0=FALSE, 1=TRUE

#append weather clusters and mean temp
station4<-station3
station4$Dates<-as.character(station4$Dates)

station4$weather.clust <- complete.weather.df$clustergroups1[match(station4$Dates, complete.weather.df$Date)]
station4$temp <- complete.weather.df$Mean_Temperature_F[match(station4$Dates, complete.weather.df$Date)]

#change F temp to Celcius
station4$temp <- (station4$temp -32) * (5/9)
station4$temp <- round(station4$temp, digits=0)

#append station clusters
station4$station.clust<- rep(0, nrow(station4))
station4$station.clust <- df.weekday$weekday.cluster.vector[match(station4$station_id, df.weekday$stationid)]

#remove unnecessary variables and rearrange columns
station5<-station4
station5[, c(4, 5, 9)]<-NULL
station5<-as.data.frame(station5)
station5<-station5[,c(1, 7, 2, 10, 3, 6, 5, 4, 8, 9)]
station5<-na.omit(station5)

#coerce to numeric class for uploading to sql
station5$available<-as.numeric(station5$available)
station5$station_id<-as.numeric(station5$station_id)
station5$station.clust<-as.numeric(station5$station.clust)
station5$month<-as.numeric(station5$month)
station5$day<-as.numeric(station5$day)
station5$weather.clust<-as.numeric(station5$weather.clust)
station5$Times<-as.factor(station5$Times)
station5$Times <- revalue(station5$Times, c("00" = "0", "01" = "1", "02" = "2","03" = "3","04" = "4","05" = "5","06" = "6","07" = "7","08" = "8","09" = "9"))
station5$Times<- as.integer(station5$Times)
station5$Times<-station5$Times -1

final<-station5

#bookmark
#write.table(final, file="FINALregression_dataset.csv", row.names=FALSE, col.names=TRUE, sep=",")

