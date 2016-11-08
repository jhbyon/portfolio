library(RMySQL); library(MASS); library(ggplot2); library(plyr); library(reshape); library(scales); library(foreach); library(lubridate); library(mclust)

########################################### LOAD & CLEAN ###########################################

#STATE
db <- dbConnect(MySQL(), user='root', password='', dbname='bike', host='localhost')
state <- fetch(dbSendQuery(db, 'select * from State'), n=-1) #data aggregated via MySQL; the 'STATE' table was by minute, 'State' is by hour

#TIME
state$time <- strptime(as.character(state$time), '%Y-%m-%d %H')
state$hour <- as.factor(as.POSIXlt(state$time)$hour)
state$day <- as.POSIXlt(state$time)$mday
state$dayw <- as.factor(state$time$wday)
state$month <- as.factor(state$time$mon+1)
state$year <- as.factor(state$time$year+1900)

#GET ZIP CODE
station <- fetch(dbSendQuery(db, 'select landmark, station_id from STATION'), n=-1)
station$landmark <- mapvalues(station$landmark, 
	from = c('San Francisco', 'Redwood City', 'Palo Alto', 'Mountain View', 'San Jose'), 
	to = c('94107', '94063', '94301', '94041', '95113'))
station <- rename(station, c(landmark='zip'))
state <- join(state, station, by='station_id')

#MERGE WEATHER
weather <- fetch(dbSendQuery(db, 'select Date, Max_Temperature_F, Mean_Temperature_F, 
	Min_TemperatureF, Max_Dew_Point_F, MeanDew_Point_F, Min_Dewpoint_F, Max_Humidity,
	Mean_Humidity, Min_Humidity, Max_Sea_Level_Pressure_In, Mean_Sea_Level_Pressure_In,
	Min_Sea_Level_Pressure_In, Max_Visibility_Miles, Mean_Visibility_Miles, Min_Visibility_Miles,
	Max_Wind_Speed_MPH, Mean_Wind_Speed_MPH, Max_Gust_Speed_MPH, Precipitation_In,
	Cloud_Cover, Events, zip
	from WEATHER'), n=-1)
#cl <- Mclust(weather[, c(-1,-21, -22, -23)], 5) #not effective in the analysis, but cool package to explore
weather <- cbind(cluster = NA, weather)
weather$zip <- gsub('\r', '', as.character(weather$zip))
weather$Cloud_Cover <- as.factor(weather$Cloud_Cover)
weather$Events <- as.factor(gsub('R', 'r', weather$Events))
weather$cluster <- as.factor(weather$cluster)
state$Date <- substr(as.character(state$time), 1, 10)
state <- join(state, weather, by=c('Date','zip'))

#RENAME VARIABLES
state <- rename(state, c(
	station_id='station',
	bikes_available='bikes',
	Max_Temperature_F='maxtemp',
	Mean_Temperature_F='meantemp',
	Min_TemperatureF='mintemp', 
	Max_Dew_Point_F='maxdew', 
	MeanDew_Point_F='meandew', 
	Min_Dewpoint_F='mindew', 
	Max_Humidity='maxhum',
	Mean_Humidity='meanhum', 
	Min_Humidity='minhum', 
	Max_Sea_Level_Pressure_In='maxsea', 
	Mean_Sea_Level_Pressure_In='meansea',
	Min_Sea_Level_Pressure_In='minsea',
	Max_Visibility_Miles='maxvis', 
	Mean_Visibility_Miles='meanvis',
	Min_Visibility_Miles='minvis', 
	Max_Wind_Speed_MPH='maxwin', 
	Mean_Wind_Speed_MPH='meanwin',
	Max_Gust_Speed_MPH='maxgus', 
	Precipitation_In='precip', 
	Cloud_Cover='cloud', 
	Events='event'
	))

#MEAN NB BIKES GIVEN HOUR AND AREA
mv <- c(27, 28, 29, 30, 31, 32, 33)
pa <- c(34, 35, 36, 37, 38)
rw <- c(21, 22, 23, 24, 25, 26, 83)
sf <- c(39, 41, 42, 45, 46, 47, 48, 49, 50, 51, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 82)
sj <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 80, 84)
state$city <- mapvalues(state$station, from = c(mv,pa,rw,sf,sj), to = c(rep('mv',length(mv)),rep('pa',length(pa)),rep('rw',length(rw)),rep('sf',length(sf)),rep('sj',length(sj))))
av_city_hour <- aggregate(bikes~city+hour+day+month+year, state, mean)

state <- join(state, av_city_hour, by=c('city','hour','day','month','year'))
colnames(state)[34] <- 'avbikes'

########################################### ANALYSIS ###########################################

d <- state

d <- d[which(d$station!=84),] #station too recent for the training set
d <- d[which(d$station!=83),]
d <- d[which(d$station!=82),]
d <- d[which(d$station!=80),]

d$time <- strptime(as.character(d$time), '%Y-%m-%d %H:%M:%S')
d$hour <- as.factor(d$time$hour)
d$day <- as.factor(d$time$mday)
d$dayw <- as.factor(d$time$wday)
d$month <- as.factor(d$time$mon+1)
d$year <- as.factor(d$time$year+1900)
d$cluster <- as.factor(d$cluster)
d$cloud <- as.factor(d$cloud)
d$event <- mapvalues(d$event, from = c('','Fog','Fog-rain','rain-Thund','rain'), to = c('','fog','fog','rain','rain'))
d$event <- as.factor(d$event)

stations <- names(table(d$station))[c(10, 39, 19, 23, 8)] #"typical" stations (1 per district, except San Francisco which has 2) used for cross-validation an plotting

#MODELS
dm <- formula(bikes~dayw+month)
hdm <- formula(bikes~hour+dayw+month)
hdmi <- formula(bikes~hour+dayw+month+dayw:hour+dayw:month)
hdmiw <- formula(bikes~hour+dayw+month+avbikes+hour:dayw+dayw:month+event+precip+meantemp+mintemp+maxtemp+meandew+meanvis+minvis+maxwin+cloud+maxhum)
hdmiwid <- formula(bikes~hour+dayw+month+avbikes+hour:dayw+dayw:month+event+precip+meantemp+mintemp+maxtemp+meandew+meanvis+minvis+maxwin+cloud+maxhum+event:dayw+precip:dayw+meantemp:dayw+mintemp:dayw+maxtemp:dayw+meandew:dayw+meanvis:dayw+minvis:dayw+maxwin:dayw+cloud:dayw+maxhum:dayw)
hdmiwim <- formula(bikes~hour+dayw+month+avbikes+hour:dayw+dayw:month+event+precip+meantemp+mintemp+maxtemp+meandew+meanvis+minvis+maxwin+cloud+maxhum+event:month+precip:month+meantemp:month+mintemp:month+maxtemp:month+meandew:month+meanvis:month+minvis:month+maxwin:month+cloud:month+maxhum:month)
hdmiwidm <- formula(bikes~hour+dayw+month+avbikes+hour:dayw+dayw:month+event+precip+meantemp+mintemp+maxtemp+meandew+meanvis+minvis+maxwin+cloud+maxhum+event:dayw+precip:dayw+meantemp:dayw+mintemp:dayw+maxtemp:dayw+meandew:dayw+meanvis:dayw+minvis:dayw+maxwin:dayw+cloud:dayw+maxhum:dayw+event:month+precip:month+meantemp:month+mintemp:month+maxtemp:month+meandew:month+meanvis:month+minvis:month+maxwin:month+cloud:month+maxhum:month)

#MEAN ABSOLUTE ERROR BY CROSS VALIDATION
# MAE <- function(mod) { 
# 	train <- d[which(d$time$year+1900 < 2015),]
# 	MAE <- c()
# 	for (j in stations) {
# 		st <- train[which(train$station==j),]
# 		st <- st[order(st$bikes),]
# 		n <- nrow(st)
# 		s <- split(1:n, ceiling(1:n/(n/5)))
# 		mae <- c()
# 		for(i in 1:5){
# 			t <- s[[i]]
# 			cv <- st[t,]
# 			CV <- st[setdiff(unlist(s), t),]
# 			reg <- glm.nb(mod, data = CV)
# 			cv$pred <- round(exp(predict(reg, cv, type='link')), 0)
# 			mae <- c(mae, abs(cv$pred-cv$bikes))
# 		}
# 		MAE <- c(MAE, mean(mae))
# 	}
# 	c(mean(MAE), nrow(as.matrix(coef(reg))))
# }
# matMAE <- matrix(, 7, 2)
# matMAE[1,] <- MAE(dm) #most regressions will return the warning that the research the optimal theta broke the time limit. We do not touch that now
# matMAE[2,] <- MAE(hdm)
# matMAE[3,] <- MAE(hdmi)
# matMAE[4,] <- MAE(hdmiw)
# matMAE[5,] <- MAE(hdmiwid)
# #matMAE[6,] <- MAE(hdmiwim) takes too much time, and produces error values such that the plot is hardly useful
# #matMAE[7,] <- MAE(hdmiwidm)
# 
# png('/home/thomas/Dropbox/DS/compLab/project/MAE.png')
# plot(matMAE[,2], matMAE[,1], pch=19, xlab='# features', ylab='|actual - predicted|', bty='n')
# lines(matMAE[,2], matMAE[,1])
# dev.off()

#We chose our model, 'hdmiw', we will produce predictions for all stations, and plot the stations from the cross-validation sample.

#STORE PREDICTIONS IN TABLE
# stations <- names(table(d$station)) #predictions will be for all stations

tabpred <- function(stations) {
	PRED <- data.frame(station=NA, bikes=NA, hour=NA, day=NA, month=NA, year=NA, pred=NA)
	start <- 1
	for (j in stations){
		st <- d[which(d$station==j),]
		train <- st[which(st$time$year+1900 < 2015),]
		reg <- glm.nb(hdmiw, data = train)
		st$pred <- round(exp(predict(reg, st, type='link')), 0)
		PRED[start:(start+nrow(st)-1),] <- st[,c(1,2,4,5,7,8,35)]
		start <- start + nrow(st)
	}
	PRED$year <- PRED$year+2012
	PRED$time <- ISOdatetime(PRED$year,as.numeric(PRED$month),PRED$day,PRED$hour,0,0)
	PRED <<- PRED[,c(1,2,7,8)]
}
# tabpred(stations)

#MERGE COORDINATES
# co <- fetch(dbSendQuery(db, 'select * from STATION'), n=-1)[,c(1,3,4)]
# co <- rename(co, c(station_id='station'))
# PRED <- join(PRED, co, by=c('station'))

#CREATE TABLE IN SQL BASE
# dbRemoveTable(db, 'PRED')
# dbWriteTable(db, 'PRED', PRED, row.names=F)

#PLOTTING

stations <- names(table(d$station))[c(10, 39, 19, 23, 8)] #back to the subsample for the dashboard
tabpred(stations)

for (j in stations){
	st <- PRED[which(PRED$station==j),]
	
	##HISTOGRAM
	stTest <- st[which(year(st$time)>2014),]
	absdif <- abs(stTest$bikes-stTest$pred) #we chose to round the predictions to get and integer number of bikes
	png(paste0('/home/thomas/Dropbox/DS/compLab/project/hist',j,'.png'))
	h <- hist(absdif)
	h$density <- h$counts/sum(h$counts)*100
	plot(h,freq=F,main='error in #bikes prediction',xlab='|actual - predicted|',ylab='percentage')
	mtext(paste('average absolute error:', round(mean(absdif), 2)))
	dev.off()

	##ACTUAL VS PREDICTED
	p <- ggplot(st, aes(time, pred)) +
		geom_point(aes(y=bikes), colour='Blue', size = 3, alpha = .02) +
		geom_point(colour='Red', size = 2, alpha = .02) +
		labs(x = '', y = '') +
		geom_vline(xintercept=1420066800, linetype='longdash', colour='black') 	#line delimiting the test sample
	ggsave(paste0('/home/thomas/Dropbox/DS/compLab/project/scatter', j,'.png'),plot=p,w=6,h=4)
}

##CUMULATIVE DISTRIBUTION OF ABSOLUTE ERROR
train <- PRED[which(year(PRED$time)>2014),]
dif <- abs(train$bikes-train$pred)
png('/home/thomas/Dropbox/DS/compLab/project/cumdistr_err.png')
plot(ecdf(dif), bty = 'n', xlab='|actual - predicted|', ylab='', main='')	
dev.off()
mean(dif)