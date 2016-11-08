#load required packages
library(RMySQL)
library(gplots)
library(ROCR)
library(gridExtra)

#connecto to db
db <- dbConnect(MySQL(), user='root', password='root', dbname='bike', host='localhost')

#read required tables
final<- dbReadTable(db, "LOGIT_ANALYSIS")

##################################### LOGIT REGRESSION ####################################################
#prepare variable class for regression
final$available<-as.numeric(final$available)
final$station_id<-as.factor(final$station_id)
final$station.clust<-as.factor(final$station.clust)
final$month<-as.factor(final$month)
final$day<-as.factor(final$day)
final$Times<-as.factor(final$Times)
final$weather.clust<-as.factor(final$weather.clust)
str(final)


################################################# model selection
############################marginal deviance analysis

unique.cluster<-unique(final$station.clust)

pred.dev<-matrix(0, 5, 4)
colnames(pred.dev)<-c("Undefined-Out", "Commute-Home", "Undefined-In", "Commute-Work")
rownames(pred.dev)<-c("Hour", "Month", "Day", "Weather", "Temp")


for ( i in 1:length(unique.cluster)){
  fit<-glm(available~Times, family="binomial"(link="logit"), data=final, subset=station.clust==i)
  pred.dev[1, i]<-round(fit$deviance, digits=0)
}  

for ( i in 1:length(unique.cluster)){
  fit<-glm(available~month, family="binomial"(link="logit"), data=final, subset=station.clust==i)
  pred.dev[2, i]<-round(fit$deviance, digits=0)
}

for ( i in 1:length(unique.cluster)){
  fit<-glm(available~day, family="binomial"(link="logit"), data=final, subset=station.clust==i)
  pred.dev[3, i]<-round(fit$deviance, digits=0)
}

for ( i in 1:length(unique.cluster)){
  fit<-glm(available~weather.clust, family="binomial"(link="logit"), data=final, subset=station.clust==i)
  pred.dev[4, i]<-round(fit$deviance, digits=0)
} 

for ( i in 1:length(unique.cluster)){
  fit<-glm(available~temp, family="binomial"(link="logit"), data=final, subset=station.clust==i)
  pred.dev[5, i]<-round(fit$deviance, digits=0)
}  

#plot marginal deviance analysis table
par(mfrow=c(1,1))

pred.dev.format<-pred.dev
pred.dev.format<-pred.dev.format[, c(4,2,1,3)]
pred.dev.format <- format(pred.dev.format, format="d", big.mark=",")
grid.table(pred.dev.format)


##############################model deviance analysis

pred.dev2<-matrix(0, 6, 4)
colnames(pred.dev2)<-c("Undefined-Out", "Commute-Home", "Undefined-In", "Commute-Work")
rownames(pred.dev2)<-c("Null", "Hour", "Hour + Month", "Hour + Month + day", "Hour + Month + day + Weather", "Hour + Month + day + Weather + Temp")


#Null
for ( i in 1:length(unique.cluster)){
  fit<-glm(available~1, family="binomial"(link="logit"), data=final, subset=station.clust==i)
  pred.dev2[1,i] <- round(fit$deviance, digits=0)
}


#times
for ( i in 1:length(unique.cluster)){
  fit<-glm(available~Times, family="binomial"(link="logit"), data=final, subset=station.clust==i)
  pred.dev2[2, i]<-round(fit$deviance, digits=0)
}

#check chi square test
chi.test1<-pred.dev2[1,] - pred.dev2[2,]
chi.test1a<- 1-pchisq(chi.test1, 23)
chi.test1a<-round(chi.test1a, digits=4)

#times+month
for ( i in 1:length(unique.cluster)){
  fit<-glm(available~Times+month, family="binomial"(link="logit"), data=final, subset=station.clust==i)
  pred.dev2[3, i]<-round(fit$deviance, digits=0)
}

#check chi square test
chi.test2<-pred.dev2[2,] - pred.dev2[3,]
chi.test2a<- 1-pchisq(chi.test2, 11)
chi.test2a<-round(chi.test2a, digits=4)

#times+month+day
for ( i in 1:length(unique.cluster)){
  fit<-glm(available~Times+month+day, family="binomial"(link="logit"), data=final, subset=station.clust==i)
  pred.dev2[4, i]<-round(fit$deviance, digits=0)
}

#check chi square test
chi.test3<-pred.dev2[3,] - pred.dev2[4,]
chi.test3a<-1-pchisq(chi.test3, 4)
chi.test3a<-round(chi.test3a, digits=4)

#times+month+day+weather
for ( i in 1:length(unique.cluster)){
  fit<-glm(available~Times+month+day+weather.clust, family="binomial"(link="logit"), data=final, subset=station.clust==i)
  pred.dev2[5, i]<-round(fit$deviance, digits=0)
}

#check chi square test
chi.test4<-pred.dev2[4,] - pred.dev2[5,]
chi.test4a<-1-pchisq(chi.test4, 2)
chi.test4a<-round(chi.test4a, digits=4)

#times+month+day+weather+temp
for ( i in 1:length(unique.cluster)){
  fit<-glm(available~Times+month+day+weather.clust+temp, family="binomial"(link="logit"), data=final, subset=station.clust==i)
  pred.dev2[6, i]<-round(fit$deviance, digits=0)
}

#check chi square test
chi.test5<-pred.dev2[5,] - pred.dev2[6,]
chi.test5a<-1-pchisq(chi.test5, 1)
chi.test5a<-round(chi.test5a, digits=4)

#plot model deviance analysis table
pred.dev2.format<-pred.dev2
pred.dev2.format<-pred.dev2.format[, c(4,2,1,3)]
pred.dev2.format <- format(pred.dev2.format, format="d", big.mark=",")
row.names(pred.dev2.format) <- NULL
grid.table(pred.dev2.format)

#plot chi square test table
blank<-rep("-", 4)
chitest<-rbind(blank, chi.test1a, chi.test2a, chi.test3a, chi.test4a, chi.test5a)
row.names(chitest)<-c("Null", "Hour", "Hour + Month", "Hour + Month + day", "Hour + Month + day + Weather", "Hour + Month + day + Weather + Temp")
df<-c("-", "23", "11", "4", "2", "1")

chitest<-cbind(chitest, df)
chitest.format<-chitest
chitest.format<-chitest.format[, c(4,2,1,3, 5)]
chitest.format <- format(chitest.format, format="d", big.mark=",")
grid.table(chitest.format)

###########################################check collinearity
full1<-glm(available~Times+month+day+weather.clust+temp, family="binomial"(link="logit"), data=final, subset=station.clust==1)
full2<-glm(available~Times+month+day+weather.clust+temp, family="binomial"(link="logit"), data=final, subset=station.clust==2)
full3<-glm(available~Times+month+day+weather.clust+temp, family="binomial"(link="logit"), data=final, subset=station.clust==3)
full4<-glm(available~Times+month+day+weather.clust+temp, family="binomial"(link="logit"), data=final, subset=station.clust==4)

#plot table of vif(full4)
vif.table<-vif(full4)
row.names(vif.table) <- c("Hour", "Month", "Day", "Weather", "Temp")
vif.table<-round(vif.table, digits=3)
vif.table <- format(vif.table, format="d", big.mark=",")

grid.table(vif.table)



########################################### statistical signif analysis
full.fit1<-glm(available~Times+month+day+weather.clust, family="binomial"(link="logit"), data=final, subset=station.clust==1)
full.fit2<-glm(available~Times+month+day+weather.clust, family="binomial"(link="logit"), data=final, subset=station.clust==2)
full.fit3<-glm(available~Times+month+day+weather.clust, family="binomial"(link="logit"), data=final, subset=station.clust==3)
full.fit4<-glm(available~Times+month+day+weather.clust, family="binomial"(link="logit"), data=final, subset=station.clust==4)


#create sig level heatmap
#extract z and p values

siglevel<-matrix(0, length(full.fit1$coefficients), 4)
colnames(siglevel) <- c("Undefined-Out", "Commute-Home", "Undefined-In", "Commute-Work")
rownames(siglevel) <- c(names(full.fit1$coefficients))

for (i in 1:length(unique.cluster)){
  reg<- glm(available~Times+month+day+weather.clust, family="binomial"(link="logit"), data=final, subset=station.clust==i)
  sign<- sign(coef(summary(reg))[,3])
  p.value<-coef(summary(reg))[,4]
  p.value.signed<-sign * p.value
  siglevel[,i] <- p.value.signed
}

# plot heatmap
siglevel2<-data.matrix(siglevel)

# manually recover signs for values that have been automatically equalized to zero
which(siglevel2[,1]==0)
siglevel2[1,1] <- -1e-10
siglevel2[28,1] <- 1e-10

x<-which(siglevel2[,2]==0)
siglevel2[x, 2] <- -1e-10

which(siglevel2[,3]==0)
siglevel2[1,3] <- -1e-10

which(siglevel2[,4]==0)
siglevel2[1, 4] <- -1e-10
y<-which(siglevel2[,4]==0)
siglevel2[y, 4] <- 1e-10

#rearrange columns
siglevel2<-siglevel2[, c(4,2,1,3)]

#plot
par(mfrow=c(1,1))

bk = c(-1, -0.05, 0, 0.05, 1)
heatmap.2(siglevel2, key=FALSE, cexRow=1, cexCol=1.2, lhei = c(0.05,0.95), margins=c(10,7), col=c("black", "green", "red", "black"), Rowv=FALSE, Colv=FALSE, breaks=bk, trace = "none", ylab = "Predictors", main = "Heatmap of Statistical Significance")










###########################################  naive forecast benchmark 1 (1 hr lag) ############################################3
#initiate loop
unique<-unique(final$station_id)
list<-list(NULL)

# naive benchmark 1 (1 hour lag)
for(i in 1:length(unique)){
  sub<-final[which(final$station_id==unique[i]),]
  
  nrow<-nrow(sub)
  storage <- sub$available
  storage2<-rep(0, nrow)
  
  storage2[1:4] <- storage[(nrow-3):nrow]
  storage2[5:nrow] <- storage[1:(nrow-4)]
  
  list[[i]] <- storage2[1:nrow]
  
}


#check

prueba2<-final[which(final$station_id==2), ]
prueba.original<-prueba2$available
length(prueba.original)

prueba.forecast<-list[[1]]
length(prueba.forecast)

prueba.comb<-cbind(prueba.original, prueba.forecast)
head(prueba.comb, n=1000)

#calculate accuracy

naive.fit<-unlist(list)
appended<-cbind(naive.fit, final)

clusters<-unique(final$station.clust)
result<-rep(NA, length(clusters))

for(i in 1:length(clusters)){
  sub2<-appended[which(appended$station.clust==clusters[i]),]
  
  outcome<-sub2$naive.fit==sub2$available
  naive.accuracy<-table(outcome)["TRUE"] / (table(outcome)["TRUE"] + table(outcome)["FALSE"])
  result[i] <- naive.accuracy
}

naive1<-t(as.matrix(result))
colnames(naive1)<-c("Undefined-Out", "Commute-Home", "Undefined-In", "Commute-Work")
naive1






###########################################  naive forecast benchmark 2 (24 hr lag) ############################################3
list2<-list(NULL)

# naive benchmark 2 (24 hour lag)
for(i in 1:length(unique)){
  sub<-final[which(final$station_id==unique[i]),]
  
  nrow<-nrow(sub)
  storage <- sub$available
  storage2<-rep(0, nrow)
  
  storage2[1:96] <- storage[(nrow-95):nrow]
  storage2[97:nrow] <- storage[1:(nrow-96)]
  
  list2[[i]] <- storage2[1:nrow]
  
}


#check

prueba2<-final[which(final$station_id==2), ]
prueba.original<-prueba2$available
length(prueba.original)

prueba.forecast<-list2[[1]]
length(prueba.forecast)

prueba.comb<-cbind(prueba.original, prueba.forecast)
head(prueba.comb, n=1000)

#calculate accuracy

naive.fit2<-unlist(list2)
appended2<-cbind(naive.fit2, final)

result2<-rep(NA, length(clusters))

for(i in 1:length(clusters)){
  sub2<-appended2[which(appended2$station.clust==clusters[i]),]
  
  outcome<-sub2$naive.fit2==sub2$available
  naive.accuracy<-table(outcome)["TRUE"] / (table(outcome)["TRUE"] + table(outcome)["FALSE"])
  result2[i] <- naive.accuracy
}

naive2<-t(as.matrix(result2))
colnames(naive2)<-c("Undefined-Out", "Commute-Home", "Undefined-In", "Commute-Work")
naive2





############################################# PREDICTION LOGIT MODEL ########################################### 
#fit
clust1.fit<-glm(available~Times+month+day+weather.clust, family="binomial"(link="logit"), data=final, subset=station.clust==1)
clust2.fit<-glm(available~Times+month+day+weather.clust, family="binomial"(link="logit"), data=final, subset=station.clust==2)
clust3.fit<-glm(available~Times+month+day+weather.clust, family="binomial"(link="logit"), data=final, subset=station.clust==3)
clust4.fit<-glm(available~Times+month+day+weather.clust, family="binomial"(link="logit"), data=final, subset=station.clust==4)

clust1<-final[which(final$station.clust==1),]
clust2<-final[which(final$station.clust==2),]
clust3<-final[which(final$station.clust==3),]
clust4<-final[which(final$station.clust==4),]

clust1.pred<-predict(clust1.fit, type="response", newdata=clust1)
clust2.pred<-predict(clust2.fit, type="response", newdata=clust2)
clust3.pred<-predict(clust3.fit, type="response", newdata=clust3)
clust4.pred<-predict(clust4.fit, type="response", newdata=clust4)


# compute auc
rocr1<-prediction(clust1.pred, clust1$available)
rocr1.perf<-performance(rocr1, "tpr", "fpr")
rocr1.auc<-round(as.numeric(performance(rocr1, "auc")@y.values), digits=2)

rocr2<-prediction(clust2.pred, clust2$available)
rocr2.perf<-performance(rocr2, "tpr", "fpr")
rocr2.auc<-round(as.numeric(performance(rocr2, "auc")@y.values), digits=2)

rocr3<-prediction(clust3.pred, clust3$available)
rocr3.perf<-performance(rocr3, "tpr", "fpr")
rocr3.auc<-round(as.numeric(performance(rocr3, "auc")@y.values), digits=2)

rocr4<-prediction(clust4.pred, clust4$available)
rocr4.perf<-performance(rocr4, "tpr", "fpr")
rocr4.auc<-round(as.numeric(performance(rocr4, "auc")@y.values), digits=2)

auc.score<-c(rocr1.auc, rocr2.auc, rocr3.auc, rocr4.auc)
auc.score<-t(as.matrix(auc.score))
colnames(auc.score)<- c("Undefined-Out", "Commute-Home", "Undefined-In", "Commute-Work")

auc.score

#plot auc
par(mfrow=c(2,2))

plot(rocr4.perf, cex.main=2, lwd=5, main="Commute-Work (AUC=0.75)")
plot(rocr2.perf, cex.main=2, lwd=5, main="Commute-Home (AUC=0.72)")
plot(rocr1.perf, cex.main=2, lwd=5, main="Undefined-Out (AUC=0.60)")
plot(rocr3.perf, cex.main=2, lwd=5, main="Undefined-In (AUC=0.59)")

#compute accuracy

accuracy1<-table(clust1$available, clust1.pred>0.3)
accuracy1a<-(accuracy1[1, "FALSE"] + accuracy1[2, "TRUE"]) / nrow(clust1)
accuracy1a

accuracy2<-table(clust2$available, clust2.pred>0.5)
accuracy2a<-(accuracy2[1, "FALSE"] + accuracy2[2, "TRUE"]) / nrow(clust2)
accuracy2a

accuracy3<-table(clust3$available, clust3.pred>0.4)
accuracy3a<-(accuracy3[1, "FALSE"] + accuracy3[2, "TRUE"]) / nrow(clust3)
accuracy3a

accuracy4<-table(clust4$available, clust4.pred>0.4)
accuracy4a<-(accuracy4[1, "FALSE"] + accuracy4[2, "TRUE"]) / nrow(clust4)
accuracy4a

accuracy.mod<-c(accuracy1a, accuracy2a, accuracy3a, accuracy4a)

############################################## plot global accuracy table
accuracy.table<-rbind(naive1, naive2, accuracy.mod)
colnames(accuracy.table)<-c("Undefined-Out", "Commute-Home", "Undefined-In", "Commute-Work")
rownames(accuracy.table)<-c("Naive 1 (1 hr lag)", "Naive 2 (24 hr lag)", "Logit")

#plot accuracy table
accuracy.table2<-accuracy.table
accuracy.table2<-accuracy.table2[, c(4,2, 1, 3)]
accuracy.table2<-round(accuracy.table2, digits=2)
accuracy.table2 <- format(accuracy.table2, format="d", big.mark=",")

grid.table(accuracy.table2)




