library(corrplot)

data<-read.csv("~/Desktop/Crime_2015.csv", header=TRUE,sep=",")
summary(data)  #summary of dataset
colnames(data) #all variables 

data[,2]<-as.numeric(data[,2]) #because it is given as character data
data[is.na(data[,2]), 2] <- mean(data[,2], na.rm = TRUE)

data[,7]<-as.numeric(data[,7]) #because it is given as character data
data[is.na(data[,7]), 7] <- mean(data[,2], na.rm = TRUE)

data[,8]<-as.numeric(data[,8]) #because it is given as character data
data[is.na(data[,8]), 8] <- mean(data[,2], na.rm = TRUE)

data[,9]<-as.numeric(data[,9]) #because it is given as character data
data[is.na(data[,9]), 9] <- mean(data[,2], na.rm = TRUE)

summary(data)

table(is.na(data)) #to see if there is any missing values
which(is.na(data), arr.ind=TRUE)
data[38,6]<-mean(as.numeric(data[-38,6])) #missing data value substitution 

data_numeric<-data[,-c(1,11,12)
cor(data_numeric)
corrplot(cor(data_numeric)) 

quantile(data$ViolentCrime)
boxplot(data$ViolentCrime,outline=F)

quantile(data$Murder)
boxplot(data$Murder,outline=F)

quantile(data$Rape)
boxplot(data$Rape,outline=F)

quantile(data$Robbery)
boxplot(data$Robbery,outline=F)

quantile(data$AggravatedAssault)
boxplot(data$AggravatedAssault,outline=F)

quantile(data$PropertyCrime)
boxplot(data$PropertyCrime,outline=F)

quantile(data$Burglary)
boxplot(data$Burglary,outline=F)

quantile(data$Theft)
boxplot(data$Theft,outline=F)

quantile(data$MotorVehicleTheft)
boxplot(data$MotorVehicleTheft,outline=F)

unique_state<-unique(data$State)
unique_city<-unique(data$City)

length(unique_state)
length(unique_city)

data_State<-as.data.frame(table(data$State))
data_State<-data_State[order(data_State[,2],decreasing=T),] 

data_City<-as.data.frame(table(data$City))
data_City<-data_City[order(data_City[,2],decreasing=T),] 

slices<- as.data.frame(table(data$State))[,2]
lbl<-as.data.frame(table(data$State))[,1]
pct<- round(slices/sum(slices)*100)
lbl<-paste(lbl,pct)
lbl<-paste(lbl,"%",sep="")
pie(slices,labels=lbl,main="Pie Chart of States") #pie chart

slices<- as.data.frame(table(data$City))[,2]
lbl<-as.data.frame(table(data$City))[,1]
pct<- round(slices/sum(slices)*100)
lbl<-paste(lbl,pct)
lbl<-paste(lbl,"%",sep="")
pie(slices,labels=lbl,main="Pie Chart of City") #pie chart



x<-c(0)
y<-c(0)

# For every unique city the numbers of Violent Crime --- We can do this also for Rape  , Robbery , AggravatedAssault ,PropertyCrime ,Burglary , Theft , MotorVehicleTheft every state city the numbers of Murders

for (i in 1:length(unique_state)) {
	
	x[i]<-unique_state[i]
	y[i]<-sum(data[data[,11]==unique_state[i] ,][,2])
	
}

Violent_state<-data.frame(x,y)
Violent_state<-Violent_state[order(Violent_state[,2],decreasing=T),] 

slices<- Violent_state[,2]
lbl<-Violent_state[,1]
pct<- round(slices/sum(slices)*100)
lbl<-paste(lbl,pct)
lbl<-paste(lbl,"%",sep="")
pie(slices,labels=lbl) #pie chart


# For every unique city the numbers of Murders --- We can do this also for Rape  , Robbery , AggravatedAssault ,PropertyCrime ,Burglary , Theft , MotorVehicleTheft every state city the numbers of Murders

x<-c(0)
y<-c(0)

for (i in 1:length(unique_state)) {
	
	x[i]<-unique_state[i]
	y[i]<-sum(data[data[,11]==unique_state[i] ,][,3])
	
}

Murder_state<-data.frame(x,y)
Murder_state<-Murder_state[order(Murder_state[,2],decreasing=T),] 

slices<- Murder_state[,2]
lbl<-Murder_state[,1]
pct<- round(slices/sum(slices)*100)
lbl<-paste(lbl,pct)
lbl<-paste(lbl,"%",sep="")
pie(slices,labels=lbl) #pie chart




# For every unique city the numbers of Murders --- We can do this also for ViolentCrime ,Rape  , Robbery , AggravatedAssault ,PropertyCrime ,Burglary , Theft , MotorVehicleTheft

x<-c(0)
y<-c(0)

for (i in 1:length(unique_city)) {
	
	x[i]<-unique_city[i]
	y[i]<-sum(data[data[,12]==unique_city[i] ,][,3])
	
}

Murder_city<-data.frame(x,y)
Murder_city<-Murder_city[order(Murder_city[,2],decreasing=T),] 

slices<- Murder_city[,2]
lbl<-Murder_city[,1]
pct<- round(slices/sum(slices)*100)
lbl<-paste(lbl,pct)
lbl<-paste(lbl,"%",sep="")
pie(slices,labels=lbl) #pie chart


