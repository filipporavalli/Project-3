### Project 3 Code

#Name: Filippo Ravalli
#Title: Predictive Model of Air Traffic to Inform Future Planning and Avoid a Shortage of Controllers 



#Yearmon Values for months
#00=JAN
#.084=FEB
#.168=MAR
#.252=APR
#.336=MAY
#0.420=JUN
#.504=JUL
#.588=AUG
#.672=SEP
#.756=OCT
#.840=NOV
#.924=DEC

##### Load Libraries####
library(zoo)
library(ggplot2)
library(dplyr)
library(forecast)
library(tseries)
library(ggpubr)
##### Data Processing ####
##Read data in
#Domestic Flights
#Dom Hours
domhours<-read.csv("~/Desktop/2nd data/DomAircraftHours.csv", head=FALSE)
domhours<-domhours[-c(1,2,3,252),]
domhours<-rename(domhours, Period=V1, Domestic_Hours=V2)

#Dom Aicraft Revenue Miles
domaircraftmiles<-read.csv("~/Desktop/2nd data/DomAircraftMiles.csv", head=FALSE)
domaircraftmiles<-domaircraftmiles[-c(1,2,3,252),]
domaircraftmiles<-rename(domaircraftmiles, Period=V1, Domestic_Aircraft_Miles=V2)

#Dom Available Seat Miles
domavailseats<-read.csv("~/Desktop/2nd data/DomAvailSeatMiles.csv", head=FALSE)
domavailseats<-domavailseats[-c(1,2,3,252),]
domavailseats<-rename(domavailseats, Period=V1, Domestic_Avail_Seats=V2)

#Dom Num Flights
domflights<-read.csv("~/Desktop/2nd data/DomFlights.csv", head=FALSE)
domflights<-domflights[-c(1,2,3,252),]
domflights<-rename(domflights, Period=V1, Domestic_Num_Flights=V2)

#Dom Pax Enplanements
dompaxenp<-read.csv("~/Desktop/2nd data/DomPaxEnplanements.csv", head=FALSE)
dompaxenp<-dompaxenp[-c(1,2,3,252),]
dompaxenp<-rename(dompaxenp, Period=V1, Domestic_Pax_Enplanements=V2)

#Dom Pax revenue miles
dompaxmiles<-read.csv("~/Desktop/2nd data/DomPaxMiles.csv", head=FALSE)
dompaxmiles<-dompaxmiles[-c(1,2,3,252),]
dompaxmiles<-rename(dompaxmiles, Period=V1, Domestic_Pax_Miles=V2)


#Internatonal Flights
#Int Hours
Inthours<-read.csv("~/Desktop/2nd data/IntAircraftHours.csv", head=FALSE)
Inthours<-Inthours[-c(1,2,3,252),]
Inthours<-rename(Inthours, Period=V1, Int_Hours=V2)

#Int Aicraft Revenue Miles
Intaircraftmiles<-read.csv("~/Desktop/2nd data/IntAircraftMiles.csv", head=FALSE)
Intaircraftmiles<-Intaircraftmiles[-c(1,2,3,252),]
Intaircraftmiles<-rename(Intaircraftmiles, Period=V1, Int_Aircraft_Miles=V2)

#Int Available Seat Miles
Intavailseats<-read.csv("~/Desktop/2nd data/IntAvailSeatMiles.csv", head=FALSE)
Intavailseats<-Intavailseats[-c(1,2,3,252),]
Intavailseats<-rename(Intavailseats, Period=V1, Int_Avail_Seats=V2)

#Int Num Flights
Intflights<-read.csv("~/Desktop/2nd data/IntFlights.csv", head=FALSE)
Intflights<-Intflights[-c(1,2,3,252),]
Intflights<-rename(Intflights, Period=V1, Int_Num_Flights=V2)

#Int Pax Enplanements
Intpaxenp<-read.csv("~/Desktop/2nd data/IntPaxEnplanements.csv", head=FALSE)
Intpaxenp<-Intpaxenp[-c(1,2,3,252),]
Intpaxenp<-rename(Intpaxenp, Period=V1, Int_Pax_Enplanements=V2)

#Int Pax revenue miles
Intpaxmiles<-read.csv("~/Desktop/2nd data/IntPaxMiles.csv", head=FALSE)
Intpaxmiles<-Intpaxmiles[-c(1,2,3,252),]
Intpaxmiles<-rename(Intpaxmiles, Period=V1, Int_Pax_Miles=V2)


##Combine Data
df<-merge(domhours,domaircraftmiles,by="Period")
df<-merge(df,domavailseats,by="Period")
df<-merge(df,domflights,by="Period")
df<-merge(df,dompaxenp,by="Period")
df<-merge(df,dompaxmiles,by="Period")
df<-merge(df,Inthours,by="Period")
df<-merge(df,Intaircraftmiles,by="Period")
df<-merge(df,Intavailseats,by="Period")
df<-merge(df,Intflights,by="Period")
df<-merge(df,Intpaxenp,by="Period")
datafullperiod<-merge(df,Intpaxmiles,by="Period")

#add months
monthvec<-c()
mon<-c()
i<-1
for (i in 1:nrow(datafullperiod)){
  mon<-strsplit(datafullperiod$Period, " ")[[i]][1]
  datafullperiod$Month[i]<-mon
  monthvec<-rbind(monthvec, mon)
}

datafullperiod$Month[which(datafullperiod$Month=="January")]<-1
datafullperiod$Month[which(datafullperiod$Month=="February")]<-2
datafullperiod$Month[which(datafullperiod$Month=="March")]<-3
datafullperiod$Month[which(datafullperiod$Month=="April")]<-4
datafullperiod$Month[which(datafullperiod$Month=="May")]<-5
datafullperiod$Month[which(datafullperiod$Month=="June")]<-6
datafullperiod$Month[which(datafullperiod$Month=="July")]<-7
datafullperiod$Month[which(datafullperiod$Month=="August")]<-8
datafullperiod$Month[which(datafullperiod$Month=="September")]<-9
datafullperiod$Month[which(datafullperiod$Month=="October")]<-10
datafullperiod$Month[which(datafullperiod$Month=="November")]<-11
datafullperiod$Month[which(datafullperiod$Month=="December")]<-12

#convert date to yearmon format + numeric
datafullperiod$Period<-as.yearmon(datafullperiod$Period)

datafullperiod<-
  datafullperiod %>% mutate_if(is.character,as.numeric)

#order data
datafullperiod<-datafullperiod[order(datafullperiod$Period),]

##Add historical times

histdates<-c(2001.672, 2008.672, 2008.756,2008.840,2008.924,
            2009, 2009.084, 2009.168, 2009.252, 2009.336,
            2009.420,2020, 2020.084, 2020.168, 2020.252,
            2020.336, 2020.420, 2020.504, 2020.588)
datafullperiod$hist<-0

#add hist dummy
datafullperiod$hist[which(datafullperiod$Period>=2002.672 & datafullperiod$Period<3002)]<-1
datafullperiod$hist[which(datafullperiod$Period <= 2010.420 & datafullperiod$Period >= 2009.672)]<-1
datafullperiod$hist[which(datafullperiod$Period>=2020)]<-1


#Aggregate
i<-1
for (i in 1:nrow(datafullperiod)){
  datafullperiod$full_enplanement[i]<-datafullperiod$Domestic_Pax_Enplanements[i]+datafullperiod$Int_Pax_Enplanements[i]
  datafullperiod$full_flights[i]<-datafullperiod$Domestic_Num_Flights[i]+datafullperiod$Int_Num_Flights[i]
  datafullperiod$full_paxrpm[i]<-datafullperiod$Domestic_Pax_Miles[i]+datafullperiod$Int_Pax_Miles[i]
  datafullperiod$full_capacity[i]<-datafullperiod$Domestic_Avail_Seats[i]+datafullperiod$Int_Avail_Seats[i]
  datafullperiod$full_miles[i]<-datafullperiod$Domestic_Aircraft_Miles[i]+datafullperiod$Domestic_Aircraft_Miles[i]
  }

#remove 2020
datafullperiod<-datafullperiod[datafullperiod$Period<2020,]

#create lagged variables
datafullperiod$lag_flights<-lag(datafullperiod$full_flights, 12)
datafullperiod$lag_miles<-lag(datafullperiod$full_miles, 12)

#Create test data
datafulltest<-datafullperiod[datafullperiod$Period<2019,]
#for 2nd arima cross val
datafulltest2<-datafulltest[datafulltest<2018,]

#Save full file
write.csv(datafullperiod, "~/Desktop/datafullperiod.csv")
datafullperiod<-read.csv("~/Desktop/datafullperiod.csv")
datafullperiod$Period<-as.yearmon(datafullperiod$Period)
### end

##### Analysis ####



#ARIMA model
paxflts<-ts(datafulltest$full_flights, start=c(2000,1), end=c(2018,12), frequency = 12)
mod_ts<-auto.arima(paxflts)

fit<-arima(paxflts, order=c(3,1,0), list(order=c(2,1,1),period=12))
acf(fit$residuals)
pred_flt_num<-predict(fit, n.ahead=12,se.fit = TRUE)
pred_fltnum_df<-as.data.frame(pred_flt_num)
pred_fltnum_df$period<-datafullperiod$Period[datafullperiod$Period>=2019 & datafullperiod$Period<2020]
ci<-as.data.frame(forecast(fit, h=12))
pred_fltnum_df$lo95<-ci$`Lo 95`
pred_fltnum_df$hi95<-ci$`Hi 95`


i<-1
for(i in 1:nrow(pred_fltnum_df)){
  pred_fltnum_df$lose[i]<-pred_fltnum_df$pred[i]-pred_fltnum_df$se[i]
  pred_fltnum_df$hise[i]<-pred_fltnum_df$pred[i]+pred_fltnum_df$se[i]
}

df_arima_fit<-as.data.frame(fitted(fit))
df_arima_fit$period<-datafullperiod$Period[datafullperiod$Period<2019]

#Diagnostic plots
par(mfrow = c(2, 2))
plot(fitted(fit), fit$residuals, xlab="Fitted Values", 
     ylab="Residuals", main="Residual vs Fitted Plot")
hist(fit$residuals, xlab="Residuals",main="Histogram of Resiudals")
plot(fitted(fit), datafulltest$full_flights,
     xlab="Fitted Values", ylab="Actual Values", main="Fitted vs Actual Data Plot")
acf(fit$residuals, main="Autocorrelation plot of Residuals")

#model fitting for ARIMA
ggplot(NULL,group=1)+
  geom_line(data=datafullperiod[datafullperiod$Period<2019,], aes(Period, full_flights, col="Actual"))+
  geom_line(data=df_arima_fit, aes(period, x, col="Model"))+
  ggtitle("Fitted vs Actual Values for ARIMA  Model")+
  xlab("Time")+
  ylab("Number of Flights")+
  theme_bw()+
  theme(legend.title = element_blank())
  


#2 year cross val
paxflts2<-ts(datafulltest2$full_flights, start=c(2000,1), end=c(2017,12), frequency = 12)
fit2<-arima(paxflts2, order=c(3,1,0), list(order=c(2,1,1),period=12))
pred_flt_num2<-predict(fit2, n.ahead=24,se.fit = TRUE)
pred_fltnum_df2<-as.data.frame(pred_flt_num2)
pred_fltnum_df2$period<-datafullperiod$Period[datafullperiod$Period>=2018 & datafullperiod$Period<2020]
ci2<-as.data.frame(forecast(fit2, h=24))
pred_fltnum_df2$lo95<-ci2$`Lo 95`
pred_fltnum_df2$hi95<-ci2$`Hi 95`

i<-1
for(i in 1:nrow(pred_fltnum_df2)){
  pred_fltnum_df2$lose[i]<-pred_fltnum_df2$pred[i]-pred_fltnum_df2$se[i]
  pred_fltnum_df2$hise[i]<-pred_fltnum_df2$pred[i]+pred_fltnum_df2$se[i]
}

#LM model
olm_flights<-lm(full_flights~hist+lag_flights+sin(lag_flights/6*pi)+cos(lag_flights/6*pi)
                +lag_miles+sin(lag_miles/6*pi)+cos(lag_miles/6*pi), data=datafulltest)
pred_flight_12<-as.data.frame(predict.lm(olm_flights, datafullperiod[datafullperiod$Period>=2019 & datafullperiod$Period<2020,], se.fit = TRUE))
pred_flight_12$period<-datafullperiod$Period[datafullperiod$Period>=2019 & datafullperiod$Period<2020]
pred_flight_12<-rename(pred_flight_12,full_flights=fit)

for(i in 1:nrow(pred_flight_12)){
  pred_flight_12$lose[i]<-pred_flight_12$full_flights[i]-pred_flight_12$se.fit[i]
  pred_flight_12$hise[i]<-pred_flight_12$full_flights[i]+pred_flight_12$se.fit[i]
}

#Diagnostic plots
par(mfrow = c(2, 2))
plot(olm_flights$fitted.values, olm_flights$residuals, xlab="Fitted Values", 
     ylab="Residuals", main="Residual vs Fitted Plot")
hist(olm_flights$residuals, xlab="Residuals",main="Histogram of Resiudals")
plot(olm_flights$fitted.values, datafulltest$full_flights[datafulltest$Period>=2001],
     xlab="Fitted Values", ylab="Actual Values", main="Fitted vs Actual Data Plot")
acf(olm_flights$residuals, main="Autocorrelation plot of Residuals")


#data for plot
df_fit<-as.data.frame(olm_flights$fitted.values)
df_fit$period<-datafullperiod$Period[datafullperiod$Period>2000 & datafullperiod$Period<2019]

data2019<-datafullperiod[datafullperiod$Period>=2019,]

#fitting model to data for lm
ggplot(NULL,group=1)+
  geom_line(data=datafullperiod, aes(Period, full_flights, col="Actual"))+
  geom_line(data=df_fit, aes(period, `olm_flights$fitted.values`, col="Model"))+
  xlim(2001,2019)+
  ggtitle("Fitted vs Actual Values for Linear Model")+
  xlab("Time")+
  ylab("Number of Flights")+
  theme_bw()+
  theme(legend.title = element_blank())



#future Forecasts
paxflt_future<-ts(datafullperiod$full_flights[datafullperiod$Period<2020], start=c(2000,1), end=c(2019,12), frequency = 12)
future<-arima(paxflt_future, order=c(3,1,0), list(order=c(2,1,1),period=12))
future_pred<-predict(future, n.ahead=60,interval=c("prediction"), level=.95)
future_df<-as.data.frame(future_pred)
future_df$period<-as.yearmon(2020+seq(0,59)/12)
ci_fut<-as.data.frame(forecast(future, h=60))
future_df$lo95<-ci_fut$`Lo 95`
future_df$hi95<-ci_fut$`Hi 95`
### end


##### Plots ####
##EDA
#enplanement
ggplot(data=datafullperiod, aes(Period,group = 1))+
  geom_line(aes(y=full_enplanement))+
  geom_line(aes(y=rollmean(full_enplanement, 12, na.pad=TRUE), col="Rolling 12-month Average"))+
  ggtitle("Total Passenger Enplanements 2000-2019")+
  ylab("Enplanements (000)")+
  xlab("Time")+
  theme_bw()+
  theme(legend.title = element_blank())

#Flt number
ggplot(data=datafullperiod, aes(Period, group = 1))+
  geom_line(aes(y=full_flights))+
  geom_line(aes(y=rollmean(full_flights, 12, na.pad=TRUE),col="Rolling 12-month Average"))+  
  ggtitle("Number of Flights 2000-2019")+
  ylab("Flights")+
  xlab("Time")+
  theme_bw()+
  theme(legend.title = element_blank())

##Cross Validation
#Lm
ggplot(NULL,group=1)+
  geom_line(data=data2019, aes(Period, full_flights, col="Actual"))+
  geom_line(data=pred_flight_12, aes(period, full_flights))+
  geom_smooth(data=pred_flight_12,aes(period, full_flights,ymin=lose, ymax=hise, col="LM Prediction (+/- SE)"),stat="identity")+
  ggtitle("Cross Validation of linear model vs actual data 2019-2020")+
  xlab("Time")+
  ylab("Number of Flights")+
  theme_bw()+
  theme(legend.title = element_blank())

#Arima
ggplot(NULL,group=1)+
  geom_line(data=data2019, aes(Period, full_flights, col="Actual"))+
  geom_line(data=pred_fltnum_df, aes(period, pred))+
  geom_smooth(data=pred_fltnum_df,aes(period, pred,ymin=lo95, ymax=hi95, col="ARIMA Prediction (95%CI)"),stat="identity")+
  ggtitle("Cross Validation of ARIMA model vs actual data 2019-2020")+
  xlab("Time")+
  theme_bw()+
  ylab("Number of Flights")+
  theme(legend.title = element_blank())

data2<-datafullperiod[datafullperiod$Period>=2018,]
ggplot(NULL,group=1)+
  geom_line(data=data2, aes(Period, full_flights, col="Actual"))+
  geom_line(data=pred_fltnum_df2, aes(period, pred))+
  geom_smooth(data=pred_fltnum_df2,aes(period, pred,ymin=lo95, ymax=hi95, col="ARIMA Prediction (95% CI)"),stat="identity")+
  ggtitle("Cross Validation of ARIMA model vs actual data 2018-2020")+
  xlab("Time")+
  theme_bw()+
  ylab("Number of Flights")+
  theme(legend.title = element_blank())

#Both models
ggplot(NULL, group=1)+
  geom_line(data=data2019, aes(Period, full_flights, col="Actual"))+
  geom_line(data=pred_fltnum_df, aes(period, pred, col="ARIMA Prediction"))+
  geom_line(data=pred_flight_12, aes(period, full_flights,col="LM Prediction"))+
  ggtitle("Cross Validation of both models vs actual data 2019-2020")+
  xlab("Time")+
  theme_bw()+
  ylab("Number of Flights")+
  theme(legend.title = element_blank())

##Forecasts
p1<-ggplot(NULL,group=1)+
  geom_line(data=datafullperiod[datafullperiod$Period<2020,], aes(Period, full_flights, col="Actual"))+
  geom_line(data=future_df, aes(period, pred))+
  geom_smooth(data=future_df,aes(period, pred,ymin=lo95, ymax=hi95, col="Prediction (95% CI)"),stat="identity")+
  xlim(2018,2022)+
  ggtitle("24 Month Forecast")+
  xlab("Time")+
  theme_bw()+
  ylab("Number of Flights")+
  theme(legend.title = element_blank())

p2<-ggplot(NULL,group=1)+
  geom_line(data=datafullperiod[datafullperiod$Period<2020,], aes(Period, full_flights, col="Actual"))+
  geom_line(data=future_df, aes(period, pred))+
  geom_smooth(data=future_df,aes(period, pred,ymin=lo95, ymax=hi95, col="Prediction (95% CI)"),stat="identity")+
  xlim(2018,2024)+
  ggtitle("48 Month Forecast")+
  xlab("Time")+
  theme_bw()+
  ylab("Number of Flights")+
  theme(legend.title = element_blank())

ggarrange(p1,p2, ncol=2, nrow=1)
### end