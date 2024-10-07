


library(openxlsx)
library(timetk)
library(dplyr)
library(lubridate)
library(recipes)
library(rsample)
library(parsnip)
library(workflows)
library(forecastML)
library(corrplot)
library(doParallel)
library(timeDate)
library(astsa)
library(ggplot2)
library(forecast)
library(bsts)
library(nlme)
library(splines)




############################ Some Functions #################################
Metrics.fun = function(x,y) {
  AE= abs(x-y)  # Absolute difference
  AE.mean = mean(AE)  # Mean AE
  AE.se = sd(AE)  # standard error of AE
  SE = AE^2  # Squared AE
  MSE = mean(SE)  # Mean of squared error
  RMSE = sqrt(MSE) # Root of MSE
  RMSE.se = sd(SE) # Standard error of SE
  Metrics = data.frame(AE.mean = AE.mean,
                       AE.se = AE.se,
                       RMSE=RMSE)
  Metrics
}


############################## 1. Data Preparation ####################################
load("C:\\Users\\YCHEN\\OneDrive - E & J Gallo Winery\\Desktop\\WIP\\2024-Weather on wine sales\\ApothicUnit.df.RData")

# setting up parallel processing
registerDoSEQ()
cl = makePSOCKcluster(10)
registerDoParallel(cl)


path="C:\\Users\\YCHEN\\OneDrive - E & J Gallo Winery\\Desktop\\YCHEN\\Completed\\2017-2018-Weather on wine\\salesAndWeather.xlsx"

df = read.xlsx(path,sheet="Sheet 1",detectDates=T)
str(df)
names(df)
head(df)

df %>% 
  dplyr::select(DATE) %>%
  dplyr::mutate(WeekOfYear=week(DATE)) %>%
  as.data.frame()

grep(names(df),pattern="apothic",value=T)

## Apothic unit
### data
ApothicUnit = df %>%
  dplyr::select(1:26,78) %>%
  dplyr::mutate(WeekOfYear = week(DATE),
                Year = year(DATE),
                Month = as.factor(month(DATE)),
                WeekOfYear=week(DATE),
                WeekNo=1:nrow(df)) %>%
  as.data.frame()
str(ApothicUnit)

ApothicUnit %>%
  dplyr::group_by(Year) %>%
  plot_time_series(DATE,apothicPremiumUnit,
                   .interactive=F,
                   .facet_scales="free",
                   .facet_ncol=2,
                   .x_lab="Date",
                   .y_lab="Apothic (Unit)")
ApothicUnit %>%
  #  dplyr::group_by(Year) %>%
  plot_time_series(DATE,apothicPremiumUnit,
                   .interactive=F,
                   .facet_scales="free",
                   .color_var=year(DATE),
                   .facet_ncol=2,
                   .x_lab="Date",
                   .y_lab="Apothic (Unit)")

str(ApothicUnit)
ggplot(ApothicUnit,aes(x=DATE,y=apothicPremiumUnit))+
  geom_point()+geom_line()+
  labs(y="Apothic (Unit)")
str(ApothicUnit)
ggplot(ApothicUnit[1:272,],aes(x=DATE,y=apothicPremiumUnit))+
  geom_point()+geom_line()+
  labs(y="Apothic (Unit)")+
  geom_point(data=ApothicUnit[273:312,],aes(x=DATE,y=apothicPremiumUnit),color="red")+
  geom_line(data=ApothicUnit[273:312,],aes(x=DATE,y=apothicPremiumUnit),color="red",lty=2)+
  geom_vline(xintercept=ApothicUnit$DATE[272],color="orange",linewidth=1)


names(ApothicUnit)
apply(is.na(ApothicUnit),2,sum)
nearZeroVar(ApothicUnit)
nearZeroVar(ApothicUnit,names=T)
apply(ApothicUnit[,c(9,12,13,17)],2,sd)
corrplot(cor(ApothicUnit[,c(2:8,10:16,18:25)]),method="square",order="hclust")


ApothicUnit.df = ApothicUnit %>%
  dplyr::select(apothicPremiumUnit,PRCP.mean, TMIN.mean,AWND.mean,
                SNOW.mean,SNWD.mean,Year, Month, WeekOfYear,WeekNo) 
names(ApothicUnit.df)

dates = ApothicUnit$DATE

corrplot(cor(ApothicUnit.df[,2:6]),method="number",order="hclust")

ggplot(ApothicUnit.df,aes(x=PRCP.mean,y=apothicPremiumUnit))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  geom_smooth(method="loess",se=F,color="red")
ggplot(ApothicUnit.df,aes(x=TMIN.mean,y=apothicPremiumUnit))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  geom_smooth(method="loess",se=F,color="red")
ggplot(ApothicUnit.df,aes(x=AWND.mean,y=apothicPremiumUnit))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  geom_smooth(method="loess",se=F,color="red")
ggplot(ApothicUnit.df,aes(x=SNOW.mean,y=apothicPremiumUnit))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  geom_smooth(method="loess",se=F,color="red")
ggplot(ApothicUnit.df,aes(x=SNWD.mean,y=apothicPremiumUnit))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  geom_smooth(method="loess",se=F,color="red")


lag2.plot(as.ts(ApothicUnitdf$apothicPremiumUnit),as.ts(ApothicUnit.df$PRCP.mean),
          max.lag=15)
lag2.plot(as.ts(ApothicUnit.df$apothicPremiumUnit),as.ts(ApothicUnit.df$TMIN.mean),
          max.lag=15)
lag2.plot(as.ts(ApothicUnit.df$apothicPremiumUnit),as.ts(ApothicUnit.df$AWND.mean),
          max.lag=15)
lag2.plot(as.ts(ApothicUnit.df$apothicPremiumUnit),as.ts(ApothicUnit.df$SNOW.mean),
          max.lag=15)
lag2.plot(as.ts(ApothicUnit.df$apothicPremiumUnit),as.ts(ApothicUnit.df$SNWD.mean),
          max.lag=15)


### Create dummies for holidays
ApothicUnit.df$week.end = seq(as.Date("2012-01-15"),as.Date("2017-12-31"),by="1 week")
ApothicUnit.df$week.start = ApothicUnit.df$week.end - 7


x = seq(ApothicUnit.df[1,]$week.start+1,ApothicUnit.df[1,]$week.end,length.out=7)

#### Christmas
Christmas.Dates = timeDate::ChristmasDay(min(ApothicUnit.df$Year):max(ApothicUnit.df$Year))
for (i in 1:nrow(ApothicUnit.df)) {
  week = seq(ApothicUnit.df[i,]$week.start+1,ApothicUnit.df[i,]$week.end,length.out=7)
  ApothicUnit.df$Christmas[i] = ifelse(length(base::intersect(week,as.Date(Christmas.Dates))) == 0,0,1)
}

#### Independence day
ThanksgivingDay.Dates = timeDate::USThanksgivingDay(min(ApothicUnit.df$Year):max(ApothicUnit.df$Year))
for (i in 1:nrow(ApothicUnit.df)) {
  week = seq(ApothicUnit.df[i,]$week.start+1,ApothicUnit.df[i,]$week.end,length.out=7)
  ApothicUnit.df$ThanksgivingDay[i] = ifelse(length(base::intersect(week,as.Date(ThanksgivingDay.Dates))) == 0,0,1)
}


#### New Year's day
NewYearsDay.Dates = timeDate::NewYearsDay(min(ApothicUnit.df$Year):max(ApothicUnit.df$Year))
for (i in 1:nrow(ApothicUnit.df)) {
  week = seq(ApothicUnit.df[i,]$week.start+1,ApothicUnit.df[i,]$week.end,length.out=7)
  ApothicUnit.df$NewYearsDay[i] = ifelse(length(base::intersect(week,as.Date(NewYearsDay.Dates))) == 0,0,1)
}

#### Independence day
IndependenceDay.Dates = timeDate::USIndependenceDay(min(ApothicUnit.df$Year):max(ApothicUnit.df$Year))
for (i in 1:nrow(ApothicUnit.df)) {
  week = seq(ApothicUnit.df[i,]$week.start+1,ApothicUnit.df[i,]$week.end,length.out=7)
  ApothicUnit.df$IndependenceDay[i] = ifelse(length(base::intersect(week,as.Date(IndependenceDay.Dates))) == 0,0,1)
}

#### Labor day
LaborDay.Dates = timeDate::USLaborDay(min(ApothicUnit.df$Year):max(ApothicUnit.df$Year))
for (i in 1:nrow(ApothicUnit.df)) {
  week = seq(ApothicUnit.df[i,]$week.start+1,ApothicUnit.df[i,]$week.end,length.out=7)
  ApothicUnit.df$LaborDay[i] = ifelse(length(base::intersect(week,as.Date(LaborDay.Dates))) == 0,0,1)
}

#### Memorial day
MemorialDay.Dates = timeDate::USMemorialDay(min(ApothicUnit.df$Year):max(ApothicUnit.df$Year))
for (i in 1:nrow(ApothicUnit.df)) {
  week = seq(ApothicUnit.df[i,]$week.start+1,ApothicUnit.df[i,]$week.end,length.out=7)
  ApothicUnit.df$MemorialDay[i] = ifelse(length(base::intersect(week,as.Date(MemorialDay.Dates))) == 0,0,1)
}

#### Presidents day
PresidentsDay.Dates = timeDate::USPresidentsDay(min(ApothicUnit.df$Year):max(ApothicUnit.df$Year))
for (i in 1:nrow(ApothicUnit.df)) {
  week = seq(ApothicUnit.df[i,]$week.start+1,ApothicUnit.df[i,]$week.end,length.out=7)
  ApothicUnit.df$PresidentsDay[i] = ifelse(length(base::intersect(week,as.Date(PresidentsDay.Dates))) == 0,0,1)
}

#### MLKing'S day
MLKingsBirthday.Dates = timeDate::USMLKingsBirthday(min(ApothicUnit.df$Year):max(ApothicUnit.df$Year))
for (i in 1:nrow(ApothicUnit.df)) {
  week = seq(ApothicUnit.df[i,]$week.start+1,ApothicUnit.df[i,]$week.end,length.out=7)
  ApothicUnit.df$MLKingsBirthday[i] = ifelse(length(base::intersect(week,as.Date(MLKingsBirthday.Dates))) == 0,0,1)
}

#### Veterans day
VeteransDay.Dates = timeDate::USVeteransDay(min(ApothicUnit.df$Year):max(ApothicUnit.df$Year))
for (i in 1:nrow(ApothicUnit.df)) {
  week = seq(ApothicUnit.df[i,]$week.start+1,ApothicUnit.df[i,]$week.end,length.out=7)
  ApothicUnit.df$VeteransDay[i] = ifelse(length(base::intersect(week,as.Date(VeteransDay.Dates))) == 0,0,1)
}



################################## 2. EDA ################################
head(ApothicUnit.df)
names(ApothicUnit.df)
# Lag effects
ApothicUnit.df = ApothicUnit.df %>%
  dplyr::mutate(Christmas.Back1 = c(Christmas[-1],0),
                Christmas.Back2 = c(Christmas[c(-1,-2)],0,0),
                SNOW.Binary = ifelse(ApothicUnit.df$SNOW.mean>=0.2,1,0),
                SNWD.Binary = ifelse(ApothicUnit.df$SNWD.mean >= 2,1,0)) %>%
  dplyr::mutate(SNOW.Binary.Back1 = c(SNOW.Binary[-1],0),
                SNWD.Binary.Back1 = c(SNWD.Binary[-1],0)) %>%
  as.data.frame()

# Christmas lag effects (1 week back)
ApothicUnit.df %>%
  dplyr::select(apothicPremiumUnit,week.start,week.end,Christmas,Christmas.Back1,
                Christmas.Back2) %>%
  as.data.frame() %>%
  View()

# Thanksgiving lag effects  (no lag effect)
ApothicUnit.df %>%
  dplyr::select(apothicPremiumUnit,week.start,week.end,ThanksgivingDay) %>%
  as.data.frame() %>%
  View()

# NewYearsDay lag effects  (Christmas)
ApothicUnit.df %>%
  dplyr::select(apothicPremiumUnit,week.start,week.end,NewYearsDay) %>%
  as.data.frame() %>%
  View()

# July4 lag effects  (no lag)
ApothicUnit.df %>%
  dplyr::select(apothicPremiumUnit,week.start,week.end,IndependenceDay) %>%
  as.data.frame() %>%
  View()


############################### 3. Train-Test split ################################
load("ApothicUnit.df.RData")
str(ApothicUnit.df)
# save(ApothicUnit.df,file="C:\\Users\\YCHEN\\OneDrive - E & J Gallo Winery\\Desktop\\WIP\\2024-Weather on wine sales\\ApothicUnit.df.RData")

date.frequency = "week"
dates=seq(as.Date("2012-01-15"),as.Date("2017-12-31"),by=date.frequency)


ApothicUnit.train = ApothicUnit.df[1:(nrow(ApothicUnit.df)-40),]
ApothicUnit.test = ApothicUnit.df[(nrow(ApothicUnit.df)-40+1):nrow(ApothicUnit.df),]
names(ApothicUnit.train)

TS_Forecast_train_df = ApothicUnit.train %>%
  dplyr::select(1:6,11) %>%
  dplyr::rename("Apothic" = "apothicPremiumUnit")
str(TS_Forecast_train_df)

TS_Forecast_test_df = ApothicUnit.test %>%
  dplyr::select(1,11) %>%
  dplyr::rename("Apothic" = "apothicPremiumUnit")
str(TS_Forecast_test_df)



write.xlsx(TS_Forecast_train_df,file="TS_Forecast_train_df.xlsx")
write.xlsx(TS_Forecast_test_df,file="TS_Forecast_test_df.xlsx")

save(ApothicUnit.train,file="ApothicUnit.train.RData")
save(ApothicUnit.test,file="ApothicUnit.test.RData")

ggplot(ApothicUnit.df,aes(x=dates,y=apothicPremiumUnit))+
  geom_line()+
  geom_vline(xintercept=dates[nrow(ApothicUnit.train)],
             color="red",linewidth=1.2)










################################## 4. Forecasting ################################
load("C:\\Users\\YCHEN\\OneDrive - E & J Gallo Winery\\Desktop\\YCHEN\\Lectures\\Gallo analytics\\2024\\ts forecast contest\\ApothicUnit.train.RData")
load("C:\\Users\\YCHEN\\OneDrive - E & J Gallo Winery\\Desktop\\YCHEN\\Lectures\\Gallo analytics\\2024\\ts forecast contest\\ApothicUnit.test.RData")



# 4.1. SARIMA model
plot.ts(ts(ApothicUnit.train[,1]))
plot.ts(log10(ts(ApothicUnit.train[,1])))  # seems better

acf2(ts(ApothicUnit.train[,1]))
acf2(log10(ts(ApothicUnit.train[,1])))  # seems better

## 4.1.1. SARIMA models without exogenous variables
### 4.1.1.1. ts Without transformation
names(ApothicUnit.train)
lag1.plot(ts(ApothicUnit.train[,1]),max.lag=15)
spectrum(ts(ApothicUnit.train[,1]),log="no")
spectrum(ts(ApothicUnit.train[,1]),log="no",plot=F)

auto.arima(ApothicUnit.train[,1],trace=T,ic="aicc",seasonal.test="hegy")  # p,d,q = 0,1,2
acf2(ApothicUnit.train[,1])  # ar3 is adequate?

#### p,d,q = 3,1,0
sarima(ts(ApothicUnit.train[,1]),3,1,0)   
sarima_model1_pred = sarima.for(ts(ApothicUnit.train[,1]),40,3,1,0)$pred 
Metrics.fun(sarima_model1_pred,ApothicUnit.test[,1])
#  AE.mean    AE.se     RMSE
# 849.6314 626.8935 1051.211

#### p,d,q = 0,1,2
sarima(ts(ApothicUnit.train[,1]),0,1,2) # best model selected by auto.arima
sarima_model2_pred = sarima.for(ts(ApothicUnit.train[,1]),40,0,1,2)$pred # 
Metrics.fun(sarima_model2_pred,ApothicUnit.test[,1])
#  AE.mean    AE.se     RMSE
# 911.9217 616.6974 1096.544

#### p,d,q, P,D,Q,S = 3,1,0,1,0,0,52 (the best so far) (log-transformed is preffered)
sarima_model3 = sarima(ts(ApothicUnit.train[,1]),3,1,0,1,0,0,S=52)
sarima_model3_pred = sarima.for(ts(ApothicUnit.train[,1]),40,3,1,0,1,0,0,S=52)$pred
Metrics.fun(sarima_model3_pred,ApothicUnit.test[,1])
#  AE.mean    AE.se   RMSE
# 517.5765 420.4517 663.51

sarima_model3_log_pred = sarima.for(ts(log(ApothicUnit.train[,1])),40,3,1,0,1,0,0,S=52)$pred
Metrics.fun(exp(sarima_model3_log_pred),ApothicUnit.test[,1])
#  AE.mean    AE.se     RMSE
# 544.3589 384.1086 663.4588


par(oma=c(3,1,0,0))
plot(1:40,ApothicUnit.test[,1],type="o",xlab="",ylab="Apothic Unit",
     xaxt="n")
points(1:40,sarima_model3_pred,col="red")
lines(1:40,sarima_model3_pred,col="red")
text(x=24,y=6700,label="Original")
text(x=27,y=7200,label="Forecasted",col="red")
text(x=10,y=7500,label=" RMSE = 663.51
     AE.mean = 517.58
AE.se = 420.45")
axis(side=1,at=1:40,labels=ApothicUnit.test$week.end,las=2)


#### p,d,q, P,D,Q,S = 3,1,0,1,0,0,52 (the best so far) (log-transformed)
sarima_model3 = sarima(ts(log10(ApothicUnit.train[,1])),3,1,0,1,0,0,S=52)
sarima_model3_pred = sarima.for(ts(log10(ApothicUnit.train[,1])),40,3,1,0,1,0,0,S=52)$pred
Metrics.fun(10^sarima_model3_pred,ApothicUnit.test[,1])
#  AE.mean    AE.se   RMSE
# 544.3584 384.1076 663.46

par(oma=c(3,1,0,0))
plot(1:40,ApothicUnit.test[,1],type="o",xlab="",ylab="Apothic Unit",
     xaxt="n")
points(1:40,10^sarima_model3_pred,col="red")
lines(1:40,10^sarima_model3_pred,col="red")
text(x=24,y=6700,label="Original")
text(x=27,y=7200,label="Forecasted",col="red")
text(x=10,y=7500,label=" RMSE = 663.51
     AE.mean = 517.58
AE.se = 420.45")
axis(side=1,at=1:40,labels=ApothicUnit.test$week.end,las=2)




#### p,d,q, P,D,Q,S = 0,1,2,1,0,0,52 
sarima(ts(ApothicUnit.train[,1]),0,1,2,1,0,0,S=52)
sarima_model4_pred = sarima.for(ts(ApothicUnit.train[,1]),40,0,1,2,1,0,0,S=52)$pred
Metrics.fun(sarima_model4_pred,ApothicUnit.test[,1])
#  AE.mean    AE.se   RMSE
# 546.7519 410.6749 680.7167


### 4.1.1.2. ts transformed (not needed)
acf2(log10(ApothicUnit.train[,1]))
auto.arima(log10(ApothicUnit.train[,1]),trace=T,ic="aicc",seasonal.test="hegy")  #

sarima(log10(ts(ApothicUnit.train[,1])),3,1,0)   
sarima_model5_pred = sarima.for(log10(ts(ApothicUnit.train[,1])),40,3,1,0)$pred 
Metrics.fun(10^sarima_model5_pred,ApothicUnit.test[,1])
#  AE.mean    AE.se     RMSE
# 962.2232 556.5031 1108.073

sarima(log10(ts(ApothicUnit.train[,1])),0,1,2) # best model selected by auto.arima
sarima_model6_pred = sarima.for(log10(ts(ApothicUnit.train[,1])),40,0,1,2)$pred # 
Metrics.fun(10^sarima_model6_pred,ApothicUnit.test[,1])
#  AE.mean    AE.se     RMSE
# 1026.966 569.2499 1170.728

sarima(log10(ts(ApothicUnit.train[,1])),3,1,0,1,0,0,S=52)
sarima_model7_pred = sarima.for(log10(ts(ApothicUnit.train[,1])),40,3,1,0,1,0,0,S=52)$pred
Metrics.fun(10^sarima_model7_pred,ApothicUnit.test[,1])
#  AE.mean    AE.se     RMSE
# 544.3584 384.1076 663.4578


## 4.1.2. SARIMA models with exogenous variables
### sarima_model3 is the best model (so far)
plot.ts(sarima_model3$fit$residuals)

cbind(ApothicUnit.train,sarima_model3$fit$residuals) %>%
  dplyr::rename("residuals"="sarima_model3$fit$residuals") %>%
  dplyr::filter(residuals >1500 | residuals < -1500) %>%
  as.data.frame()

sarima_model3_df = cbind(ApothicUnit.train,sarima_model3$fit$residuals) %>%
  dplyr::rename("residuals"="sarima_model3$fit$residuals") %>%
  as.data.frame()
str(sarima_model3_df)
names(sarima_model3_df)

apply(sarima_model3_df[,c(2:6,9:10,13:23)],
      MARGIN=2,FUN=function(x,y) cor(x,y),
      y=sarima_model3_df$residuals)

names(ApothicUnit.train)

### 4.1.2.1. exogenous variables prediction
#### two predictions: one with sarima.for() and one with bsts. Which is better?
names(ApothicUnit.train)
plot.ts(ts(ApothicUnit.train[,1:6]))

spectrum(ts(ApothicUnit.train[,2]),log="no")
spectrum(ts(ApothicUnit.train[,3]),log="no")
spectrum(ts(ApothicUnit.train[,4]),log="no")
spectrum(ts(ApothicUnit.train[,5]),log="no")
spectrum(ts(ApothicUnit.train[,6]),log="no")

spectrum(ts(ApothicUnit.train[,2]),log="no",plot=F)
spectrum(ts(ApothicUnit.train[,3]),log="no",plot=F)
spectrum(ts(ApothicUnit.train[,4]),log="no",plot=F)
spectrum(ts(ApothicUnit.train[,5]),log="no",plot=F)
spectrum(ts(ApothicUnit.train[,6]),log="no",plot=F)


lag1.plot(ApothicUnit.train[,2],8)  # 6
lag1.plot(ApothicUnit.train[,3],15) # 11
lag1.plot(ApothicUnit.train[,4],8)  # 8
lag1.plot(ApothicUnit.train[,5],8) # 5
lag1.plot(ApothicUnit.train[,6],8) # 6

acf2(ApothicUnit.train[,2],max.lag=100)
acf2(ApothicUnit.train[,3],max.lag=100)
acf2(ApothicUnit.train[,4],max.lag=100)
acf2(ApothicUnit.train[,5],max.lag=100)
acf2(ApothicUnit.train[,6],max.lag=100)

##### sand box
plot.ts(ApothicUnit.train[,3])
x = 1:272
y1 = 2*sin(2*pi*x*5/288)
y2 = 2*sin(2*pi*(x+20)*5/288)
y3 = 2*sin(2*pi*(x-20)*5/288)
plot.ts(cbind(y1,y2,y3))

plot(x,y1,type="l",xaxt = "n")
axis(1,at=x)

y11 = 2*sin(2*pi*x*5/288) + 2* cos(2*pi*x*5/288)
plot.ts(cbind(y1,y11))
##### end of sand box

#### 4.1.2.1.1. PRCP.mean
##### 4.1.2.1.1.1. sarima.for()
plot.ts(ts(ApothicUnit.train[,2]))
plot.ts(ts(ApothicUnit.train[,2]))
auto.arima(ts(ApothicUnit.train[,2]))
sarima(ts(ApothicUnit.train[,2]),2,0,2)
sarima(ts(ApothicUnit.train[,2]),2,0,2,1,0,0,S=52)
PRCP.sarima = sarima.for(ts(ApothicUnit.train[,2]),40,2,0,2)
PRCP.sarima.S = sarima.for(ts(ApothicUnit.train[,2]),40,2,0,2,1,0,0,S=52)

##### 4.1.2.1.1.2. bsts()
ss = AddLocalLevel(list(),ApothicUnit.train[,2]) 
ss = AddAutoAr(ss,ApothicUnit.train[,2])
ss = AddSeasonal(ss,ApothicUnit.train[,2],nseasons=52)
PRCP.bsts = bsts(ApothicUnit.train[,2],state.specification=ss,niter=500)
PRCP.bsts.pred = predict(PRCP.bsts,horizon=40)
plot(PRCP.bsts.pred)
PRCP.bsts.pred$mean

##### 4.1.2.1.1.3. comparing
plot(1:40,ApothicUnit.test$PRCP.mean,type="o")
points(1:40,as.vector(PRCP.sarima$pred),type="o",col="red")
points(1:40,as.vector(PRCP.sarima.S$pred),type="o",col="blue")
points(1:40,PRCP.bsts.pred$mean,type="o",col="orange")

Metrics.fun(ApothicUnit.test$PRCP.mean,as.vector(PRCP.sarima$pred))
Metrics.fun(ApothicUnit.test$PRCP.mean,as.vector(PRCP.sarima.S$pred))
Metrics.fun(ApothicUnit.test$PRCP.mean,PRCP.bsts.pred$mean) # the best?


#### 4.1.2.1.2. TMIN.mean
##### 4.1.2.1.2.1. sarima.for()
plot.ts(ts(ApothicUnit.train[,3]))
auto.arima(ts(ApothicUnit.train[,3]))
sarima(ts(ApothicUnit.train[,3]),1,0,1)
sarima(ts(ApothicUnit.train[,3]),1,0,1,1,0,0,S=52)
TMIN.sarima = sarima.for(ts(ApothicUnit.train[,3]),40,1,0,1)
TMIN.sarima.S = sarima.for(ts(ApothicUnit.train[,3]),40,1,0,1,1,0,0,S=52)

##### 4.1.2.1.2.2. bsts()
ss = AddLocalLinearTrend(list(),ApothicUnit.train[,3]) 
ss = AddAutoAr(ss,ApothicUnit.train[,3])
ss = AddSeasonal(ss,ApothicUnit.train[,3],nseasons=52)
TMIN.bsts = bsts(ApothicUnit.train[,3],state.specification=ss,niter=500)
TMIN.bsts.pred = predict(TMIN.bsts,horizon=40)
plot(TMIN.bsts.pred)
TMIN.bsts.pred$mean

##### 4.1.2.1.2.3. comparing
plot(1:40,ApothicUnit.test$TMIN.mean,type="o")
points(1:40,as.vector(TMIN.sarima$pred),type="o",col="red")
points(1:40,as.vector(TMIN.sarima.S$pred),type="o",col="blue")
points(1:40,TMIN.bsts.pred$mean,type="o",col="orange")

Metrics.fun(ApothicUnit.test$TMIN.mean,as.vector(TMIN.sarima$pred))
Metrics.fun(ApothicUnit.test$TMIN.mean,as.vector(TMIN.sarima.S$pred))
Metrics.fun(ApothicUnit.test$TMIN.mean,TMIN.bsts.pred$mean) # the best!


#### 4.1.2.1.3. AWND.mean
##### 4.1.2.1.3.1. sarima.for()
plot.ts(ts(ApothicUnit.train[,4]))
spectrum(ApothicUnit.train[,4],log="no")
spectrum(ApothicUnit.train[,4],log="no",plot=F)

auto.arima(ts(ApothicUnit.train[,4]))
sarima(ts(ApothicUnit.train[,4]),1,0,1)
sarima(ts(ApothicUnit.train[,4]),1,0,1,1,0,0,S=52)
AWND.sarima = sarima.for(ts(ApothicUnit.train[,4]),40,1,0,1)
AWND.sarima.S = sarima.for(ts(ApothicUnit.train[,4]),40,1,0,1,1,0,0,S=52)

##### 4.1.2.1.3.2. bsts()
ss = AddLocalLevel(list(),ApothicUnit.train[,4]) 
ss = AddAutoAr(ss,ApothicUnit.train[,4])
ss = AddSeasonal(ss,ApothicUnit.train[,4],nseasons=52)
AWND.bsts = bsts(ApothicUnit.train[,4],state.specification=ss,niter=500)
AWND.bsts.pred = predict(AWND.bsts,horizon=40)
plot(AWND.bsts.pred)
AWND.bsts.pred$mean

##### 4.1.2.1.3.3. comparing
plot(1:40,ApothicUnit.test$AWND.mean,type="o")
points(1:40,as.vector(AWND.sarima$pred),type="o",col="red")
points(1:40,as.vector(AWND.sarima.S$pred),type="o",col="blue")
points(1:40,AWND.bsts.pred$mean,type="o",col="orange")

Metrics.fun(ApothicUnit.test$AWND.mean,as.vector(AWND.sarima$pred))
Metrics.fun(ApothicUnit.test$AWND.mean,as.vector(AWND.sarima.S$pred))
Metrics.fun(ApothicUnit.test$AWND.mean,AWND.bsts.pred$mean) # the best


#### 4.1.2.1.4. SNOW.mean
##### 4.1.2.1.4.1. sarima.for()
plot.ts(ts(ApothicUnit.train[,5]))
auto.arima(ts(ApothicUnit.train[,5]))
sarima(ts(ApothicUnit.train[,5]),1,0,3)
sarima(ts(ApothicUnit.train[,5]),1,0,3,1,0,0,S=52)
SNOW.sarima = sarima.for(ts(ApothicUnit.train[,5]),40,1,0,3)
SNOW.sarima.S = sarima.for(ts(ApothicUnit.train[,5]),40,1,0,3,1,0,0,S=52)

##### 4.1.2.1.4.2. bsts()
ss = AddLocalLevel(list(),ApothicUnit.train[,5]) 
ss = AddAutoAr(ss,ApothicUnit.train[,5])
ss = AddSeasonal(ss,ApothicUnit.train[,5],nseasons=52)
SNOW.bsts = bsts(ApothicUnit.train[,5],state.specification=ss,niter=500)
SNOW.bsts.pred = predict(SNOW.bsts,horizon=40)
plot(SNOW.bsts.pred)
SNOW.bsts.pred$mean

##### 4.1.2.1.4.3. comparing
plot(1:40,ApothicUnit.test$SNOW.mean,type="o")
points(1:40,as.vector(SNOW.sarima$pred),type="o",col="red")
points(1:40,as.vector(SNOW.sarima.S$pred),type="o",col="blue")
points(1:40,SNOW.bsts.pred$mean,type="o",col="orange")

Metrics.fun(ApothicUnit.test$SNOW.mean,as.vector(SNOW.sarima$pred))
Metrics.fun(ApothicUnit.test$SNOW.mean,as.vector(SNOW.sarima.S$pred))
Metrics.fun(ApothicUnit.test$SNOW.mean,SNOW.bsts.pred$mean)


#### 4.1.2.1.5. SNWD.mean
##### 4.1.2.1.5.1. sarima.for()
names(ApothicUnit.df)
tail(ApothicUnit.df)
plot.ts(ts(ApothicUnit.df[,1]))
plot.ts(ts(ApothicUnit.df[,5]))
plot.ts(ts(ApothicUnit.df[,6]))

ApothicUnit.df %>%
  dplyr::select(apothicPremiumUnit,SNWD.mean,WeekNo,week.start) %>%
  dplyr::filter(SNWD.mean > 3)

plot.ts(ts(ApothicUnit.train[,6]))

auto.arima(ts(ApothicUnit.train[,6]))
sarima(ts(ApothicUnit.train[,6]),1,0,5)
sarima(ts(ApothicUnit.train[,6]),1,0,5,1,0,0,S=52)
SNWD.sarima = sarima.for(ts(ApothicUnit.train[,6]),40,1,0,5)
SNWD.sarima.S = sarima.for(ts(ApothicUnit.train[,6]),40,1,0,5,1,0,0,S=52)

##### 4.1.2.1.5.2. bsts()
ss = AddLocalLevel(list(),ApothicUnit.train[,6]) 
ss = AddAutoAr(ss,ApothicUnit.train[,6])
ss = AddSeasonal(ss,ApothicUnit.train[,6],nseasons=52)
SNWD.bsts = bsts(ApothicUnit.train[,6],state.specification=ss,niter=500)
SNWD.bsts.pred = predict(SNWD.bsts,horizon=40)
plot(SNWD.bsts.pred)
SNWD.bsts.pred$mean

##### 4.1.2.1.5.3. comparing
plot(1:40,ApothicUnit.test$SNWD.mean,type="o")
points(1:40,as.vector(SNWD.sarima$pred),type="o",col="red")
points(1:40,as.vector(SNWD.sarima.S$pred),type="o",col="blue")
points(1:40,SNWD.bsts.pred$mean,type="o",col="orange")

Metrics.fun(ApothicUnit.test$SNWD.mean,as.vector(SNWD.sarima$pred))
Metrics.fun(ApothicUnit.test$SNWD.mean,as.vector(SNWD.sarima.S$pred))
Metrics.fun(ApothicUnit.test$SNWD.mean,SNWD.bsts.pred$mean)


#### 4.1.2.1.6. CPI
setwd("C:\\Users\\YCHEN\\OneDrive - E & J Gallo Winery\\Desktop\\YCHEN\\Lectures\\Gallo analytics\\2024\\ts forecast contest")
(CPI_Monthly_file = list.files(pattern="^SeriesReport"))

CPI_Monthly_df = read.xlsx(CPI_Monthly_file,sheet="BLS Data Series",
                           startRow = 12)
str(CPI_Monthly_df)
names(CPI_Monthly_df)
unique(CPI_Monthly_df$Month)

CPI_Monthly_df = CPI_Monthly_df %>%
  pivot_longer(cols=2:13,names_to= "Month",values_to="CPI") %>%
  dplyr::select(-HALF1,-HALF2) %>%
  as.data.frame()

CPI_Monthly_df = CPI_Monthly_df %>%
  dplyr::mutate(Month_Num = plyr::mapvalues(CPI_Monthly_df$Month,
                                            from=Month_Mapping$Month_Letter,
                                            to=Month_Mapping$Replacement)) %>%
  dplyr::select(-Month) %>%
  dplyr::mutate(Month = as.factor(Month_Num)) %>%
  dplyr::select(-Month_Num) %>%
  as.data.frame()


Month_Mapping = data.frame(Month_Letter = c("Jan","Feb","Mar","Apr","May","Jun",
                                           "Jul","Aug","Sep","Oct","Nov","Dec"),
                          Replacement=1:12)


names(ApothicUnit.train)
str(ApothicUnit.train)

ApothicUnit.train = ApothicUnit.train %>%
  dplyr::left_join(CPI_Monthly_df,by=c("Year","Month")) %>%
  as.data.frame()

str(ApothicUnit.train)

ggplot(ApothicUnit.train,aes(x=week.end,y=CPI))+
  geom_point()+geom_line()

##### 4.1.2.1.6.1. bsts()
ss = AddLocalLevel(list(),ApothicUnit.train[,22]) 
ss = AddAutoAr(ss,ApothicUnit.train[,22])
ss = AddSeasonal(ss,ApothicUnit.train[,22],nseasons=52)
CPI.bsts = bsts(ApothicUnit.train[,22],state.specification=ss,niter=500)
CPI.bsts.pred = predict(CPI.bsts,horizon=40)
plot(CPI.bsts.pred)
CPI.bsts.pred$mean

CPI_Forecasted_df = data.frame(CPI_Forecasted = CPI.bsts.pred$mean,
                               week.end=ApothicUnit.test$week.end)
CPI_Forecasted_df = CPI_Forecasted_df %>%
  tidyr::separate(week.end,into=c("Year","Month","Day"),sep="-") %>%
  as.data.frame()


CPI_Forecasted_df %>%
  group_by(Year,Month) %>%
  summarize(CPI_Month_Mean = signif(mean(CPI_Forecasted),5)) %>%
  as.data.frame()



#### 4.1.2.1.6. newreg
##### 4.1.2.1.6.1. newreg
newxreg = data.frame(PRCP.mean = PRCP.bsts.pred$mean,
                     TMIN.mean=TMIN.bsts.pred$mean,
                     AWND.mean=AWND.bsts.pred$mean,
                     SNOW.mean=SNOW.bsts.pred$mean,
                     SNWD.mean=SNWD.bsts.pred$mean,
                     Christmas=ApothicUnit.test$Christmas,
                     Christmas.Back1 = ApothicUnit.test$Christmas.Back1,
                     Christmas.Back2 = ApothicUnit.test$Christmas.Back2,
                     NewYearsDay=ApothicUnit.test$NewYearsDay,
                     IndependenceDay=ApothicUnit.test$IndependenceDay,
                     LaborDay=ApothicUnit.test$LaborDay,
                     ThanksgivingDay=ApothicUnit.test$ThanksgivingDay,
                     MemorialDay=ApothicUnit.test$MemorialDay,
                     PresidentsDay=ApothicUnit.test$PresidentsDay,
                     MLKingsBirthday=ApothicUnit.test$MLKingsBirthday,
                     VeteransDay=ApothicUnit.test$VeteransDay,
                     WeekOfYear = ApothicUnit.test$WeekOfYear,
                     WeekNo=ApothicUnit.test$WeekNo,
                     Month=as.factor(ApothicUnit.test$Month),
                     Year=ApothicUnit.test$Year,
                     CPI=CPI.bsts.pred$mean)
newxreg = newxreg %>%
  dplyr::mutate(SNOW.Binary = ifelse(newxreg$SNOW.mean>=0.2,1,0),
                SNWD.Binary = ifelse(newxreg$SNWD.mean>=2,1,0)) %>%
  dplyr::mutate(SNOW.Binary.Back1 = c(SNOW.Binary[-1],0),
                SNWD.Binary.Back1 = c(SNWD.Binary[-1],0)) %>%
  as.data.frame()


names(newxreg)
str(newxreg)

names(ApothicUnit.train)
str(ApothicUnit.train)

auto.arima(ApothicUnit.train[,1])

##### 4.1.2.1.6.2. prediction
sarima_model3_exo = sarima(ts(ApothicUnit.train[,1]),0,1,2,1,0,0,S=52,
                       xreg=ApothicUnit.train[,c(3,26,27)])
acf2(sarima_model3_exo$fit$residuals,max.lag=100)
auto.arima(sarima_model3_exo$fit$residuals)

apply(ApothicUnit.train[,c(2:6,13:23)],
      MARGIN=2,
      FUN = function(x,y) cor(x,y),
      y = sarima_model3_exo$fit$residuals)


sarima_model3_exo_pred = sarima.for(ts(ApothicUnit.train[,1]),40,0,1,2,1,0,0,S=52,
                                xreg=ApothicUnit.train[,c(3,26,27,28)],
                                newxreg=newxreg[,c(2,24,25,21)])
Metrics.fun(sarima_model3_exo_pred$pred,ApothicUnit.test[,1])  
#   AE.mean    AE.se     RMSE
# 496.6725 439.7365 659.7102
sd(ApothicUnit.test[,1])









sarima_model3 = sarima(ts(ApothicUnit.train[,1]),3,1,0,1,0,0,S=52)
sarima_model3_pred = sarima.for(ts(ApothicUnit.train[,1]),40,3,1,0,1,0,0,S=52)$pred
Metrics.fun(sarima_model3_pred,ApothicUnit.test[,1])
#  AE.mean    AE.se   RMSE
# 517.5765 420.4517 663.51

###### sand box (to see effect of prediction period on accuracy)
####### 
sarima_model3_pred_20 = sarima.for(ts(ApothicUnit.train[,1]),20,3,1,0,1,0,0,S=52)$pred
Metrics.fun(sarima_model3_pred_20,ApothicUnit.test[1:20,1])

sarima_model3_pred_25 = sarima.for(ts(ApothicUnit.train[,1]),25,3,1,0,1,0,0,S=52)$pred
Metrics.fun(sarima_model3_pred_25,ApothicUnit.test[1:25,1])

sarima_model3_pred_30 = sarima.for(ts(ApothicUnit.train[,1]),30,3,1,0,1,0,0,S=52)$pred
Metrics.fun(sarima_model3_pred_30,ApothicUnit.test[1:30,1])

plot(1:30,sarima_model3_pred_30,type="o")
points(1:25,sarima_model3_pred_25,col="red")
lines(1:25,sarima_model3_pred_25,col="red")
###### end of sand box (to see effect of prediction period on accuracy)



###### plot sarima_model3_exo
plot(ApothicUnit.train$WeekNo,ApothicUnit.train$apothicPremiumUnit,
     type="o",xlim=c(0,313))
points(ApothicUnit.test$WeekNo,ApothicUnit.test$apothicPremiumUnit,type="o",col="red")
points(ApothicUnit.test$WeekNo,sarima_model3_exo_pred$pred,type="o",col="blue")


plot(ApothicUnit.test$WeekNo,ApothicUnit.test$apothicPremiumUnit,type="o",
     ylim=c(0,9000),xlab="Week No",ylab="Apothic Unit",
     xaxt="n",yaxt="n")
axis(1,at=ApothicUnit.test$WeekNo,labels=ApothicUnit.test$WeekNo)
axis(2,at=seq(0,9000,500),labels=seq(0,9000,500))
points(ApothicUnit.test$WeekNo,sarima_model3_exo_pred$pred,
       col="red")
lines(ApothicUnit.test$WeekNo,sarima_model3_exo_pred$pred,
       col="red",lwd=2)
lines(ApothicUnit.test$WeekNo,sarima_model3_exo_pred$pred+sarima_model3_exo_pred$se,
      type="l",lty=2,col="blue",lwd=2)
lines(ApothicUnit.test$WeekNo,sarima_model3_exo_pred$pred-sarima_model3_exo_pred$se,
      type="l",lty=2,col="blue",lwd=2)
text(x=282,y=8000,labels="Original",col="black")
text(x=282.5,y=7500,labels="Predicted",col="red")
text(x=287,y=7500,labels="(+/- 1SE)",col="blue")

###### plot sarima_model3 and sarima_model3_exo + bsts models (see below)
plot(ApothicUnit.test$WeekNo,ApothicUnit.test$apothicPremiumUnit,type="o",
     ylim=c(0,9000),xlab="Week No",ylab="Apothic Unit",
     xaxt="n",yaxt="n")
axis(1,at=ApothicUnit.test$WeekNo,labels=ApothicUnit.test$WeekNo)
axis(2,at=seq(0,9000,500),labels=seq(0,9000,500))
text(x=282,y=8000,labels="Original",col="black")  # original

points(ApothicUnit.test$WeekNo,sarima_model3_pred,col="orange",pch=5)
lines(ApothicUnit.test$WeekNo,sarima_model3_pred,col="orange",lwd=2)
text(x=286.25,y=7500,labels="SARIMA Without Exogenous",col="orange") # sarima 1

points(ApothicUnit.test$WeekNo,sarima_model3_exo_pred$pred,
       col="red")
lines(ApothicUnit.test$WeekNo,sarima_model3_exo_pred$pred,
      col="red",lwd=2)
text(x=285.75,y=7000,labels="SARIMA With Exogenous",col="red") # sarima 2

points(ApothicUnit.test$WeekNo,bsts.Local.Level.AutoAR.Pred$mean,
       col="cyan")
lines(ApothicUnit.test$WeekNo,bsts.Local.Level.AutoAR.Pred$mean,
      col="cyan",lwd=2)
text(x=282.1,y=6500,labels="STS-1S",col="cyan") # bsts 1

points(ApothicUnit.test$WeekNo,bsts.Local.Level.AutoAR.2S.Pred$mean,
       col="blue")
lines(ApothicUnit.test$WeekNo,bsts.Local.Level.AutoAR.2S.Pred$mean,
      col="blue",lwd=2)
text(x=282.1,y=6000,labels="STS-2S",col="blue") # bsts 2



mean(ApothicUnit.test$apothicPremiumUnit)
sd(ApothicUnit.test$apothicPremiumUnit)/sqrt(40)

mean(sarima_model3_pred)
sd(sarima_model3_pred)/sqrt(40)

mean(sarima_model3_exo_pred$pred)
sd(sarima_model3_exo_pred$pred)/sqrt(40)

ApothicUnit_Test_Pred_df = data.frame(Model = rep(c("Original","Without Exo","With Exo","STS-1S","STS-2S"),
                                                  each=40),
                                      Unit = c(ApothicUnit.test$apothicPremiumUnit,
                                               sarima_model3_pred,
                                               sarima_model3_exo_pred$pred,
                                               bsts.Local.Level.AutoAR.Pred$mean,
                                               bsts.Local.Level.AutoAR.2S.Pred$mean))
ApothicUnit_Test_Pred_Summary =  ApothicUnit_Test_Pred_df %>%
  dplyr::group_by(Model) %>%
  dplyr::summarize(Mean = mean(Unit),
                   SE = sd(Unit)/sqrt(40)) %>%
  as.data.frame()
ApothicUnit_Test_Pred_Summary

ggplot(ApothicUnit_Test_Pred_Summary,aes(x=Model,y=Mean,fill=Model))+
  geom_bar(stat="identity",width=0.5)+
  ylim(c(0,7000))+
  geom_errorbar(data=ApothicUnit_Test_Pred_Summary,
                aes(x=Model,ymin=Mean-2*SE,ymax=Mean+2*SE),
                width=0.1)+
  labs(y="Mean (+/- 2SE)")+
  annotate("text",x=1:5,y=3500,
           label=paste("mean = ",signif(ApothicUnit_Test_Pred_Summary$Mean,4),
                       sep=""))+
  annotate("text",x=1:5,y=3000,
           label=paste("SE = ",signif(ApothicUnit_Test_Pred_Summary$SE,4),
                       sep=""))+
  annotate("text",x=1:5,y=ApothicUnit_Test_Pred_Summary$Mean+
             2*ApothicUnit_Test_Pred_Summary$SE+300,
           label=c("a","a","a","a","a"))


ApothicUnit_Test_Pred_gls = gls(Unit~Model,data=ApothicUnit_Test_Pred_df,
                                weights=varIdent(form=~1|Model))
summary(ApothicUnit_Test_Pred_gls)
plot(ApothicUnit_Test_Pred_gls,resid(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
qqPlot(residuals(ApothicUnit_Test_Pred_gls,type="normalized"))
pairs(emmeans(ApothicUnit_Test_Pred_gls,"Model"))


ApothicUnit_Test_Pred_df_wider = data.frame(Original=ApothicUnit.test$apothicPremiumUnit,
                                           sarima_model3 = sarima_model3_pred,
                                           sarima_model3_exo = sarima_model3_exo_pred$pred,
                                           bsts_Local_Level_AutoAR=bsts.Local.Level.AutoAR.Pred$mean,
                                           bsts_Local_Level_AutoAR_2S = bsts.Local.Level.AutoAR.2S.Pred$mean)
apply(X=ApothicUnit_Test_Pred_df_wider[,2:5],
      MARGIN=2,
      FUN=function(x,y) cor(x,y),
      y=ApothicUnit_Test_Pred_df_wider[,1])
apply(X=ApothicUnit_Test_Pred_df_wider[,2:5],
      MARGIN=2,
      FUN=function(x,y) cor(x,y,method="spearman"),
      y=ApothicUnit_Test_Pred_df_wider[,1])



plot(ApothicUnit_Test_Pred_df_wider[,1],ApothicUnit_Test_Pred_df_wider[,2],
     xlab="Original",ylab="Predicted")
abline(a=0,b=1,lty=2)
text(x=6200,y=6300,label="y = x",srt=45)

points(ApothicUnit_Test_Pred_df_wider[,1],ApothicUnit_Test_Pred_df_wider[,2],
       col="orange")
text(x=4000,y=6800,label="r = 0.81 (sarima)",col="orange")

points(ApothicUnit_Test_Pred_df_wider[,1],ApothicUnit_Test_Pred_df_wider[,3],
       col="red")
text(x=4250,y=6600,label="r = 0.81 (sarima with exo)",col="red")

points(ApothicUnit_Test_Pred_df_wider[,1],ApothicUnit_Test_Pred_df_wider[,4],
       col="cyan")
text(x=4020,y=6400,label="r = 0.78 (STS-1S)",col="cyan")

points(ApothicUnit_Test_Pred_df_wider[,1],ApothicUnit_Test_Pred_df_wider[,5],
       col="blue")
text(x=4020,y=6200,label="r = 0.78 (STS-2S)",col="blue")





plot(ApothicUnit_Test_Pred_df_wider[,1],ApothicUnit_Test_Pred_df_wider[,3],
     xlab="Original",ylab="SARIMA with Exo")
abline(a=0,b=1,lty=2)
text(x=6200,y=6300,label="y = x",srt=45)
text(x=4000,y=6800,label="r = 0.81")







# 4.2. bsts 
names(ApothicUnit.train)

## 4.2.1. Local level model with S=52
ss = AddLocalLevel(list(),ApothicUnit.train[,1]) 
ss = AddSeasonal(ss,ApothicUnit.train[,1],nseasons=52)
bsts.Local.Level = bsts(ApothicUnit.train[,1],state.specification=ss,niter=1000)
bsts.Local.Level.Pred = predict(bsts.Local.Level,horizon=40,burn=200)
plot(bsts.Local.Level.Pred)
Metrics.fun(bsts.Local.Level.Pred$mean,ApothicUnit.test[,1])
### AE.mean   AE.se     RMSE
### 1 667.1934 395.169 772.9176

## 4.2.2. Local level with AutoAR and S=52
ss = AddLocalLevel(list(),ApothicUnit.train[,1]) 
ss = AddAutoAr(ss,ApothicUnit.train[,1])
ss = AddSeasonal(ss,ApothicUnit.train[,1],nseasons=52)
bsts.Local.Level.AutoAR = bsts(ApothicUnit.train[,1],state.specification=ss,niter=1000)
bsts.Local.Level.AutoAR.Pred = predict(bsts.Local.Level.AutoAR,horizon=40,burn=200)
plot(bsts.Local.Level.AutoAR.Pred)
Metrics.fun(bsts.Local.Level.AutoAR.Pred$mean,ApothicUnit.test[,1])
###    AE.mean    AE.se     RMSE
### 1 619.6444 390.5337 729.8376


## 4.2.3. Local trend model with AutoAR and S = 52
ss = AddLocalLinearTrend(list(),ApothicUnit.train[,1]) 
ss = AddAutoAr(ss,ApothicUnit.train[,1])
ss = AddSeasonal(ss,ApothicUnit.train[,1],nseasons=52)
bsts.Local.Trend.AutoAR = bsts(ApothicUnit.train[,1],state.specification=ss,niter=1000)
bsts.Local.Trend.AutoAR.Pred = predict(bsts.Local.Trend.AutoAR,horizon=40,burn=200)
plot(bsts.Local.Trend.AutoAR.Pred)
Metrics.fun(bsts.Local.Trend.AutoAR.Pred$mean,ApothicUnit.test[,1])
### AE.mean   AE.se     RMSE
### 1 619.3997 403.865 736.6718



## 4.2.4. Semi local trend model and S = 52
ss = AddSemilocalLinearTrend(list(),ApothicUnit.train[,1]) 
ss = AddSeasonal(ss,ApothicUnit.train[,1],nseasons=52)
bsts.SemiLocal.Trend = bsts(ApothicUnit.train[,1],state.specification=ss,niter=1000)
bsts.SemiLocal.Trend.Pred = predict(bsts.SemiLocal.Trend,horizon=40,burn=200)
plot(bsts.SemiLocal.Trend.Pred)
Metrics.fun(bsts.SemiLocal.Trend.Pred$mean,ApothicUnit.test[,1])
### AE.mean    AE.se    RMSE
### 1 612.7983 402.9007 730.611


## 4.2.5. Local level with AutoAR, S = 4, and S=52
ss = AddLocalLevel(list(),ApothicUnit.train[,1]) 
ss = AddAutoAr(ss,ApothicUnit.train[,1])
ss = AddSeasonal(ss,ApothicUnit.train[,1],nseasons=4)
ss = AddSeasonal(ss,ApothicUnit.train[,1],nseasons=52)
bsts.Local.Level.AutoAR.2S = bsts(ApothicUnit.train[,1],state.specification=ss,niter=1000)
bsts.Local.Level.AutoAR.2S.Pred = predict(bsts.Local.Level.AutoAR.2S,horizon=40,burn=200)
plot(bsts.Local.Level.AutoAR.2S.Pred)
Metrics.fun(bsts.Local.Level.AutoAR.2S.Pred$mean,ApothicUnit.test[,1])
###    AE.mean    AE.se     RMSE
### 1 604.9933 399.4791 722.2263


CompareBstsModels(list("Local Level" = bsts.Local.Level,
                       "Local Level AutoAR" = bsts.Local.Level.AutoAR,
                       "Local Trend AutoAR" = bsts.Local.Trend.AutoAR),
                  colors = c("black", "red", "blue")) # Local.Level.AutoAR preferred

CompareBstsModels(list("Local Level AutoAR" = bsts.Local.Level.AutoAR,
                       "Semical Local Trend" = bsts.SemiLocal.Trend,
                       "Local Level AutoAR-2 Seasons"=bsts.Local.Level.AutoAR.2S),
                  colors = c("black", "red", "blue"))  # Local.Level.AutoAR preferred

mean(ApothicUnit.test[,1])
mean(bsts.Local.Level.Pred$mean)
mean(bsts.Local.Level.AutoAR.Pred$mean)
mean(bsts.Local.Trend.AutoAR.Pred$mean)
mean(bsts.SemiLocal.Trend.Pred$mean)
mean(bsts.Local.Level.AutoAR.2S.Pred$mean)


ss = AddLocalLevel(list(),ApothicUnit.train[,1]) 
ss = AddAutoAr(ss,ApothicUnit.train[,1])
ss = AddSeasonal(ss,ApothicUnit.train[,1],nseasons=4)
ss = AddSeasonal(ss,ApothicUnit.train[,1],nseasons=52)
bsts.Local.Level.AutoAR.2S = bsts(ApothicUnit.train[,1]~PRCP.mean+TMIN.mean+AWND.mean+
                                    SNOW.mean+SNWD.mean+Christmas+Christmas.Back1+
                                    Christmas.Back2+NewYearsDay+IndependenceDay+LaborDay+
                                    ThanksgivingDay+MemorialDay+PresidentsDay+
                                    MLKingsBirthday+VeteransDay+
                                    SNOW.Binary+SNWD.Binary+SNOW.Binary.Back1+SNWD.Binary.Back1,
                                  state.specification=ss,niter=1000,
                                  data=ApothicUnit.train)
plot(bsts.Local.Level.AutoAR.2S)
plot(bsts.Local.Level.AutoAR.2S,"coef")
bsts.Local.Level.AutoAR.2S.Pred = predict(bsts.Local.Level.AutoAR.2S,
                                          newdata=newxreg,
                                          horizon=40,burn=200)
plot(bsts.Local.Level.AutoAR.2S.Pred)

Metrics.fun(bsts.Local.Level.AutoAR.2S.Pred$mean,ApothicUnit.test[,1])
###    AE.mean    AE.se     RMSE
### 1 604.9933 399.4791 722.2263










# 4.3. GLS (not good enough)
## 4.3.1. data
str(ApothicUnit.train)
plot.ts(ApothicUnit.df$apothicPremiumUnit)
plot.ts(log10(ApothicUnit.df$apothicPremiumUnit))
acf2(ApothicUnit.df$apothicPremiumUnit)

## 4.3.2. baseline
gls0 = gls(log10(apothicPremiumUnit)~WeekNo+PRCP.mean+TMIN.mean+AWND.mean+SNOW.mean+SNWD.mean+
             Christmas+NewYearsDay+IndependenceDay+LaborDay+ThanksgivingDay+MemorialDay+
             PresidentsDay+MLKingsBirthday+VeteransDay,
           data=ApothicUnit.train)
summary(gls0)
plot(gls0,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
acf2(residuals(gls0,type="normalized"),max.lag=100)

## 4.3.3. bs(WeekOfYear)
gls1 = gls(log10(apothicPremiumUnit)~bs(WeekNo)+PRCP.mean+TMIN.mean+AWND.mean+SNOW.mean+SNWD.mean+
             Christmas+NewYearsDay+IndependenceDay+LaborDay+ThanksgivingDay+MemorialDay+
             PresidentsDay+MLKingsBirthday+VeteransDay,
           data=ApothicUnit.train)
plot(gls1,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
anova(gls0,gls1)  # gls1 is preferred, but won't use it here

## 4.3.4. AR1
gls2 = gls(log10(apothicPremiumUnit)~WeekNo+PRCP.mean+TMIN.mean+AWND.mean+SNOW.mean+SNWD.mean+
             Christmas+NewYearsDay+IndependenceDay+LaborDay+ThanksgivingDay+MemorialDay+
             PresidentsDay+MLKingsBirthday+VeteransDay,
           correlation=corAR1(form=~1|Year),
           data=ApothicUnit.train)
plot(gls2,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
acf2(residuals(gls2,type="normalized"),max.lag=80)  # ar1 is inadequate
anova(gls0,gls2)  # gls2 is preferred

## 4.3.5. AR3
gls3 = gls(log10(apothicPremiumUnit)~WeekNo+PRCP.mean+TMIN.mean+AWND.mean+SNOW.mean+SNWD.mean+
             Christmas+NewYearsDay+IndependenceDay+LaborDay+ThanksgivingDay+MemorialDay+
             PresidentsDay+MLKingsBirthday+VeteransDay,
           correlation=corARMA(form=~1|Year,p=3,q=0),
           data=ApothicUnit.train)
plot(gls3,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
acf2(residuals(gls3,type="normalized"),max.lag=80)  # ar3 is Okay. Seasonality exists
anova(gls2,gls3)  # gls3 is preferred


## 4.3.6. AR3 and varcomp
gls4 = gls(log10(apothicPremiumUnit)~WeekNo+PRCP.mean+TMIN.mean+AWND.mean+SNOW.mean+SNWD.mean+
             Christmas+NewYearsDay+IndependenceDay+LaborDay+ThanksgivingDay+MemorialDay+
             PresidentsDay+MLKingsBirthday+VeteransDay,
           correlation=corARMA(form=~1|Year,p=3,q=0),
           weights=varPower(),
           method="ML",
           data=ApothicUnit.train)
plot(gls4,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))
acf2(residuals(gls4,type="normalized"),max.lag=80)  # ar3 is Okay. Seasonality exists
anova(gls3,gls4)  # gls3 is preferred


## 4.3.7. AR3 and seasonality
ApothicUnit.train$WeekNo
str(ApothicUnit.train)
spectrum(residuals(gls4,type="normalized"),log="no")
spectrum(residuals(gls4,type="normalized"),log="no",plot=F)

plot.ts(ApothicUnit.df$TMIN.mean)
TMIN.mean=sarima.for(ts(ApothicUnit.train[,3]),40,3,0,0,1,0,0,S=52)


gls5 = gls(log10(apothicPremiumUnit)~WeekNo+TMIN.mean+
             Christmas+NewYearsDay+ThanksgivingDay+MemorialDay+
             MLKingsBirthday+
             sin(2*pi*WeekNo*5/288)+
                                     cos(2*pi*WeekNo*17/288)+
                                     cos(2*pi*WeekNo*18/288)+
                                     cos(2*pi*WeekNo*26/288)+
                                     cos(2*pi*WeekNo*27/288)+
                                     cos(2*pi*WeekNo*34/288)+
             sin(2*pi*WeekNo*38/288)+
                                     cos(2*pi*WeekNo*44/288)+
                                     cos(2*pi*WeekNo*46/288)+
             sin(2*pi*WeekNo*49/288)+
             sin(2*pi*WeekNo*50/288)+cos(2*pi*WeekNo*50/288)+
                                     cos(2*pi*WeekNo*51/288)+
                                     cos(2*pi*WeekNo*55/288)+
             sin(2*pi*WeekNo*56/288)+
             sin(2*pi*WeekNo*61/288)+cos(2*pi*WeekNo*61/288)+
             sin(2*pi*WeekNo*66/288)+cos(2*pi*WeekNo*66/288)+
                                     cos(2*pi*WeekNo*67/288)+
                                     cos(2*pi*WeekNo*72/288)+
             sin(2*pi*WeekNo*78/288)+
             sin(2*pi*WeekNo*96/288)+
             sin(2*pi*WeekNo*101/288)+cos(2*pi*WeekNo*101/288)+
             sin(2*pi*WeekNo*106/288)+cos(2*pi*WeekNo*106/288)+
             sin(2*pi*WeekNo*107/288)+
             sin(2*pi*WeekNo*116/288)+cos(2*pi*WeekNo*116/288)+
             sin(2*pi*WeekNo*120/288)+
                                      cos(2*pi*WeekNo*122/288)+
             sin(2*pi*WeekNo*124/288)+
             sin(2*pi*WeekNo*129/288)+
             sin(2*pi*WeekNo*134/288),
           correlation=corARMA(form=~1|Year,p=3,q=0),
        #   method="ML",
           data=ApothicUnit.train)
plot(gls5,residuals(.,type="normalized")~fitted(.),
     type=c("p","smooth"))

anova(gls4,gls5)
acf2(residuals(gls5,type="normalized"),max.lag=100)  # looks good?

spectrum(residuals(gls5,type="normalized"),log="no")
spectrum(residuals(gls5,type="normalized"),log="no",plot=F)

summary(gls5)

gls5.pred = predict(gls5,newdata=newxreg)

Metrics.fun(10^gls5.pred,ApothicUnit.test[,1])  # overfitting?
#   AE.mean    AE.se     RMSE
#  1196.068 855.1682 1464.107





#### forecastML pkg
##### Data preparation
names(ApothicUnit.train)
horizons = c(1,10,20,30,40)
lookback = c(1:10,20,30,40,52)
dynamic_features = c("Christmas", "NewYearsDay","IndependenceDay", "LaborDay",
                     "ThanksgivingDay", "MemorialDay", "PresidentsDay",
                     "MLKingsBirthday","VeteransDay")
date_frequency = "1 week"
ApothicUnit.train.list = forecastML::create_lagged_df(ApothicUnit.train[,c(1:6,13:21)],
                                                outcome_col=1,
                                                type="train",
                                                horizons=horizons,
                                                lookback=lookback,
                                          #     dynamic_features=dynamic_features,
                                                date=dates[1:nrow(ApothicUnit.train)],
                                                frequency=date_frequency)
str(ApothicUnit.train.list)


plot(ApothicUnit.train.list)

ApothicUnit.windows <- forecastML::create_windows(lagged_df = ApothicUnit.train.list,
                                                  window_length = 12, 
                                                  skip = 80,
                                                  window_start = NULL, 
                                                  window_stop = NULL,
                                                  include_partial_window = TRUE)
ApothicUnit.windows

plot(ApothicUnit.windows, ApothicUnit.train.list, show_labels = TRUE)


##### Model training
###### User-defined modeling function
rf.train.fun <- function(data) {
  outcome_names <- names(data)[1]
  model_formula <- formula(paste0(outcome_names,  "~ ."))
  model <- randomForest::randomForest(formula = model_formula, 
                                      data = data, ntree = 1500)
  return(model) }


rf.train.fun <- function(data) {
  outcome_names <- names(data)[1]
  rfGrid = data.frame(mtry=1:(length(names(data))-1))
  model <- caret::train(x=data[,-1],
                        y=data[,1],
                        method="rf",
                        metric="RMSE",
                        tuneGrid=rfGrid,
                        preprocess=c("scale","center"),
                        ntree = 1000)
  return(model) }



lasso.train.fun <- function(data) {
  constant_features <- which(unlist(lapply(data[, -1], function(x) {!(length(unique(x)) > 1)})))
  if (length(constant_features) > 1) {
    data <- data[, -c(constant_features + 1)]  # +1 because we're skipping over the outcome column.
  }
  
  x <- data[, -(1), drop = FALSE]
  y <- data[, 1, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))
  
  model <- glmnet::cv.glmnet(x, y, nfolds = 3)
  return(list("model" = model, "constant_features" = constant_features))
}





###### model training
system.time(
  ApothicUnit.train.rf.results <- forecastML::train_model(ApothicUnit.train.list, 
                                                        ApothicUnit.windows, 
                                                        model_name = "RF",
                                                        rf.train.fun,
                                                        use_future = FALSE)
)

system.time(
  ApothicUnit.train.lasso.results <- forecastML::train_model(ApothicUnit.train.list, 
                                                          ApothicUnit.windows, 
                                                          model_name = "LASSO",
                                                          lasso.train.fun,
                                                          use_future = FALSE)
)



###### User-defined prediction function
rf.pred.fun <- function(model, data_features) {
  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)}


lasso.pred.fun <- function(model, data_features) {
  if (length(model$constant_features) > 1) {  # 'model' was passed as a list.
    data_features <- data_features[, -c(model$constant_features )]
  }
  x <- as.matrix(data_features, ncol = ncol(data_features))
  data_pred <- data.frame("y_pred" = predict(model$model, x, s = "lambda.min"))
  return(data_pred)
}



##### Predict on historical data
ApothicUnit.train.pred <- predict(ApothicUnit.train.rf.results,ApothicUnit.train.lasso.results,
                                  prediction_function = list(rf.pred.fun,lasso.pred.fun), 
                                  data = ApothicUnit.train.list)


plot(ApothicUnit.train.pred, type = "prediction", horizons = horizons)
plot(ApothicUnit.train.pred, type = "residual", horizons = horizons)
plot(ApothicUnit.train.pred, type = "forecast_stability", windows=2)
plot(ApothicUnit.train.pred, type = "forecast_stability", windows=3)




#### Model performance
ApothicUnit.train.error = forecastML::return_error(ApothicUnit.train.pred)
ApothicUnit.train.error

plot(ApothicUnit.train.error, type = "window", 
     facet = ~ horizon, horizons = horizons)
plot(ApothicUnit.train.error, type = "horizon", 
     facet = ~ horizon, horizons = horizons)
plot(ApothicUnit.train.error, type = "global", 
     facet = ~ horizon, horizons = horizons)



### Hyperparameters
#### User-defined hyperparameter function




### Forecast with Multiple Models from Nested CV
#### new data
ApothicUnit.forecast.list <- forecastML::create_lagged_df(ApothicUnit.train[,c(1:6,11:19)],
                                                          outcome_col=1,
                                                          type="forecast",
                                                          horizons=horizons,
                                                          lookback=lookback,
                                                      #    dynamic_features=dynamic_features,
                                                          date=dates[1:nrow(ApothicUnit.train)],
                                                          frequency=date_frequency)

#### dynamic features (for test set)
for (i in seq_along(ApothicUnit.forecast.list)) {
  ApothicUnit.forecast.list[[i]]$X =  one_value
   
}


####- forecast resutls
ApothicUnit.forecast = predict(ApothicUnit.train.rf.results,
                               prediction_function = list(rf.pred.fun), 
                               data =ApothicUnit.forecast.list)



ApothicUnit.forecast.40 = ApothicUnit.forecast %>%
  dplyr::filter(model_forecast_horizon == 40 & window_number==3)
  
Metrics.fun(ApothicUnit.forecast.40$apothicPremiumUnit_pred,
            ApothicUnit.test$apothicPremiumUnit)

# holidays included
#    AE.mean   AE.se     RMSE
#   555.8225 535.368 767.0672

# Holidays not included 
#    AE.mean    AE.se     RMSE
# 1 579.6102 596.0708 826.0544



#### compare forecast and original
plot(ApothicUnit.forecast,
     data_actual = ApothicUnit.df[,c(1:6,11:19)],  
     actual_indices = dates, 
     horizons = horizons)


### Forecast error
ApothicUnit.forcest.error <- forecastML::return_error(ApothicUnit.forecast,
                                       data_test = ApothicUnit.test,
                                       test_indices = dates[(nrow(ApothicUnit.train) + 1):length(dates)])

plot(ApothicUnit.forcest.error, facet = ~ horizon, type = "window")
plot(ApothicUnit.forcest.error, facet = ~ horizon, type = "horizon")
plot(ApothicUnit.forcest.error, facet = ~ horizon, type = "global")


### Model selection and re-training (without validation)
#### retraining
names(ApothicUnit.train)
ApothicUnit.train.list = forecastML::create_lagged_df(ApothicUnit.train[,c(1:9,13:21)],
                                                      outcome_col=1,
                                                      type="train",
                                                      horizons=horizons,
                                                      lookback=lookback,
                                                      #     dynamic_features=dynamic_features,
                                                      date=dates[1:nrow(ApothicUnit.train)],
                                                      frequency=date_frequency)
ApothicUnit.windows <- forecastML::create_windows(ApothicUnit.train.list, window_length = 0)

plot(ApothicUnit.windows, ApothicUnit.train.list, show_labels = TRUE)


ApothicUnit.train.rf.results <- forecastML::train_model(ApothicUnit.train.list, 
                                                        ApothicUnit.windows, 
                                                        model_name = "RF",
                                                        rf.train.fun,
                                                        use_future = FALSE)
ApothicUnit.train.pred <- predict(ApothicUnit.train.rf.results,
                                  prediction_function = list(rf.pred.fun), 
                                  data = ApothicUnit.train.list)


plot(ApothicUnit.train.pred, type = "prediction", horizons = c(1,10,20,30,40))
     
#### Training error      
ApothicUnit.all.error <- forecastML::return_error(ApothicUnit.train.pred)   
plot(ApothicUnit.all.error, type = "window", 
     facet = ~ horizon, horizons = horizons)
plot(ApothicUnit.all.error, type = "horizon", 
     facet = ~ horizon, horizons = horizons)    
   

#### re-forecasting without validation
ApothicUnit.forecast.list <- forecastML::create_lagged_df(ApothicUnit.train[,c(1:9,13:21)],
                                                          outcome_col=1,
                                                          type="forecast",
                                                          horizons=horizons,
                                                          lookback=lookback,
                                                    #     dynamic_features=dynamic_features,
                                                          date=dates[1:nrow(ApothicUnit.train)],
                                                          frequency=date_frequency)
####- forecast resutls
ApothicUnit.forecast = predict(ApothicUnit.train.rf.results,
                               prediction_function = list(rf.pred.fun), 
                               data =ApothicUnit.forecast.list)



ApothicUnit.forecast.40 = ApothicUnit.forecast %>%
  dplyr::filter(model_forecast_horizon == 40)

Metrics.fun(ApothicUnit.forecast.40$apothicPremiumUnit_pred,
            ApothicUnit.test$apothicPremiumUnit)   

#    AE.mean    AE.se     RMSE
# 1 639.9435 469.3502 790.1329


#### compare forecast and original
plot(ApothicUnit.forecast,
     data_actual = ApothicUnit.df[,c(1:9,13:21)],  
     actual_indices = dates, 
     horizons = horizons)



### Forecast with 1 model per horizon    
ApothicUnit.forecast.list <- forecastML::create_lagged_df(ApothicUnit.train[,c(1:9,13:21)],
                                                          outcome_col=1,
                                                          type="forecast",
                                                          horizons=horizons,
                                                          lookback=lookback,
                                                    #     dynamic_features=dynamic_features,
                                                          date=dates[1:nrow(ApothicUnit.train)],
                                                          frequency=date_frequency)
ApothicUnit.forecast <- predict(ApothicUnit.train.rf.results,
                                prediction_function = list(rf.pred.fun), 
                                  data = ApothicUnit.forecast.list)
plot(ApothicUnit.forecast,
     data_actual = ApothicUnit.df,  
     actual_indices = dates, 
     horizons = horizons)

#### Forecast error 
ApothicUnit.all.error <- forecastML::return_error(ApothicUnit.forecast,
                                                  data_test=ApothicUnit.test,
                                                  test_indices = dates[(nrow(ApothicUnit.train) + 1):length(dates)])
plot(ApothicUnit.all.error, type = "horizon", 
     facet = ~ horizon, horizons = horizons) 


### Forecast Combination with forecastML::combine_forecasts
ApothicUnit.combined <- forecastML::combine_forecasts(ApothicUnit.forecast)

plot(ApothicUnit.combined,
     data_actual = ApothicUnit.df,  
     actual_indices = dates, 
     horizons = horizons)

Metrics.fun(ApothicUnit.combined$apothicPremiumUnit_pred,
            ApothicUnit.test$apothicPremiumUnit)
#   AE.mean    AE.se     RMSE
#  643.2795 448.9753 781.2477

Metrics.fun(ApothicUnit.combined$apothicPremiumUnit_pred[1:10],
            ApothicUnit.test$apothicPremiumUnit[1:10])
Metrics.fun(ApothicUnit.combined$apothicPremiumUnit_pred[1:20],
            ApothicUnit.test$apothicPremiumUnit[1:20])
Metrics.fun(ApothicUnit.combined$apothicPremiumUnit_pred[1:30],
            ApothicUnit.test$apothicPremiumUnit[1:30])




### Summary

ApothicUnit_pred_df = data.frame(Date = ApothicUnit.test$week.end,
                                 Ori = ApothicUnit.test$apothicPremiumUnit,
                                 AR1.Pred = sarima_model1_pred,
                                 AR1.S1.Pred = sarima_model2_pred,
                                 Exogenous.Pred = sarima_model3_pred,
                                 GLS.Pred = gls5.pred,
                                 forecastML.Pred = ApothicUnit.combined$apothicPremiumUnit_pred)

ggplot(ApothicUnit_pred_df,aes(x=Date,y=Ori))+
  geom_point()+geom_line()+
  geom_point(aes(y=AR1.Pred),color="blue")+
  geom_line(aes(y=AR1.Pred),color="blue")+
  geom_point(aes(y=AR1.S1.Pred),color="red")+
  geom_line(aes(y=AR1.S1.Pred),color="red")+
  geom_point(aes(y=Exogenous.Pred),color="orange")+
  geom_line(aes(y=Exogenous.Pred),color="orange")+
  geom_point(aes(y=GLS.Pred),color="green")+
  geom_line(aes(y=GLS.Pred),color="green")+
  geom_point(aes(y=forecastML.Pred),color="cyan")+
  geom_line(aes(y=forecastML.Pred),color="cyan")+
  labs(y="Original and Predicted")



# 3.5. bsts package
names(ApothicUnit.train)
names(ApothicUnit.test)

plot.ts(ApothicUnit.train[,1])

## 3.5.1. without exogenous predictors
### 3.5.1.1. ts without transformation
#### 3.5.1.1.1. Local Linear Trend model + AR
ss = AddLocalLinearTrend(list(),ApothicUnit.train[,1])
ss = AddAutoAr(ss,ApothicUnit.train[,1])
bsts.model1 = bsts(ApothicUnit.train[,1],state.specification=ss,niter=500)
bsts.model1.pred = predict(bsts.model1,horizon=40)
plot(bsts.model1.pred,plot.original=272)
Metrics.fun(bsts.model1.pred$mean,ApothicUnit.test[,1])
#  AE.mean    AE.se     RMSE
# 1022.233 737.4752 1255.083

#### 3.5.1.1.2. Local Linear Trend model + AR(auto) + Seasonality (better)
ss = AddLocalLinearTrend(list(),ApothicUnit.train[,1])
ss = AddAutoAr(ss,ApothicUnit.train[,1])
ss = AddSeasonal(ss,ApothicUnit.train[,1],nseasons=52)
bsts.model2 = bsts(ApothicUnit.train[,1],state.specification=ss,niter=500)
bsts.model2.pred = predict(bsts.model2,horizon=40)
plot(bsts.model2.pred,plot.original=272)
Metrics.fun(bsts.model2.pred$mean,ApothicUnit.test[,1]) 
#  AE.mean    AE.se     RMSE
# 649.0201 403.5309 761.5729
plot(1:40,ApothicUnit.test[,1],type="o")
points(1:40,bsts.model2.pred$mean,col="red")
lines(1:40,bsts.model2.pred$mean,col="red")


#### 3.5.1.1.3. Local rend model + AR(auto) + Seasonality
ss = AddLocalLevel(list(),ApothicUnit.train[,1])
ss = AddAutoAr(ss,ApothicUnit.train[,1])
ss = AddSeasonal(ss,ApothicUnit.train[,1],nseasons=52)
bsts.model2 = bsts(ApothicUnit.train[,1],state.specification=ss,niter=2000)
bsts.model2.pred = predict(bsts.model2,horizon=40,burn=300)
plot(bsts.model2.pred,plot.original=272)
Metrics.fun(bsts.model2.pred$mean,ApothicUnit.test[,1]) 
#   AE.mean    AE.se     RMSE
#  615.8597 385.0242 723.7546
plot(1:40,ApothicUnit.test[,1],type="o")
points(1:40,bsts.model2.pred$mean,col="red")
lines(1:40,bsts.model2.pred$mean,col="red")



#### 3.5.1.1.4. Local Linear Trend model + AR3 + Seasonality (similar)
ss = AddLocalLinearTrend(list(),ApothicUnit.train[,1])
ss = AddAr(ss,ApothicUnit.train[,1],lags=1)
ss = AddAr(ss,ApothicUnit.train[,1],lags=2)
ss = AddAr(ss,ApothicUnit.train[,1],lags=3)
ss = AddSeasonal(ss,ApothicUnit.train[,1],nseasons=52)
bsts.model3 = bsts(ApothicUnit.train[,1],state.specification=ss,niter=500,seed=123)
bsts.model3.pred = predict(bsts.model3,horizon=40)
plot(bsts.model3.pred,plot.original=272)
Metrics.fun(bsts.model3.pred$mean,ApothicUnit.test[,1])
#  AE.mean    AE.se     RMSE
# 622.4465 443.799 761.2314
plot(1:40,ApothicUnit.test[,1],type="o")
points(1:40,bsts.model3.pred$mean,col="red")
lines(1:40,bsts.model3.pred$mean,col="red")



### 3.5.1.2. ts transformed  (not needed)
#### 3.5.1.2.1. Local Linear Trend + AR(auto) + Seasonal
ss = AddLocalLinearTrend(list(),log10(ApothicUnit.train[,1]))
ss = AddAutoAr(ss,log10(ApothicUnit.train[,1]))
ss = AddSeasonal(ss,log10(ApothicUnit.train[,1]),nseasons=52)
bsts.model4 = bsts(log10(ApothicUnit.train[,1]),state.specification=ss,niter=500)
bsts.model4.pred = predict(bsts.model4,horizon=40)
plot(bsts.model4.pred,plot.original=272)
Metrics.fun(10^bsts.model4.pred$mean,ApothicUnit.test[,1])
#  AE.mean    AE.se     RMSE
# 638.4872 448.4288 776.9988

#### 3.5.1.2.2. Local Linear Trend model + AR3 + Seasonality
ss = AddLocalLinearTrend(list(),log10(ApothicUnit.train[,1]))
ss = AddAr(ss,log10(ApothicUnit.train[,1]),lags=1)
#ss = AddAr(ss,log10(ApothicUnit.train[,1]),lags=2)
#ss = AddAr(ss,log10(ApothicUnit.train[,1]),lags=3)
ss = AddSeasonal(ss,log10(ApothicUnit.train[,1]),nseasons=52)
bsts.model5 = bsts(log10(ApothicUnit.train[,1]),state.specification=ss,niter=500)
bsts.model5.pred = predict(bsts.model5,horizon=40)
plot(bsts.model5.pred,plot.original=272)
Metrics.fun(10^bsts.model5.pred$mean,ApothicUnit.test[,1])
#  AE.mean    AE.se     RMSE
# 636.5058 441.348 771.4


## 3.5.2. with exogenous predictors
### 3.5.2.1. ts without transformation
names(ApothicUnit.train)
ss = AddLocalLinearTrend(list(),ApothicUnit.train[,1])
ss = AddAutoAr(ss,ApothicUnit.train[,1])
ss = AddSeasonal(ss,ApothicUnit.train[,1],nseasons=52)
bsts.model6 = bsts(apothicPremiumUnit~PRCP.mean+TMIN.mean+AWND.mean+
                     SNOW.mean+SNWD.mean+WeekOfYear+WeekNo+Christmas+
                     ThanksgivingDay+NewYearsDay+Christmas.Back1,
                   state.specification=ss,niter=500,
                   data=ApothicUnit.train,
                   expected.model.size=3)
bsts.model6.pred = predict(bsts.model6,horizon=40,newdata=ApothicUnit.test)
plot(bsts.model6.pred,plot.original=272)
plot(bsts.model6,"coefficient")
Metrics.fun(bsts.model6.pred$mean,ApothicUnit.test[,1])
#  AE.mean    AE.se     RMSE
# 631.6605 403.85 747.0023
plot(1:40,ApothicUnit.test[,1],type="o")
points(1:40,bsts.model6.pred$mean,col="red")
lines(1:40,bsts.model6.pred$mean,col="red")

### 3.5.2.2. ts without transformation
ss = AddLocalLinearTrend(list(),log10(ApothicUnit.train[,1]))
ss = AddAutoAr(ss,log10(ApothicUnit.train[,1]))
ss = AddSeasonal(ss,log10(ApothicUnit.train[,1]),nseasons=52)
bsts.model7 = bsts(log10(apothicPremiumUnit)~PRCP.mean+TMIN.mean+AWND.mean+
                     SNOW.mean+SNWD.mean+WeekOfYear+WeekNo+Christmas+
                     ThanksgivingDay+NewYearsDay+Christmas.Back1,
                   state.specification=ss,niter=500,
                   data=ApothicUnit.train,
                   expected.model.size=3)
bsts.model7.pred = predict(bsts.model7,horizon=40,newdata=ApothicUnit.test)
plot(bsts.model7.pred,plot.original=272)
plot(bsts.model7,"coefficient")
Metrics.fun(10^bsts.model7.pred$mean,ApothicUnit.test[,1])
#  AE.mean    AE.se     RMSE
# 640.125 448.7437 778.5221
plot(1:40,ApothicUnit.test[,1],type="o")
points(1:40,10^bsts.model7.pred$mean,type="o",col="red")
#lines(1:40,10^bsts.model7.pred$mean,col="red")



CompareBstsModels(list("Model1"=bsts.model1,
                       "Model2"=bsts.model2,
                       "Model3"=bsts.model3,
                       "Model6"=bsts.model6))  # model6 or 2the best

CompareBstsModels(list("Model4"=bsts.model4,
                       "Model5"=bsts.model5,
                       "Model7"=bsts.model7))  # similar






#### mbsts package
names(ApothicUnit.train)
names(ApothicUnit.test)

plot.ts(ApothicUnit.train[,c(1:6,13:15)])
plot.ts(ApothicUnit.train[,3])
plot.ts(ApothicUnit.train[,4])

Ytrain = as.matrix(ApothicUnit.train[,c(1:6)])
Xtrain = as.matrix(ApothicUnit.train[,c(13:21)])
dim(Ytrain)
dim(Xtrain)
Xtrain.star = cbind(Xtrain,Xtrain,Xtrain,Xtrain,Xtrain,Xtrain)

##### no regression component
spectrum(ApothicUnit.train[,1],log="no")
spectrum(ApothicUnit.train[,1],log="no",plot=F)

spectrum(ApothicUnit.train[,2],log="no")
spectrum(ApothicUnit.train[,2],log="no",plot=F)

spectrum(ApothicUnit.train[,3],log="no")
spectrum(ApothicUnit.train[,3],log="no",plot=F)

spectrum(ApothicUnit.train[,4],log="no")
spectrum(ApothicUnit.train[,4],log="no",plot=F)

spectrum(ApothicUnit.train[,5],log="no")
spectrum(ApothicUnit.train[,5],log="no",plot=F)

spectrum(ApothicUnit.train[,6],log="no")
spectrum(ApothicUnit.train[,6],log="no",plot=F)

mbsts_no_reg = tsc.setting(Ytrain,
                           mu=c(1,1,1,1,1,1),
                           rho=c(0.8,0,0,0,0,0),
                           S=c(52,52,52,52,52,52),
                           vrho=c(0.9,0.2,0.2,0.2,0.2,0.2),
                           lambda=c(10*pi/288,10*pi/288,10*pi/288,12*pi/288,12*pi/288,12*pi/288))
mbsts_no_reg_model = mbsts_function(Ytrain,Xtrain.star,mbsts_no_reg,
                                    ki=seq(9,54,9),
                                    pii=matrix(rep(0.5,54),nrow=54),
                                    v0=5,mc=400,burn=100)

mbsts_forecast <- mbsts.forecast(mbsts_no_reg_model,
                                 mbsts_no_reg,
                                 newdata=as.matrix(newxreg[,c(1,3:5)]),
                                 steps=40)
mbsts_forecast$pred.mean[,1]


Metrics.fun(mbsts_forecast$pred.mean[,1],
            ApothicUnit.test$apothicPremiumUnit)
#   AE.mean    AE.se     RMSE
#  570.3134 386.4671 686.2072


#### bsts package
##### No regression
ss1 <- AddLocalLinearTrend(list(), ApothicUnit.df[,1])
ss1 <- AddAr(ss1, ApothicUnit.df[,1],lags=1)
ss1 <- AddSeasonal(ss1, ApothicUnit.df[,1],nseasons=52)

model1 <- bsts(ApothicUnit.df[,1],
                   state.specification = ss1, niter=1000)
plot(model1)
plot(model1,"components")

model1_forecast = predict(model1,horizon=40,burn=400)
plot(model1_forecast)


Metrics.fun(model1_forecast$mean,
            ApothicUnit.test$apothicPremiumUnit)
#  AE.mean   AE.se     RMSE
#  1277.63 820.307 1512.752

ss2 <- AddSemilocalLinearTrend(list(), ApothicUnit.df[,1])
ss2 <- AddAr(ss2, ApothicUnit.df[,1],lags=1)
ss2 <- AddSeasonal(ss2, ApothicUnit.df[,1],nseasons=52)

model2 <- bsts(ApothicUnit.df[,1],
               state.specification = ss2, niter=1000)
plot(model2)
plot(model2,"components")

model2_forecast = predict(model2,horizon=40,burn=700)
plot(model2_forecast)


Metrics.fun(model2_forecast$mean,
            ApothicUnit.test$apothicPremiumUnit)
#   AE.mean    AE.se     RMSE
#  1379.905 975.7902 1683.003








##### with regression
names(ApothicUnit.df)
bsts_reg_train <- bsts(apothicPremiumUnit~.,
                       state.specification = ss2, 
                       niter=2000,
                       data=ApothicUnit.df[,c(1,2:6,13:21)],
                       expected.model.size=6)
plot(bsts_reg_train,"coef")

burn = bsts::SuggestBurn(0.2,bsts_reg_train)

bsts_reg_forecast = predict(bsts_reg_train,horizon=40,burn=burn,
                            newdata=newxreg)
plot(bsts_reg_forecast)


Metrics.fun(bsts_reg_forecast$mean,
            ApothicUnit.test$apothicPremiumUnit)
#   AE.mean    AE.se    RMSE
#  1340.163 964.3562 1644.01


model1 <- bsts(sp500, state.specification = ss1, niter = 1000)
pred1 <- predict(model1, horizon = 360)



# VARMA (MTS package)
VARMA_P2_Q1 = VARMA(ApothicUnit.train[,2:6],p=2,q=0)
str(VARMA_P2_Q1)


VARMA_P2_Q1_Pred = VARMApred(VARMA_P2_Q1,h=40)$pred
VARMA_P2_Q1_Pred = as.data.frame(VARMA_P2_Q1_Pred)
names(VARMA_P2_Q1_Pred) = names(ApothicUnit.train[,2:6])


ss1 <- AddLocalLinearTrend(list(), ApothicUnit.train[,1])
ss1 <- AddAutoAr(ss1, ApothicUnit.train[,1])
ss1 <- AddSeasonal(ss1, ApothicUnit.train[,1],nseasons=52)
bsts_new = bsts(apothicPremiumUnit~PRCP.mean+TMIN.mean+AWND.mean+
                  SNOW.mean+SNWD.mean+Month+WeekOfYear+Christmas+
                  ThanksgivingDay+NewYearsDay+IndependenceDay+
                  LaborDay+MemorialDay+PresidentsDay+MLKingsBirthday+
                  VeteransDay,
                state.specification = ss1,
                niter=1000,seed=1234,
                data=ApothicUnit.train)
plot(bsts_new)
plot(bsts_new,"coefficient")
bsts_pred = predict(bsts_new,horizon=40,burn=100,
                    newdata=ApothicUnit.test)

Metrics.fun(bsts_pred$mean,
            ApothicUnit.test$apothicPremiumUnit)

