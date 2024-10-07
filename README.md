Weather on wine sales
October 7, 2024

# Objective
The objective of the project is to forecast weekly wine sales using historical data. The description of the data and variables as follows.  
<br>
<br>

# Modeling methodologies
  Time domain approach, frequency domain approach, and machine learning models are utilized and compared. Specific methods include  
## Time domain approach  
   (1) Classical time series approaches (e.g., arima and sarima);   
   (2) Bayesian structural time series approach using R package "bsts"  
## Frequency domain approach  
   (3) Classical statistical models using generalized least square regression using R package "nlme"  
## Machine learning algorithms  
   (4) Machine learning using R package "forecastML"    

                             
# Data description
## Site/Location: Chicago O’Hare International Airport, IL
## Weather variables
(1) Daily records of snow fall (SNOW; inch)
(2) snow accumulation on the ground (SNWD; inch)
(3) precipitation (PRCP; inch)
(4) average wind speed (AWND)
(5) maximum temperature (TMAX)
(6) minimum temperature (TMIN)
(7) average temperature (TAVG), were downloaded from National Climatic Data Center (NCDC). 

Since wine sales data were collected in a weekly base, weekly weather data (i.e., sum, average, minimum, and maximum; minimum was excluded from because they were almost all zero) for SNOW, SNWD, PRCP, TMAX, TMIN were generated from daily records. Maximum, minimum, and 
average of each weather variable are highly correlated. Furthermore, SNOW and SNWD, and TMAX and TMIX are moderately correlated. All these indicate that not all variables should be simultaneously included into a model. 

Sales data: a total of 312 weeks from 2012-01-15 (week ending date) to 2017-12-31
