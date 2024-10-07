## 1 Load required Libraries

    library(openxlsx)  
    library(timetk)  
    library(dplyr)  

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(lubridate)  

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    library(recipes)  

    ## 
    ## Attaching package: 'recipes'

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

    library(rsample)  
    library(parsnip)  
    library(workflows)  
    library(forecastML)  
    library(corrplot)  

    ## corrplot 0.92 loaded

    library(doParallel)  

    ## Loading required package: foreach

    ## Loading required package: iterators

    ## Loading required package: parallel

    library(timeDate)  
    library(astsa)  

    ## 
    ## Attaching package: 'astsa'

    ## The following object is masked from 'package:parsnip':
    ## 
    ##     bart

    library(ggplot2)  
    library(forecast)  

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## 
    ## Attaching package: 'forecast'

    ## The following object is masked from 'package:astsa':
    ## 
    ##     gas

    library(bsts)  

    ## Loading required package: BoomSpikeSlab

    ## Loading required package: Boom

    ## 
    ## Attaching package: 'Boom'

    ## The following object is masked from 'package:stats':
    ## 
    ##     rWishart

    ## 
    ## Attaching package: 'BoomSpikeSlab'

    ## The following object is masked from 'package:stats':
    ## 
    ##     knots

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: xts

    ## 
    ## ######################### Warning from 'xts' package ##########################
    ## #                                                                             #
    ## # The dplyr lag() function breaks how base R's lag() function is supposed to  #
    ## # work, which breaks lag(my_xts). Calls to lag(my_xts) that you type or       #
    ## # source() into this session won't work correctly.                            #
    ## #                                                                             #
    ## # Use stats::lag() to make sure you're not using dplyr::lag(), or you can add #
    ## # conflictRules('dplyr', exclude = 'lag') to your .Rprofile to stop           #
    ## # dplyr from breaking base R's lag() function.                                #
    ## #                                                                             #
    ## # Code in packages is not affected. It's protected by R's namespace mechanism #
    ## # Set `options(xts.warn_dplyr_breaks_lag = FALSE)` to suppress this warning.  #
    ## #                                                                             #
    ## ###############################################################################

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

    ## 
    ## Attaching package: 'bsts'

    ## The following object is masked from 'package:BoomSpikeSlab':
    ## 
    ##     SuggestBurn

    library(nlme)  

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:forecast':
    ## 
    ##     getResponse

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    library(splines) 
    library(caret)

    ## Loading required package: lattice

## 2 Set Parallel Processing

    # setting up parallel processing
    registerDoSEQ()
    cl = makePSOCKcluster(4)
    registerDoParallel(cl)

## 3 Function for Performance Metrics

    Metrics.fun = function(x,y) {
      AE= abs(x-y)  # Absolute difference
      AE.mean = mean(AE)  # Mean AE
      SE = AE^2  # Squared AE
      MSE = mean(SE)  # Mean of squared error
      RMSE = sqrt(MSE) # Root of MSE
      RMSE.se = sd(SE) # Standard error of SE
      Metrics = data.frame(MAE = AE.mean,
                           RMSE=RMSE)
      Metrics
    }

## 4 Load Dataset

    setwd("C:\\Users\\YCHEN\\Git\\Weather on wine sales")

    load("Weekly wine sales data.RData")
    str(ApothicUnit)

    ## 'data.frame':    312 obs. of  31 variables:
    ##  $ ID                : num  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ AWND.sum          : num  58.8 73.4 68 58.6 82.5 ...
    ##  $ AWND.mean         : num  8.4 10.48 9.72 8.37 11.79 ...
    ##  $ AWND.max          : num  12.3 15 16.8 14.8 15.2 ...
    ##  $ AWND.min          : num  2.68 4.92 4.03 3.8 8.05 5.59 6.04 7.16 8.5 6.49 ...
    ##  $ SNOW.sum          : num  5.1 5.9 1 0 1.5 0.3 3.8 0.3 0 0 ...
    ##  $ SNOW.mean         : num  0.729 0.843 0.143 0 0.214 ...
    ##  $ SNOW.max          : num  4.7 5.4 0.6 0 1.5 0.3 3.5 0.3 0 0 ...
    ##  $ SNOW.min          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SNWD.sum          : num  12 13 1 0 2 0 6 0 0 0 ...
    ##  $ SNWD.mean         : num  1.714 1.857 0.143 0 0.286 ...
    ##  $ SNWD.max          : num  4 5 1 0 1 0 3 0 0 0 ...
    ##  $ SNWD.min          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ PRCP.sum          : num  0.36 1.07 0.38 0 0.2 0.21 0.94 0.48 0.28 0.19 ...
    ##  $ PRCP.mean         : num  0.0514 0.1529 0.0543 0 0.0286 ...
    ##  $ PRCP.max          : num  0.35 0.54 0.29 0 0.19 0.16 0.54 0.19 0.28 0.19 ...
    ##  $ PRCP.min          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ TMAX.sum          : num  261 227 265 327 233 288 285 290 405 542 ...
    ##  $ TMAX.mean         : num  37.3 32.4 37.9 46.7 33.3 ...
    ##  $ TMAX.max          : num  54 45 46 57 37 49 53 59 69 82 ...
    ##  $ TMAX.min          : num  19 24 31 39 20 36 27 32 37 65 ...
    ##  $ TMIN.sum          : num  126 99 190 219 145 187 172 208 239 355 ...
    ##  $ TMIN.mean         : num  18 14.1 27.1 31.3 20.7 ...
    ##  $ TMIN.max          : num  27 30 34 43 30 34 33 35 54 58 ...
    ##  $ TMIN.min          : num  5 5 21 22 10 15 16 23 21 46 ...
    ##  $ DATE              : Date, format: "2012-01-15" "2012-01-22" ...
    ##  $ apothicPremiumUnit: num  881 848 839 1081 1388 ...
    ##  $ WeekOfYear        : num  3 4 5 6 7 8 9 10 11 12 ...
    ##  $ Year              : num  2012 2012 2012 2012 2012 ...
    ##  $ Month             : Factor w/ 12 levels "1","2","3","4",..: 1 1 1 2 2 2 2 3 3 3 ...
    ##  $ WeekNo            : int  1 2 3 4 5 6 7 8 9 10 ...

## 5 Exploratory Data Analysis (EDA)

    ApothicUnit %>%
      plot_time_series(DATE,apothicPremiumUnit,
                       .interactive=F,
                       .facet_scales="free",
                       .color_var=year(DATE),
                       .facet_ncol=2,
                       .x_lab="Date",
                       .y_lab="Apothic (Unit)")+
      labs(title = "Fig. 1 Weekly Wine Sales")

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-5-1.png)
Note the uptrending of sales over the years.

    ApothicUnit %>%
      dplyr::group_by(Year) %>%
      plot_time_series(DATE,apothicPremiumUnit,
                       .interactive=F,
                       .facet_scales="free",
                       .facet_ncol=2,
                       .x_lab="Date",
                       .y_lab="Apothic (Unit)")+
      labs(title = "Fig. 2 Weekly Wine Sales by Year")

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-6-1.png)
Monthly variation of sales is obvious.

    corrplot(cor(ApothicUnit[,c(2:8,10:16,18:25)]),
             method="square",order="hclust",
             title = "Fig. 3 Correlation Plot/Matrix of All Variables",
             mar = c(1,3,1,1))

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-7-1.png)
Note the extremely high correlation among many related variables.

Select a subset of variables for prediction.

    ApothicUnit_df = ApothicUnit %>%
      dplyr::select(apothicPremiumUnit,PRCP.mean, TMIN.mean,AWND.mean,
                    SNOW.mean,SNWD.mean,Year, Month, WeekOfYear,WeekNo) 
    names(ApothicUnit_df)

    ##  [1] "apothicPremiumUnit" "PRCP.mean"          "TMIN.mean"         
    ##  [4] "AWND.mean"          "SNOW.mean"          "SNWD.mean"         
    ##  [7] "Year"               "Month"              "WeekOfYear"        
    ## [10] "WeekNo"

    corrplot(cor(ApothicUnit_df[,2:6]),method="number",order="hclust",
             title = "Fig. 4 Correlation Plot/Matrix of Selected Variables",
             mar = c(1,3,1,1))

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-8-1.png)

Check NAs and zero variance or near-zero variance

    apply(is.na(ApothicUnit_df),2,sum)

    ## apothicPremiumUnit          PRCP.mean          TMIN.mean          AWND.mean 
    ##                  0                  0                  0                  0 
    ##          SNOW.mean          SNWD.mean               Year              Month 
    ##                  0                  0                  0                  0 
    ##         WeekOfYear             WeekNo 
    ##                  0                  0

    nearZeroVar(ApothicUnit_df,names=T)

    ## character(0)

Plots of wine sales against weather variables

    ggplot(ApothicUnit_df,aes(x=PRCP.mean,y=apothicPremiumUnit))+
      geom_point()+
      geom_smooth(method="lm",se=F)+
      geom_smooth(method="loess",se=F,color="red")

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    ggplot(ApothicUnit_df,aes(x=TMIN.mean,y=apothicPremiumUnit))+
      geom_point()+
      geom_smooth(method="lm",se=F)+
      geom_smooth(method="loess",se=F,color="red")

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-10-2.png)

    ggplot(ApothicUnit_df,aes(x=AWND.mean,y=apothicPremiumUnit))+
      geom_point()+
      geom_smooth(method="lm",se=F)+
      geom_smooth(method="loess",se=F,color="red")

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-10-3.png)

    ggplot(ApothicUnit_df,aes(x=SNOW.mean,y=apothicPremiumUnit))+
      geom_point()+
      geom_smooth(method="lm",se=F)+
      geom_smooth(method="loess",se=F,color="red")

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at -0.0165

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 0.030786

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 4.5519e-30

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : There are other near singularities as well. 0.00020408

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-10-4.png)

    ggplot(ApothicUnit_df,aes(x=SNWD.mean,y=apothicPremiumUnit))+
      geom_point()+
      geom_smooth(method="lm",se=F)+
      geom_smooth(method="loess",se=F,color="red")

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : at -0.073571

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : radius 0.0054128

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : all data on boundary of neighborhood. make span bigger

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at -0.073571

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 0.073571

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 1

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : zero-width neighborhood. make span bigger

    ## Warning: Failed to fit group -1.
    ## Caused by error in `predLoess()`:
    ## ! NA/NaN/Inf in foreign function call (arg 5)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-10-5.png)

Check time series autocorrelation.

    lag2.plot(as.ts(ApothicUnit_df$apothicPremiumUnit),
              as.ts(ApothicUnit_df$PRCP.mean),
              max.lag=15)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    lag2.plot(as.ts(ApothicUnit_df$apothicPremiumUnit),
              as.ts(ApothicUnit_df$TMIN.mean),
              max.lag=15)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-11-2.png)

    lag2.plot(as.ts(ApothicUnit_df$apothicPremiumUnit),
              as.ts(ApothicUnit_df$AWND.mean),
              max.lag=15)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-11-3.png)

    lag2.plot(as.ts(ApothicUnit_df$apothicPremiumUnit),
              as.ts(ApothicUnit_df$SNOW.mean),
              max.lag=15)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-11-4.png)

    lag2.plot(as.ts(ApothicUnit_df$apothicPremiumUnit),
              as.ts(ApothicUnit_df$SNWD.mean),
              max.lag=15)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-11-5.png)

## 6 Create Dummy Holiday Variables

    ApothicUnit_df$week.end = seq(as.Date("2012-01-15"),as.Date("2017-12-31"),by="1 week")
    ApothicUnit_df$week.start = ApothicUnit_df$week.end - 7

Christmas

    Christmas.Dates = timeDate::ChristmasDay(min(ApothicUnit_df$Year):max(ApothicUnit_df$Year))
    for (i in 1:nrow(ApothicUnit_df)) {
      week = seq(ApothicUnit_df[i,]$week.start+1,ApothicUnit_df[i,]$week.end,length.out=7)
      ApothicUnit_df$Christmas[i] =  ifelse(length(base::intersect(week,as.Date(Christmas.Dates))) == 0,0,1)
    }

Thanksgiving

    ThanksgivingDay.Dates = timeDate::USThanksgivingDay(min(ApothicUnit_df$Year):max(ApothicUnit_df$Year))
    for (i in 1:nrow(ApothicUnit_df)) {
      week = seq(ApothicUnit_df[i,]$week.start+1,ApothicUnit_df[i,]$week.end,length.out=7)
      ApothicUnit_df$ThanksgivingDay[i] = ifelse(length(base::intersect(week,as.Date(ThanksgivingDay.Dates))) == 0,0,1)
    }

New Year’s day

    NewYearsDay.Dates = timeDate::NewYearsDay(min(ApothicUnit_df$Year):max(ApothicUnit_df$Year))
    for (i in 1:nrow(ApothicUnit_df)) {
      week = seq(ApothicUnit_df[i,]$week.start+1,ApothicUnit_df[i,]$week.end,length.out=7)
      ApothicUnit_df$NewYearsDay[i] = ifelse(length(base::intersect(week,as.Date(NewYearsDay.Dates))) == 0,0,1)
    }

Independence day

    IndependenceDay.Dates = timeDate::USIndependenceDay(min(ApothicUnit_df$Year):max(ApothicUnit_df$Year))
    for (i in 1:nrow(ApothicUnit_df)) {
      week = seq(ApothicUnit_df[i,]$week.start+1,ApothicUnit_df[i,]$week.end,length.out=7)
      ApothicUnit_df$IndependenceDay[i] = ifelse(length(base::intersect(week,as.Date(IndependenceDay.Dates))) == 0,0,1)
    }

Labor day

    LaborDay.Dates = timeDate::USLaborDay(min(ApothicUnit_df$Year):max(ApothicUnit_df$Year))
    for (i in 1:nrow(ApothicUnit_df)) {
      week = seq(ApothicUnit_df[i,]$week.start+1,ApothicUnit_df[i,]$week.end,length.out=7)
      ApothicUnit_df$LaborDay[i] = ifelse(length(base::intersect(week,as.Date(LaborDay.Dates))) == 0,0,1)
    }

Memorial day

    MemorialDay.Dates = timeDate::USMemorialDay(min(ApothicUnit_df$Year):max(ApothicUnit_df$Year))
    for (i in 1:nrow(ApothicUnit_df)) {
      week = seq(ApothicUnit_df[i,]$week.start+1,ApothicUnit_df[i,]$week.end,length.out=7)
      ApothicUnit_df$MemorialDay[i] = ifelse(length(base::intersect(week,as.Date(MemorialDay.Dates))) == 0,0,1)
    }

Presidents day

    PresidentsDay.Dates = timeDate::USPresidentsDay(min(ApothicUnit_df$Year):max(ApothicUnit_df$Year))
    for (i in 1:nrow(ApothicUnit_df)) {
      week = seq(ApothicUnit_df[i,]$week.start+1,ApothicUnit_df[i,]$week.end,length.out=7)
      ApothicUnit_df$PresidentsDay[i] = ifelse(length(base::intersect(week,as.Date(PresidentsDay.Dates))) == 0,0,1)
    }

MLKing’S day

    MLKingsBirthday.Dates = timeDate::USMLKingsBirthday(min(ApothicUnit_df$Year):max(ApothicUnit_df$Year))
    for (i in 1:nrow(ApothicUnit_df)) {
      week = seq(ApothicUnit_df[i,]$week.start+1,ApothicUnit_df[i,]$week.end,length.out=7)
      ApothicUnit_df$MLKingsBirthday[i] = ifelse(length(base::intersect(week,as.Date(MLKingsBirthday.Dates))) == 0,0,1)
    }

Veterans day

    VeteransDay.Dates = timeDate::USVeteransDay(min(ApothicUnit_df$Year):max(ApothicUnit_df$Year))
    for (i in 1:nrow(ApothicUnit_df)) {
      week = seq(ApothicUnit_df[i,]$week.start+1,ApothicUnit_df[i,]$week.end,length.out=7)
      ApothicUnit_df$VeteransDay[i] = ifelse(length(base::intersect(week,as.Date(VeteransDay.Dates))) == 0,0,1)
    }

## 7 Train and Test Data Split

271 weeks for training and 40 weeks for testing.

    ApothicUnit_train = ApothicUnit_df[1:(nrow(ApothicUnit_df)-40),]
    ApothicUnit_test = ApothicUnit_df[(nrow(ApothicUnit_df)-40+1):nrow(ApothicUnit_df),]
    names(ApothicUnit_train)

    ##  [1] "apothicPremiumUnit" "PRCP.mean"          "TMIN.mean"         
    ##  [4] "AWND.mean"          "SNOW.mean"          "SNWD.mean"         
    ##  [7] "Year"               "Month"              "WeekOfYear"        
    ## [10] "WeekNo"             "week.end"           "week.start"        
    ## [13] "Christmas"          "ThanksgivingDay"    "NewYearsDay"       
    ## [16] "IndependenceDay"    "LaborDay"           "MemorialDay"       
    ## [19] "PresidentsDay"      "MLKingsBirthday"    "VeteransDay"

    TS_Forecast_train_df = ApothicUnit_train %>%
      dplyr::select(1:6,11) %>%
      dplyr::rename("Apothic" = "apothicPremiumUnit")
    str(TS_Forecast_train_df)

    ## 'data.frame':    272 obs. of  7 variables:
    ##  $ Apothic  : num  881 848 839 1081 1388 ...
    ##  $ PRCP.mean: num  0.0514 0.1529 0.0543 0 0.0286 ...
    ##  $ TMIN.mean: num  18 14.1 27.1 31.3 20.7 ...
    ##  $ AWND.mean: num  8.4 10.48 9.72 8.37 11.79 ...
    ##  $ SNOW.mean: num  0.729 0.843 0.143 0 0.214 ...
    ##  $ SNWD.mean: num  1.714 1.857 0.143 0 0.286 ...
    ##  $ week.end : Date, format: "2012-01-15" "2012-01-22" ...

    TS_Forecast_test_df = ApothicUnit_test %>%
      dplyr::select(1,11) %>%
      dplyr::rename("Apothic" = "apothicPremiumUnit")
    str(TS_Forecast_test_df)

    ## 'data.frame':    40 obs. of  2 variables:
    ##  $ Apothic : num  4700 5649 6181 4071 4252 ...
    ##  $ week.end: Date, format: "2017-04-02" "2017-04-09" ...

## 8 Model Building and Testing

### 8.1 SARIMA models

logarithm transformation does not help too much.

    plot.ts(ts(TS_Forecast_train_df[,1]))

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-23-1.png)

    acf2(ts(TS_Forecast_train_df[,1]))

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-23-2.png)

    ##      [,1] [,2] [,3] [,4] [,5]  [,6] [,7] [,8]  [,9] [,10] [,11] [,12] [,13]
    ## ACF  0.88 0.81 0.80 0.78 0.75  0.72 0.72 0.71  0.67  0.66  0.67  0.64  0.60
    ## PACF 0.88 0.17 0.25 0.11 0.05 -0.02 0.12 0.03 -0.08  0.07  0.12 -0.09 -0.07
    ##      [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25]
    ## ACF   0.56  0.54  0.51  0.50  0.49  0.48  0.47  0.46  0.45  0.43  0.42  0.41
    ## PACF -0.10 -0.06 -0.04  0.08  0.01  0.04  0.05  0.02  0.03 -0.07  0.07 -0.01
    ##      [,26] [,27]
    ## ACF   0.39  0.38
    ## PACF -0.02  0.05

    plot.ts(log10(ts(TS_Forecast_train_df[,1])))  # seems better

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-23-3.png)

    acf2(log10(ts(TS_Forecast_train_df[,1])))  # seems better

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-23-4.png)

    ##      [,1] [,2] [,3] [,4] [,5]  [,6] [,7] [,8]  [,9] [,10] [,11] [,12] [,13]
    ## ACF  0.92 0.87 0.85 0.83 0.81  0.78 0.76 0.76  0.74  0.73  0.72  0.70  0.68
    ## PACF 0.92 0.18 0.20 0.07 0.07 -0.05 0.07 0.12 -0.06  0.06  0.02 -0.06 -0.03
    ##      [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25]
    ## ACF   0.66  0.64  0.63  0.61  0.60  0.58  0.57  0.56  0.54  0.52  0.51  0.51
    ## PACF  0.01 -0.05  0.02  0.05 -0.01 -0.05  0.04 -0.04  0.00 -0.05  0.09 -0.01
    ##      [,26] [,27]
    ## ACF   0.49  0.48
    ## PACF -0.03  0.05

#### 8.1.1 SARIMA models without exogenous variables

Autocorrelation strong after 15 lags.

    lag1.plot(ts(TS_Forecast_train_df[,1]),max.lag=15)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-24-1.png)
Perioditiy of 5 weeks?

    spectrum(ts(TS_Forecast_train_df[,1]),log="no")

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-25-1.png)

    spectrum(ts(TS_Forecast_train_df[,1]),log="no",plot=F)

    ## $freq
    ##   [1] 0.003472222 0.006944444 0.010416667 0.013888889 0.017361111 0.020833333
    ##   [7] 0.024305556 0.027777778 0.031250000 0.034722222 0.038194444 0.041666667
    ##  [13] 0.045138889 0.048611111 0.052083333 0.055555556 0.059027778 0.062500000
    ##  [19] 0.065972222 0.069444444 0.072916667 0.076388889 0.079861111 0.083333333
    ##  [25] 0.086805556 0.090277778 0.093750000 0.097222222 0.100694444 0.104166667
    ##  [31] 0.107638889 0.111111111 0.114583333 0.118055556 0.121527778 0.125000000
    ##  [37] 0.128472222 0.131944444 0.135416667 0.138888889 0.142361111 0.145833333
    ##  [43] 0.149305556 0.152777778 0.156250000 0.159722222 0.163194444 0.166666667
    ##  [49] 0.170138889 0.173611111 0.177083333 0.180555556 0.184027778 0.187500000
    ##  [55] 0.190972222 0.194444444 0.197916667 0.201388889 0.204861111 0.208333333
    ##  [61] 0.211805556 0.215277778 0.218750000 0.222222222 0.225694444 0.229166667
    ##  [67] 0.232638889 0.236111111 0.239583333 0.243055556 0.246527778 0.250000000
    ##  [73] 0.253472222 0.256944444 0.260416667 0.263888889 0.267361111 0.270833333
    ##  [79] 0.274305556 0.277777778 0.281250000 0.284722222 0.288194444 0.291666667
    ##  [85] 0.295138889 0.298611111 0.302083333 0.305555556 0.309027778 0.312500000
    ##  [91] 0.315972222 0.319444444 0.322916667 0.326388889 0.329861111 0.333333333
    ##  [97] 0.336805556 0.340277778 0.343750000 0.347222222 0.350694444 0.354166667
    ## [103] 0.357638889 0.361111111 0.364583333 0.368055556 0.371527778 0.375000000
    ## [109] 0.378472222 0.381944444 0.385416667 0.388888889 0.392361111 0.395833333
    ## [115] 0.399305556 0.402777778 0.406250000 0.409722222 0.413194444 0.416666667
    ## [121] 0.420138889 0.423611111 0.427083333 0.430555556 0.434027778 0.437500000
    ## [127] 0.440972222 0.444444444 0.447916667 0.451388889 0.454861111 0.458333333
    ## [133] 0.461805556 0.465277778 0.468750000 0.472222222 0.475694444 0.479166667
    ## [139] 0.482638889 0.486111111 0.489583333 0.493055556 0.496527778 0.500000000
    ## 
    ## $spec
    ##   [1]   627221.945  3414847.589  1892501.359   487760.096 20117580.805
    ##   [6] 14034193.857  4857257.803   780758.433   848440.798  2154759.425
    ##  [11]  7150239.510  2472237.676   700838.557  1500198.930    59972.067
    ##  [16]   465543.735  1919010.333   556616.783   187857.234   865405.670
    ##  [21]  1645928.665   651398.029   510475.333   169386.049   951285.006
    ##  [26]  1392727.070  2319393.502  2569541.175   565724.226    46216.701
    ##  [31]    14222.242    10763.850  1045913.200   940606.220    57127.598
    ##  [36]     2350.438   216070.745  1007118.973   389666.162    66183.736
    ##  [41]   113747.483   160888.882   679503.345  1955304.231   549938.239
    ##  [46]   700974.456    53672.342   153870.887   872257.437  1145990.353
    ##  [51]   393432.433   392237.513    14272.982    26856.947  1204965.497
    ##  [56]   259387.051    99981.877    45572.074   360631.425    18857.203
    ##  [61]   946367.794    90868.136   320472.222    24016.617    49835.860
    ##  [66]  1648354.316    31695.557   297609.173    32995.186   703140.196
    ##  [71]   560679.020  1965600.327    30342.542   324427.803     9891.672
    ##  [76]   105252.284  1667072.478  1904373.691    70123.020   107555.666
    ##  [81]   145824.794   616798.782  1094567.372   274179.028    25707.468
    ##  [86]    22848.622    98874.805   205923.668    52223.540   137077.623
    ##  [91]    18357.529   213001.948    14954.725   171587.667    62184.216
    ##  [96]   488336.376    90042.980   129745.652   300154.313   437289.532
    ## [101]   349145.553     2822.503   120139.139    35957.492   695573.557
    ## [106]  1002132.952   267575.849   226223.008   167714.614   479204.493
    ## [111]   105559.142   111252.649   152310.188    66944.876   295692.599
    ## [116]   692381.551   217583.146    20427.456    59776.378   228174.535
    ## [121]   153842.612   214521.373    29858.548    92691.645   117729.035
    ## [126]     7856.084   138900.334   381950.842   424685.558   151861.230
    ## [131]   147154.679   106601.878   472096.057   361842.655    11386.466
    ## [136]     1447.327     3098.971    12698.246   357569.387    42493.163
    ## [141]    75797.788   195257.978    41942.416   138551.507
    ## 
    ## $coh
    ## NULL
    ## 
    ## $phase
    ## NULL
    ## 
    ## $kernel
    ## NULL
    ## 
    ## $df
    ## [1] 1.692058
    ## 
    ## $bandwidth
    ## [1] 0.001002344
    ## 
    ## $n.used
    ## [1] 288
    ## 
    ## $orig.n
    ## [1] 272
    ## 
    ## $series
    ## [1] "x"
    ## 
    ## $snames
    ## NULL
    ## 
    ## $method
    ## [1] "Raw Periodogram"
    ## 
    ## $taper
    ## [1] 0.1
    ## 
    ## $pad
    ## [1] 0
    ## 
    ## $detrend
    ## [1] TRUE
    ## 
    ## $demean
    ## [1] FALSE
    ## 
    ## attr(,"class")
    ## [1] "spec"

Autodetection of arima model (p,d,q = 0,1,2)

    auto.arima(TS_Forecast_train_df[,1],trace=T,ic="aicc",seasonal.test="hegy") 

    ## 
    ##  Fitting models using approximations to speed things up...
    ## 
    ##  ARIMA(2,1,2) with drift         : 4318.205
    ##  ARIMA(0,1,0) with drift         : 4360.928
    ##  ARIMA(1,1,0) with drift         : 4348.098
    ##  ARIMA(0,1,1) with drift         : 4326.272
    ##  ARIMA(0,1,0)                    : 4358.98
    ##  ARIMA(1,1,2) with drift         : 4315.459
    ##  ARIMA(0,1,2) with drift         : 4312.516
    ##  ARIMA(0,1,3) with drift         : 4314.443
    ##  ARIMA(1,1,1) with drift         : 4316.79
    ##  ARIMA(1,1,3) with drift         : 4316.876
    ##  ARIMA(0,1,2)                    : 4311.341
    ##  ARIMA(0,1,1)                    : 4324.699
    ##  ARIMA(1,1,2)                    : 4314.2
    ##  ARIMA(0,1,3)                    : 4313.191
    ##  ARIMA(1,1,1)                    : 4315.788
    ##  ARIMA(1,1,3)                    : Inf
    ## 
    ##  Now re-fitting the best model(s) without approximations...
    ## 
    ##  ARIMA(0,1,2)                    : 4324.993
    ## 
    ##  Best model: ARIMA(0,1,2)

    ## Series: TS_Forecast_train_df[, 1] 
    ## ARIMA(0,1,2) 
    ## 
    ## Coefficients:
    ##           ma1      ma2
    ##       -0.3718  -0.2501
    ## s.e.   0.0602   0.0617
    ## 
    ## sigma^2 = 491355:  log likelihood = -2159.45
    ## AIC=4324.9   AICc=4324.99   BIC=4335.71

    acf2(TS_Forecast_train_df[,1])  # ar3 is adequate?

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-26-1.png)

    ##      [,1] [,2] [,3] [,4] [,5]  [,6] [,7] [,8]  [,9] [,10] [,11] [,12] [,13]
    ## ACF  0.88 0.81 0.80 0.78 0.75  0.72 0.72 0.71  0.67  0.66  0.67  0.64  0.60
    ## PACF 0.88 0.17 0.25 0.11 0.05 -0.02 0.12 0.03 -0.08  0.07  0.12 -0.09 -0.07
    ##      [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25]
    ## ACF   0.56  0.54  0.51  0.50  0.49  0.48  0.47  0.46  0.45  0.43  0.42  0.41
    ## PACF -0.10 -0.06 -0.04  0.08  0.01  0.04  0.05  0.02  0.03 -0.07  0.07 -0.01
    ##      [,26] [,27]
    ## ACF   0.39  0.38
    ## PACF -0.02  0.05

Fitting model with p,d,q = 3,1,0

    sarima(ts(TS_Forecast_train_df[,1]),3,1,0)   

    ## initial  value 6.649503 
    ## iter   2 value 6.578895
    ## iter   3 value 6.561506
    ## iter   4 value 6.561015
    ## iter   5 value 6.561012
    ## iter   5 value 6.561012
    ## iter   5 value 6.561012
    ## final  value 6.561012 
    ## converged
    ## initial  value 6.556201 
    ## iter   2 value 6.556198
    ## iter   3 value 6.556197
    ## iter   3 value 6.556197
    ## iter   3 value 6.556197
    ## final  value 6.556197 
    ## converged
    ## <><><><><><><><><><><><><><>
    ##  
    ## Coefficients: 
    ##          Estimate      SE t.value p.value
    ## ar1       -0.3553  0.0602 -5.9050  0.0000
    ## ar2       -0.3468  0.0601 -5.7707  0.0000
    ## ar3       -0.1484  0.0599 -2.4787  0.0138
    ## constant  14.6493 23.1521  0.6327  0.5274
    ## 
    ## sigma^2 estimated as 494465 on 267 degrees of freedom 
    ##  
    ## AIC = 15.98717  AICc = 15.98773  BIC = 16.05363 
    ## 

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-27-1.png)

    sarima_model1_pred = sarima.for(ts(TS_Forecast_train_df[,1]),40,3,1,0)$pred 

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-27-2.png)

    Metrics.fun(sarima_model1_pred,TS_Forecast_test_df[,1])

    ##        MAE     RMSE
    ## 1 849.6314 1051.211

Fitting model with p,d,q = 0,1,2

    sarima(ts(TS_Forecast_train_df[,1]),0,1,2) # best model selected by auto.arima

    ## initial  value 6.644109 
    ## iter   2 value 6.559995
    ## iter   3 value 6.550834
    ## iter   4 value 6.548249
    ## iter   5 value 6.547283
    ## iter   6 value 6.547278
    ## iter   7 value 6.547215
    ## iter   8 value 6.547213
    ## iter   8 value 6.547213
    ## final  value 6.547213 
    ## converged
    ## initial  value 6.547952 
    ## iter   2 value 6.547948
    ## iter   3 value 6.547947
    ## iter   3 value 6.547947
    ## iter   3 value 6.547947
    ## final  value 6.547947 
    ## converged
    ## <><><><><><><><><><><><><><>
    ##  
    ## Coefficients: 
    ##          Estimate      SE t.value p.value
    ## ma1       -0.3760  0.0601 -6.2545  0.0000
    ## ma2       -0.2545  0.0622 -4.0948  0.0001
    ## constant  14.4985 15.7912  0.9181  0.3594
    ## 
    ## sigma^2 estimated as 486171.4 on 268 degrees of freedom 
    ##  
    ## AIC = 15.96329  AICc = 15.96362  BIC = 16.01646 
    ## 

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-28-1.png)

    sarima_model2_pred = sarima.for(ts(TS_Forecast_train_df[,1]),40,0,1,2)$pred # 

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-28-2.png)

    Metrics.fun(sarima_model2_pred,TS_Forecast_test_df[,1])

    ##        MAE     RMSE
    ## 1 911.9217 1096.544

Fitting model with p,d,q, P,D,Q,S = 3,1,0,1,0,0,52 (the best so far)

    sarima_model3 = sarima(ts(TS_Forecast_train_df[,1]),3,1,0,1,0,0,S=52)

    ## initial  value 6.723548 
    ## iter   2 value 6.499097
    ## iter   3 value 6.474125
    ## iter   4 value 6.460384
    ## iter   5 value 6.460204
    ## iter   6 value 6.460183
    ## iter   7 value 6.460183
    ## iter   8 value 6.460182
    ## iter   9 value 6.460182
    ## iter   9 value 6.460182
    ## iter   9 value 6.460182
    ## final  value 6.460182 
    ## converged
    ## initial  value 6.428568 
    ## iter   2 value 6.421875
    ## iter   3 value 6.419780
    ## iter   4 value 6.419631
    ## iter   5 value 6.419629
    ## iter   6 value 6.419629
    ## iter   6 value 6.419629
    ## iter   6 value 6.419629
    ## final  value 6.419629 
    ## converged
    ## <><><><><><><><><><><><><><>
    ##  
    ## Coefficients: 
    ##          Estimate      SE t.value p.value
    ## ar1       -0.4199  0.0612 -6.8648  0.0000
    ## ar2       -0.2966  0.0635 -4.6697  0.0000
    ## ar3       -0.1098  0.0608 -1.8065  0.0720
    ## sar1       0.5484  0.0537 10.2064  0.0000
    ## constant  10.0721 36.1323  0.2788  0.7806
    ## 
    ## sigma^2 estimated as 351378.3 on 266 degrees of freedom 
    ##  
    ## AIC = 15.72141  AICc = 15.72225  BIC = 15.80117 
    ## 

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-29-1.png)

    sarima_model3_pred = sarima.for(ts(TS_Forecast_train_df[,1]),40,3,1,0,1,0,0,S=52)$pred

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-29-2.png)

    Metrics.fun(sarima_model3_pred,TS_Forecast_test_df[,1])

    ##        MAE   RMSE
    ## 1 517.5765 663.51

#### 8.1.2 SARIMA models with exogenous variables

One pro of using exogenous variables is that exogenous variables may
significantly increase forecasting accuracy. One con of using exogenous
variables is that all the exogenous variables for the forecasting
periods need to be forecasted first.

##### 8.1.2.1 Forecasting five weather variables for 40 weeks

    names(TS_Forecast_train_df)

    ## [1] "Apothic"   "PRCP.mean" "TMIN.mean" "AWND.mean" "SNOW.mean" "SNWD.mean"
    ## [7] "week.end"

    names(TS_Forecast_test_df)

    ## [1] "Apothic"  "week.end"

PRCP.mean

    plot.ts(ts(TS_Forecast_train_df[,2]))

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-31-1.png)

    auto.arima(ts(TS_Forecast_train_df[,2]))

    ## Series: ts(TS_Forecast_train_df[, 2]) 
    ## ARIMA(2,0,2) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ar2      ma1     ma2    mean
    ##       0.3694  -0.8694  -0.2703  0.8494  0.1001
    ## s.e.  0.1214   0.0754   0.1280  0.0814  0.0069
    ## 
    ## sigma^2 = 0.01204:  log likelihood = 217.54
    ## AIC=-423.07   AICc=-422.76   BIC=-401.44

    sarima(ts(TS_Forecast_train_df[,2]),2,0,2,1,0,0,S=52)

    ## initial  value -2.140455 
    ## iter   2 value -2.145930
    ## iter   3 value -2.150588
    ## iter   4 value -2.150824
    ## iter   5 value -2.151091
    ## iter   6 value -2.151194
    ## iter   7 value -2.151466
    ## iter   8 value -2.152542
    ## iter   9 value -2.154595
    ## iter  10 value -2.158954
    ## iter  11 value -2.161518
    ## iter  12 value -2.164669
    ## iter  13 value -2.171143
    ## iter  14 value -2.172118
    ## iter  15 value -2.172857
    ## iter  16 value -2.173622
    ## iter  17 value -2.174314
    ## iter  18 value -2.174751
    ## iter  19 value -2.175283
    ## iter  20 value -2.175564
    ## iter  21 value -2.178969
    ## iter  22 value -2.181846
    ## iter  23 value -2.185735
    ## iter  24 value -2.186214
    ## iter  25 value -2.187427
    ## iter  26 value -2.188552
    ## iter  27 value -2.192668
    ## iter  28 value -2.201640
    ## iter  29 value -2.203662
    ## iter  30 value -2.207079
    ## iter  31 value -2.210133
    ## iter  32 value -2.212442
    ## iter  33 value -2.213109
    ## iter  34 value -2.213294
    ## iter  35 value -2.213817
    ## iter  36 value -2.213820
    ## iter  37 value -2.213915
    ## iter  38 value -2.214321
    ## iter  38 value -2.214321
    ## iter  39 value -2.215019
    ## iter  40 value -2.215019
    ## iter  40 value -2.215019
    ## iter  41 value -2.215592
    ## iter  41 value -2.215592
    ## iter  42 value -2.215623
    ## iter  42 value -2.215623
    ## iter  43 value -2.215736
    ## iter  43 value -2.215736
    ## iter  44 value -2.215743
    ## iter  44 value -2.215743
    ## iter  45 value -2.215744
    ## iter  45 value -2.215744
    ## iter  45 value -2.215744
    ## final  value -2.215744 
    ## converged
    ## initial  value -2.194557 
    ## iter   2 value -2.208770
    ## iter   3 value -2.209064
    ## iter   4 value -2.211248
    ## iter   5 value -2.212000
    ## iter   6 value -2.212075
    ## iter   7 value -2.212114
    ## iter   8 value -2.212451
    ## iter   9 value -2.212975
    ## iter  10 value -2.213805
    ## iter  11 value -2.213934
    ## iter  12 value -2.214139
    ## iter  13 value -2.214692
    ## iter  14 value -2.216224
    ## iter  15 value -2.217029
    ## iter  16 value -2.217284
    ## iter  17 value -2.217877
    ## iter  18 value -2.218999
    ## iter  19 value -2.219461
    ## iter  20 value -2.219771
    ## iter  21 value -2.219963
    ## iter  22 value -2.220198
    ## iter  23 value -2.220580
    ## iter  24 value -2.220605
    ## iter  25 value -2.220718
    ## iter  26 value -2.220809
    ## iter  27 value -2.220936
    ## iter  28 value -2.220964
    ## iter  29 value -2.221001
    ## iter  30 value -2.221016
    ## iter  31 value -2.221040
    ## iter  32 value -2.221067
    ## iter  33 value -2.221088
    ## iter  34 value -2.221095
    ## iter  35 value -2.221096
    ## iter  35 value -2.221096
    ## iter  35 value -2.221096
    ## final  value -2.221096 
    ## converged
    ## <><><><><><><><><><><><><><>
    ##  
    ## Coefficients: 
    ##       Estimate     SE  t.value p.value
    ## ar1     0.3768 0.1241   3.0362  0.0026
    ## ar2    -0.8711 0.0787 -11.0626  0.0000
    ## ma1    -0.2798 0.1353  -2.0683  0.0396
    ## ma2     0.8489 0.0793  10.7013  0.0000
    ## sar1   -0.0729 0.0638  -1.1430  0.2541
    ## xmean   0.1005 0.0065  15.3993  0.0000
    ## 
    ## sigma^2 estimated as 0.01174804 on 266 degrees of freedom 
    ##  
    ## AIC = -1.552844  AICc = -1.551679  BIC = -1.460048 
    ## 

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-31-2.png)

    PRCP.sarima.S = sarima.for(ts(TS_Forecast_train_df[,2]),40,2,0,2,1,0,0,S=52)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-31-3.png)

TMIN.mean

    plot.ts(ts(TS_Forecast_train_df[,3]))

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-32-1.png)

    auto.arima(ts(TS_Forecast_train_df[,3]))

    ## Series: ts(TS_Forecast_train_df[, 3]) 
    ## ARIMA(1,0,1) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ma1     mean
    ##       0.9527  -0.2273  39.3986
    ## s.e.  0.0192   0.0621   6.6156
    ## 
    ## sigma^2 = 51.1:  log likelihood = -920.42
    ## AIC=1848.83   AICc=1848.98   BIC=1863.25

    sarima(ts(TS_Forecast_train_df[,3]),1,0,1,1,0,0,S=52)

    ## initial  value 2.935968 
    ## iter   2 value 2.473706
    ## iter   3 value 2.200587
    ## iter   4 value 2.073992
    ## iter   5 value 2.053077
    ## iter   6 value 1.972465
    ## iter   7 value 1.964650
    ## iter   8 value 1.962497
    ## iter   9 value 1.962167
    ## iter  10 value 1.962131
    ## iter  11 value 1.962129
    ## iter  12 value 1.962127
    ## iter  13 value 1.962125
    ## iter  14 value 1.962125
    ## iter  15 value 1.962122
    ## iter  16 value 1.962119
    ## iter  17 value 1.962115
    ## iter  18 value 1.962114
    ## iter  19 value 1.962114
    ## iter  19 value 1.962114
    ## final  value 1.962114 
    ## converged
    ## initial  value 1.950904 
    ## iter   2 value 1.950447
    ## iter   3 value 1.950315
    ## iter   4 value 1.950299
    ## iter   5 value 1.950297
    ## iter   6 value 1.950274
    ## iter   7 value 1.950262
    ## iter   8 value 1.950255
    ## iter   9 value 1.950250
    ## iter  10 value 1.950242
    ## iter  11 value 1.950238
    ## iter  12 value 1.950234
    ## iter  13 value 1.950232
    ## iter  14 value 1.950228
    ## iter  15 value 1.950227
    ## iter  16 value 1.950227
    ## iter  17 value 1.950227
    ## iter  18 value 1.950226
    ## iter  19 value 1.950225
    ## iter  20 value 1.950225
    ## iter  21 value 1.950224
    ## iter  22 value 1.950224
    ## iter  23 value 1.950224
    ## iter  24 value 1.950224
    ## iter  25 value 1.950223
    ## iter  26 value 1.950223
    ## iter  26 value 1.950223
    ## iter  26 value 1.950223
    ## final  value 1.950223 
    ## converged
    ## <><><><><><><><><><><><><><>
    ##  
    ## Coefficients: 
    ##       Estimate     SE t.value p.value
    ## ar1     0.9452 0.0214 44.2279  0.0000
    ## ma1    -0.2876 0.0675 -4.2592  0.0000
    ## sar1    0.2031 0.0716  2.8357  0.0049
    ## xmean  40.0023 6.2861  6.3637  0.0000
    ## 
    ## sigma^2 estimated as 48.72046 on 268 degrees of freedom 
    ##  
    ## AIC = 6.775088  AICc = 6.775639  BIC = 6.841372 
    ## 

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-32-2.png)

    TMIN.sarima.S = sarima.for(ts(TS_Forecast_train_df[,3]),40,1,0,1,1,0,0,S=52)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-32-3.png)

AWND.mean

    plot.ts(ts(TS_Forecast_train_df[,4]))

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-33-1.png)

    auto.arima(ts(TS_Forecast_train_df[,4]))

    ## Series: ts(TS_Forecast_train_df[, 4]) 
    ## ARIMA(1,0,1) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ma1    mean
    ##       0.8983  -0.6688  9.8604
    ## s.e.  0.0395   0.0607  0.3387
    ## 
    ## sigma^2 = 3.112:  log likelihood = -539.06
    ## AIC=1086.12   AICc=1086.27   BIC=1100.54

    sarima(ts(TS_Forecast_train_df[,4]),1,0,1,1,0,0,S=52)

    ## initial  value 0.699667 
    ## iter   2 value 0.635087
    ## iter   3 value 0.594779
    ## iter   4 value 0.593929
    ## iter   5 value 0.593594
    ## iter   6 value 0.592175
    ## iter   7 value 0.583811
    ## iter   8 value 0.571498
    ## iter   9 value 0.565930
    ## iter  10 value 0.562209
    ## iter  11 value 0.557037
    ## iter  12 value 0.552848
    ## iter  13 value 0.551536
    ## iter  14 value 0.551088
    ## iter  15 value 0.550906
    ## iter  16 value 0.550891
    ## iter  17 value 0.550889
    ## iter  18 value 0.550861
    ## iter  19 value 0.550831
    ## iter  20 value 0.550828
    ## iter  21 value 0.550828
    ## iter  21 value 0.550828
    ## iter  21 value 0.550828
    ## final  value 0.550828 
    ## converged
    ## initial  value 0.551339 
    ## iter   2 value 0.551122
    ## iter   3 value 0.550982
    ## iter   4 value 0.550955
    ## iter   5 value 0.550923
    ## iter   6 value 0.550882
    ## iter   7 value 0.550860
    ## iter   8 value 0.550855
    ## iter   9 value 0.550854
    ## iter  10 value 0.550854
    ## iter  11 value 0.550854
    ## iter  12 value 0.550853
    ## iter  13 value 0.550853
    ## iter  14 value 0.550853
    ## iter  15 value 0.550853
    ## iter  16 value 0.550853
    ## iter  16 value 0.550853
    ## iter  16 value 0.550853
    ## final  value 0.550853 
    ## converged
    ## <><><><><><><><><><><><><><>
    ##  
    ## Coefficients: 
    ##       Estimate     SE  t.value p.value
    ## ar1     0.8915 0.0442  20.1479  0.0000
    ## ma1    -0.6963 0.0653 -10.6702  0.0000
    ## sar1    0.1793 0.0697   2.5747  0.0106
    ## xmean   9.8364 0.3372  29.1721  0.0000
    ## 
    ## sigma^2 estimated as 2.987152 on 268 degrees of freedom 
    ##  
    ## AIC = 3.976347  AICc = 3.976898  BIC = 4.04263 
    ## 

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-33-2.png)

    AWND.sarima.S = sarima.for(ts(TS_Forecast_train_df[,4]),40,1,0,1,1,0,0,S=52)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-33-3.png)

SNOW.mean

    plot.ts(ts(TS_Forecast_train_df[,5]))

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-34-1.png)

    auto.arima(ts(TS_Forecast_train_df[,5]))

    ## Series: ts(TS_Forecast_train_df[, 5]) 
    ## ARIMA(1,0,3) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ma1      ma2     ma3    mean
    ##       0.7482  -0.6249  -0.0311  0.2352  0.1327
    ## s.e.  0.0932   0.1057   0.0734  0.0627  0.0458
    ## 
    ## sigma^2 = 0.112:  log likelihood = -85.88
    ## AIC=183.77   AICc=184.08   BIC=205.4

    sarima(ts(TS_Forecast_train_df[,5]),1,0,3,1,0,0,S=52)

    ## initial  value -0.931148 
    ## iter   2 value -0.932970
    ## iter   3 value -0.991648
    ## iter   4 value -0.996204
    ## iter   5 value -0.999057
    ## iter   6 value -1.000959
    ## iter   7 value -1.006353
    ## iter   8 value -1.010230
    ## iter   9 value -1.011975
    ## iter  10 value -1.013059
    ## iter  11 value -1.014521
    ## iter  12 value -1.014851
    ## iter  13 value -1.016046
    ## iter  14 value -1.016360
    ## iter  15 value -1.016376
    ## iter  16 value -1.016470
    ## iter  17 value -1.016567
    ## iter  18 value -1.016586
    ## iter  19 value -1.016599
    ## iter  20 value -1.016602
    ## iter  21 value -1.016605
    ## iter  22 value -1.016607
    ## iter  23 value -1.016607
    ## iter  24 value -1.016607
    ## iter  24 value -1.016607
    ## iter  24 value -1.016607
    ## final  value -1.016607 
    ## converged
    ## initial  value -1.102728 
    ## iter   2 value -1.102840
    ## iter   3 value -1.102965
    ## iter   4 value -1.103087
    ## iter   5 value -1.103188
    ## iter   6 value -1.103263
    ## iter   7 value -1.103291
    ## iter   8 value -1.103296
    ## iter   9 value -1.103296
    ## iter  10 value -1.103297
    ## iter  11 value -1.103298
    ## iter  12 value -1.103299
    ## iter  13 value -1.103299
    ## iter  13 value -1.103299
    ## iter  13 value -1.103299
    ## final  value -1.103299 
    ## converged
    ## <><><><><><><><><><><><><><>
    ##  
    ## Coefficients: 
    ##       Estimate     SE t.value p.value
    ## ar1     0.7512 0.0923  8.1434  0.0000
    ## ma1    -0.6263 0.1042 -6.0084  0.0000
    ## ma2    -0.0309 0.0732 -0.4222  0.6732
    ## ma3     0.2360 0.0625  3.7754  0.0002
    ## sar1   -0.0150 0.0618 -0.2430  0.8082
    ## xmean   0.1331 0.0458  2.9069  0.0040
    ## 
    ## sigma^2 estimated as 0.1098805 on 266 degrees of freedom 
    ##  
    ## AIC = 0.6827495  AICc = 0.6839149  BIC = 0.7755459 
    ## 

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-34-2.png)

    SNOW.sarima.S = sarima.for(ts(TS_Forecast_train_df[,5]),40,1,0,3,1,0,0,S=52)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-34-3.png)

SNWD.mean

    plot.ts(ts(TS_Forecast_train_df[,6]))

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-35-1.png)

    auto.arima(ts(TS_Forecast_train_df[,6]))

    ## Series: ts(TS_Forecast_train_df[, 6]) 
    ## ARIMA(1,0,5) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ma1      ma2     ma3     ma4      ma5    mean
    ##       0.7760  -0.2090  -0.1914  0.1831  0.1946  -0.1862  0.5648
    ## s.e.  0.1147   0.1305   0.0843  0.0611  0.0747   0.0842  0.2739
    ## 
    ## sigma^2 = 1.719:  log likelihood = -456.59
    ## AIC=929.18   AICc=929.73   BIC=958.03

    sarima(ts(TS_Forecast_train_df[,6]),1,0,5,1,0,0,S=52)

    ## initial  value 0.670526 
    ## iter   2 value 0.595919
    ## iter   3 value 0.454276
    ## iter   4 value 0.381925
    ## iter   5 value 0.346875
    ## iter   6 value 0.344976
    ## iter   7 value 0.342175
    ## iter   8 value 0.341980
    ## iter   9 value 0.341762
    ## iter  10 value 0.341740
    ## iter  11 value 0.341613
    ## iter  12 value 0.341535
    ## iter  13 value 0.341396
    ## iter  14 value 0.341322
    ## iter  15 value 0.341252
    ## iter  16 value 0.341078
    ## iter  17 value 0.339724
    ## iter  18 value 0.339132
    ## iter  19 value 0.338172
    ## iter  20 value 0.337835
    ## iter  21 value 0.337400
    ## iter  22 value 0.335050
    ## iter  23 value 0.334497
    ## iter  24 value 0.334389
    ## iter  25 value 0.334378
    ## iter  26 value 0.334374
    ## iter  27 value 0.334367
    ## iter  28 value 0.334357
    ## iter  29 value 0.334350
    ## iter  30 value 0.334348
    ## iter  31 value 0.334348
    ## iter  32 value 0.334348
    ## iter  32 value 0.334348
    ## iter  32 value 0.334348
    ## final  value 0.334348 
    ## converged
    ## initial  value 0.241287 
    ## iter   2 value 0.240154
    ## iter   3 value 0.239983
    ## iter   4 value 0.239917
    ## iter   5 value 0.239805
    ## iter   6 value 0.239646
    ## iter   7 value 0.239519
    ## iter   8 value 0.239505
    ## iter   9 value 0.239504
    ## iter  10 value 0.239504
    ## iter  11 value 0.239504
    ## iter  12 value 0.239504
    ## iter  13 value 0.239504
    ## iter  14 value 0.239504
    ## iter  15 value 0.239504
    ## iter  15 value 0.239504
    ## iter  15 value 0.239504
    ## final  value 0.239504 
    ## converged
    ## <><><><><><><><><><><><><><>
    ##  
    ## Coefficients: 
    ##       Estimate     SE t.value p.value
    ## ar1     0.7843 0.1151  6.8128  0.0000
    ## ma1    -0.2381 0.1302 -1.8289  0.0685
    ## ma2    -0.2405 0.0853 -2.8200  0.0052
    ## ma3     0.2179 0.0605  3.6000  0.0004
    ## ma4     0.1487 0.0790  1.8815  0.0610
    ## ma5    -0.1723 0.0818 -2.1068  0.0361
    ## sar1    0.2029 0.0599  3.3878  0.0008
    ## xmean   0.5341 0.3014  1.7720  0.0775
    ## 
    ## sigma^2 estimated as 1.59634 on 264 degrees of freedom 
    ##  
    ## AIC = 3.383061  AICc = 3.385074  BIC = 3.502371 
    ## 

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-35-2.png)

    SNWD.sarima.S = sarima.for(ts(TS_Forecast_train_df[,6]),40,1,0,5,1,0,0,S=52)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-35-3.png)

New data for forecasting

    names(ApothicUnit_train)

    ##  [1] "apothicPremiumUnit" "PRCP.mean"          "TMIN.mean"         
    ##  [4] "AWND.mean"          "SNOW.mean"          "SNWD.mean"         
    ##  [7] "Year"               "Month"              "WeekOfYear"        
    ## [10] "WeekNo"             "week.end"           "week.start"        
    ## [13] "Christmas"          "ThanksgivingDay"    "NewYearsDay"       
    ## [16] "IndependenceDay"    "LaborDay"           "MemorialDay"       
    ## [19] "PresidentsDay"      "MLKingsBirthday"    "VeteransDay"

    names(ApothicUnit_test)

    ##  [1] "apothicPremiumUnit" "PRCP.mean"          "TMIN.mean"         
    ##  [4] "AWND.mean"          "SNOW.mean"          "SNWD.mean"         
    ##  [7] "Year"               "Month"              "WeekOfYear"        
    ## [10] "WeekNo"             "week.end"           "week.start"        
    ## [13] "Christmas"          "ThanksgivingDay"    "NewYearsDay"       
    ## [16] "IndependenceDay"    "LaborDay"           "MemorialDay"       
    ## [19] "PresidentsDay"      "MLKingsBirthday"    "VeteransDay"

    newxreg = data.frame(PRCP.mean = PRCP.sarima.S$pred,
                         TMIN.mean=TMIN.sarima.S$pred,
                         AWND.mean=AWND.sarima.S$pred,
                         SNOW.mean=SNOW.sarima.S$pred,
                         SNWD.mean=SNWD.sarima.S$pred,
                         Christmas=ApothicUnit_test$Christmas,
                         NewYearsDay=ApothicUnit_test$NewYearsDay,
                         IndependenceDay=ApothicUnit_test$IndependenceDay,
                         LaborDay=ApothicUnit_test$LaborDay,
                         ThanksgivingDay=ApothicUnit_test$ThanksgivingDay,
                         MemorialDay=ApothicUnit_test$MemorialDay,
                         PresidentsDay=ApothicUnit_test$PresidentsDay,
                         MLKingsBirthday=ApothicUnit_test$MLKingsBirthday,
                         VeteransDay=ApothicUnit_test$VeteransDay,
                         WeekOfYear = ApothicUnit_test$WeekOfYear,
                         WeekNo=ApothicUnit_test$WeekNo,
                         Month=as.factor(ApothicUnit_test$Month),
                         Year=ApothicUnit_test$Year)
    newxreg = newxreg %>%
      dplyr::mutate(SNOW.Binary = ifelse(newxreg$SNOW.mean>=0.2,1,0),
                    SNWD.Binary = ifelse(newxreg$SNWD.mean>=2,1,0)) %>%
      dplyr::mutate(SNOW.Binary.Back1 = c(SNOW.Binary[-1],0),
                    SNWD.Binary.Back1 = c(SNWD.Binary[-1],0)) %>%
      as.data.frame()

str(newxreg)

##### 8.1.2.2 Five weather variables as exogenous

Model

    sarima_model3_exo = sarima(ts(TS_Forecast_train_df[,1]),0,1,2,1,0,0,S=52,
                           xreg=TS_Forecast_train_df[,2:6])

    ## initial  value 6.695515 
    ## iter   2 value 6.481597
    ## iter   3 value 6.452188
    ## iter   4 value 6.432403
    ## iter   5 value 6.426817
    ## iter   6 value 6.425883
    ## iter   7 value 6.425720
    ## iter   8 value 6.425696
    ## iter   9 value 6.425681
    ## iter  10 value 6.425680
    ## iter  10 value 6.425680
    ## iter  10 value 6.425680
    ## final  value 6.425680 
    ## converged
    ## initial  value 6.401464 
    ## iter   2 value 6.395081
    ## iter   3 value 6.393144
    ## iter   4 value 6.392835
    ## iter   5 value 6.392822
    ## iter   6 value 6.392817
    ## iter   7 value 6.392816
    ## iter   8 value 6.392815
    ## iter   8 value 6.392815
    ## iter   8 value 6.392815
    ## final  value 6.392815 
    ## converged
    ## <><><><><><><><><><><><><><>
    ##  
    ## Coefficients: 
    ##           Estimate       SE t.value p.value
    ## ma1        -0.4736   0.0616 -7.6842  0.0000
    ## ma2        -0.1804   0.0675 -2.6710  0.0080
    ## sar1        0.5451   0.0542 10.0587  0.0000
    ## PRCP.mean 136.6078 265.6846  0.5142  0.6076
    ## TMIN.mean -10.3337   5.3204 -1.9423  0.0532
    ## AWND.mean  23.1834  19.2006  1.2074  0.2284
    ## SNOW.mean -14.2153  88.6499 -0.1604  0.8727
    ## SNWD.mean  17.4670  25.9689  0.6726  0.5018
    ## 
    ## sigma^2 estimated as 333113.1 on 263 degrees of freedom 
    ##  
    ## AIC = 15.68993  AICc = 15.69196  BIC = 15.80956 
    ## 

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-37-1.png)

    acf2(sarima_model3_exo$fit$residuals,max.lag=100)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-37-2.png)

    ##      [,1] [,2] [,3]  [,4] [,5]  [,6]  [,7]  [,8]  [,9] [,10] [,11] [,12] [,13]
    ## ACF     0 0.03 0.03 -0.05 0.04 -0.11 -0.05 -0.05 -0.02 -0.06  0.08  0.08  0.02
    ## PACF    0 0.03 0.03 -0.05 0.04 -0.11 -0.04 -0.05 -0.01 -0.07  0.09  0.07  0.01
    ##      [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25]
    ## ACF  -0.04 -0.07 -0.04 -0.12 -0.04 -0.04  0.01 -0.01  0.09 -0.04  0.09 -0.01
    ## PACF -0.07 -0.07 -0.05 -0.11 -0.02 -0.02  0.02 -0.02  0.09 -0.09  0.05 -0.05
    ##      [,26] [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37]
    ## ACF  -0.09  0.02 -0.11  0.01 -0.02 -0.02  0.06  0.04 -0.01 -0.02 -0.08 -0.08
    ## PACF -0.07 -0.01 -0.06  0.00  0.00 -0.02  0.03  0.01 -0.06 -0.05 -0.13 -0.08
    ##      [,38] [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46] [,47] [,48] [,49]
    ## ACF  -0.05 -0.05  0.06  0.04 -0.04  0.03  0.05  0.17  0.00 -0.06 -0.02 -0.03
    ## PACF -0.06  0.00  0.07  0.07 -0.07 -0.03 -0.03  0.13 -0.05 -0.04 -0.01  0.00
    ##      [,50] [,51] [,52] [,53] [,54] [,55] [,56] [,57] [,58] [,59] [,60] [,61]
    ## ACF  -0.11  0.06  0.08  0.05  0.02 -0.01  0.07  0.00 -0.01 -0.08  0.11 -0.09
    ## PACF -0.10  0.06  0.06  0.03 -0.03 -0.01 -0.01 -0.05  0.02 -0.05  0.19 -0.06
    ##      [,62] [,63] [,64] [,65] [,66] [,67] [,68] [,69] [,70] [,71] [,72] [,73]
    ## ACF  -0.03  0.08 -0.06  0.03 -0.07  0.06 -0.05  0.02 -0.03 -0.07 -0.01 -0.08
    ## PACF  0.02  0.04 -0.09 -0.09 -0.03  0.03 -0.02  0.07  0.01 -0.05 -0.08 -0.09
    ##      [,74] [,75] [,76] [,77] [,78] [,79] [,80] [,81] [,82] [,83] [,84] [,85]
    ## ACF   0.02 -0.02 -0.06  0.00  0.07 -0.03 -0.04  0.01  0.00  0.06  0.04  0.04
    ## PACF -0.02 -0.03 -0.10  0.05  0.06 -0.01 -0.08  0.04 -0.03  0.07 -0.01  0.00
    ##      [,86] [,87] [,88] [,89] [,90] [,91] [,92] [,93] [,94] [,95] [,96] [,97]
    ## ACF   0.00  0.01 -0.04 -0.01 -0.03 -0.07  0.03 -0.02  0.06 -0.01  0.02  0.05
    ## PACF -0.03 -0.01  0.01 -0.02 -0.05 -0.07  0.02 -0.05  0.06  0.00  0.01 -0.01
    ##      [,98] [,99] [,100]
    ## ACF  -0.06  0.06   0.04
    ## PACF -0.05  0.04   0.06

    auto.arima(sarima_model3_exo$fit$residuals)

    ## Series: sarima_model3_exo$fit$residuals 
    ## ARIMA(0,0,0) with zero mean 
    ## 
    ## sigma^2 = 331888:  log likelihood = -2114.86
    ## AIC=4231.72   AICc=4231.73   BIC=4235.32

Prediction

    sarima_model3_exo_pred = sarima.for(ts(TS_Forecast_train_df[,1]),40,0,1,2,1,0,0,S=52,
                                    xreg=TS_Forecast_train_df[,2:6],
                                    newxreg=newxreg[,1:5])

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-38-1.png)

    Metrics.fun(sarima_model3_exo_pred$pred,TS_Forecast_test_df[,1])  

    ##        MAE     RMSE
    ## 1 581.9214 709.9742

Note (1) The performance of the model with five weather variables is
worse than that with any exogenous variables. (2) Adding holidays does
not improve model performance (data not shown).

### 8.2 bsts (Bayesian Structural Time Series) models

#### 8.2.1 Local level model with S=52

    ss = AddLocalLevel(list(),TS_Forecast_train_df[,1]) 
    ss = AddSeasonal(ss,TS_Forecast_train_df[,1],nseasons=52)
    bsts.Local.Level = bsts(TS_Forecast_train_df[,1],
                            state.specification=ss,niter=1000)

    ## =-=-=-=-= Iteration 0 Mon Oct  7 15:26:05 2024
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 100 Mon Oct  7 15:26:06 2024
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 200 Mon Oct  7 15:26:08 2024
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 300 Mon Oct  7 15:26:09 2024
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 400 Mon Oct  7 15:26:11 2024
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 500 Mon Oct  7 15:26:12 2024
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 600 Mon Oct  7 15:26:14 2024
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 700 Mon Oct  7 15:26:15 2024
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 800 Mon Oct  7 15:26:17 2024
    ##  =-=-=-=-=
    ## =-=-=-=-= Iteration 900 Mon Oct  7 15:26:18 2024
    ##  =-=-=-=-=

    bsts.Local.Level.Pred = predict(bsts.Local.Level,horizon=40,burn=200)
    plot(bsts.Local.Level.Pred)

![](Weather-on-wine-sales_files/figure-markdown_strict/unnamed-chunk-39-1.png)

    Metrics.fun(bsts.Local.Level.Pred$mean,TS_Forecast_test_df[,1])

    ##        MAE     RMSE
    ## 1 667.1934 772.9176
