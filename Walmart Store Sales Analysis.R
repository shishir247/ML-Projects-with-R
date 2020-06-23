# Cleaning global environment

rm(list = ls())

###################################################################################
                # Project 1 Retail Analysis with Walmart Data #
###################################################################################

# Reading dataset and assigning it as WSS

WSS <- read.csv("Walmart_Store_sales.csv", header = T)

dim(WSS)
head(WSS)
summary(WSS)
str(WSS)

## changing date formats ##

# install.packages("lubridate", dependencies = T)
library(lubridate)

str(WSS$Date) # structure of date is factor

# Factor w/ 143 levels "01-04-2011","01-06-2012",..: 20 53 86 119 21 54 87 120 6 39 ...


## changing it to Date format

WSS$Date = as.Date(WSS$Date, "%d-%m-%Y")

str(WSS$Date) # structure of date is now changed to Date format

# Date[1:6435], format: "2010-02-05" "2010-02-12" "2010-02-19" "2010-02-26" "2010-03-05" "2010-03-12" "2010-03-19" 
# "2010-03-26" "2010-04-02" ...



####################################################################################
#                                Analysis Tasks                                    #
####################################################################################
## 1. Which store has maximum sales?
####################################################################################

# install.packages("dplyr", dependencies = T)
library(dplyr)

## store that has maximum sales 

WSS %>% group_by(Store) %>%                               # grouping by stores
  summarise(Total_Sales = sum(Weekly_Sales)) %>%          # aggregating sales wrt individual stores
  filter(Total_Sales == max(Total_Sales))                 # filtering for maximum sale


#       Store Total_Sales
# <int>       <dbl>
#   1    20  301397792.


# store 20 has maximum sales of 301397792.


####################################################################################
## 2.1 Which store has maximum standard deviation?
####################################################################################


WSS %>% group_by(Store) %>%                               # grouping by stores
  summarise(standard_deviation = sd(Weekly_Sales)) %>%    # calculating standard deviation
  filter(standard_deviation == max(standard_deviation))   # filtering for maximum sd


#       Store standard_deviation
# <int>                <dbl>
#   1    14            317570.

# store 14 has maximum standard deviation of 317570.


####################################################################################
## 2.2 coefficient mean to standard deviation
####################################################################################


WSS %>% group_by(Store) %>%                                                              # grouping by stores
  summarise(standard_deviation = sd(Weekly_Sales), mean_value = mean(Weekly_Sales)) %>%  # calculating standard deviation
  mutate(cv = standard_deviation/mean_value*100) %>%                                     # coefficient mean to standard deviation
  arrange(desc(cv))                                                                      # arranging in decreasing order of cv



#   Store standard_deviation mean_value    cv
# <int>              <dbl>      <dbl> <dbl>
# 1    35            211243.    919725.  23.0
# 2     7            112585.    570617.  19.7
# 3    15            120539.    623312.  19.3
# 4    29             99120.    539451.  18.4
# 5    23            249788.   1389864.  18.0
# 6    21            128753.    756069.  17.0
# 7    45            130169.    785981.  16.6
# 8    16             85770.    519248.  16.5
# 9    18            176642.   1084718.  16.3
# 10   36             60725.    373512.  16.3
# # … with 35 more rows

# store 35 has maximum coefficient mean to standard deviation.


####################################################################################
## 3 Which stores has good quarterly growth rate in Q3’2012?
####################################################################################


# fetching 2012 Q2 data from main data (Walmart_Store_sales)

Weekly_sales_Q2.2012 <- WSS %>% group_by(Store) %>% 
  filter(Date >= as.Date("2012-04-01") & Date <= as.Date("2012-06-30")) %>%  # filterring for Q2 (2012-04-01 to 2012-06-30)
  summarise(sum(Weekly_Sales))                                               # summarising for sum of weekly sales

# fetching 2012 Q3 data from main data (Walmart_Store_sales)

Weekly_sales_Q3.2012 <- WSS %>% group_by(Store) %>% 
  filter(Date >= as.Date("2012-07-01") & Date <= as.Date("2012-09-30")) %>%  # filterring for Q2 (2012-07-01 to 2012-09-30)
  summarise(sum(Weekly_Sales))                                               # summarising for sum of weekly sales

# Growth Rate = (Weekly_Sales.Q3_2012-Weekly_Sales.Q2_2012)/Weekly_Sales.Q2_2012

Growth_Rate_Q3.2012 <- 
  mutate(Weekly_sales_Q3.2012, Performance =                                                 # putting values in formula Growth Q3 = ((Q3 - Q2) / Q2) * 100
  ((Weekly_sales_Q3.2012$`sum(Weekly_Sales)` - Weekly_sales_Q2.2012$`sum(Weekly_Sales)`) /
  Weekly_sales_Q2.2012$`sum(Weekly_Sales)`)*100) %>% arrange(desc(Performance))              # arranging in decreasing order

head(Growth_Rate_Q3.2012)


# Store `sum(Weekly_Sales)` Performance
# <int>               <dbl>       <dbl>
# 1     7            8262787.        13.3 
# 2    16            7121542.        8.49
# 3    35           11322421.        4.47
# 4    26           13675692.        3.96
# 5    39           20715116.        2.48
# 6    41           18093844.        2.46

# store 7 has higher growth rate in Q3.2012 with 13.3 %

# comparison bargraph

H <- Growth_Rate_Q3.2012$Performance
M <- Growth_Rate_Q3.2012$Store

# Ploting the bar chart 
barplot(H,names.arg=M,xlab="Store",ylab="Sales_comparison_%",col="blue",
        main="Sales_Performance_Q3'2012",border="white")



####################################################################################
##4. Some holidays have a negative impact on sales.
#    Find out holidays which have higher sales than the mean sales in non-holiday season 
#    for all stores together
####################################################################################

# Holidays #

# Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
# Labour Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
# Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
# Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13


# average of weekly sales for non holiday week    

mean_of_non_holiday_Sales <-
  WSS %>%
  filter(Holiday_Flag == "0") %>%   # filtering for non holiday week
  summarise(mean(Weekly_Sales))     # summarising mean of weekly sales


#         mean(Weekly_Sales)
# # 1            1041256


# mutating comparision column for holiday weekly sales with mean sales of non holiday week in data

Sales_comparison <- WSS %>%
  group_by(Date) %>%                                                                                  # grouping by date
  filter(Holiday_Flag == "1") %>%                                                                     # filtering for holiday week
  summarise(Total_Sales_holiday_week = sum(Weekly_Sales)) %>%                                         # summarising for sum of weekly sales
  mutate(Greater_then_mean_of_non_holiday_week = Total_Sales_holiday_week>mean_of_non_holiday_Sales)  # mutating comparison column
  
  Sales_comparison
  
  # Date       Total_Sales_holiday_week   Greater_then_mean_of_non_holiday_week
  # <date>                        <dbl>   <lgl>                                 
  # 1 2010-02-12                48336678. TRUE                                  
  # 2 2010-09-10                45634398. TRUE                                  
  # 3 2010-11-26                65821003. TRUE                                  
  # 4 2010-12-31                40432519  TRUE                                  
  # 5 2011-02-11                47336193. TRUE                                  
  # 6 2011-09-09                46763228. TRUE                                  
  # 7 2011-11-25                66593605. TRUE                                  
  # 8 2011-12-30                46042461. TRUE                                  
  # 9 2012-02-10                50009408. TRUE                                  
  # 10 2012-09-07               48330059. TRUE      
  

# Adding holidays column to sales comparision
  
Sales_comparison$Holiday <- ifelse(month(ymd(Sales_comparison$Date)) == 2,"Super Bowl" , 
                               ifelse(month(ymd(Sales_comparison$Date)) == 9,"Labour Day" , 
                                  ifelse(month(ymd(Sales_comparison$Date)) == 11,"Thanksgiving" , "Christmas")))
  
  
Sales_comparison


# Date       Total_Sales_holiday_week  Greater_then_mean_of_non_holiday_week   Holiday     
# <date>                        <dbl>   <lgl>                                    <chr>       
# 1 2010-02-12                48336678. TRUE                                  Super Bowl  
# 2 2010-09-10                45634398. TRUE                                  Labour Day  
# 3 2010-11-26                65821003. TRUE                                  Thanksgiving
# 4 2010-12-31                40432519  TRUE                                  Christmas   
# 5 2011-02-11                47336193. TRUE                                  Super Bowl  
# 6 2011-09-09                46763228. TRUE                                  Labour Day  
# 7 2011-11-25                66593605. TRUE                                  Thanksgiving
# 8 2011-12-30                46042461. TRUE                                  Christmas   
# 9 2012-02-10                50009408. TRUE                                  Super Bowl  
# 10 2012-09-07               48330059. TRUE                                  Labour Day

# Above holidays have sales greater then mean of non holiday week



####################################################################################
## 5 Provide a monthly and semester view of sales in units and give insights
####################################################################################

### monthly view of sales ###

monthly_view = WSS %>% 
  mutate(Month = month(Date), Year = year(Date)) %>%  # mutating Month and Year column
  group_by(Month, Year) %>%                           # grouping by Month and Year
  summarise(Weekly_sales = sum(Weekly_Sales)) %>%     # summarising for sum of weekly sales
  arrange(desc(Weekly_sales))                         # arranging wrt Weekly Sales

head(monthly_view)

# Month  Year Weekly_sales
# <dbl> <dbl>        <dbl>
# 1    12  2010   288760533.
# 2    12  2011   288078102.
# 3     6  2012   240610329.
# 4     8  2012   236850766.
# 5     7  2010   232580126.
# 6     3  2012   231509650.



### semester view of sales ###

semester_view <- WSS %>% 
  mutate(sem.year = paste(semester(ymd(Date)),year(ymd(Date)),sep = ".")) %>%  # mutating sem.year column
  group_by(sem.year) %>%                                                       # grouping by semester
  summarise(Weekly_sales = sum(Weekly_Sales)) %>%                              # summarising for sum of weekly sales
  arrange(desc(Weekly_sales))                                                  # arranging wrt Weekly Sales

head(semester_view)

# sem.year Weekly_sales
# <chr>           <dbl>
# 1 2.2011    1320860210.
# 2 2.2010    1306263860.
# 3 1.2012    1210765416.
# 4 1.2011    1127339797.
# 5 1.2010     982622260.
# 6 2.2012     789367443.


############################################################################################
                              ## Statistical Model ##
############################################################################################
# For Store 1 – Build  prediction models to forecast demand
# Linear Regression – Utilize variables like date and restructure dates as 1 for 5 Feb 2010 
# (starting from the earliest date in order).
# Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
############################################################################################


## Restructuring dates ##

# mutating days column such that first date i.e, 2010-02-05 will become Day 1

data_dates <- WSS %>% mutate(Days = yday(Date), margin = 35) %>%  # mutating days column and margin (35 days) column
 mutate(Days = (Days - margin), margin = NULL)              # substracting Days with margin column so as to make it day 1 

data_dates <- data_dates %>% filter(Days>0)

head(data_dates)


#   Store       Date   Weekly_Sales Holiday_Flag Temperature Fuel_Price    CPI Unemployment Days
# 1     1 2010-02-05      1643691            0       42.31      2.572 211.0964        8.106    1
# 2     1 2010-02-12      1641957            1       38.51      2.548 211.2422        8.106    8
# 3     1 2010-02-19      1611968            0       39.93      2.514 211.2891        8.106   15
# 4     1 2010-02-26      1409728            0       46.63      2.561 211.3196        8.106   22
# 5     1 2010-03-05      1554807            0       46.50      2.625 211.3501        8.106   29
# 6     1 2010-03-12      1439542            0       57.79      2.667 211.3806        8.106   36


## filtering the data set for store-1

store_1_data <- WSS %>% filter(Store==1)

head(store_1_data)

#    Store  Date        Weekly_Sales Holiday_Flag Temperature Fuel_Price  CPI Unemployment
# 1     1 2010-02-05      1643691            0       42.31      2.572 211.0964        8.106
# 2     1 2010-02-12      1641957            1       38.51      2.548 211.2422        8.106
# 3     1 2010-02-19      1611968            0       39.93      2.514 211.2891        8.106
# 4     1 2010-02-26      1409728            0       46.63      2.561 211.3196        8.106
# 5     1 2010-03-05      1554807            0       46.50      2.625 211.3501        8.106
# 6     1 2010-03-12      1439542            0       57.79      2.667 211.3806        8.106


################################################################################################################
## Hypothesis testing if CPI, unemployment, and fuel price have any impact on sales using Muliple Linear Model ##
################################################################################################################


## Multiple linear model for impact of CPI, Uemployment and fuel price on sales

model <- lm(formula = Weekly_Sales ~ CPI + Unemployment + Fuel_Price, data = store_1_data) # lm(dependent ~ independent, data)

### Model Evaluation Technique - R-Squared Value ###

Rsqd <- summary(model)$r.squared

Rsqd 

#  0.08 = 8 %


### Model Evaluation Technique - RMSE ###

predicted_sales <- predict(model, store_1_data)

RMSE = sqrt(mean((store_1_data$Weekly_Sales - predicted_sales)^2)) # root mean squared error

RMSE 

# 148683

summary(model)

# lm(formula = Weekly_Sales ~ CPI + Unemployment + Fuel_Price, 
#    data = store_1_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -287777  -86699  -23987   61849  882877 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)   
#   (Intercept)  -3887096    1740276  -2.234  0.02711 * 
#   CPI             21792       6785   3.212  0.00164 **
#   Unemployment   124064      58779   2.111  0.03659 * 
#   Fuel_Price     -64838      46842  -1.384  0.16851   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 150800 on 139 degrees of freedom
# Multiple R-squared:  0.08499,	Adjusted R-squared:  0.06524 
# F-statistic: 4.303 on 3 and 139 DF,  p-value: 0.006162

###########################################################################################

## summary model ##

# Ho: CPI has no effect on Sales
# Ha: CPI has effect on Sales

# comparing pvalues

pvalue <- 0.00164
alpha <- 0.05

pvalue < alpha 

# TRUE 

# Rej Ho, CPI has impact on sales

###########################################################################################

# Ho: Unemployment has no effect on Sales
# Ha: Unemployment has effect on Sales

# comparing pvalues

pvalue <- 0.03659
alpha <- 0.05

pvalue < alpha 

# TRUE 

# Rej Ho, Unemployment has impact on sales

###########################################################################################

# Ho: Fuel Price has no effect on Sales
# Ha: Fuel Price has effect on Sales

# comparing pvalues

pvalue <- 0.16851
alpha <- 0.05

pvalue < alpha 

# FALSE 

# Do not rej Ho, Fuel cost has no impact on sales


#==========================================================================================#
                                     ## conclusion ##
#==========================================================================================#


# CPI and Unemployment has significant impact on sales.

# Fuel cost has no impact on sales.

# As explained variation (R -Sqaured) is too small in each model,
# the current models does not have much explanatory power, and we have to look for other model.


############################################################################################
                  ## Demand forcast using Multiple Linear Model ##
############################################################################################
                                       # Model-1 #
############################################################################################


# Building model = mod1 for dependent variable ~ independent variables

mod1 <- lm(formula = Weekly_Sales ~ Holiday_Flag + Temperature +Fuel_Price + CPI + Unemployment , data = store_1_data)

### Model Evaluation Technique - R-Squared Value ###

Rsqd <- summary(mod1)$r.squared
Rsqd 

# 0.1494714 = 14 %


### Model Evaluation Technique - RMSE ###

predicted_sales <- predict(mod1, store_1_data)

RMSE = sqrt(mean((store_1_data$Weekly_Sales - predicted_sales)^2)) # root mean squared error
RMSE

# 143348

summary(mod1)

# lm(formula = Weekly_Sales ~ Holiday_Flag + Temperature + Fuel_Price + 
#      CPI + Unemployment, data = store_1_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -305166  -78247  -18260   53643  854412 
# 
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  -2427856    1752958  -1.385   0.1683  
# Holiday_Flag    89376      49338   1.811   0.0723 .
# Temperature     -2160        922  -2.343   0.0206 *
# Fuel_Price     -24337      47335  -0.514   0.6080  
# CPI             16632       6786   2.451   0.0155 *
# Unemployment    80209      58726   1.366   0.1742  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 146500 on 137 degrees of freedom
# Multiple R-squared:  0.1495,	Adjusted R-squared:  0.1184 
# F-statistic: 4.815 on 5 and 137 DF,  p-value: 0.0004359



############################################################################################
                  ## Demand forcast using Multiple Linear Model ##
############################################################################################
                                      # Model-2 #
############################################################################################

# Eliminating insignificant variables Holiday Flag and Fuel Price due to high p value 
# compared to significance level (0.05) and Building model = mod2 for dependent variable ~ remaining independent variables

mod2 <- lm(formula = Weekly_Sales ~ Temperature + CPI + Unemployment , data = store_1_data)

### Model Evaluation Technique - R-Squared Value ###

Rsqd <- summary(mod2)$r.squared
Rsqd 

# 0.1263056 = 12 %


### Model Evaluation Technique - RMSE ###

predicted_sales <- predict(mod2, store_1_data)

RMSE = sqrt(mean((store_1_data$Weekly_Sales - predicted_sales)^2)) # root mean squared error
RMSE

# 145287

summary(mod2)

# lm(formula = Weekly_Sales ~ Temperature + CPI + Unemployment, 
#    data = store_1_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -311770  -85435  -10998   55936  841075 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  -2048494.5  1430740.6  -1.432  0.15445   
# Temperature     -2587.7      883.4  -2.929  0.00397 **
# CPI             14730.4     4893.0   3.011  0.00310 **
# Unemployment    78679.8    56007.2   1.405  0.16231   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 147400 on 139 degrees of freedom
# Multiple R-squared:  0.1263,	Adjusted R-squared:  0.1074 
# F-statistic: 6.698 on 3 and 139 DF,  p-value: 0.0002957



############################################################################################
                  ## Demand forcast using Multiple Linear Model ##
############################################################################################
                                     # Model-3 #
############################################################################################

# Eliminating insignificant variable Unemployment due to high p value compared to significance level (0.05) and
# Building model = mod3 for dependent variable ~ remaining independent variables

mod3 <- lm(formula = Weekly_Sales ~ Temperature + CPI , data = store_1_data)

### Model Evaluation Technique - R-Squared Value ###

Rsqd <- summary(mod3)$r.squared
Rsqd 

# 0.113901 = 11 %


### Model Evaluation Technique - RMSE ###

predicted_sales <- predict(mod3, store_1_data)

RMSE = sqrt(mean((store_1_data$Weekly_Sales - predicted_sales)^2)) # root mean squared error
RMSE

# 146314

summary(mod3)

# lm(formula = Weekly_Sales ~ Temperature + CPI, data = store_1_data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -312205  -85704   -9198   57222  830489 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  -233190     616327  -0.378  0.70574   
# Temperature    -2769        877  -3.157  0.00195 **
# CPI             9156       2872   3.187  0.00177 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 147900 on 140 degrees of freedom
# Multiple R-squared:  0.1139,	Adjusted R-squared:  0.1012 
# F-statistic: 8.998 on 2 and 140 DF,  p-value: 0.0002107



############################################################################################
# For both the independent variables in model-3 we have p value less then significance level (0.05)
# so summarising model-3
############################################################################################

## summary of model-3 ##

# Ho: Temperature has no effect on Sales
# Ha: Temperature has effect on Sales

# comparing pvalues

pvalue <- 0.00195
alpha <- 0.05

pvalue < alpha 

# TRUE 

# Rej Ho, Temperature has impact on sales

###########################################################################################

# Ho: CPI has no effect on Sales
# Ha: CPI has effect on Sales

# comparing pvalues

pvalue <- 0.00177
alpha <- 0.05

pvalue < alpha 

# TRUE 

# Rej Ho, CPI has impact on sales

###########################################################################################

## Multiple linear regression equation ##

# y = b0 + b1x1 + b2x2 + b3x3 + b4x4
 
# Weekly_Sales = 89376 * Holiday_Flag + (-2160 ) * Temperature + (-24337) * Fuel_Price +  16632 * CPI + 80209 * Unemployment

# CPI coefficient

Weekly_Sales_1 = 89376 * 1 + (-2160 ) * 1 + (-24337) * 1 +  16632 * (1) + 80209 * 7
Weekly_Sales_1

Weekly_Sales_2 = 89376 * 1 + (-2160 ) * 1 + (-24337) * 1 +  16632 * (1) + 80209 * 6
Weekly_Sales_2

Weekly_Sales_2 - Weekly_Sales_1

# 640974 - 560765 = -80209

# With 1 unit change (decrease) in CPI, sales is affected by -80209

############################################################################################

# Temperature coefficient

Weekly_Sales_1 = 89376 * 1 + (-2160 ) * 69.2 + (-24337) * 1 +  16632 * (1) + 80209 * 1
Weekly_Sales_1

Weekly_Sales_2 = 89376 * 1 + (-2160 ) * 35.40 + (-24337) * 1 +  16632 * (1) + 80209 * 1
Weekly_Sales_2

Weekly_Sales_2 - Weekly_Sales_1

# 12408 - 85416 = 73008

# With decrease in 1 degree Celsius (33.8 Fahrenheit), sales is affected by 73008 sales unit.

summary(store_1_data)

#        Store        Date         Weekly_Sales      Holiday_Flag      Temperature      Fuel_Price         CPI         Unemployment  
# Min.   :1   Min.   :2010-02-05   Min.   :1316899   Min.   :0.00000   Min.   :35.40   Min.   :2.514   Min.   :210.3   Min.   :6.573  
# 1st Qu.:1   1st Qu.:2010-10-11   1st Qu.:1458105   1st Qu.:0.00000   1st Qu.:58.27   1st Qu.:2.764   1st Qu.:211.5   1st Qu.:7.348  
# Median :1   Median :2011-06-17   Median :1534850   Median :0.00000   Median :69.64   Median :3.290   Median :215.5   Median :7.787  
# Mean   :1   Mean   :2011-06-17   Mean   :1555264   Mean   :0.06993   Mean   :68.31   Mean   :3.220   Mean   :216.0   Mean   :7.610  
# 3rd Qu.:1   3rd Qu.:2012-02-20   3rd Qu.:1614892   3rd Qu.:0.00000   3rd Qu.:80.48   3rd Qu.:3.594   3rd Qu.:220.5   3rd Qu.:7.838  
# Max.   :1   Max.   :2012-10-26   Max.   :2387950   Max.   :1.00000   Max.   :91.65   Max.   :3.907   Max.   :223.4   Max.   :8.106  

############################################################################################
#==========================================================================================#
                                     ## conclusion ##
#==========================================================================================#
############################################################################################

# As explained variation (R -Sqaured) is too small in each model,
# the current models does not have much explanatory power, and we have to look for other model.

# However, below points can be noted:

# 1. Fuel cost and Unemployment has no impact on weekly sales for store-1.

# 2. Whereas, Temperature and CPI has significant impact on sales for store-1.

# 3. As temperature has Max. value 91.65 Fahrenheit in summer and Min. 35.40 Fahrenheit in winter,
#    With decrease in 1 degree Celsius (33.8 Fahrenheit), sales is affected and increase by 73008 sales unit.

# 4. With the consistent decrease in CPI ( from 8.106 on 2010-02-05 to 6.573 on 2012-10-26 ),
#    sales are getting affected, with 1 unit change (decrease) in CPI,
#    sales is affected and decreased by -80209 sales unit.


