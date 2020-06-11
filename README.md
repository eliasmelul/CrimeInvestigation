# CrimeInvestigation

## The Relationship between the Crime Rate and Weather in Boston
### Introduction
Previous research has demonstrated the significant role weather plays on the occurrence of crime-related events. This study analyzes the weather and crime incidents in Boston, Massachusetts to find a possible correlation between weather and crime rate. The aim from gathering data and carrying out the results is to inform the city and especially the police department on the distinct locations and other important details to take note of at certain times of the year. This analysis on the relationship between the weather and the criminal incidents in Boston will add benefit to the Boston Police Department. Although, according to the Federal Bureau of Investigation’s Uniform Crime Report, the crime rate has been steadily declining between 1999 and 2017, a study like this will help continue to decrease the crime rate by providing the police department with detailed information like the effects of temperature changes and the significance in knowing the specific locations. As a result, the Boston Police Department may be able to allocate their resources more strategically and effectively based on the time of the year.

## Data Collection
The study used two datasets provided by Kaggle: the Boston Weather Report from January 2014 to April 2018, and the Boston Crime Report from January 2015 to October 2018. Since the study requires the use of both datasets to compare the two variables based on the time of the year, both datasets need to be matched by date. Therefore, the reports recorded between January 2015 to April 2018 will be used to carry out the analysis.

The Boston Weather Report contains data on contents such as the daily average temperature, humidity level, and precipitation count, and the Boston Crime Report is comprised of data on the time of the incidents, the offense types, and the location of the incidents, among others. In this study, we have extracted data on the criminal incidents to analyze it with the corresponding weather attributes on those days. Since the aim is to find the correlation between weather and crime rates in Boston, the target variables will be the location, date, crime rate, and weather (temperature).

## Exploratory Data Analysis: Top 15 Offense Groups
Due to the nature of our data, we decided to explore the datasets in terms of the frequency of the number of daily crimes, categorical variables, such as the offense group and the day of the week, weather predictors and time series.
![](https://i.postimg.cc/T1HhppT0/Box-Plot-Overall.png)
_Figure 1. Boxplot on the Frequency for Each Offense Group_

As seen on Figure 1, a boxplot was constructed to display the frequencies for each offense category listed in the dataset. There are 67 categories of crime. The outliers observed for all the crimes are seen to be higher in frequency, and the crimes that tend to be more common have a much larger frequency than the mean crime frequency by type. We can observe that most crimes can be represented by a small number of offenses. More specifically, the top 15 most frequent offense group represent 75% of the total crimes, as can be observed from the Pareto Chart below.
![Pareto Chart](https://i.postimg.cc/K87FWRCW/Pareto-Chart.png)
_Figure 2: The Pareto Chart shows that around 75% of crimes are part of the top 15 most frequent offense groups. Note that representation above has been “cut” to hide infinitesimally small frequency offense groups._

[![Boxplot-Top15.png](https://i.postimg.cc/Y9MvP5X6/Boxplot-Top15.png)](https://postimg.cc/67b62mxq)
_Figure 3. Boxplot on the Frequency for the Top 15 Offense Group_

Figure 3 takes a closer look into the 15 highest offense groups and their daily crime frequency as displayed on Figure 1. As seen on this figure, Motor Vehicle Accident Response is by far the most common crime, followed by Larceny, Medical Assistance, Investigate Person and Other. Note the high-leverage outliers of Towed and Other.

## Exploratory Data Analysis II: Day of Week and Crimes Time Series Decomposition
Taking a closer look to the data, we can determine when the extraordinary outliers of the Towed and the Other offense groups occurred and whether there is time-wise trend and seasonality in the model. 

![BaseTimeSeries](https://i.postimg.cc/3NpJJ9b8/Time-Series.png)
_Figure 4. Time Series of the crime rate_

From the time-series represented in Figure 4 of total daily crimes (black) and a 30-day moving average (blue), we can observe a clear yearly seasonality with the trough during winter and the peak during summer. However, the variance about the moving average of the total daily crimes is very high, indicating that there may be another seasonality. 

![Day of Week Boxplot](https://i.postimg.cc/X7GYsNjM/Freq-Do-W-Boxplot.png)
_Figure 5. Boxplot on the Frequency of Each Offense Group_

Crime during the weekends is significantly lower than that in weekdays. It increases steadily from Monday to Friday and drops suddenly in Saturday and even further on Sunday. Therefore, there is evidence about a 7 day seasonality. Reasons can be criminals would like to rest in weekends or people are at home during weekends, so it is harder to commit crimes. The lowest crime per day is approximately 220 and the highest is approximately 305. The range of crime per day is about 85. More specifically, Tuesday has smallest range and Wednesday has the largest ones. The ranges of weekends do not deviate lot from those of weekdays. Wednesday has no outliers and other days have 2-4 outliers. 

![ACF](https://i.postimg.cc/pdXTYfyC/Rplot08.png)
_Figure 6. Autocorrelation Function Plot (ACF)_

The ACF shows there's a correlation between points separated by 7 days (by 7 time-lags) and multiples of seven, therefore confirming that there is a weekly seasonality.

### Timeseries Decomposition
![Timeseries Decomposition](https://i.postimg.cc/L40skgMB/Decomposition.png)
_Figure 7. Timeseries Decomposition_

Figure 7 shows that there is a slight positive trend in the overall timeseries from 2015 to 2017, and a decreasing trend for 2017. This means that in average the number of crimes increased from 2015 to 2017 and decreased thereafter. The seasonal component of the decomposition shows evidence of the yearly and weekly seasonality we observed earlier. The random component of the timeseries shows a high range, 80+ crimes, for the randomness attributed to this model. 

## Exploratory Data Analysis III: A Note about Outliers

![OtherTowed](https://i.postimg.cc/4dWxSmHG/DCo-TOGOther-Towed.png)
_Figure 8. Daily Crimes for the 'Other' and 'Towed' offense groups over time_

As we mentioned in _Exploratory Data Analysis I: Top 15 Offense Groups_ there were notable outliers in some offense groups, most conspicuously the 'Other' and the 'Towed' offense groups. Figure 8 above shows the daily crimes time series for these offense groups and displayed some notable dates:

For the ‘Other’ offense group:
1. Freq: 77; March 20th 2016
   * Red-Sox versus Mets
   * St. Patrick’s Day Parade
2. Freq: 56, Jan. 4th 2018
   * Blizzard
3. Freq: 32, Jan. 8th 2018
   * Blizzard

For the ‘Towed’ offense group:
1. Freq: 88; March 13th 2018 
   * Blizzard + Outages
2. Freq: 64, Feb. 9th 2017
   * Blizzard + Outages 
3. Freq: 49, March 14th 2017
   * Blizzard

As we can observe, blizzards present opportunities for different crimes to occur sporadically. The Boston Police Department (BPD) should focus efforts investigating what effects of blizzards cause different types of robberies. 

Since Blizzards are the most common cause for radical spikes in crime rates, possibly because of side-effects of blizzards, such as outages, we can predict that the 'Event' attribute in the dataset (denoting four levels: None, Snow, Rain, or Both) and possibly the 'Snowfall' variable are good predictors for the frequency of crime.

## Exploratory Data Analysis IV: Weather Parameters and Correlations
When comparing weather parameters to the crime frequency, we first considered a correlation plot between frequency and the weather parameters.

![Correlation Plot](https://i.postimg.cc/15ffbcKj/CorrPlot.png)
_Figure 9. Correlation Plot_

The correlation plot depicted in Figure 9 shows that the only parameters that correlate to the frequency of crimes are average temperature (F) and average dew point (F), both of which are significantly correlated between them. This may be an indicator for potential significant predictors for crime rates.

![4x1TimePlotsWeather](https://i.postimg.cc/grFFSWsd/Rplot.png)
_Figure 10. Timeseries of daily crimes over different weather parameters_

The correlation between frequency and average temperature and average dew point can also be observed in the timeseries plots in Figure 10, where the black represents daily crimes, the blue line represents the 30 day moving average of the daily crimes, and the red and yellow lines represent the same characteristics but for the weather parameters respectively with respect to the second y-axis (right). 

## Exploratory Data Analysis V: More on Weather and Offense Groups
In conducting analysis of daily crimes for the Top 15 most frequent offense groups, we concluded that there are no strong, significant relationships between different weather parameters and the crime rates of different offense groups. Take 'Motor Vehicle Accident Response' as an example. It is the most frequent occuring offense in Boston. When the temperatire is cold enough, Boston starts being full of snow and icy roads, hence potentially provoking more accidents. However, the general seasonality of crimes, including 'Motor Vehicle Accident Response', states  that crime rates are higher during summer than winter - contradicting the logic that icy roads lead to more accidents. Because of instances like such were the offense groups analyzed separately. However, as previously mentioned, this analysis did not generate conclusive or relevant insights.

### Scatterplots Facetted by Offense Groups
![Top5Scatter](https://i.postimg.cc/cJMHfDsr/Cp-D-Avg-Temp-OLS-Top5.png)
_Figure 11. Scatterplots of the top 5 most frequent offense groups against temperature_

Figure 11 shows that given the data we have on Boston crimes and temperature, it is not reasonable to make any statement on the correlation of two variables. The only crime that seems to show a correlation is larceny. This could be because larceny is more common when people have more activities and people are less likely to go outside when the temperature is below 25. However, as showed in the graph above, the distribution of the data is heavily entered around the 50-75 temperature range which might result in bias in the OLS process. Additional scatterplots on the next 10 most frequent offense groups can be seen in the appendix below.

![Second5Scatter](https://i.postimg.cc/fTpJst2T/Cp-D-Avg-Temp-OLS-Second5.png)
_Figure 12. Scatterplots of the second 5 most frequent offense groups against temperature_

![Third5Scatter](https://i.postimg.cc/d1s0681n/Cp-D-Avg-Temp-OLS-Third5.png)
_Figure 13. Scatterplots of the third 5 most frequent offense groups against temperature_

### Crimes Per Day versus Day of the Week Dacetted by Offense Groups
![BoxDoWTop5](https://i.postimg.cc/3Rf3ZVzJ/Freq-Do-W-Boxplots-Top5.png)
_Figure 14. Crimes Per Day against Doy of Week Grouped by Offense Groups; Top 5_

The trend of first 5 most frequent crimes from first day to last day of the week is consistent with the trend of all crimes. That is, in weekends there are fewer crimes and in week day there are more. However, this trend is the least evident in the motor vehicle accident, which may be due to traffic accidents happen on the road instead of indoor. The range of first 5 most frequent crimes is large in weekdays and small in weekends. This finding is similar to that of overall crimes.  Motor vehicle accident and investigate person has few outliers while the others have more (Figure 7). Additional graphs on the next 10 frequent crimes can be seen in the appendix. In general, we find that the first 5 frequent crimes tend to happen less in weekdays and more in weekends while the second and third 5 frequent crimes tend to happen more in weekdays and less in weekends. The weighting average of them happen less in weekdays and more in weekends. The range of crimes do not show explicit signs. The second 5 frequent crimes have more outliers while others are roughly the same.  

![DoWFacettedSecond5](https://i.postimg.cc/HL2YrV2M/Freq-Do-W-Boxplots-Second5.png)
_Figure 15. Crimes Per Day against Doy of Week Grouped by Offense Groups: Second 5_

The trend of second 5 most frequent crimes from Sunday to Saturday in a week do not follow that of overall crimes. Drug violation and towed have less crimes on weekends and more on weekdays, which is similar to overall situation. Simple assault, vandalism and verbal disputes have less in weekdays and more on weekends. The range of crimes per day of second 5 most frequent crimes follow no clear rules between weekends and weekdays. The range of drug violation is larger than other crimes. The second 5 most frequent crimes have more outliers than first 5 most frequent crimes.

![ThirdDoW5](https://i.postimg.cc/QtdX1h4y/Freq-Do-W-Boxplots-Third5.png)
_Figure 15. Crimes Per Day against Doy of Week Grouped by Offense Groups: Third 5_

The trend of third 5 most frequent crimes from Sunday to Saturday in a week do not follow that of overall crimes. Only warrant arrests have less crimes on weekends and more on weekdays, which is similar to overall situation. Aggravated assault, investigate property, larceny from motor vehicle and property lost have less in weekdays and more on weekends.
The range of crimes per day of third 5 most frequent crimes follow no clear rules between weekends and weekdays. In detail, larceny from motor vehicle has the largest average range in a week. The third 5 most frequent crimes have less outliers than second 5 most frequent crimes and approximately the same as first 5 crimes and the overall one.

## Modeling: Linear Regression
Numerous models and transformations were considered and constructed in addition to the models discussed in this section. However, we concluded that it wasn’t appropriate to make any transformations due to the nature and relationships of the datasets being used.

### Intuitive Linear Regression Model

A linear regression model was conducted, in which the estimation model can be illustrated as: 

Frequency = Intercept + Avg_Temp + DoW + Events   

where Avg_Temp is represented with integers, and DoW and Events are binary indicators of each day of the week and the events of precipitation, respectively. Based on our data exploration, we believe that that the best predictors for crime rate are (1) the average temperature, (2) the day of the week, and (3) the event – Rain, None, Snow, or Both. Average Dew Point was excluded due to the extremely high correlation with average temperature. The model is summarized in the following table:

![Intuitive](https://i.postimg.cc/CLTpwqBm/Intuitive.png)

The intuitive model of Figure 8 delivers an adjusted R-squared of 0.3573 and AIC of 9682.215. All variables are significant at 99.9% except for EventRain and EventSnow. According to this model, with everything else constant, weekdays tend to have a higher average for the number of crimes per day with Sunday being the day with the least average amount of crimes by 27 crimes. Furthermore, crimes tend to increase by 12 crimes per day if doesn’t rain and snow and increase by 0.8 crimes for every increase in the average temperature (F). Since our range for average temperature is 86 Fahrenheit, with a minimum of 2 Fahrenheit, the increase amount of the count of the daily crimes can be up to 69 daily crimes within the set of average temperatures recorded. 

### Backward Selection Model

A backward selection model was conducted, in which the estimation model can be illustrated as: 

Frequency = Avg_Dew_Point + Avg_Humidity + Avg_Visibility + DoW

where Avg_Dew_Point, Avg_Humidity, and Avg_Visibility are represented with integers, and DoW is a binary factorized variable for each day of the week. A summary of the model is described below:

![BackwardsModel](https://i.postimg.cc/hPLWvtVg/Backwards.png)

From running a backward selection model on our dataset, we found the weekdays to have a higher average crime rate than the weekends, with Sunday at the lowest and Saturday as the second lowest. We see that the average dew point and humidity are also highly statistically significant in predicting the crime rates in this model. With the daily crime rate as the independent variable, the model gets an adjusted R-squared value of 0.3663 and an AIC of 9666.663. With a resembling interpretation of the coefficients as the Intuitive model, we also have a level-log relationship between the average humidity (represented as a percentage) and the crime rate. This relationship can be interpreted as, with all other factors unchanged, per every percent increment of the average humidity, the crime rate decreases by roughly half percent.

### Forwards and Stepwise Selection Model
Both selection models had the same outcome.

A forward and stepwise selection model was conducted, in which the estimation model can be illustrated as: 

Frequency = Avg_Temp + Avg_Visibility + DoW  

where Avg_Temp and Avg_Visibility are represented with integers, and DoW is a binary indicator of each day of the week. A summary of the model is described below:

![ForwardandStep](https://i.postimg.cc/BZ1GWg6j/Forwardand-Step.png)

The forward selection model and stepwise selection model end up yielding the exact same model. As figure 10 shows, all the variables in the model are highly significant to the model. When looking at “day of the week”, the model shows that, compared to Sundays, weekdays have a higher average crime rate holding all other variables constant. With the daily crime rate as the independent variable, we get the average temperature, the average visibility, and the day of the week with an adjusted R-squared value of 0.3616 and an AIC of 9673.246. When comparing the R-squared value and the AIC between the backward selection model and the forward and stepwise selection model, we can see that the values are very similar to each other. 

### Assumption Check - Intuitive Model

![Assumptions](https://i.postimg.cc/tR1sMNkm/Rplot02.png)
_Figure 16. Residual vs. Fitter (upper left), Normal Quantile-Quantile (upper right), Scale vs. Location (bottom left), Residuals vs. Leverage (upper right)_

1. From the residual versus fitted graph, we can observe that the number of data points above and below the red line are roughly the same, demonstrating that the mean of the residuals is 0. Furthermore, we observe a quasi-constant distribution of data, indicating linearity of the model.
2. From the normal quantile to quantile plot, we can see that the data passes the “fat pencil test” indicating normality of data except for the lower tail of data.
3. The residual versus leverage plot shows that there is only one high-leverage observation with a cook’s distance (distance from centroid) below 0.5, hence the model doesn’t seem to be significantly influenced by outliers.

### Discussion and Evaluation

A correlation between weather and crime rate in Boston has been observed from the years 2015 to 2018. The frequency of crimes tends to move drastically with Boston’s seasonal changes, where there is an overall higher count during the summer following a lower count in the winter. According to the intuitive linear regression model, a unit increase in the temperature (degree) results in an increase in the daily crime rate by a statistically significant value of approximately 187 records. This could possibly be explained by the inclination of people to be more active during warmer, endurable temperatures than during the winter of Boston, which is commonly known to be very cold. Furthermore, it has been observed that the crime rate is significantly higher during the weekdays than during the weekend. As depicted in both the forward and backward selection models, the crime rate recorded between the years 2015 and 2018 gradually increases from Monday to Friday and declines on Saturday and even further on Sunday, all of which have been established as statistically significant results.  

![SummaryModelsLM](https://i.postimg.cc/XYWZBqhg/Summary-Models-LM.png)

From the table above, we can see that the backward selection model has the largest R-square value and the smallest AIC value compared to the other two models. Accordingly, the backward selection model interprets the crime frequency relatively better but is not as intuitive as the intuitive linear regression model. We also notice that all the models provide a similar story with most important variables relating to the average temperature and day of week, with the goodness of fit coefficients of the three models being extremely close. Therefore, we choose to keep the Intuitive Linear Regression Model for crime rate prediction.	

It is important to address that the results obtained from this study do not assure complete knowledge and prediction of the crime tendencies within specific locations of Boston during each season. This study only observes the past occurrences of crime in order to help raise awareness of possible criminal activity in certain areas of Boston during a certain period of the year. In addition, it is likely that not all crimes have been reported to the police department for multiple reasons such as fear to report. Therefore, a dataset covering more years of records on crime and on temperature would increase the accuracy of our results.  

## Modeling: Exponential Smoothing (Holt Winters)
![Holt Winters Fitted](https://i.postimg.cc/G3YG4Wtd/HoltWintersFitted.png)

_Figure 17. Holt Winter Actual (black) and Fitted (red) timeseries_

As we can observe, the exponential smoothing model fits the values pretty accurately. It follows the yearly seasonality, and overestimates the effect of the 7-day seasonality. However, the volatility of the fitted values about the seasonality moving average seems to be decreasing towards the dates closer to the present.

![Holt Winters Forecast](https://i.postimg.cc/Vs3JQ0Hs/Rplot03.png)

_Figure 18. Holt Winters Forecast of 365 days (blue and gray for actual values and the 95% confidence intervals around those values respectively) 
NB: Glitch in HW model shifted time-values (x-axis) by 182 days_

Holt Winters Model accuracy:

     ME          RMSE         MAE           MPE        MAPE         MASE          ACF1
     -1.028769   31.54892     21.82376     -1.224957   8.383455     0.7280578     0.08765785

This forecasting model allows for better predictions than the Linear Regression models, but, less intuitive ones. This model has a mean absolute percentage error (MAPE) of 8.38. It also has a root mean square error (RMSE) of 31.54 - the standard deviation of the residuals. 
