##########          Team A8 Project         ##########

###-1-###         Importing the datasets          ########
library(readxl)
library(tidyverse)       

# Boston Crime Data

bostoncrime <- read_excel(choose.files(), 
                          col_types = c("text", "numeric", "text", 
                                        "text", "text", "numeric", "numeric", 
                                        "date", "numeric", "numeric", "text", 
                                        "numeric", "text", "text", "numeric", 
                                        "numeric", "text"))
names(bostoncrime) <- c("incident_id", "offense_code", "offense_group", "offense_description", "district", "reporting_area", "shooting","DateOcc","Year","Month","Weekday","Hour","UCR_Part","Street","Lat","Long","Location")

#Add column with Date as a date-format type and without hours/minutes/seconds
bostoncrime$Date <- as.Date(format(bostoncrime$DateOcc, format="%Y-%m-%d"))
bostoncrime$DateMonth <- (paste(bostoncrime$Year, bostoncrime$Month, sep = "/"))
bostoncrime$DayofWeek <- weekdays(bostoncrime$Date) #This line creates a column called DayofWeek that includes the name of the day in every record (Monday, Tuesday, Wednesday....)

# Boston Weather Data       

bostonweather <- read_excel(choose.files())

bostonweather$Date <- as.Date(paste(bostonweather$Year, bostonweather$Month, bostonweather$Day, sep = "-"))
bostonweather$DayofWeek <- weekdays(bostonweather$Date) #This line creates a column called DayofWeek that includes the name of the day in every record (Monday, Tuesday, Wednesday....)

names(bostonweather) <- c("Year","Month","Day","Date","High_Temp","Avg_Temp","Low_Temp","High_Dew_Point","Avg_Dew_Point","Low_Dew_Point","High_Humidity","Avg_Humidity","Low_Humidity","High_Sea_Level_Press","Avg_Sea_Level_Press","Low_Sea_Level_Press","High_Visibility","Avg_Visibility","Low_Visibility","High_Wind","Avg_Wind","High_Wind_Gust","Snowfall","Precip","Events","DayofWeek")

bostonweatherdf <- bostonweather %>%
  filter(Date >= "2015-06-15")

bostoncrimedf <- bostoncrime %>%
  filter(Date <= "2018-04-08") %>%
  group_by(Date) %>%
  tally()

names(bostoncrimedf) <- c("Date", "Frequency")

bostoncrimedf2 <- bostoncrime %>%
  filter(Date <= "2018-04-08") %>%
  group_by(Date, offense_group) %>%
  tally()
names(bostoncrimedf2) <- c("Date", "offense_group", "Frequency")

#Join boston weather and boston crime dataframes
boston <- left_join(bostoncrimedf, bostonweatherdf, by = c("Date", "Date"))
boston <- na.exclude(boston)
boston2 <- left_join(bostoncrimedf2, bostonweatherdf, by =c("Date", "Date"))
boston2 <- na.exclude(boston2)
#Clean new dataframe from unnecessary/repeated data
boston$Year <- NULL
boston$Month <- NULL
boston$Day <- NULL


#####         Creating Severity Ratings         #####
Severity1 <- c("Fire Related Reports", "Ballistics", "Drug Violation", "Missing Person Reported", "Firearm Violations", "Arson", "Bomb Hoax", "Firearm Discovery", "Operating Under the Influence", "Offenses Against Child / Family", "Prostitution", "Prisoner Related Incidents", "Homicide", "Explosives", "Criminal Harassment","Aircraft","Biological Threat","Manslaughter","HUMAN TRAFFICKING", "HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE")
Severity2 <- c("Larceny","Vandalism","Towed","Investigate Property","Motor Vehicle Accident Response","Larceny From Motor Vehicle","Residential Burglary","Fraud","Other Burglary","Landlord/Tenant Disputes","Confidence Games","License Violation","Commercial Burglary","Evading Fare","Phone Call Complaints","Burglary - No Property Taken")

bostoncrime$Severity <- ifelse(bostoncrime$offense_group %in% Severity1, "High", 
                               ifelse(bostoncrime$offense_group %in% Severity2, "Low", 0))
bostoncrime %>%
  group_by(Severity)%>%
  tally()


bostoncrimedf3 <- bostoncrime %>%
  filter(Date <= "2018-04-08") %>%
  group_by(Date, Severity, offense_group) %>%
  tally()
names(bostoncrimedf3) <- c("Date","Severity", "offense_group", "Frequency")

boston3 <- left_join(bostoncrimedf3, bostonweatherdf, by = c("Date", "Date"))


#####         Creating Crime Groups         #####
Violentlist <- c("Simple Assault", "Restraining Order Violations", "Harassment", "Ballistics", "Property Related Damage", "Aggravated Assault", "Firearm Violations", "Arson", "Bomb Hoax", "Firearm Discovery", "Home Invasion", "Offenses Against Child / Family", "Homicide", "Explosives", "Criminal Harassment", "Manslaughter")
Humanlist <- c("Prostitution", "HUMAN TRAFFICKING", "HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE")
Burglarylist <- c("Auto Theft", "Robbery", "Residential Burglary", "Property Found", "Other Burglary", "Commercial Burglary", "HOME INVASION", "Burglary - No Property Taken")

bostoncrime$CrimeGroups <- ifelse(bostoncrime$offense_group %in% Violentlist, "Violent", 
                                  ifelse(bostoncrime$offense_group %in% Humanlist, "Human",
                                         ifelse(bostoncrime$offense_group %in% Burglarylist, "Burglary", 0)))

bostoncrimedf4 <- bostoncrime %>%
  filter(Date <= "2018-04-08") %>%
  group_by(Date, Severity, offense_group, CrimeGroups) %>%
  tally()
names(bostoncrimedf4) <- c("Date","Severity","offense_group", "CrimeGroups", "Frequency")

boston4 <- left_join(bostoncrimedf4, bostonweatherdf, by = c("Date", "Date"))

boston4 <- na.omit(boston4)
boston3 <- na.exclude(boston3)
boston2 <- na.exclude(boston2)
boston <- na.exclude(boston)

#####         Creating a full subset per Month          #####
bostoncrimedf5 <- bostoncrime %>%
  filter(Date <= "2018-04-08") %>%
  group_by(Year, Month, Severity, offense_group, CrimeGroups) %>%
  tally()
names(bostoncrimedf5) <- c("Year","Month","Severity","offense_group", "CrimeGroups", "Frequency")

bostoncrimedf5$DateMonth <- (paste(bostoncrimedf5$Year, bostoncrimedf5$Month, sep = "-"))


bostonweatherdf5 <- bostonweather %>%
  filter(Date <= "2018-04-08") %>%
  group_by(Year, Month)%>%
  summarize(Avg_Temp = mean(Avg_Temp), Avg_Dew_Point = mean(Avg_Dew_Point), Avg_Humidity = mean(Avg_Humidity), Avg_Sea_Level_Press = mean(Avg_Sea_Level_Press), Avg_Visibility = mean(Avg_Visibility), Avg_Wind = mean(Avg_Wind))

bostonweatherdf5$DateMonth <- (paste(bostonweatherdf5$Year, bostonweatherdf5$Month, sep = "-"))

bostonmonth <- left_join(bostoncrimedf5, bostonweatherdf5, by = c("DateMonth", "DateMonth"))

###-2-###         Exploratory Data Analysis         ########
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(zoo)
library(qcc)
library(lubridate)

#####         Summary Statistics          #####
#Boson Crime Summary stats
names(bostoncrime)

#Average number of crimes per day
bostoncrimedf %>%
  select(Frequency) %>%
  summarize_all(n_distinct)

#Boston Weather Summary stats (use bostonweatherdf to include date parameter)
names(bostonweatherdf)
head(bostonweatherdf)

#####         Exploration  Data: Per Day       #####

#Subset by Date
BCDate <- bostoncrime %>%
  group_by(Date) %>%
  tally()
names(BCDate) <- c("Date", "Frequency")

#Subset by date and offense group
BC_Date_OGroup <- bostoncrime %>%
  group_by(Date, offense_group) %>%
  tally()
names(BC_Date_OGroup) <- c("Date", "OffenseGroup", "Frequency")


### Determining the offense groups with the highest units ###

#Create table ordered from month to least frequency 
OrderedBCday <- BC_Date_OGroup %>%
  group_by(OffenseGroup) %>%
  summarize(mean = mean(Frequency))%>%
  arrange(desc(mean))
head(OrderedBCday, 20)

#Determine OffenseGroups with most offenses
offensesday <- list("Motor Vehicle Accident Response", "Larceny", "Medical Assistance", "Investigate Person", "Other", "Drug Violation","Simple Assault","Vandalism","Verbal Disputes","Towed","Investigate Property","Larceny From Motor Vehicle","Property Lost","Warrant Arrests","Aggravated Assault")

### Pareto Graph to see the cumulative percent of frequency per offense group
pareto <- boston4 %>%
  group_by(offense_group) %>%
  summarise(total = sum(Frequency)) %>%
  arrange(desc(total))

#Since the difference is infantesimaly small, we take off the crime groups with frequencies under 100 to make the visual readable
pareto <- pareto %>%
  filter(total >= 100)
parchar <- pareto$total
names(parchar) <- pareto$offense_group

pareto.chart(parchar, xlab = NULL, main = "Pareto Chart Offense Groups")

### BOXPLOTS  ###
#ALL Offense groups - Box plot Daily Frequency per Offense group 
boston4 %>%
  ggplot(aes(x=offense_group, y=Frequency, fill = offense_group)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  theme(legend.position = "bottom")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


### Top 15 offense groups ordered from most to least
boston4 %>%
  filter(offense_group %in% offensesday) %>%
  ggplot(aes(x=reorder(offense_group, -Frequency), y=Frequency, fill = offense_group)) +
  geom_boxplot() +
  theme_bw()+
  ggtitle('Boxplot of Daily Crimes Frequency: Top 15 Offense Groups')+  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9)) +
  xlab("Offense Group")+
  theme(legend.position = "bottom")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Top Offense groups - Box plot Daily frequency per offense group
BC_Date_OGroup %>%
  filter(OffenseGroup %in% offensesday) %>%
  ggplot(aes(x=OffenseGroup, y=Frequency, fill = OffenseGroup)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  guides(fill=FALSE)

#####         Exploration  Data: Per Month       #####

#Subset by month on date per offense group
BC_Month_OGroup <- bostoncrime %>%
  group_by(DateMonth, offense_group) %>%
  tally()
names(BC_Month_OGroup) <- c("DateMonth", "OffenseGroup", "Frequency")

#Create table ordered from month to least frequency 
OrderedBC <- BC_Month_OGroup %>%
  group_by(OffenseGroup) %>%
  summarize(mean = mean(Frequency))%>%
  arrange(desc(mean))
head(OrderedBC, 20)

#Define OffenseGroups with most offenses
offenses <- list("Motor Vehicle Accident Response", "Larceny", "Medical Assistance", "Investigate Person", "Other", "Drug Violation","Simple Assault","Vandalism","Verbal Disputes","Towed","Investigate Property","Larceny From Motor Vehicle","Property Lost","Warrant Arrests","Aggravated Assault")

#Box plot Daily Frequency per Offense group
BC_Month_OGroup %>%
  filter(OffenseGroup %in% offenses) %>%
  ggplot(aes(x=OffenseGroup, y=Frequency, fill = OffenseGroup)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  guides(fill=FALSE)

#####         Correlation Plot         #####

#Create correlation plot NOTE: perform this task before adding categorical variables to the dataframe
library(corrplot) #Load library corrplot to plot correlation plot
bostoncorr <- boston
bostoncorr$Events <- NULL
bostoncorr$Date <- NULL

myvar <- c("Frequency", "Avg_Temp", "Avg_Dew_Point", "Avg_Humidity", "Avg_Sea_Level_Press", "Avg_Visibility", "Avg_Wind", "Snowfall", "Precip")
bostoncorr <- bostoncorr[myvar]
bostoncorr <- na.omit(bostoncorr)
corr <- cor(bostoncorr) #Create correlation matrix
corrplot(corr, method = "number", order = "hclust", tl.col = "black", tl.srt = 45) #Generate a numerical correlation plot  

#Pairs plot for 
pairs(bostoncorr, lower.panel = NULL)

#####         Boxplot Days of Week        #####
boston$DayofWeek <- factor(boston$DayofWeek, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
boston2$DayofWeek <- factor(boston2$DayofWeek, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
boston3$DayofWeek <- factor(boston3$DayofWeek, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
boston4$DayofWeek <- factor(boston4$DayofWeek, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

offenses1 <- list("Motor Vehicle Accident Response", "Larceny", "Medical Assistance", "Investigate Person", "Other")
offenses2 <- list("Drug Violation","Simple Assault","Vandalism","Verbal Disputes","Towed")
offenses3 <- list("Investigate Property","Larceny From Motor Vehicle","Property Lost","Warrant Arrests","Aggravated Assault")



# Boxplots of crimes per day vs day of week faceted by the top, second and third 5-most frequent crime groups
# Change the offense list (offenses1, offenses2, offenses3) from filter to get the different combinations of graphs
# NB: The change in the offense list applies to ALL the following graphs
boston4 %>%
  filter(offense_group %in% offenses1)%>%
  ggplot(aes(x = DayofWeek, y = Frequency, fill = DayofWeek))+
  geom_boxplot() +
  xlab("Day of Week")+
  ylab("Crimes Per Day")+
  ggtitle("Frequency to Day of Week Boxplots: Third 5 Most Frequent Crimes Groups")+
  ylim(0,40)+
  facet_wrap(~offense_group)+ 
  theme_bw()+
  theme(legend.position = c(0.85, 0.1))+
  theme(legend.background = element_rect(color="black", size=0.4, linetype="solid"))+
  theme(axis.text.x = element_text(angle = 90))

# Crimes per day to averate temperature faceted by offenses from offense list
boston4 %>%
  filter(offense_group %in% offenses2)%>%
  ggplot(aes(x = Avg_Temp, y = Frequency))+
  geom_point(alpha = 0.4, size = 1)+
  geom_smooth(method = "lm") +
  xlab("Temperature")+
  ylab("Crimes Per Day")+
  ggtitle("Crimes Per Day to Average Temperature OLS Fitted: Third 5 Most Frequent Crimes Groups")+
  facet_wrap(~offense_group)+ 
  theme_bw()

# Boxplot of overall freuqnecy of crimes per day to day of the week
boston %>%
  ggplot(aes(x = DayofWeek, y = Frequency, fill = DayofWeek))+
  geom_boxplot()+
  xlab("Day of Week")+
  ylab("Crimes Per Day")+
  ggtitle("Frequency of Crimes per Day to Day of Week")+
  theme_bw()

# PER MONTH: (with linear regression line) crimes per month to average temperatuve divided by Crime Group (team created categorical value)
# Human: Sexual offenses, prostitution, etc. The violent and burglary group are self explanatory
bostonmonth %>%
  filter(CrimeGroups != "0") %>%
  ggplot(aes(x = Avg_Temp, y = Frequency))+
  geom_point(alpha = 0.5, color = "gray")+
  geom_smooth(method = "lm") +
  xlab("Average Temperature (F)")+
  ylab("Crimes Per Month")+
  ylim(0,200)+
  ggtitle("Crimes Per Month to Average Temperature: Per Crime Group")+
  facet_wrap(~CrimeGroups)+ 
  theme_bw()

#####         PER MONTH: Creation of histogram of the frequency of daily crimes divided by Top15 offense group
g1 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Aggravated Assault"))
g2 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Drug Violation"))
g3 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Investigate Person"))
g4 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Investigate Property"))
g5 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Larceny"))
g6 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Larceny From Motor Vehicle"))
g7 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Medical Assistance"))
g8 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Motor Vehicle Accident Response"))
g9 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Other"))
g10 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Property Lost"))
g11 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Simple Assault"))
g12 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Towed"))
g13 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Vandalism"))
g14 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Verbal Disputes"))
g15 <- mean(subset(bostonmonth$Frequency, bostonmonth$offense_group == "Warrant Arrests"))


bostonmonth %>%
  filter(offense_group %in% offenses) %>%
  ggplot(aes(x = Frequency, fill = offense_group))+
  geom_histogram()+
  scale_fill_manual(values = c("coral","chartreuse4","cadetblue4","burlywood4","brown4","blue3","blueviolet", "gray35","deeppink","darksalmon","olivedrab","red","steelblue","thistle4","turquoise1"))+
  geom_vline(xintercept = c(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15), color = c("coral","chartreuse4","cadetblue4","burlywood4","brown4","blue3","blueviolet", "gray35","deeppink","darksalmon","olivedrab","red","steelblue","thistle4","turquoise1"), linetype = "dashed", size = 0.9, alpha = 0.7)

###-3-###         Time-Series: Exploratory Data Analysis         ########

#Calculating the average difference between boston frequency values and boston average temperature values
bostontimetrial <- data.frame(boston$Date, boston$Frequency, boston$Avg_Temp)
checkdiff <- mean(boston$Frequency, na.rm = TRUE)- mean(boston$Avg_Temp, na.rm = TRUE)
checkdiff

# Plot Total Daily Crimes over time (black) and a 30-day moving average (blue)
ggplot(boston) +
  geom_line(mapping = aes(x=boston$Date, y=boston$Frequency))+
  geom_line(aes(x = boston$Date, y = rollmean(boston$Frequency, 30, na.pad=TRUE)), color = "darkblue", size = 1)+
  theme_bw()+
  xlab("Date")+
  ylab("Total Daily Crimes")+
  coord_cartesian(ylim = c(200,350))

# Same as previous plot but with the average temperature (red) and the 30-day moving average (yellow) on the second y-axis
ggplot(boston) +
  geom_line(mapping = aes(x=boston$Date, y=boston$Frequency))+
  geom_line(mapping = aes(x = boston$Date, y = (217+boston$Avg_Temp)), color = "red", size = 0.9)+
  geom_line(aes(x = boston$Date, y = rollmean(boston$Frequency, 30, na.pad=TRUE)), color = "darkblue", size = 1)+
  geom_line(aes(x = boston$Date, y = rollmean(217+boston$Avg_Temp,30 , na.pad=TRUE)), color = "yellow", size = 0.9) +
  theme_bw()+
  xlab("Date")+
  ylab("Total Daily Crimes")+
  scale_y_continuous(sec.axis = sec_axis(~.-217, name = "Average Temperature (F)", breaks = c(0,20,40,60,80,100)))+
  coord_cartesian(ylim = c(200,350))

# Same as above but smoothed
ggplot(boston) +
  geom_smooth(mapping = aes(x=boston$Date, y=boston$Frequency))+
  geom_smooth(mapping = aes(x = boston$Date, y = (217+boston$Avg_Temp)), color = "red", size = 0.9)+
  geom_smooth(aes(x = boston$Date, y = rollmean(boston$Frequency, 30, na.pad=TRUE)), color = "darkblue", size = 0.9)+
  geom_smooth(aes(x = boston$Date, y = rollmean(217+boston$Avg_Temp, 30, na.pad=TRUE)), color = "yellow", size = 0.9) +
  theme_bw()+
  xlab("Date")+
  ylab("Total Daily Crimes")+
  scale_y_continuous(sec.axis = sec_axis(~.-217, name = "Average Temperature (F)", breaks = c(0,20,40,60,80,100)))+
  coord_cartesian(ylim = c(200,350))

#####         Checking overall trend and seasonality of Total Daily Crimes per Every Weather Item         #####
#Saving the last plots to multiplot
p1 <-
  ggplot(boston) +
  geom_line(mapping = aes(x=boston$Date, y=boston$Frequency))+
  geom_line(mapping = aes(x = boston$Date, y = (217+boston$Avg_Temp)), color = "red", size = 0.7)+
  geom_line(aes(x = boston$Date, y = rollmean(boston$Frequency, 30, na.pad=TRUE)), color = "darkblue", size = 1)+
  geom_line(aes(x = boston$Date, y = rollmean(217+boston$Avg_Temp,30 , na.pad=TRUE)), color = "yellow", size = 1) +
  theme_bw()+
  xlab("Date")+
  ylab("Total Daily Crimes")+
  ggtitle("Daily Crimes & Average Temperature over Time")+
  scale_y_continuous(sec.axis = sec_axis(~.-217, name = "Average Temperature (F)", breaks = c(0,20,40,60,80,100)))+
  coord_cartesian(ylim = c(200,350))

#And checking the differences to adjust second y-axis
checkdiff <- mean(boston$Frequency, na.rm = TRUE)- mean(boston$Avg_Dew_Point, na.rm = TRUE)
checkdiff

p2 <-
  ggplot(boston) +
  geom_line(mapping = aes(x=boston$Date, y=boston$Frequency))+
  geom_line(mapping = aes(x = boston$Date, y = (229+boston$Avg_Dew_Point)), color = "red", size = 0.7)+
  geom_line(aes(x = boston$Date, y = rollmean(boston$Frequency, 30, na.pad=TRUE)), color = "darkblue", size = 1)+
  geom_line(aes(x = boston$Date, y = rollmean(229+boston$Avg_Dew_Point,30 , na.pad=TRUE)), color = "yellow", size = 1) +
  theme_bw()+
  xlab("Date")+
  ylab("Total Daily Crimes")+
  ggtitle("Daily Crimes & Average Dew Point over Time")+
  scale_y_continuous(sec.axis = sec_axis(~.-217, name = "Average Dew Point (F)", breaks = c(0,20,40,60,80,100)))+
  coord_cartesian(ylim = c(200,350))


checkdiff <- mean(boston$Frequency, na.rm = TRUE)- mean(boston$Avg_Humidity, na.rm = TRUE)
checkdiff

p3 <-
  ggplot(boston) +
  geom_line(mapping = aes(x=boston$Date, y=boston$Frequency))+
  geom_line(mapping = aes(x = boston$Date, y = (203+boston$Avg_Humidity)), color = "red", size = 0.7)+
  geom_line(aes(x = boston$Date, y = rollmean(boston$Frequency, 30, na.pad=TRUE)), color = "darkblue", size = 1)+
  geom_line(aes(x = boston$Date, y = rollmean(203+boston$Avg_Humidity, 30, na.pad=TRUE)), color = "yellow", size = 1) +
  theme_bw()+
  xlab("Date")+
  ylab("Total Daily Crimes")+
  ggtitle("Daily Crimes & Average Humidity over Time")+
  scale_y_continuous(sec.axis = sec_axis(~.-217, name = "Average Humidity (%)", breaks = c(0,20,40,60,80,100)))+
  coord_cartesian(ylim = c(200,350))


checkdiff <- mean(boston$Frequency, na.rm = TRUE)- mean(boston$High_Wind_Gust, na.rm = TRUE)
checkdiff

p4 <-
  ggplot(boston) +
  geom_line(mapping = aes(x=boston$Date, y=boston$Frequency))+
  geom_line(mapping = aes(x = boston$Date, y = (242+boston$High_Wind_Gust)), color = "red", size = 0.7)+
  geom_line(aes(x = boston$Date, y = rollmean(boston$Frequency, 30, na.pad=TRUE)), color = "darkblue", size = 1)+
  geom_line(aes(x = boston$Date, y = rollmean(242+boston$High_Wind_Gust, 30, na.pad=TRUE)), color = "yellow", size = 1) +
  theme_bw()+
  xlab("Date")+
  ylab("Total Daily Crimes")+
  ggtitle("Daily Crimes & High Wind Gust over Time")+
  scale_y_continuous(sec.axis = sec_axis(~.-217, name = "High Wind Gust (mph)", breaks = c(0,20,40,60,80,100)))+
  coord_cartesian(ylim = c(200,350))

# NB: The following multiplot function is from an outside source cited in the report
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Using the multiplot function (you can use it with as many plots and column combinations as you need with GGPLOT)
multiplot(p1,p2,p3,p4, cols=1)

###-4-###         Modeling: Linear Regression         ########
forreg <- data.frame(boston$Frequency, 
                     boston$Avg_Temp, 
                     boston$Avg_Dew_Point, 
                     boston$Avg_Humidity, 
                     boston$Avg_Sea_Level_Press, 
                     boston$Avg_Visibility, 
                     boston$Avg_Wind, 
                     boston$High_Wind_Gust, 
                     boston$Snowfall, 
                     boston$Precip, 
                     as.factor(boston$Events), 
                     as.factor(boston$DayofWeek))
names(forreg)
names(forreg) = c("Frequency","Avg_Temp","Avg_Dew_Point","Avg_Humidity","Avg_Sea_Level_Press","Avg_Visibility","Avg_Wind","High_Wind_Gust","Snowfall","Precip","Events","DoW")

#####         Intuitive Model         #####
# Intuition Tells Us that Avg_Temp would have an effect on crimes, and also day of the week and event
lmintuit <- lm(Frequency ~ Avg_Temp + DoW + Events + Avg_Visibility*Events, data = forreg)
summary(lmintuit)
AIC(lmintuit)

par(mfrow=c(2,2))
plot(lmintuit)
par(mfrow=c(1,1))
#####         Backwards Selection Model         #####
lmALL <- lm(Frequency~., data = forreg)
summary(lmALL)

lmback <- step(lmALL, direction = 'backward')
summary(lmback)
AIC(lmback)

#####         Forward Selection Model         #####
lmNONE <- lm(Frequency~1, data = forreg)
summary(lmNONE)

lmfor <- step(lmNONE, direction = 'forward', scope = formula('lmALL'))
summary(lmfor)
AIC(lmfor)

#####         Stepwise Selection Model         #####
lmboth <- step(lmNONE, direction = 'both', scope = formula('lmALL'))
summary(lmboth)
AIC(lmboth)


###-5-###         Timeseries defining and ARIMA and Exponential smoothing forecasting         #####
myts <- ts(boston$Frequency, start=c(2015, 06), end=c(2018, 04), frequency=365)
plot(myts)
lines(rollmeanweek, col = "red")
par(new = T)
lines(RMAvgtemp, col = "green")
ggplot(boston, aes(x = Date, y = Frequency))+
  geom_line()+
  geom_line()

#Decompose of myts: observed vs trend vs seasonal vs random
components_ts <- decompose(myts)
plot(components_ts)

library("fUnitRoots")
urkpssTest(myts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary <- diff(myts, differences=1)
plot(tsstationary)
detach("package:fUnitRoots", unload=TRUE)

library(forecast)
#ACF plot show there's a 7 day seasonality.
# THis shows there's a correlation between points seperated by 7 days (by 7 time-lags)
acf(boston$Frequency)
# We fit an auto.arima model
mod1 <- auto.arima(boston$Frequency)
summary(mod1)

#Forecasting 150 days with the model. ARIMA is not our best option here since its not
# shape doesn't follow the trend (because the autoregression explained is for the 7 day seaosnality and not the yearly seasonality)
# even though the mean absolute percentage error is 9.12%
for1 <- forecast(mod1,150)
plot(for1)



#####         Exponential Smoothing Using Holt Winters          #####
#Holt Winters model composition from time series created
hwmodel <- HoltWinters(myts) #creates Holt Winters exponential smoothing model
forecast1 <- forecast(hwmodel, 365)
plot(forecast1)
accuracy(forecast1)

###-6-###         Mapping         ########
# NB: All are functions that need to be used as defined
## NB: NEED OWN GOOGLE API registration key
library(ggmap)

#Registration to Google's API. If you want to do this, you'll have to get your own google_API key online, which is simple enough and free as long as you unsusbcribe within a year
register_google("your_google_API_key", "standard")

#Save source information on the map we are looking for
mapcenter <- geocode('Dorchester, MA')
median(bostoncrime$Lat, na.rm = TRUE)
median(bostoncrime$Long, na.rm = TRUE)

#Create a dataframe containing only the variables wanted
LatLongOG <- data.frame(bostoncrime$Lat, bostoncrime$Long, bostoncrime$offense_group, bostoncrime$CrimeGroups, bostoncrime$Year, bostoncrime$Month, bostoncrime$Severity, bostoncrime$DayofWeek)
names(LatLongOG) <- c("y","x","OG","CrimeGroup","Year", "Month", "Severity", "DoW")

#Generate map and zoom at specific level
Bos_map <- qmap(c(lon=mapcenter$lon, lat=mapcenter$lat), zoom=12)

## Ofense map PER OFFENSE, PER YEAR, PER MONTH
mapoffenseyearmonth <- function(offense, year, month){
  tempdf <- subset(LatLongOG, OG == offense & Year == year & Month == month)
  g <- Bos_map + geom_point(aes(x=x, y=y), data = tempdf, size=3, alpha=0.2, color="red") + 
    ggtitle("Offense Density Map Boston")
  g
}

## Ofense map PER OFFENSE, PER YEAR
mapoffenseyear <- function(offense, year){
  tempdf <- subset(LatLongOG, OG == offense & Year == year)
  g <- Bos_map + geom_point(aes(x=x, y=y), data = tempdf, size=2, alpha=0.1, color="red") + 
    ggtitle("Offense Density Map Boston")
  g
}

## Ofense map PER OFFENSE, ALL YEARS
mapoffense <- function(offense){
  tempdf <- subset(LatLongOG, OG == offense)
  g <- Bos_map + geom_point(aes(x=x, y=y), data = tempdf, size=3, alpha=0.2, color="red") + 
    ggtitle("Offense Density Map Boston")
  g
}

## Ofense map PER YEAR, PER MONTH, and TOP OFFENSES
mapyearmonthtop <- function(year, month){
  tempdf <- subset(LatLongOG, Year == year & Month == month & OG == c("Motor Vehicle Accident Response", "Larceny", "Medical Assistance", "Investigate Person", "Other", "Drug Violation","Simple Assault","Vandalism","Verbal Disputes","Towed","Investigate Property","Larceny From Motor Vehicle","Property Lost","Warrant Arrests","Aggravated Assault"))
  g <- Bos_map + geom_point(aes(x=x, y=y, color = OG), data = tempdf, size=4, alpha=0.3) + 
    ggtitle("Offense Density Map Boston")
  g
}

## Ofense map PER YEAR, and TOP OFFENSES
mapyeartop <- function(year){
  tempdf <- subset(LatLongOG, Year == year & OG == offenses)
  g <- Bos_map + geom_point(aes(x=x, y=y, color = OG), data = tempdf, size=2, alpha=0.2) + 
    ggtitle("Offense Density Map Boston")
  g
}

## Crime Group
mapcrimegroup <- function(CrimeGroupSing, sev, DayWeek){
  tempdf <- subset(LatLongOG, CrimeGroup == CrimeGroupSing & Severity == sev & DoW == DayWeek)
  g <- Bos_map + geom_point(aes(x=x, y=y, color = OG), data = tempdf, size=4, alpha=0.5, ) + 
    ggtitle(DayWeek)
  g
}
