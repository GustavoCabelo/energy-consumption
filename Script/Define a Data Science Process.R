#Library####
pacman::p_load(tidyr,
               magrittr,
               dplyr,
               readr,
               lattice,
               ggplot2,
               lubridate,
               chron,
               # forecast,
               # tslm,
               stats,
               ggrepel,
               gridExtra,
               viridis,
               ggExtra)

#Loading file####

household_power_consumption <- read_delim("D:/Profissional/Ubiqum/Data Analytics - Understanding customers/Programa 2017.02/Course 3 - Deep Analytics and Visualization 2017/Task 1 - Define a Data Science Process/household_power_consumption.txt", 
                                          ";", 
                                          escape_double = FALSE, 
                                          col_types = cols(Global_active_power = col_number(),
                                                           Global_intensity = col_number(),
                                                           Global_reactive_power = col_number(), 
                                                           Sub_metering_1 = col_number(), 
                                                           Sub_metering_2 = col_number(),
                                                           Sub_metering_3 = col_number(), Voltage = col_number()), 
                                          trim_ws = TRUE)
View(household_power_consumption)

#Pre-process####
{

#Join Date and time columns####
  household_power_consumption$DateTime <- paste(household_power_consumption$Date, sep = ":", household_power_consumption$Time)

#Change Column Name####  
colnames(household_power_consumption)[7:10] <- c("Kitchen", "Laundry", "Water_AC", "DateTime")
 
#Reorder columns####
household_power_consumption <- household_power_consumption[,
                                                           c(ncol(household_power_consumption), 
                                                             1:(ncol(household_power_consumption)-1))] 

#adjust attribute 'Date' type as date####
{household_power_consumption$Date <- as.Date(household_power_consumption$Date, 
                                             "%d/%m/%Y",
                                             tz = "")}

#Transform DateTime as POSIXct####
household_power_consumption$DateTime <- as.POSIXct(household_power_consumption$DateTime,
                                "%d/%m/%Y:%H:%M:%S",
                                tz="")
  which(is.na(household_power_consumption$DateTime))

#Add Columns for Year, Month, Day####
household_power_consumption$Year <- format(household_power_consumption$Date, "%Y")
household_power_consumption$Month <- format(household_power_consumption$Date, "%m")
household_power_consumption$Day <- format(household_power_consumption$Date, "%d")
household_power_consumption$wday <- wday(household_power_consumption$Date, week_start = 1)
household_power_consumption$Quarter <- household_power_consumption$DateTime %>%  quarter(with_year = FALSE)

#Adjust  Time just with hours####
household_power_consumption$Hour <- as.POSIXlt(household_power_consumption$Time)$hour
#Adjust all Energy measurement to Kwh####
{#Global_active_power adjusted from (KWm to KWh)
household_power_consumption$Global_active_power = household_power_consumption$Global_active_power/60
household_power_consumption$Global_reactive_power = household_power_consumption$Global_reactive_power/60
#Submentering adjusted from (Wh to KWh)
household_power_consumption$Kitchen = household_power_consumption$Kitchen/1000 
household_power_consumption$Laundry = household_power_consumption$Laundry/1000 
household_power_consumption$Water_AC = household_power_consumption$Water_AC/1000 
}
#Sum of all Submeters####
household_power_consumption$ALL_Sub_meterings <- household_power_consumption$Kitchen + household_power_consumption$Laundry + household_power_consumption$Water_AC
#Not measured parts of the house####
household_power_consumption$NotMeasured = household_power_consumption$Global_active_power - household_power_consumption$ALL_Sub_meterings

#Relative NotMeasured x Global Active####
household_power_consumption$NotMeasured_Relative <- household_power_consumption$NotMeasured/household_power_consumption$Global_active_power 
#Round all numeric attributes by 4####
is.num <- sapply(household_power_consumption, is.numeric) 
household_power_consumption[is.num] %<>% round(4)
rm(is.num)

#Save pre-processing in .txt
write_delim(household_power_consumption,
            "D:/Profissional/Ubiqum/Data Analytics - Understanding customers/Programa 2017.02/Course 3 - Deep Analytics and Visualization 2017/Task 1 - Define a Data Science Process/household_power_consumption2.txt", 
            ";", 
            na = "NA",
            append = FALSE,
            col_names = TRUE)
}
#HEATMAP Consumption####
{    
  #General Subset####
  {
    heatmap <- household_power_consumption  %>% 
      filter(Year>="2007") %>%
      group_by(Month, Day, Hour)  %>%
      summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC"), mean, na.rm = TRUE)
    is.num <- sapply(heatmap, is.numeric) #Function to get all numeric attributes from a DF
    heatmap[is.num] %<>% round(4)
    rm(is.num)
    
    #Adjust attributes types
    heatmap$Month %<>% as.factor() 
    levels(heatmap$Month) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dic")
    
    heatmap$Day %<>% as.integer()
    heatmap$Hour %<>% as.integer()
    
    #Gather Zones x Energy  
    Gathered_Energy_Zone <- gather(heatmap,"Zones","EnerGY",4:7)
    Gathered_Energy_Zone$Zones %<>% as.factor() 
    levels(Gathered_Energy_Zone$Zones) = c("Global", "Kitchen", "Laundry", "Water_AC")
  }
  
  #Gathered only Global#### 
  {Gathered_Energy_Global <- Gathered_Energy_Zone %>%
    filter(Zones=="Global")
  
  
  statno <-unique(Gathered_Energy_Global$Zones)
  
  
  ######## Plotting starts here#####################
  HMG <-ggplot(Gathered_Energy_Global,aes(Day,Hour,fill=EnerGY))+
    geom_tile(color= "white",size=0.1) + 
    scale_fill_viridis(name="Energy Consumption KWh",option ="D") +
    facet_grid(.~Month) +
    scale_y_continuous(trans = "reverse", breaks = unique(Gathered_Energy_Global$Hour)) +
    scale_x_continuous(breaks =c(1,10,20,31)) +
    theme_minimal(base_size = 8) +
    labs(title= paste("Hourly Consumption - ",statno), x="Day", y="Hour Commencing") +
    theme(legend.position = "bottom")+
    theme(plot.title=element_text(size = 14))+
    theme(axis.text.y=element_text(size=6)) +
    theme(strip.background = element_rect(colour="white"))+
    theme(plot.title=element_text(hjust=0))+
    theme(axis.ticks=element_blank())+
    theme(axis.text=element_text(size=7))+
    theme(legend.title=element_text(size=8))+
    theme(legend.text=element_text(size=6))+
    removeGrid()#ggExtra
  
  # you will want to expand your plot screen before this bit!
  HMG #awesomeness
  }
  #Gathered only Kitchen#### 
  {Gathered_Energy_Kitchen <- Gathered_Energy_Zone %>%
    filter(Zones=="Kitchen")
  
  
  statno <-unique(Gathered_Energy_Kitchen$Zones)
  
  
  ######## Plotting starts here#####################
  HMK <-ggplot(Gathered_Energy_Kitchen,aes(Day,Hour,fill=EnerGY))+
    geom_tile(color= "white",size=0.1) + 
    scale_fill_viridis(name="Energy Consumption KWh",option ="D") +
    facet_grid(.~Month) +
    scale_y_continuous(trans = "reverse", breaks = unique(Gathered_Energy_Kitchen$Hour)) +
    scale_x_continuous(breaks =c(1,10,20,31)) +
    theme_minimal(base_size = 8) +
    labs(title= paste("Hourly Consumption - ",statno), x="Day", y="Hour Commencing") +
    theme(legend.position = "bottom")+
    theme(plot.title=element_text(size = 14))+
    theme(axis.text.y=element_text(size=6)) +
    theme(strip.background = element_rect(colour="white"))+
    theme(plot.title=element_text(hjust=0))+
    theme(axis.ticks=element_blank())+
    theme(axis.text=element_text(size=7))+
    theme(legend.title=element_text(size=8))+
    theme(legend.text=element_text(size=6))+
    removeGrid()#ggExtra
  
  # you will want to expand your plot screen before this bit!
  HMK #awesomeness
  }
  #Gathered only Laundry####   
  {Gathered_Energy_Laundry <- Gathered_Energy_Zone %>%
    filter(Zones=="Laundry")
  
  
  statno <-unique(Gathered_Energy_Laundry$Zones)
  
  
  ######## Plotting starts here#####################
  HML <-ggplot(Gathered_Energy_Laundry,aes(Day,Hour,fill=EnerGY))+
    geom_tile(color= "white",size=0.1) + 
    scale_fill_viridis(name="Energy Consumption KWh",option ="D") +
    facet_grid(.~Month) +
    scale_y_continuous(trans = "reverse", breaks = unique(Gathered_Energy_Laundry$Hour)) +
    scale_x_continuous(breaks =c(1,10,20,31)) +
    theme_minimal(base_size = 8) +
    labs(title= paste("Hourly Consumption - ",statno), x="Day", y="Hour Commencing") +
    theme(legend.position = "bottom")+
    theme(plot.title=element_text(size = 14))+
    theme(axis.text.y=element_text(size=6)) +
    theme(strip.background = element_rect(colour="white"))+
    theme(plot.title=element_text(hjust=0))+
    theme(axis.ticks=element_blank())+
    theme(axis.text=element_text(size=7))+
    theme(legend.title=element_text(size=8))+
    theme(legend.text=element_text(size=6))+
    removeGrid()#ggExtra
  
  # you will want to expand your plot screen before this bit!
  HML #awesomeness
  }
  #Gathered only Water_AC#### 
  {Gathered_Energy_Water_AC <- Gathered_Energy_Zone %>%
    filter(Zones=="Water_AC")
  
  
  statno <-unique(Gathered_Energy_Water_AC$Zones)
  
  
  ######## Plotting starts here#####################
  HMW <-ggplot(Gathered_Energy_Water_AC,aes(Day,Hour,fill=EnerGY))+
    geom_tile(color= "white",size=0.1) + 
    scale_fill_viridis(name="Energy Consumption KWh",option ="D") +
    facet_grid(.~Month) +
    scale_y_continuous(trans = "reverse", breaks = unique(Gathered_Energy_Water_AC$Hour)) +
    scale_x_continuous(breaks =c(1,10,20,31)) +
    theme_minimal(base_size = 8) +
    labs(title= paste("Hourly Consumption - ",statno), x="Day", y="Hour Commencing") +
    theme(legend.position = "bottom")+
    theme(plot.title=element_text(size = 14))+
    theme(axis.text.y=element_text(size=6)) +
    theme(strip.background = element_rect(colour="white"))+
    theme(plot.title=element_text(hjust=0))+
    theme(axis.ticks=element_blank())+
    theme(axis.text=element_text(size=7))+
    theme(legend.title=element_text(size=8))+
    theme(legend.text=element_text(size=6))+
    removeGrid()#ggExtra
  
  # you will want to expand your plot screen before this bit!
  HMW #awesomeness
  }
}
#NAs####
{
#Create subset for NAs####
NAs <- household_power_consumption[(which(is.na(household_power_consumption$Water_AC))),]
#Where are my NAs?####
{
NA_by_Year <- NAs %>% group_by(Year) %>%
  summarise(Year_NA = n())

NA_by_Month <- household_power_consumption %>% 
  group_by(Year,Month)  %>%
  summarise(Month_n = sum(is.na(Global_active_power)))

NA_by_Day <- NAs %>% group_by(Date) %>%
  summarise(Day_n = n())

NA_by_WDay <- NAs %>% group_by(Month, wday) %>%
  summarise(Day_n = n())
}
#NAs Plot by month by Year####
ggplot(NA_by_Month, aes(y=Month_n, x=Year, color=Year, fill=Year)) + 
  geom_bar( stat="identity") +    
  facet_wrap(~Month)


#Drops NAs (NAs are around 1% and are all in the same observations for all attrbiutes)####
household_power_consumption <-  na.omit(household_power_consumption) 



}

#SubSets####
{
  #Subset per Hourly information####
  {
    Hourly <- household_power_consumption
    
    Hourly$Date <- as.character(Hourly$Date)
    Hourly$Time <- as.character(Hourly$Time)
    
    Hourly <- Hourly %>%
      group_by(Date, Year, Month, Day, Hour, wday) %>%
      summarise(Global_active_power = mean(Global_active_power),
                Global_reactive_power = mean(Global_reactive_power),
                Kitchen = mean(Kitchen),
                Laundry = mean(Laundry),
                Water_AC = mean(Water_AC),
                NotMeasured = mean(NotMeasured))
    Hourly$DateTime <- paste(Hourly$Date, sep = "-", Hourly$Hour)
    is.num <- sapply(Hourly, is.numeric) 
    Hourly[is.num] %<>% round(4)
    rm(is.num)
    
  }
  #Subset per 15 minutes information####
  {
    Q_Hour <- household_power_consumption[-c(1:6),] #Remove first 6 rows to start the time counting on 30min
    Q_Hour2 <-  cut(household_power_consumption[7:nrow(household_power_consumption),]$DateTime, "15 mins") 
    Q_Hour$Q_Hour <- Q_Hour2 
    
    Q_Hour %<>% group_by(Q_Hour)  %>%
      summarise_at(c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Kitchen", "Laundry", "Water_AC", "ALL_Sub_meterings", "NotMeasured"),mean, na.rm = TRUE)
    rm(Q_Hour2)
    Q_Hour$Q_Hour %<>% as.POSIXct("%Y-%m-%d %H:%M:%S", tz = "") #From Factor to Date POSIXct
    is.num <- sapply(Q_Hour, is.numeric) 
    Q_Hour[is.num] %<>% round(4)
    rm(is.num)

    }
  
  
  #Subset a tipical day per Q YEAR####
  season <- household_power_consumption  %>% 
    group_by(Year, Quarter, Hour)  %>%
    summarise_at(c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Kitchen", "Laundry", "Water_AC", "ALL_Sub_meterings", "NotMeasured"), mean, na.rm = TRUE)
  season$Quarter %<>% as.factor() 
  levels(season$Quarter) = c("Winter", "Spring", "Summer", "Fall")
  is.num <- sapply(season, is.numeric) #Function to get all numeric attributes from a DF
  season[is.num] %<>% round(4)
  rm(is.num)
  season <- season[-c(1:24),] 
  #Season p/ year
  {
  #2007 Season####
  season_2007 <- season %>%
    filter(Year =="2007")
  #2008 Season####
  season_2008 <- season %>%
    filter(Year =="2008")
  #2009 Season####
  season_2009 <- season %>%
    filter(Year =="2009")
  #2009 Season####
  season_2010 <- season %>%
    filter(Year =="2010")
  }

  #Subset p/week####
  
powerweek <- subset(household_power_consumption, subset = (DateTime >= "2008-03-03 00:00:00" & DateTime <= "2008-03-09 23:59:00"))
  
  #Subset for 2 days in Feb 2008####
  powerdays <- subset(household_power_consumption, subset = (Date >= "2008-02-01" & Date <= "2008-02-02"))
  #Subset p/ year####
  {
    #Subset 2007####
    Year2007 <- household_power_consumption %>%
      filter(Year =="2007") #Filter Years in Subset

    #Subset 2008####
    Year2008 <- household_power_consumption %>%
      filter(Year =="2008") #Filter Years in Subset
    
    #Subset 2009####
    Year2009 <- household_power_consumption %>%
      filter(Year =="2009") #Filter Years in Subset
   
    #Subset 2010####
    ts_MxY <- household_power_consumption %>%
      filter(Year >="2009")  
    ts_MxY <- ts_MxY[470684:nrow(ts_MxY),] #Filter Years in Subset
    ts_MxY <- mutate(ts_MxY, Year = paste("2010"))

  }
  #Weekends p/ month (Fri - Sun)####
  #2007
  {Weekends_2007 <- household_power_consumption  %>% 
    filter(Year=="2007", wday>= "5" & wday<= "7")  %>%
    group_by(Year, Month, wday, Hour) %>% 
    summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
  is.num <- sapply(Weekends, is.numeric) #Function to get all numeric attributes from a DF
  Weekends[is.num] %<>% round(4)
  rm(is.num)
  #Global####
  ggplot(Weekends) + 
    geom_line(aes(x = Hour, y = Global_active_power, color = "Global")) + 
    labs(title= "Weekend 2007 - Global") +
    xlab("Hour") + ylab("Global_active_power") +
    facet_grid(Month~wday) +
    scale_x_continuous(breaks =c(0,6,12,18)) +
    theme(legend.position = "bottom")
  
  #Kitchen####
  ggplot(Weekends) + 
    geom_line(aes(x = Hour, y = Kitchen, color = "green")) +
    labs(title= "Weekend 2007 - Kitchen") +
    xlab("Hour") + ylab("Kitchen") +
    facet_grid(Month~wday) +
    scale_x_continuous(breaks =c(0,6,12,18)) +
    theme(legend.position = "bottom")
  
  #Laundry####
  ggplot(Weekends) + 
    geom_line(aes(x = Hour, y = Laundry, color = "Laundry")) +
    labs(title= "Weekend 2007 - Laundry") +
    xlab("Hour") + ylab("Laundry") +
    facet_grid(wday~Month) +
    scale_x_continuous(breaks =c(0,6,12,18)) +
    theme(legend.position = "bottom")
  
  #Water_AC####
  ggplot(Weekends) + 
    geom_line(aes(x = Hour, y = Kitchen, color = "Water_AC")) +
    labs(title= "Weekend 2007 - Water_AC") +
    xlab("Hour") + ylab("Water_AC") +
    facet_grid(wday~Month) +
    scale_x_continuous(breaks =c(0,6,12,18))+
    theme(legend.position = "bottom")
  
  
  }
  #2008
  {Weekends_2008 <- household_power_consumption  %>% 
      filter(Year=="2008", wday>= "5" & wday<= "7")  %>%
      group_by(Year, Month, wday, Hour) %>% 
      summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
    is.num <- sapply(Weekends, is.numeric) #Function to get all numeric attributes from a DF
    Weekends[is.num] %<>% round(4)
    rm(is.num)
    #Global####
    ggplot(Weekends) + 
      geom_line(aes(x = Hour, y = Global_active_power, color = "Global")) + 
      labs(title= "Weekend 2008 - Global") +
      xlab("Hour") + ylab("Global_active_power") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Kitchen####
    ggplot(Weekends) + 
      geom_line(aes(x = Hour, y = Kitchen, color = "green")) +
      labs(title= "Weekend 2008 - Kitchen") +
      xlab("Hour") + ylab("Kitchen") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Laundry####
    ggplot(Weekends) + 
      geom_line(aes(x = Hour, y = Laundry, color = "Laundry")) +
      labs(title= "Weekend 2008 - Laundry") +
      xlab("Hour") + ylab("Laundry") +
      facet_grid(wday~Month) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Water_AC####
    ggplot(Weekends) + 
      geom_line(aes(x = Hour, y = Kitchen, color = "Water_AC")) +
      labs(title= "Weekend 2008 - Water_AC") +
      xlab("Hour") + ylab("Water_AC") +
      facet_grid(wday~Month) +
      scale_x_continuous(breaks =c(0,6,12,18))+
      theme(legend.position = "bottom")
    
    
  }
  #2009
  {Weekends_2009 <- household_power_consumption  %>% 
      filter(Year=="2009", wday>= "5" & wday<= "7")  %>%
      group_by(Year, Month, wday, Hour) %>% 
      summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
    is.num <- sapply(Weekends, is.numeric) #Function to get all numeric attributes from a DF
    Weekends[is.num] %<>% round(4)
    rm(is.num)
    #Global####
    ggplot(Weekends) + 
      geom_line(aes(x = Hour, y = Global_active_power, color = "Global")) + 
      labs(title= "Weekend 2009 - Global") +
      xlab("Hour") + ylab("Global_active_power") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Kitchen####
    ggplot(Weekends) + 
      geom_line(aes(x = Hour, y = Kitchen, color = "green")) +
      labs(title= "Weekend 2009 - Kitchen") +
      xlab("Hour") + ylab("Kitchen") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Laundry####
    ggplot(Weekends) + 
      geom_line(aes(x = Hour, y = Laundry, color = "Laundry")) +
      labs(title= "Weekend 2009 - Laundry") +
      xlab("Hour") + ylab("Laundry") +
      facet_grid(wday~Month) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Water_AC####
    ggplot(Weekends) + 
      geom_line(aes(x = Hour, y = Kitchen, color = "Water_AC")) +
      labs(title= "Weekend 2009 - Water_AC") +
      xlab("Hour") + ylab("Water_AC") +
      facet_grid(wday~Month) +
      scale_x_continuous(breaks =c(0,6,12,18))+
      theme(legend.position = "bottom")
    
    
  }
  #2010
  {Weekends_2010 <- household_power_consumption  %>% 
      filter(Year=="2010", wday>= "5" & wday<= "7")  %>%
      group_by(Year, Month, wday, Hour) %>% 
      summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
    is.num <- sapply(Weekends, is.numeric) #Function to get all numeric attributes from a DF
    Weekends[is.num] %<>% round(4)
    rm(is.num)
    #Global####
    ggplot(Weekends) + 
      geom_line(aes(x = Hour, y = Global_active_power, color = "Global")) + 
      labs(title= "Weekend 2010 - Global") +
      xlab("Hour") + ylab("Global_active_power") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Kitchen####
    ggplot(Weekends) + 
      geom_line(aes(x = Hour, y = Kitchen, color = "green")) +
      labs(title= "Weekend 2010 - Kitchen") +
      xlab("Hour") + ylab("Kitchen") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Laundry####
    ggplot(Weekends) + 
      geom_line(aes(x = Hour, y = Laundry, color = "Laundry")) +
      labs(title= "Weekend 2010 - Laundry") +
      xlab("Hour") + ylab("Laundry") +
      facet_grid(wday~Month) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Water_AC####
    ggplot(Weekends) + 
      geom_line(aes(x = Hour, y = Kitchen, color = "Water_AC")) +
      labs(title= "Weekend 2010 - Water_AC") +
      xlab("Hour") + ylab("Water_AC") +
      facet_grid(wday~Month) +
      scale_x_continuous(breaks =c(0,6,12,18))+
      theme(legend.position = "bottom")
    
    
  }
  #Weekdays p/ month (Mon - Thu)####
  #2007
  {WeekDays_2007 <- household_power_consumption  %>% 
    filter(Year=="2007", wday>= "1" & wday<= "4")  %>%
    group_by(Year, Month, wday, Hour) %>% 
    summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
  is.num <- sapply(WeekDays_2007, is.numeric) #Function to get all numeric attributes from a DF
  WeekDays_2007[is.num] %<>% round(4)
  rm(is.num)
  #Global####
  ggplot(WeekDays_2007) + 
    geom_line(aes(x = Hour, y = Global_active_power, color = "Global")) + 
    labs(title= "WeekDays 2007 - Global") +
    xlab("Hour") + ylab("Global_active_power") +
    facet_grid(Month~wday) +
    scale_x_continuous(breaks =c(0,6,12,18)) +
    theme(legend.position = "bottom")
  
  #Kitchen####
  ggplot(WeekDays_2007) + 
    geom_line(aes(x = Hour, y = Kitchen, color = "green")) +
    labs(title= "WeekDays 2007 - Kitchen") +
    xlab("Hour") + ylab("Kitchen") +
    facet_grid(Month~wday) +
    scale_x_continuous(breaks =c(0,6,12,18)) +
    theme(legend.position = "bottom")
  
  #Laundry####
  ggplot(WeekDays_2007) + 
    geom_line(aes(x = Hour, y = Laundry, color = "Laundry")) +
    labs(title= "WeekDays 2007 - Laundry") +
    xlab("Hour") + ylab("Laundry") +
    facet_grid(Month~wday) +
    scale_x_continuous(breaks =c(0,6,12,18)) +
    theme(legend.position = "bottom")
  
  #Water_AC####
  ggplot(WeekDays_2007) + 
    geom_line(aes(x = Hour, y = Kitchen, color = "Water_AC")) +
    labs(title= "WeekDays 2007 - Water_AC") +
    xlab("Hour") + ylab("Water_AC") +
    facet_grid(Month~wday) +
    scale_x_continuous(breaks =c(0,6,12,18))+
    theme(legend.position = "bottom")
  
  
  }
  #2008
  {WeekDays_2008 <- household_power_consumption  %>% 
      filter(Year=="2008", wday>= "1" & wday<= "4")  %>%
      group_by(Year, Month, wday, Hour) %>% 
      summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
    is.num <- sapply(WeekDays_2008, is.numeric) #Function to get all numeric attributes from a DF
    WeekDays_2008[is.num] %<>% round(4)
    rm(is.num)
    #Global####
    ggplot(WeekDays_2008) + 
      geom_line(aes(x = Hour, y = Global_active_power, color = "Global")) + 
      labs(title= "WeekDays 2008 - Global") +
      xlab("Hour") + ylab("Global_active_power") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Kitchen####
    ggplot(WeekDays_2008) + 
      geom_line(aes(x = Hour, y = Kitchen, color = "green")) +
      labs(title= "WeekDays 2008 - Kitchen") +
      xlab("Hour") + ylab("Kitchen") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Laundry####
    ggplot(WeekDays_2008) + 
      geom_line(aes(x = Hour, y = Laundry, color = "Laundry")) +
      labs(title= "WeekDays 2008 - Laundry") +
      xlab("Hour") + ylab("Laundry") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Water_AC####
    ggplot(WeekDays_2008) + 
      geom_line(aes(x = Hour, y = Kitchen, color = "Water_AC")) +
      labs(title= "WeekDays 2008 - Water_AC") +
      xlab("Hour") + ylab("Water_AC") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18))+
      theme(legend.position = "bottom")
    
    
  }
  #2009
  {WeekDays_2009 <- household_power_consumption  %>% 
      filter(Year=="2009", wday>= "1" & wday<= "4")  %>%
      group_by(Year, Month, wday, Hour) %>% 
      summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
    is.num <- sapply(WeekDays_2009, is.numeric) #Function to get all numeric attributes from a DF
    WeekDays_2009[is.num] %<>% round(4)
    rm(is.num)
    #Global####
    ggplot(WeekDays_2009) + 
      geom_line(aes(x = Hour, y = Global_active_power, color = "Global")) + 
      labs(title= "WeekDays 2009 - Global") +
      xlab("Hour") + ylab("Global_active_power") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Kitchen####
    ggplot(WeekDays_2009) + 
      geom_line(aes(x = Hour, y = Kitchen, color = "green")) +
      labs(title= "WeekDays 2009 - Kitchen") +
      xlab("Hour") + ylab("Kitchen") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Laundry####
    ggplot(WeekDays_2009) + 
      geom_line(aes(x = Hour, y = Laundry, color = "Laundry")) +
      labs(title= "WeekDays 2009 - Laundry") +
      xlab("Hour") + ylab("Laundry") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18)) +
      theme(legend.position = "bottom")
    
    #Water_AC####
    ggplot(WeekDays_2009) + 
      geom_line(aes(x = Hour, y = Kitchen, color = "Water_AC")) +
      labs(title= "WeekDays 2009 - Water_AC") +
      xlab("Hour") + ylab("Water_AC") +
      facet_grid(Month~wday) +
      scale_x_continuous(breaks =c(0,6,12,18))+
      theme(legend.position = "bottom")
    
    
  }
  #2010
  {WeekDays_2010 <- household_power_consumption  %>% 
    filter(Year=="2010", wday>= "1" & wday<= "4")  %>%
    group_by(Year, Month, wday, Hour) %>% 
    summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
  is.num <- sapply(WeekDays_2010, is.numeric) #Function to get all numeric attributes from a DF
  WeekDays_2010[is.num] %<>% round(4)
  rm(is.num)
  #Global####
  ggplot(WeekDays_2010) + 
    geom_line(aes(x = Hour, y = Global_active_power, color = "Global")) + 
    labs(title= "WeekDays 2010 - Global") +
    xlab("Hour") + ylab("Global_active_power") +
    facet_grid(Month~wday) +
    scale_x_continuous(breaks =c(0,6,12,18)) +
    theme(legend.position = "bottom")
  
  #Kitchen####
  ggplot(WeekDays_2010) + 
    geom_line(aes(x = Hour, y = Kitchen, color = "green")) +
    labs(title= "WeekDays 2010 - Kitchen") +
    xlab("Hour") + ylab("Kitchen") +
    facet_grid(Month~wday) +
    scale_x_continuous(breaks =c(0,6,12,18)) +
    theme(legend.position = "bottom")
  
  #Laundry####
  ggplot(WeekDays_2010) + 
    geom_line(aes(x = Hour, y = Laundry, color = "Laundry")) +
    labs(title= "WeekDays 2010 - Laundry") +
    xlab("Hour") + ylab("Laundry") +
    facet_grid(Month~wday) +
    scale_x_continuous(breaks =c(0,6,12,18)) +
    theme(legend.position = "bottom")
  
  #Water_AC####
  ggplot(WeekDays_2010) + 
    geom_line(aes(x = Hour, y = Kitchen, color = "Water_AC")) +
    labs(title= "WeekDays 2010 - Water_AC") +
    xlab("Hour") + ylab("Water_AC") +
    facet_grid(Month~wday) +
    scale_x_continuous(breaks =c(0,6,12,18))+
    theme(legend.position = "bottom")
  
  
  }
  #Weekly consumption p/ month All Years####
  {Week_Month <- Hourly  %>% 
    filter(Year>="2007")  %>%
    group_by(Year, Month, wday, Hour) %>% 
    summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
  Week_Month$wday_H <- paste(formatC(Week_Month$wday, width = 2, flag = "0"), sep = ":", formatC(Week_Month$Hour, width = 2, flag = "0"))
  is.num <- sapply(Week_Month, is.numeric) #Function to get all numeric attributes from a DF
  Week_Month[is.num] %<>% round(4)
  rm(is.num)
  Week_Month$wday_H <-  as.numeric(as.factor(Week_Month$wday_H))
  
  #Global####
  ggplot(Week_Month) + 
    geom_line(aes(x = wday_H, y = Global_active_power, color=Year)) + 
    labs(title= "Weekly consumption - Global") +
    xlab("Weekly_Hour") + ylab("Global_active_power") +
    facet_grid(Year~Month) +
    theme(legend.position = "bottom")
  #Kitchen####
  ggplot(Week_Month) + 
    geom_line(aes(x = wday_H, y = Kitchen, color=Year)) + 
    labs(title= "Weekly consumption - Kitchen") +
    xlab("Weekly_Hour") + ylab("Kitchen") +
    facet_grid(Year~Month) +
    theme(legend.position = "bottom")
  #Laundry####
  ggplot(Week_Month) + 
    geom_line(aes(x = wday_H, y = Laundry, color=Year)) + 
    labs(title= "Weekly consumption - Laundry") +
    xlab("Weekly_Hour") + ylab("Laundry") +
    facet_grid(Year~Month) +
    theme(legend.position = "bottom")
  #Water_AC####
  ggplot(Week_Month) + 
    geom_line(aes(x = wday_H, y = Water_AC, color=Year)) + 
    labs(title= "Weekly consumption - Water_AC") +
    xlab("Weekly_Hour") + ylab("Water_AC") +
    facet_grid(Year~Month) +
    theme(legend.position = "bottom")
  }
  
  #Subset a tipical week per Q YEAR####
  {
    season_week <- household_power_consumption  %>% 
    group_by(Year, Quarter, wday, Hour)  %>%
    summarise_at(c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Kitchen", "Laundry", "Water_AC", "ALL_Sub_meterings", "NotMeasured"), mean, na.rm = TRUE)
  season_week$wday_H <- paste(formatC(season_week$wday, width = 2, flag = "0"), sep = ":", formatC(season_week$Hour, width = 2, flag = "0"))
  season_week$Quarter %<>% as.factor() 
  levels(season_week$Quarter) = c("Winter", "Spring", "Summer", "Fall")
  is.num <- sapply(season_week, is.numeric) #Function to get all numeric attributes from a DF
  season_week[is.num] %<>% round(4)
  rm(is.num)
  season_week <-  filter(season_week,Year>="2007") 
  season_week$wday_H <-  as.numeric(as.factor(season_week$wday_H))
  
  #Global####
  ggplot(season_week) + 
    geom_line(aes(x = wday_H, y = Global_active_power, color=Year)) + 
    labs(title= "Weekly consumption - Global") +
    xlab("Weekly_Hour") + ylab("Global_active_power") +
    facet_grid(Year~Quarter) +
    theme(legend.position = "bottom")
  #Kitchen####
  ggplot(season_week) + 
    geom_line(aes(x = wday_H, y = Kitchen, color=Year)) + 
    labs(title= "Weekly consumption - Kitchen") +
    xlab("Weekly_Hour") + ylab("Kitchen") +
    facet_grid(Year~Quarter) +
    theme(legend.position = "bottom")
  #Laundry####
  ggplot(season_week) + 
    geom_line(aes(x = wday_H, y = Laundry, color=Year)) + 
    labs(title= "Weekly consumption - Laundry") +
    xlab("Weekly_Hour") + ylab("Laundry") +
    facet_grid(Year~Quarter) +
    theme(legend.position = "bottom")
  #Water_AC####
  ggplot(season_week) + 
    geom_line(aes(x = wday_H, y = Water_AC, color=Year)) + 
    labs(title= "Weekly consumption - Water_AC") +
    xlab("Weekly_Hour") + ylab("Water_AC") +
    facet_grid(Year~Quarter) +
    theme(legend.position = "bottom")
  }
}
 

# TS#### 
#Global Consumption Month x Year
{
  ts_MxY <- Hourly %>% filter(Year>="2007")

# We change the months to numbers
ts_MxY$Month %<>% as.numeric()
# Now we create a time tag
ts_MxY <- mutate(ts_MxY, month_year = paste(Year, formatC(Month, width = 2, flag = "0")))
head(unique(ts_MxY$month_year))

# Let's build a dataframe with the Energy consumption by month and year
# Check the group by tutorial if necessary
ts_MxY <- ts_MxY %>% 
  group_by(month_year) %>% 
  summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
is.num <- sapply(ts_MxY, is.numeric) #Function to get all numeric attributes from a DF
ts_MxY[is.num] %<>% round(4)
rm(is.num)

# Now let's build the time series. 
# We have 12 points per year so let's define our time series
# The frequency is how many points per year are in your dataset
# We have to define as well the starting point so the time series understands how to build the dates
myts <- ts(ts_MxY$Global_active_power,frequency=12,start=c(2007))
plot(myts)

# Now we decompose the time series
myds <- stl(myts, s.window = 11, t.window = 69)

# A time series consists of different elements:
# 1. The seasonal part is the periodic part.
# The frequency forces the time series to find a period so it could be artificial
# 2. The trend is non seasonal part of the time series
# It defines if the value we measure increases, decreases or it's flat
# 3. The random part tries to smooth the trend 
# It tries to remove random behavior from the time series
plot(myds)

# Forecast

## Forecast TS
forecast_Year <- forecast(myts, h = 12)
plot(forecast_Year)

## Forecast HoltWinters
### Fit the model and get predictions
HWplot3<-function(myts,  n.ahead=4,  CI=.85,  error.ribbon='green', line.size=1){
  
  hw_object<-HoltWinters(myts,
                         alpha = 0.05,
                         beta = 0,
                         gamma = 0.34)
  #   Notes:  beta = FALSE and gamma = FALSE gives EWMA
  #           gamma = FALSE gives Double Exponential Smoothing
  forecast<-predict(hw_object,  n.ahead=n.ahead,  prediction.interval=T,  level=CI)
  
  
  for_values<-data.frame(time=round(time(forecast),  3),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
  
  fitted_values<-data.frame(time=round(time(hw_object$fitted),  3),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
  
  actual_values<-data.frame(time=round(time(hw_object$x),  3),  Actual=c(hw_object$x))
  
  
  graphset<-merge(actual_values,  fitted_values,  by='time',  all=TRUE)
  graphset<-merge(graphset,  for_values,  all=TRUE,  by='time')
  graphset[is.na(graphset$dev),  ]$dev<-0
  
  graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
  
  
  graphset.melt<-melt(graphset[, c('time', 'Actual', 'Fitted')], id='time')
  
  p<-ggplot(graphset.melt,  aes(x=time,  y=value)) + 
    geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) + 
    geom_line(aes(colour=variable), size=line.size) + 
    geom_vline(xintercept=max(actual_values$time),  lty=2) + 
    xlab('Time') + ylab('Value') + 
    labs(legend.position='bottom') + 
    labs(title="Holt Winters - Prediction") +
    theme(panel.background = element_blank())+
    theme(panel.grid.major.y = element_line(linetype = "dashed", colour = "grey")) +
    theme(panel.grid.minor.y = element_line(linetype = "dashed", colour = "grey")) +
    scale_colour_hue('')
  return(p)
  
}

### Plot HW forecast

forecast_Year_HW <- ts(myts, start = c(2007), frequency = 12)
HWplot3 (forecast_Year_HW, n.ahead = 12)
}
# Global Consumption by Day of the Week
# 2009 03
{ts_week_2009 <- household_power_consumption %>%
    filter(Year == "2009", Month == "03") 
  
  ts_week_2009$week <- isoweek(ts_week_2009$Date)
  
  # We change the attributes date to numbers
  ts_week_2009$week %<>% as.numeric()
  ts_week_2009$wday %<>% as.numeric()
  
  # Now we create a time tag
  ts_week_2009 <- mutate(ts_week_2009, week_day = paste(formatC(week, width = 2, flag = "0"), formatC(wday, width = 2, flag = "0")))
  head(unique(ts_week_2009$week_day ))
  
  ts_week_2009 <- ts_week_2009 %>%
    filter(week_day >= "10 01", week_day <= "13 07") %>%
    group_by(week_day) %>% 
    summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
  is.num <- sapply(ts_week_2009, is.numeric) #Function to get all numeric attributes from a DF
  ts_week_2009[is.num] %<>% round(4)
  rm(is.num)
  
  # Now let's build the time series. 
  # We have 12 points per year so let's define our time series
  # The frequency is how many points per year are in your dataset
  # We have to define as well the starting point so the time series understands how to build the dates
  myts_week_2009 <- ts(ts_week_2009$Global_active_power,frequency=7,start=c(1))
  plot(myts_week_2009)
  
  # Now we decompose the time series
  myds_week_2009 <- stl(myts_week_2009, s.window = 7)
  plot(myds_week_2009)
  }
# Global Consumption Hour x Quarter Day
# Winter Day
{
  ts_Winter_day <- season %>% filter(Year>="2007", Quarter == "Winter")
  
  # We change the months to numbers
  ts_Winter_day$Year %<>% as.numeric()
  ts_Winter_day$Hour %<>% as.numeric()
  
  # Now we create a time tag
  ts_Winter_day <- mutate(ts_Winter_day, hour_year = paste(Year, formatC(Hour, width = 2, flag = "0")))
  head(unique(ts_Winter_day$hour_year))
  
  # Let's build a dataframe with the Energy consumption by month and year
  # Check the group by tutorial if necessary
  ts_Winter_day <- ts_Winter_day %>% 
    group_by(hour_year) %>% 
    summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
  is.num <- sapply(ts_Winter_day, is.numeric) #Function to get all numeric attributes from a DF
  ts_Winter_day[is.num] %<>% round(4)
  rm(is.num)
  
  # Now let's build the time series. 
  # We have 12 points per year so let's define our time series
  # The frequency is how many points per year are in your dataset
  # We have to define as well the starting point so the time series understands how to build the dates
  myts_Winter_day <- ts(ts_Winter_day$Global_active_power,frequency=24,start=c(2007))
  plot(myts_Winter_day)
  
  # Now we decompose the time series
  myds_Winter_day <- stl(myts_Winter_day, s.window = 7)
  
  # A time series consists of different elements:
  # 1. The seasonal part is the periodic part.
  # The frequency forces the time series to find a period so it could be artificial
  # 2. The trend is non seasonal part of the time series
  # It defines if the value we measure increases, decreases or it's flat
  # 3. The random part tries to smooth the trend 
  # It tries to remove random behavior from the time series
  plot(myds_Winter_day)
  
  # Forecast
  
  ## Forecast TS
  forecast_Winter_day <- forecast(myts_Winter_day, h = 24)
  plot(forecast_Winter_day)
  
  ## Forecast HoltWinters
  ### Fit the model and get predictions
  myts_Winter_day_HW <- HoltWinters(myts_Winter_day,
                                    alpha = 0.07,
                                    #beta = FALSE,
                                    gamma = TRUE)
  #   Notes:  beta = FALSE and gamma = FALSE gives EWMA
  #           gamma = FALSE gives Double Exponential Smoothing
  
  ### Plot HW forecast
  
  forecast_Winter_day_HW <- forecast(myts_Winter_day_HW, h = 24)
  plot(forecast_Winter_day_HW)
}
# Global Consumption by Day of the week
# Winter Week
{
  ts_Winter_week <- household_power_consumption  %>% 
    group_by(Year, Quarter, wday)  %>%
    summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC"), mean, na.rm = TRUE)
  
  # Now we create a time tag
  ts_Winter_week <- mutate(ts_Winter_week, Week_year = paste(Year, formatC(wday, width = 2, flag = "0")))
  
  # Let's build a dataframe with the Energy consumption by weekday and year
  # Check the group by tutorial if necessary
  ts_Winter_week <- ts_Winter_week %>% 
    group_by(Week_year)  %>%
    summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC"), mean, na.rm = TRUE) %>%
    filter(Week_year>="2007 01")
  is.num <- sapply(ts_Winter_week, is.numeric) #Function to get all numeric attributes from a DF
  ts_Winter_week[is.num] %<>% round(4)
  rm(is.num)
  
  
  
  # Now let's build the time series. 
  # We have 12 points per year so let's define our time series
  # The frequency is how many points per year are in your dataset
  # We have to define as well the starting point so the time series understands how to build the dates
  myts_Winter_week <- ts(ts_Winter_week$Global_active_power,frequency=7,start=c(2007))
  plot(myts_Winter_week)
  
  # Now we decompose the time series
  myds_Winter_week <- stl(myts_Winter_week, s.window = 7, t.window = 27)
  
  # A time series consists of different elements:
  # 1. The seasonal part is the periodic part.
  # The frequency forces the time series to find a period so it could be artificial
  # 2. The trend is non seasonal part of the time series
  # It defines if the value we measure increases, decreases or it's flat
  # 3. The random part tries to smooth the trend 
  # It tries to remove random behavior from the time series
  plot(myds_Winter_week)
  
  # Forecast
  
  ## Forecast TS
  forecast_Winter_week <- forecast(myts_Winter_week, h = 7)
  plot(forecast_Winter_week)
  
  ## Forecast HoltWinters
  ### Fit the model and get predictions
  myts_Winter_week_HW <- HoltWinters(myts_Winter_week,
                                      alpha = 0.1,
                                      beta = FALSE,
                                      gamma = TRUE)
  #   Notes:  beta = FALSE and gamma = FALSE gives EWMA
  #           gamma = FALSE gives Double Exponential Smoothing
  
  ### Plot HW forecast
  
  forecast_Winter_week_HW <- forecast(myts_Winter_week_HW, h = 7)
  plot(forecast_Winter_week_HW)
}
{# (OLD with day subset by 4)Global Consumption by Weekly Time of the Day
# Winter Week
{
  ts_Winter_week <- household_power_consumption  %>% 
    group_by(Year, Quarter, wday, Hour)  %>%
    summarise_at(c("DateTime","Global_active_power", "Kitchen", "Laundry", "Water_AC"), mean, na.rm = TRUE)
  ts_Winter_week$wday_H <- paste(formatC(ts_Winter_week$wday, width = 2, flag = "0"), sep = ":", formatC(ts_Winter_week$Hour, width = 2, flag = "0"))
  ts_Winter_week$Quarter %<>% as.factor() 
  levels(ts_Winter_week$Quarter) = c("Winter", "Spring", "Summer", "Fall")
  
  ts_Winter_week <-  filter(ts_Winter_week,Year>="2007") 
  ts_Winter_week$wday_H <-  as.numeric(as.factor(ts_Winter_week$wday_H))
  
  
  Hour <-  cut(ts_Winter_week$DateTime, format= "%Y-%m-%d:%H%:%M:%S", "6 hours")
  ts_Winter_week$Hour <- Hour 
  rm(Hour)
  
  ts_Winter_week <- ts_Winter_week %>% 
    group_by(Year, Quarter, wday, Hour)  %>%
    summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC"), mean, na.rm = TRUE) %>%
    filter(Quarter == "Winter")
  is.num <- sapply(ts_Winter_week, is.numeric) #Function to get all numeric attributes from a DF
  ts_Winter_week[is.num] %<>% round(4)
  rm(is.num)
  
  ts_Winter_week$Hour <- ts_Winter_week$Hour %>% hour()
  
  
  # We change the months to numbers
  ts_Winter_week$Year %<>% as.numeric()
  ts_Winter_week$Hour %<>% as.numeric()
  
  # Now we create a time tag
  ts_Winter_week <- mutate(ts_Winter_week, hour_week = paste(Year, formatC(wday, width = 2, flag = "0"), formatC(Hour, width = 2, flag = "0")))
  head(unique(ts_Winter_week$hour_week))
  
  # Let's build a dataframe with the Energy consumption by month and year
  # Check the group by tutorial if necessary
  ts_Winter_week <- ts_Winter_week %>% 
    group_by(hour_week) %>% 
    summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC"), mean, na.rm = TRUE)
  is.num <- sapply(ts_Winter_week, is.numeric) #Function to get all numeric attributes from a DF
  ts_Winter_week[is.num] %<>% round(4)
  rm(is.num)
  
  # Now let's build the time series. 
  # We have 12 points per year so let's define our time series
  # The frequency is how many points per year are in your dataset
  # We have to define as well the starting point so the time series understands how to build the dates
  myts_Winter_week <- ts(ts_Winter_week$Global_active_power,frequency=28,start=c(2007))
  plot(myts_Winter_week)
  
  # Now we decompose the time series
  myds_Winter_week <- stl(myts_Winter_week, s.window = 7)
  
  # A time series consists of different elements:
  # 1. The seasonal part is the periodic part.
  # The frequency forces the time series to find a period so it could be artificial
  # 2. The trend is non seasonal part of the time series
  # It defines if the value we measure increases, decreases or it's flat
  # 3. The random part tries to smooth the trend 
  # It tries to remove random behavior from the time series
  plot(myds_Winter_week)
}
}
#Plot####
{
  cols <- c("Kitchen"= "red", "Laundry" = "green", "Water_AC" = "blue", "Global" = "black", "All submeters" = "red", "NM" = "purple")
#Consumption p/week p/house region####
  
    
  ggplot(powerweek) +
    geom_line(aes(Q_Hour, Kitchen, color = "Kitchen")) +
    geom_line(aes(Q_Hour, Laundry, color = "Laundry")) +
    geom_line(aes(Q_Hour, Water_AC, color = "Water_AC")) +
    labs(x = "Date", y = "Energy sub metering") +
    scale_colour_manual(name="",values=cols) +
    theme(legend.position = "bottom") 
    #theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.title = element_blank())
  
#Consumption p/week Global x Total Submeters####
  
    ggplot(powerweek) +
    geom_line(aes(DateTime, Global_active_power, color = "Global")) +
    geom_line(aes(DateTime, Sub_meterings, color = "All submeters")) +
    labs(x = "", y = "Energy sub metering") +
    scale_colour_manual(name="",values=cols) +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.title = element_blank())
    
    
    
#Consumption p/2 days Global x Submeter broke down####
    
    ggplot(powerdays) +
      geom_line(aes(DateTime, Global_active_power, color = "Global")) +
      geom_line(aes(DateTime, Kitchen, color = "Kitchen")) +
      geom_line(aes(DateTime, Laundry, color = "Laundry room")) +
      geom_line(aes(DateTime, Water_AC, color = "Water heater / AC"))
    labs(x = "", y = "Energy sub metering") +
      scale_colour_manual(name="",values=cols) +
      theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.title = element_blank())
#Consumption p/2 days Global x Total Submeters####
    
    ggplot(powerdays) +
      geom_line(aes(DateTime, Global_active_power, color = "Global")) +
      geom_line(aes(DateTime, Sub_meterings, color = "All submeters")) +
      labs(x = "", y = "Energy sub metering") +
      scale_colour_manual(name="",values=cols) +
      theme(legend.justification = c(1, 1), legend.position = c(1, 1), legend.title = element_blank())
#Consumption history####
ggplot(data = household_power_consumption) +
  geom_smooth(mapping = aes(x=DateTime, y=Global_active_power))+
  geom_point(mapping = aes(x=DateTime, y=Global_active_power, color = Year))
#Histogram####
ggplot(household_power_consumption, aes(x = Global_active_power, color = factor(Year))) +
  geom_histogram()    

    Histogram <- function() {
      hist(household_power_consumption$Global_active_power, 
           main = paste("Global Active Power"), 
           col="red", xlab="Global Active Power (kilowatts)")
    }
    plot1()      
#Typical Daily consumption by Season of the Year p/ submeter####
    
    ## Dataset Adjustment
    {
      season_allY <- household_power_consumption  %>% 
        filter(Year>=2007) %>%
        group_by(Quarter, Hour)  %>%
        summarise_at(c("Global_active_power", "Kitchen", "Laundry", "Water_AC", "NotMeasured"), mean, na.rm = TRUE)
      season_allY$Quarter %<>% as.factor() 
      levels(season_allY$Quarter) = c("Winter", "Spring", "Summer", "Fall")
      is.num <- sapply(season_allY, is.numeric) #Function to get all numeric attributes from a DF
      season_allY[is.num] %<>% round(4)
      rm(is.num)
      
      season_allY <- season_allY[,c(1:3,7,4:6)] #Reorder columns
      
      Gathered_season_all <- gather(season_allY,"Zones","EnerGY",4:7)
      Gathered_season_all$Zones %<>% as.factor() 
      
      ## To reorder the levels:
      Gathered_season_all$Zones %<>% factor(levels(Gathered_season_all$Zones)[c(3,4,2,1)])
      
      #Gathered_season_all$Zones %<>% as.factor() 
      rm(season_allY)
    }
    ## Area Plot
    {
      Typical_Day_by_Season <-ggplot(Gathered_season_all, aes(x=Hour, y=EnerGY, Group = Hour, fill=Zones)) + 
        geom_area(color="black", size=.2, alpha=.8) +
        labs(x = "Hour", y = "Consumption (KWh)") +
        theme(legend.title = element_blank(), legend.position = "bottom")+
        labs(title="Daily Energy Consumption by Season - Submeter view") +
        theme(panel.grid.major.y = element_line(linetype = "dashed", colour = "grey")) +
        theme(axis.text.y=element_text(size=12)) +
        theme(axis.text=element_text(size=12))+
        scale_x_continuous(breaks =c(0,6,12,18,23)) +
        theme(legend.title=element_text(size=12, face = "bold"))+
        theme(legend.text=element_text(size=10.5))+
        theme(panel.background = element_rect(fill = "white")) +
        #scale_fill_manual(Gathered_season_all$Quarter, values=c("red","blue","green","brown"))
        facet_grid(.~Quarter)
        #theme(strip.background = element_rect(fill = alpha("lightblue"), beta("orange"), gamma("red"), delta("grey"))) 
      Typical_Day_by_Season  
      
      

    }
#Typical Weekly consumption####
    
    ## Dataset Adjustment
    {
      Weeks <- household_power_consumption  %>% 
        filter(Year>="2007")  %>%
        group_by(wday, Hour) %>% 
        summarise_at(c("Global_active_power", "NotMeasured", "Kitchen", "Laundry", "Water_AC"), mean, na.rm = TRUE)
      
      ## We change the wday and to Factors
      #Weeks$wday %<>% as.factor() 
      #levels(Weeks$wday) = c("weekday","weekday","weekday","weekday","weekday","weekends","weekends")

      ## Group by daytype
      Weeks <- Weeks  %>% 
        group_by(wday, Hour) %>% 
        summarise_at(c("Global_active_power", "NotMeasured", "Kitchen", "Laundry", "Water_AC"), mean, na.rm = TRUE)
      
      ## Now we create a time tag
      Weeks <- mutate(Weeks, weekdays = paste(formatC(wday, width = 2, flag = "0"), formatC(Hour, width = 2, flag = "0")))
      head(unique(Weeks$hour_day))
      
      
      is.num <- sapply(Weeks, is.numeric) #Function to get all numeric attributes from a DF
      Weeks[is.num] %<>% round(4)
      rm(is.num)
      
      ## Gather Zones
      Gathered_weeks <- gather(Weeks,"Zones","EnerGY",4:7)
      Gathered_weeks$Zones %<>% as.factor()
      
      ## To reorder the levels:
      Gathered_weeks$Zones  %<>% factor(levels(Gathered_weeks$Zones)[c(3,4,2,1)])
    }
    ## Area Plot
      {
        Typical_Week <-ggplot(Gathered_weeks, aes(x=Hour, y=EnerGY, Group = Hour, fill=Zones)) + 
          geom_area(color="black", size=.2, alpha=.8) +
          labs(x = "Hour", y = "Consumption (KWh)") +
          theme(legend.title = element_blank(), legend.position = "bottom")+
          labs(title="Daily Energy Consumption by Weekday - Submeter view") +
          theme(panel.grid.major.y = element_line(linetype = "dashed", colour = "grey")) +
          theme(axis.text.y=element_text(size=12)) +
          theme(axis.text=element_text(size=12))+
          scale_x_continuous(breaks =c(0,6,12,18,23)) +
          theme(legend.title=element_text(size=12, face = "bold"))+
          theme(legend.text=element_text(size=10.5))+
          theme(panel.background = element_rect(fill = "white")) +
          facet_grid(.~wday)
        Typical_Week  
      }
      
      
    
# Donut Chart Consumption by Zone####
 
    ## Adjust Dataset
    {Pie_Zones <- household_power_consumption  %>% 
        filter(Year>="2007")  %>%
        group_by(Year) %>% 
        summarise_at(c("NotMeasured", "Kitchen", "Laundry", "Water_AC"), mean, na.rm = TRUE)
      
      ## Gather Zones
      Pie_Zones <- gather(Pie_Zones,"Zones","EnerGY",2:5)
      Pie_Zones$Zones %<>% as.factor()
      Pie_Zones <- Pie_Zones  %>% 
        group_by(Zones) %>% 
        summarise_at(c("EnerGY"), mean, na.rm = TRUE)
      
      is.num <- sapply(Weeks, is.numeric) #Function to get all numeric attributes from a DF
      Weeks[is.num] %<>% round(4)
      rm(is.num)
      
      ## To reorder the levels:
      Pie_Zones$Zones %<>% factor(levels(Pie_Zones$Zones)[c(3,4,2,1)])
      
      ## Calculate PCT and lbls
      pct <- round(Pie_Zones$EnerGY/sum(Pie_Zones$EnerGY)*100, digits = 1)
      lbls <- paste(Pie_Zones$Zones, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      
      ## Add Vectors (pct and lbls) to DF
      Pie_Zones$pct <- pct
      Pie_Zones$lbls <- lbls
      
      ## Add addition columns, needed for drawing with geom_rect.
      Pie_Zones = Pie_Zones[order(Pie_Zones$pct), ]
      Pie_Zones$ymax = cumsum(Pie_Zones$pct)
      Pie_Zones$ymin = c(0, head(Pie_Zones$ymax, n=-1))
      
      rm(pct)
      rm(lbls)
    }
    
    ##Donut Plot
    {
      donut <-  ggplot(Pie_Zones, aes(fill=Zones, ymax=ymax, ymin=ymin, xmax=4, xmin=3), label = Pie_Zones$lbls) +
        geom_rect() +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        theme(panel.grid=element_blank()) +
        theme(axis.text=element_blank()) +
        theme(axis.ticks=element_blank()) +
        theme(legend.position=c(.5, .5)) + 
        ggtitle("") +
        theme(panel.grid=element_blank()) +
        theme(axis.title=element_blank()) +
        theme(panel.background = element_blank())+
        theme(legend.title = element_text(size=16, face="bold")) +
        theme(legend.text = element_text(size = 14)) +
        annotate("text", x = NULL, y = NULL, label = "My Ring plot !") +
        geom_label(aes(label = paste(pct,"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE) +
        labs(title="Energy Consumption p/ Zone (%)")
      #How To Add Labels with "%" in the pie??
      donut
    }

# Chart Consumption by Season####
    
    ## Adjust Dataset
    {
      Pie_seasons <- household_power_consumption  %>% 
        filter(Year>="2007")  %>%
        group_by(Quarter) %>% 
        summarise_at(c("Global_active_power"), mean, na.rm = TRUE)
      
      is.num <- sapply(Weeks, is.numeric) #Function to get all numeric attributes from a DF
      Weeks[is.num] %<>% round(4)
      rm(is.num)
      
      ## Gather Zones
      #Pie_seasons <- gather(Pie_seasons,"Zones","EnerGY",2:5)
      Pie_seasons$Quarter %<>% as.factor()
      levels(Pie_seasons$Quarter) <- c("Winter", "Spring", "Summer", "Fall")
      
      ## Calculate PCT and lbls
      Pie_seasons$pct <- round(Pie_seasons$Global_active_power/sum(Pie_seasons$Global_active_power)*100, digits = 1)
      Pie_seasons$lbls <- paste(Pie_seasons$pct,"%")
      
      ## Add addition columns, needed for drawing with geom_rect.
      Pie_seasons$ymax = cumsum(Pie_seasons$pct)
      Pie_seasons$ymin = c(0, head(Pie_seasons$ymax, n=-1))
      
    }
    
    ##Donut Plot
    {
      donut_season <-  ggplot(Pie_seasons, aes(fill=Quarter, ymax=ymax, ymin=ymin, xmax=4, xmin=3), label = Pie_seasons$lbls) +
        geom_rect() +
        coord_polar(theta="y") +
        xlim(c(0, 4)) +
        scale_fill_manual(values = c("lightblue", "orange", "red", "grey")) +
        theme(panel.grid=element_blank()) +
        theme(axis.text=element_blank()) +
        theme(axis.ticks=element_blank()) +
        theme(legend.position=c(.5, .5)) + 
        ggtitle("") +
        theme(panel.grid=element_blank()) +
        theme(axis.title=element_blank()) +
        theme(panel.background = element_blank())+
        theme(legend.title = element_text(size=16, face="bold")) +
        theme(legend.text = element_text(size = 14)) +
        annotate("text", x = NULL, y = NULL, label = "My Ring plot !") +
        geom_label(aes(label = lbls,x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE) +
        labs(title="Energy Consumption p/ Season (%)")
      #How To Add Labels with "%" in the pie??
      donut_season
    }

#Consumption Comparison p/Week p/Season of the Year####
    
    #Dataset Adjustment
    {
      season_week_allY <- season_week[,-c(6:8,ncol(season_week)-2)]
      
      season_week_allY <- season_week_allY[,c(1:4, ncol(season_week_allY),5:(ncol(season_week_allY)-1))] 
      
      Gathered_season_week_all <- gather(season_week_allY,"Zones","EnerGY",6:9)
      Gathered_season_week_all$Zones %<>% as.factor() 
      levels(Gathered_season_week_all$Zones) = c("Not Measured", "Kitchen", "Laundry", "Water_AC")
      rm(season_week_allY)
    }
    #Area Plot
    {
      Typical_Week_by_Season <-ggplot(Gathered_season_week_all, aes(x=wday_H, y=EnerGY, Group = wday_H, fill=Zones)) + 
        geom_area(color="black", size=.2, alpha=.8) +
        labs(x = "wday_Hour", y = "Consumption (KWh)") +
        theme(legend.title = element_blank(), legend.position = "bottom")+
        labs(title="Typical Week by Season") +
        facet_grid(Quarter~ Year)
      Typical_Week_by_Season  
    }
}
