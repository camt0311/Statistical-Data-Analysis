# set working directory
getwd()

# install and load packages
#install.packages("stargazer")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("printr")
#install.packages("splitstackshape")
#install.packages("moments")
#install.packages("maps")
library(stargazer) # df to pretty table
library(tidyverse) # data manipulation and visualization
library(ggplot2) # plotting
library(tidyr) # tidying
library(dplyr) # data manipulation
library(printr) # displaying dataframes as tables
library(splitstackshape) # string manipulation
library(moments) # for skewness
library(maps) # for plotting later

# data exploration
gv <- read.csv('data/gun_violence.csv')
head(gv, 1)
class(gv)
colnames(gv)
sapply(gv, class)
dim(gv) # 239677 x 29
length(unique(gv$incident_id))
gv %>% map_dbl(~sum(is.na(.))) # NA values

length(unique(gv$state)) # returns 51
# all US states plus DC experience gun violence
sum(gv$n_injured) # returns 118402
sum(gv$n_killed) # returns 60468

gvd <- data.frame(gv) # copy
# create new column variable year
gvd$year <- as.numeric(format(as.Date(gvd$date, format="%Y-%m-%d"),"%Y"))
unique(gvd$year)
# create new column variable for total victims
gvd$n_victims <- gvd$n_killed + gvd$n_injured

# drop unnecessary columns
gvd_keep <- select(gvd, n_killed, n_injured, n_victims, n_guns_involved)
# summary statistics
summary(gvd_keep)
stargazer(gvd_keep, type = "text")

# new column variable
# is the incident classified as a mass shooting?
ms_check = c("Mass Shooting|mass shooting|Mass shooting|mass Shooting")
gvd$is_mass_shooting = str_detect(gvd$incident_characteristics,ms_check)
gvd$is_mass_shooting = ifelse(gvd$is_mass_shooting == T,1,0)
head(gvd$is_mass_shooting)

# number of incidents per state
n_state <- gvd %>% 
  group_by(state) %>% 
  summarise(n = n()) %>%
  arrange(desc(n))
n_state

# plot of incidents per state
ggplot(n_state, aes(x = state, y = n)) + 
  geom_bar(stat = "identity", fill = "blue") + theme_bw() + 
  theme(axis.text.x=element_text(angle=90))

# top 10 cities with most gun violence
top10cities <- gvd %>% 
  group_by(city_or_county) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>% 
  tail(10)
top10cities
top10cities$city_or_county <- factor(top10cities$city_or_county, 
                                     levels = top10cities$city_or_county)
ggplot(top10cities, aes(x = city_or_county, y = n)) + 
  geom_bar(stat = "identity") + coord_flip() + theme_bw()

# 10 most common characteristics of incidents
indv_ic = cSplit(gvd, c("incident_characteristics"), sep = "||", 
                 direction = "long", drop = FALSE)
temp = indv_ic %>% 
  group_by(incident_characteristics) %>% 
  summarise(count = n()) %>% 
  mutate(perc = round((count/nrow(indv_ic))*100, 2)) %>% 
  arrange(desc(count))
temp
ggplot(head(temp, 10), aes(factor(incident_characteristics, 
                                  incident_characteristics), 
                           count, fill = incident_characteristics)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Incident Characteristics", y = "Count", title = "") + 
  coord_flip() + theme_bw() + theme(legend.position = "none") + 
  geom_text(aes(label = paste0(perc, "%", sep = " ")), hjust = 1, vjust = 0.25, 
            size = 2.75, color = 'black', fontface = 'bold')

# Average number of victims and guns per state
state_v_gi <- gvd %>% 
  group_by(state) %>% 
  summarise(avg_n_victims = mean(n_victims, na.rm = TRUE), 
            avg_n_guns = mean(n_guns_involved, na.rm = TRUE))
head(state_v_gi)
summary_by_state <- state_v_gi %>% 
  gather(Condition, Frequency, avg_n_victims:avg_n_guns)
# plot
ggplot(summary_by_state, aes(fill = Condition, y = Frequency, x = state)) + 
  geom_bar(position = "stack", stat = "identity") + coord_flip() + theme_bw()
# model doesn't make sense
model1 <- lm(avg_n_victims ~ avg_n_guns, data = state_v_gi)
summary(model1)

# mean and median guns per state
gun_by_state <- gvd %>% 
  group_by(state) %>% 
  summarise(mean = mean(n_guns_involved, na.rm = TRUE), 
            median = median(n_guns_involved, na.rm = TRUE))
gun_dist_by_state <- gun_by_state %>% 
  gather(Statistic, Guns, mean:median)
ggplot(data = gun_dist_by_state) + 
  geom_bar(mapping = aes(fill = Statistic, x = state, y = Guns), 
           stat = "identity", position = "dodge") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90))
# the median is 1 for all states?

# mean and median victims per state
vic_by_state <- gvd %>% 
  group_by(state) %>% 
  summarise(mean = mean(n_victims, na.rm = TRUE), 
            median = median(n_victims, na.rm = TRUE))
vic_dist_by_state <- vic_by_state %>% 
  gather(Statistic, Victims, mean:median)
ggplot(data = vic_dist_by_state) + 
  geom_bar(mapping = aes(fill = Statistic, x = state, y = Victims), 
           stat = "identity", position = "dodge") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90))

# Victims Summary By State
VictimsByState <- gvd %>% group_by(state) %>% 
  summarize(sumVic = sum(n_victims), sumInj = sum(n_injured), 
            sumDeath = sum(n_killed), PercDeath = round(sumDeath/sumVic, 2), 
            sumIncidents = n(), vicPerInc = round(sumVic/sumIncidents, 2))
head(VictimsByState)
# plot of Victims Per Incident
VictimsByState %>% filter(vicPerInc > 0.8) %>%
  ggplot(aes(x = reorder(state, -vicPerInc), y = vicPerInc)) + 
  geom_bar(stat = 'identity', fill = 'blue') +
  labs(x = 'State', y = 'Victims per Incident') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Victims and Incidents Per Year
temp <- subset(gvd, year != 2013 & year != 2018)
temp = temp %>% group_by(year) %>% 
  summarise(injured_and_killed = sum(n_victims), number_of_incidents = n())
# Scatter Plot
ggplot(temp, aes(number_of_incidents, injured_and_killed)) + 
  geom_point(aes(size = injured_and_killed, col = year)) + 
  theme(legend.position="bottom") + 
  labs(x="Number of Incidents",y="Number of Victims",
       title="",size="Victims",col="Year") + 
  theme_bw() + theme(plot.title = element_text(size = 16, hjust = 0.5)) + 
  geom_smooth(method=lm, colour="darkgray", se=F)

# 2013 and 2018 aren't full years
max(gvd$date)
min(gvd$date)

# number of daily gun violence incidents
bydate <- gvd %>% group_by(date) %>% summarize(n = n())
bydate$date <- as.Date(bydate$date)
ggplot(bydate, aes(x = date, y = n)) + geom_point() + 
  geom_smooth(method=lm) + theme_bw()
# number of daily gun violence incidents, fixed
bydate <- subset(gvd, year != 2013 & year != 2018)
bydate <- bydate %>% group_by(date) %>% summarize(n = n())
bydate$date <- as.Date(bydate$date)
ggplot(bydate, aes(x = date, y = n)) + geom_point() + 
  geom_smooth(method=lm) + theme_bw()

# Injured and Killed Per Year
bydate <- subset(gvd, year != 2013 & year != 2018) %>% 
  group_by(date) %>% 
  summarize(n = n(), killed = sum(n_killed), injured = sum(n_injured)) %>% 
  gather(key = "type", value = "count", killed, injured)
bydate$date <- as.Date(bydate$date)
bydate <- bydate %>% mutate(year = format(date, "%y"))
# Box Plot of Injured and Killed
ggplot(bydate, aes(x = year, y = count, fill = type)) + 
  geom_boxplot() + theme_bw()

# how many stolen guns involved in incidents per state across the years
gvd_yrs <- subset(gvd, year != 2013 & year != 2018)
gvd_yrs <- group_by(gvd_yrs, year)
gvd_yrs$Stolen_Gun_Count <- str_count(gvd_yrs$gun_stolen, "Stolen")
sum_stolen_guns <- summarize(gvd_yrs, total_guns = sum(n_guns_involved, na.rm = TRUE), 
                             stolen_guns = sum(Stolen_Gun_Count, na.rm = TRUE), 
                             Percent_Stolen_Guns = stolen_guns/total_guns)
sum_stolen_guns
# Plot
plot(sum_stolen_guns$year, sum_stolen_guns$Percent_Stolen_Guns, 
     xlab = "Year", ylab = "% Stolen Guns", 
     main = "Stolen Guns Over Time",
     type = "o", pch = 16) #ylim=c(0,10)
abline(h = mean(sum_stolen_guns$Percent_Stolen_Guns), lty = "longdash", col = "red")
#text(2014, 0.086, "Mean", col = "red", pos = 4, cex = 0.6)
abline(h = mean(sum_stolen_guns$Percent_Stolen_Guns), lty = "dotted", col = "blue")
#text(2014, 0.083, "Median", col = "blue", pos = 4, cex = 0.6)

# victim-state regression
model2 <- lm(n_victims ~ state, data = gvd)
summary(model2)
# too crazy

# Plot Victims on US Map
global <- map_data("state")
ggplot(global, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), fill = "white", col = "black") + 
  coord_fixed(1.3, xlim = c(-130,-60), ylim = c(20,50)) +
  geom_point(data = gvd, aes(x = longitude, y = latitude, col = n_victims), 
             size = 0.001, alpha = .1) +  
  theme_void() + theme(legend.position = "none")
#+ggtitle("title")

model3 <- lm(n_victims ~ longitude + latitude, data = gvd)
summary(model3)

model4 <- lm(n_victims ~ congressional_district +
             state_house_district +
             state_senate_district, 
             data = gvd)
summary(model4)

# create new column variable, region for states
NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")
region.list <- list(
  Northeast=NE.name,
  Midwest=MW.name,
  South=S.name,
  West=W.name)
gvd$region <- sapply(gvd$state, 
                     function(x) names(region.list)[grep(x,region.list)])

model5 <- lm(n_victims ~ region, data = gvd)
summary(model5)
plot(model5)

stargazer(model3, model4, model5, type="html", align=TRUE, out="vic_models.htm")

m1 <- lm(n_guns_involved ~ longitude + latitude, data = gvd)
m2 <- lm(n_guns_involved ~ congressional_district +
           state_house_district +
           state_senate_district, data = gvd)
m3 <- lm(n_guns_involved ~ region, data = gvd)
stargazer(m1, m2, m3, type="html", align=TRUE, out="gun_models.htm")

# more measure check
skewness(gvd$n_victims) # right skewed
kurtosis(gvd$n_victims)
qplot(gvd$n_victims, geom = 'histogram')

