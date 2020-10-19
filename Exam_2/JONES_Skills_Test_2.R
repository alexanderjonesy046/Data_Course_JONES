# Exam 2
#read in data
df <- read.csv("landdata-states.csv")

ls(df)
view(df)
#read in packages
library(tidyr)
library(tidyverse)
library(scales)

#Start ggplotting Land Value Data####

FIG_1 <-ggplot(df, aes(x = Year, y = Land.Value )) + geom_smooth(aes(color = region)) +
  theme_minimal() + labs(x= "Year", y = "Land Value (USD)") +
  scale_y_continuous(labels = dollar)
#Still need to squish it and change the labels from region to "Region"








#Subset Land Value Data####

tail(df$region)
head(df$region)




#Read in Unicef Data







#Read in Unicef Data

df2 <- read.csv("unicef-u5mr.csv")

head(df2)
ls(df2)
view(df2)

#TIDY THE UNICEF DATA####
GATHER <- df2 %>% 
  gather(key = "Year", value = "Mortality.Rate", c(-CountryName,-Region,-Continent)) %>% 
  drop_na() %>% 
  mutate(Year = as.factor(Year))

view(GATHER)

GATHER2 <- df2 %>% 
  gather(key = "Year", value = "Mortality.Rate", c(-CountryName,-Region,-Continent)) %>% 
  drop_na() %>% 
  mutate(Year = as.numeric(as.factor(Year)))
  
GATHER2$Year <- GATHER2$Year + 1949
view(GATHER2)


#GGPLOT FIG 2####
ggplot(GATHER2, aes(x = Year, y = Mortality.Rate, color = Continent)) +
  geom_point(size = 2.5) + theme_minimal() +
  labs(y = "MortalityRate")
  
#Still need to figure out how to Squish


#Remutate Data for mean####
MEAN <- df2 %>% 
  gather(key = "Year", value = "Mortality.Rate", c(-CountryName,-Region,-Continent)) %>% 
  drop_na() %>% 
  mutate(Year = as.numeric(as.factor(Year))) %>% 
  group_by(Continent, Year) %>% 
  summarise(mean = mean(Mortality.Rate), n = n() ) 
MEAN$Year <- MEAN$Year + 1949
view(MEAN)

#GGPLOT Fig 3####

ggplot(MEAN, aes(x = Year, y = mean, color = Continent)) +
  geom_point() +geom_line(size = 2) +
  labs(y = "Mean Mortality Rate (deaths per 1000 live births)") +theme_minimal()


#Remutate for FIG 4####

 PROP <- df2 %>% 
  gather(key = "Year", value = "Mortality.Rate", c(-CountryName,-Region,-Continent)) %>% 
  drop_na() %>% 
  mutate(Year = as.numeric(as.factor(Year))) %>% 
  group_by(CountryName, Region, Mortality.Rate, Year) %>%
  summarise(Mean = mean(Mortality.Rate)) %>% 
  mutate(Prop = Mortality.Rate/Mean)
PROP$Year <- PROP$Year + 1949
view(PROP)


sum(PROP$Mortality.Rate)
#GGPLOT FIG 4####

ggplot(PROP, aes(x = Year, y = Mean)) + geom_point(color = "blue") + 
  facet_wrap( ~Region) +theme_minimal()

#SAVE AS JPEG









