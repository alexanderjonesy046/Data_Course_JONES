#Read In the Data####
df <- read.csv("DNA_Conc_by_Extraction_Date.csv")

#Install Packages####
library(tidyr)
library(tidyverse)



#Histograms####
hist(df$DNA_Concentration_Katy, xlab = "DNA Concentration", main = "Katy DNA Concentration")
hist(df$DNA_Concentration_Ben, xlab = "DNA Concentration", main = "Ben DNA Concentration")



#Boxplot 1####
boxplot(df$DNA_Concentration_Katy~df$Year_Collected, main = "Katy's Extractions", ylab = "DNA Concentration", xlab = "YEAR")


jpeg("JONES_Plot1.jpg")
boxplot(df$DNA_Concentration_Katy~df$Year_Collected, main = "Katy's Extractions", ylab = "DNA Concentration", xlab = "YEAR")
dev.off()

#Boxplot 2####

boxplot(df$DNA_Concentration_Ben~df$Year_Collected, main = "Ben's Extractions", ylab = "DNA Concentration", xlab = "YEAR")


jpeg("JONES_Plot2.jpeg")
boxplot(df$DNA_Concentration_Ben~df$Year_Collected, main = "Ben's Extractions", ylab = "DNA Concentration", xlab = "YEAR")
dev.off()

#Katy Vs Ben####
KA <- select(df, DNA_Concentration_Ben,DNA_Concentration_Katy, Year_Collected) %>%
  group_by(Year_Collected) %>%
  summarise(Average_DNA_Concentration_Katy = mean(DNA_Concentration_Katy))
     
KB <- select(df, DNA_Concentration_Ben, Year_Collected) %>% 
  group_by(Year_Collected) %>% 
  summarise(Average_DNA_Concentration_Ben = mean(DNA_Concentration_Ben))

                                             
KBA <- full_join(KB, KA) %>% 
  mutate(Yield_Margin = Average_DNA_Concentration_Ben - Average_DNA_Concentration_Katy)

#Downstairs Subset####
DOWNSTAIRS <- df$Lab == "Downstairs"
DC <- df$Date_Collected[DOWNSTAIRS]
BEN_DOWNSTAIRS <- df$DNA_Concentration_Ben[DOWNSTAIRS]


NDC <- as.POSIXct(DC, TZ = "", format("%Y%m%d"))
plot(x = NDC , y = BEN_DOWNSTAIRS)  
plot(x = NDC , y = BEN_DOWNSTAIRS, xlab = "Date Collected", ylab = "DNA Concentration Ben", main = "Downstairs")  


jpeg("Ben_DNA_over_time.jpeg")
plot(x = NDC , y = BEN_DOWNSTAIRS, xlab = "Date Collected", ylab = "DNA Concentration Ben", main = "Downstairs")  
dev.off()

#BONUS Problem####

df2 <- select(df, DNA_Concentration_Ben, Year_Collected) %>% 
  group_by(Year_Collected) %>% 
  summarise(Average_DNA_Concentration = mean(DNA_Concentration_Ben))

write.csv(df2, "Ben_Average_Conc.csv", row.names = FALSE)

TP <- df2 %>% arrange(desc(Average_DNA_Concentration))
glimpse(TP)






