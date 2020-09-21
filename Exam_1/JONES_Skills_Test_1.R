library(tidyverse) #I Really hope we are allowed to use tidyverse on this exam#

Year <- df$Year_Collected
DNA_BEN <- df$DNA_Concentration_Ben
DNA_KATY <- df$DNA_Concentration_Katy
ggplot(data = df, aes(x = Year, y = DNA)) + geom_point(y = DNA_BEN, color = "red") +geom_point(y = DNA_KATY, color = 'blue')
ggplot(data = df, aes(x = Year, y = DNA, color = group)) + geom_point(y = DNA_BEN, color = "red") +geom_point(y = DNA_KATY, color = 'blue')


jpeg("Katy vs. Ben.jpeg")
ggplot(data = df, aes(x = Year, y = DNA, color = group)) + geom_point(y = DNA_BEN, color = "red") +geom_point(y = DNA_KATY, color = 'blue')
dev.off()
DC <- df$Date_Collected[DOWNSTAIRS]
DOWNSTAIRS <- df$Lab == "Downstairs"
DC <- df$Date_Collected[DOWNSTAIRS]
BEN_DOWNSTAIRS <- df$DNA_Concentration_Ben[DOWNSTAIRS]
plot(x = DC, y = BEN_DOWNSTAIRS)
ggplot(data = df, aes(x = DC, y = BEN_DOWNSTAIRS)) + geom_point()
DC
DOWNSTAIRS
BEN_DOWNSTAIRS

NDC <- as.POSIXct(DC, TZ = "", format("%Y%m%d"))
plot(x = NDC , y = BEN_DOWNSTAIRS)  
plot(x = NDC , y = BEN_DOWNSTAIRS, xlab = "Date Collected", ylab = "DNA Concentration Ben", main = "Downstairs")  


jpeg("Ben_DNA_over_time.jpeg")
plot(x = NDC , y = BEN_DOWNSTAIRS, xlab = "Date Collected", ylab = "DNA Concentration Ben", main = "Downstairs")  
dev.off()



