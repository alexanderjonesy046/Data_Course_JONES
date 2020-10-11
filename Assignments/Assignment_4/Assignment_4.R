df <- read.csv("../../Data/landdata-states.csv")

head(df)

class(df$State)

class(df$date)

dim(df)
str(df)
summary(df)
summary(df$Home.Value)
names(df)[4]

hist(df$Land.Value)

plot(x = df$Year, y = df$Land.Value,)
df2 <-read.csv("../../Data/ITS_mapping.csv", sep = "\t") 

head(df2)

summary(df2)[1:15]

class(df2$BarcodeSequence)

str(df2)

boxplot(df2$Ecosystem~df2$Lat)

plot(x = df2$Ecosystem, y = df2$Lat)
class(df2$Ecosystem)
ECO <- as.factor(as.character(df2$Ecosystem))
plot(x = ECO , y = df2$Lat, xlab = "Ecosystem", ylab = "Latitude",)
class(ECO)

png(filename = "silly_boxplot.png")
plot(x = ECO , y = df2$Lat, xlab = "Ecosystem", ylab = "Latitude",)
dev.off()
