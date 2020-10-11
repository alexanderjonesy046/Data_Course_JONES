 data(iris)

df <- data(iris) 
library(tidyverse)

library(ggimage)
head(iris)
head(iris)


ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point(aes(color = Species)) + theme_minimal() +
   geom_smooth(aes(color = Species), method = "lm") + 
   labs(title = "Sepal length vs petal length", subtitle = "for three iris species")

 png("iris_fig1.png")                                                 
 ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) + geom_point(aes(color = Species)) + theme_minimal() +
   geom_smooth(aes(color = Species), method = "lm") + 
   labs(title = "Sepal length vs petal length", subtitle = "for three iris species")
 dev.off()
 
 
 ggplot(iris, aes(x = Petal.Width, color = Species, fill = Species)) + 
   geom_density(alpha = 0.5) +
   labs(title = "Distribution of Petal Widths", 
                                    subtitle = "for three iris species", x = "Petal Width")
 
 
 png("iris_fig2.png")
 ggplot(iris, aes(x = Petal.Width, color = Species, fill = Species)) + 
   geom_density(alpha = 0.5) +
   labs(title = "Distribution of Petal Widths", 
        subtitle = "for three iris species", x = "Petal Width")
 dev.off()

df$Petal.Width
 
 #ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, fill = Species )) + geom_boxplot(aes(color = Species)) + theme_minimal()
 
  
 P <- iris$Petal.Width/ iris$Sepal.Width
 
 
 
 ggplot(iris, aes(x = Species , y = P, fill = Species )) + 
   geom_boxplot() +theme_minimal() +
   labs(title = "Sepal- to Petal-Width Ratio", subtitle = "for three iris species", x = "Species", y = "Ratio of Sepal Width to Petal Width")
 
 png("iris_fig3.png") 
 ggplot(iris, aes(x = Species , y = P, fill = Species )) + 
   geom_boxplot() +theme_minimal() +
   labs(title = "Sepal- to Petal-Width Ratio", subtitle = "for three iris species", x = "Species", y = "Ratio of Sepal Width to Petal Width")
 dev.off()
 
 
 
 MEAN <- mean(iris$Sepal.Length)
 DIVERGE <- iris$Sepal.Length -MEAN
 
 
 ggplot(iris, aes(x = Sepal.Length , y = DIVERGE)) + theme_minimal() +
   geom_bar(stat= 'identity',aes( color = Species, fill = Species, width = 0.08), position = position_dodge(preserve = "total")) +
coord_flip() + labs(title = "Sepal Length deviance from the mean of all observations",
                     y = "Deviance From the Mean", x = "", 
                    caption = "Note: Deviance = Sepal.Length - mean(Sepal.Length)")

 
 
 
 png("iris_fig4.png")
 ggplot(iris, aes(x = Sepal.Length , y = DIVERGE)) + theme_minimal() +
   geom_bar(stat= 'identity',aes( color = Species, fill = Species, width = 0.08), position = position_dodge(preserve = "total")) +
   coord_flip() + labs(title = "Sepal Length deviance from the mean of all observations",
                       y = "Deviance From the Mean", x = "", 
                       caption = "Note: Deviance = Sepal.Length - mean(Sepal.Length)")
 dev.off()
 
 
 
 