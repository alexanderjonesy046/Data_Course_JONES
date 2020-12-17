#Load Packages####
library(tidyr)
library(tidyverse)
#Load MTCARS####
data("mtcars")
original <- mtcars
mtcars2 <- mtcars
#Subset Data Set####
mtcars <- filter(mtcars, am == 1)
write.csv(mtcars, "automatic_mtcars.csv" )

#Plot the effect of HP on MPG####

ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point() + geom_smooth(method = "lm") +
  xlab("Horsepower") + ylab("Miles Per Gallon") + 
  labs(title = "Miles Per Gallon vs Horsepower") +
  theme_minimal()

ggsave("mpg_vs_hp_auto.png", last_plot(), device = "png")


#Plot the effect of weight on miles per gallon####

ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + geom_smooth(method = "lm") +
  xlab("Weight") + ylab("Miles Per Gallon") + 
  labs(title = "Miles Per Gallon vs Weight") +
  theme_minimal()

ggsave("mpg_vs_wt_auto.tiff", last_plot(), device = "tiff")


#Subset the original mtcars again for displacements less than or = to 200####
mtcars2 <- filter(mtcars2, disp <= 200)

write.csv(mtcars2, "mtcars_max200_displ.csv" )

automax <- summarise(mtcars, max_automatic = max(hp))
omax <- summarise(original, max_original = max(hp))
dispmax <- summarise(mtcars2, max_200 = max(hp))

maxhp <- bind_cols(automax, omax, dispmax)

write.table(maxhp, file = "hp_maximum.txt")
