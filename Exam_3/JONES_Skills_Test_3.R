#Exam 3 Skills Test


#PART 1

#Read In Packages
library(tidyverse)
library(tidyr)
library(broom)


#Read In Data

df <- read.csv("FacultySalaries_1995.csv")

#Observe Data####
str(df)
names(df)
#Clean Data Salary####

dfc <- df %>%  arrange(Tier)
dfc <- dfc %>%  rename( "Full"= "AvgFullProfSalary", 
                        "Assoc"= "AvgAssocProfSalary",
                        "Assist" = "AvgAssistProfSalary")
Full<- dfc %>% select(c(1:5)) %>% mutate("Rank" = "Full") %>% 
  rename("Salary" = "Full")


Assist <- dfc %>% select(c(1:4,7)) %>% mutate("Rank" = "Assist") %>% 
  rename("Salary" = "Assist")


Assoc<- dfc %>%  select(c(1:4,6)) %>% mutate("Rank" = "Assoc")%>% 
  rename("Salary" = "Assoc")


#Recombine####

CLEAN <- bind_rows(Assoc,Assist,Full)
CLEAN$Type <- as.factor(CLEAN$Type)
CLEAN$State <- as.factor(CLEAN$State)
GGCLEAN <- CLEAN %>% filter(Tier!="VIIB")

#Visualize Salary####
fig1 <-ggplot(GGCLEAN, aes(x=Rank, y=Salary, fill=Rank)) + geom_boxplot() +
  facet_wrap(. ~Tier) +theme_minimal()
#Export
jpeg("JONES_Fig_1.jpg")
fig1
dev.off()
#ANOVA####

ANOVA <- aov(Salary ~ State + Rank + Tier, data = CLEAN)
summary(ANOVA)
#Export
CAP <- summary(ANOVA)
capture.output(CAP, file = "Salary_ANOVA_Summary.txt")


#PART 2####

#Import Data

df2 <- read.csv("Juniper_Oils.csv")
#Clean Oil####
CHEM <- df2[c("alpha.pinene","para.cymene","alpha.terpineol",
              "cedr.9.ene","alpha.cedrene","beta.cedrene",
              "cis.thujopsene","alpha.himachalene","beta.chamigrene",
              "cuparene","compound.1","alpha.chamigrene","widdrol",
              "cedrol","beta.acorenol","alpha.acorenol","gamma.eudesmol",
              "beta.eudesmol","alpha.eudesmol","cedr.8.en.13.ol",
              "cedr.8.en.15.ol","compound.2","thujopsenal", "YearsSinceBurn")]

CHEM <- CHEM %>% gather("ChemicalID","Concentration", 1:23)

#Visualize Oil####

ggplot(data = CHEM, aes(x= YearsSinceBurn, y= Concentration)) +geom_smooth() +
  facet_wrap(facets = "ChemicalID", scales = "free")
  

fig2 <- ggplot(data = CHEM, aes(x= YearsSinceBurn, y= Concentration)) +geom_smooth() +
  facet_wrap(facets = "ChemicalID", scales = "free")

#Export

jpeg("JONES_Fig_2.jpg")
fig2
dev.off()

#Linear Model####

math <- CHEM$Concentration/CHEM$YearsSinceBurn

model <- CHEM %>% glm(formula = math~ ChemicalID,
                      family = gaussian())
summary(model)

tidy(model, subset(p<0.05))
?tidy







