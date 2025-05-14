library(dplyr)
library(tidyverse)
library(lme4)
library(pscl)
MazeData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/refs/heads/main/Maze%20Data%20Raw")


MazeData <- MazeData %>%
  mutate(HighInfoChoice = case_when(
    Info_Treatment == "HH" & Choice1HV == "0" ~ "0",
    Info_Treatment == "HH" & Choice1HV == "1" ~ "1",
    Info_Treatment == "HL" & Choice1HV == "0" ~ "1",
    Info_Treatment == "HL" & Choice1HV == "1" ~ "0",
    Info_Treatment == "EQ" & Choice1HV == "0" ~ "EQ",
    Info_Treatment == "EQ" & Choice1HV == "1" ~ "EQ"
  ))
#mazemod = glm(HighInfoChoice ~ Info_Treatment +factor(Horizon), family = binomial (link="logit"), data = MazeData)
#summary(mazemod)
#pR2(mazemod)

#predicted_probs <- predict(mazemod, newdata = MazeData, type = "response")

#ggplot(MazeData, aes(x = factor(Info_Treatment), y = predicted_probs, fill = factor(Horizon))) +
  #geom_boxplot() +
  #labs(x = "Treatment", y = "Probability of Choosing High Value Flower") +
  #theme_minimal()

MazeData_condensed <- MazeData %>%
 group_by(Bee, Info_Treatment, Horizon, ConcentrationDifference) %>%
  summarise(
    Avg_Choice1HV = mean(Choice1HV),
   .groups = 'drop'  
  )

chisq.test(table(MazeData$Info_Treatment,MazeData$Choice1HV))

mazemod <- glm(Choice1HV ~ as.factor(Info_Treatment) + as.factor(Horizon)+ as.factor(ConcentrationDifference), 
                    family = binomial(link = "logit"), 
                    data = MazeData)
summary(mazemod)
pR2(mazemod)


mazemod2 = glm(Choice1HV ~ as.factor(Info_Treatment) + as.factor(Horizon)+ as.factor(ConcentrationDifference), family = binomial, data = MazeData)
summary(mazemod2)
pR2(mazemod2)

Anova = aov(Avg_Choice1HV ~ as.factor(Info_Treatment), data= MazeData_condensed)
summary(Anova)
leveneTest(Avg_Choice1HV ~ as.factor(Info_Treatment),
           data = MazeData_condensed
)
shapiro.test(Anova$residuals)

ggplot(MazeData, aes(x = factor(Info_Treatment), y = Choice1HV)) +
  geom_boxplot() +
  labs(x = "Treatment", y = "Probability of Choosing High Value Flower") +
  theme_minimal()
#look at unequal info only
Maze_subset <- MazeData %>%
  filter(HighInfoChoice != "EQ")
mazemod3 = glm(as.numeric(HighInfoChoice)~ Info_Treatment + factor(Horizon), family = binomial (link="logit"),data = Maze_subset)
summary(mazemod3)

