library(dplyr)
library(tidyverse)
library(lme4)
library(pscl)
library(emmeans)
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

unequal <- MazeData[MazeData$Info_Treatment %in% c("HH", "HL"), ]
equal <- MazeData[MazeData$Info_Treatment %in% "EQ", ]
unequal <- unequal %>%
  mutate(NewConcentrationDiff = ifelse(Info_Treatment == "HL", -ConcentrationDifference, ConcentrationDifference))
unequal <- unequal %>%
  mutate(HighInfoChoice_num = as.numeric(HighInfoChoice)) %>%
  group_by(Bee) %>%
  mutate(mean_HighInfoChoice = mean(HighInfoChoice_num, na.rm = TRUE)) %>%
  ungroup()
#make data frame for looking at chose left or right
LRdata <- MazeData
LRdata <- LRdata %>%
  mutate(RightConcentrationDiff = ifelse(Choice1HV == "1" & Side == "Right" | Choice1HV == "0" & Side == "Left", ConcentrationDifference, -ConcentrationDifference)) %>%
  mutate(chose_R = ifelse(Side == "Right", "1", "0")) %>%
  mutate(RightInfo = ifelse(HighInfoChoice == "EQ", "EQ", ifelse(HighInfoChoice == "1"&Side == "Right" | HighInfoChoice == "0"& Side =="Left","HighInfo","LowInfo")))
LRdata <- LRdata %>%
  mutate(chose_R_numeric = as.numeric(chose_R)) %>%  # step 1
  group_by(Bee) %>%
  mutate(mean_chose_R = mean(chose_R_numeric, na.rm = TRUE)) %>%  # step 2 & 3
  ungroup()
LRmod = glm(mean_chose_R~ RightConcentrationDiff + RightInfo, data = LRdata)
summary(LRmod)
library(emmeans)

# Estimated marginal means for RightInfo
emm <- emmeans(LRmod, ~ RightInfo)

# Pairwise comparisons with Tukey adjustment
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")

summary(pairwise_comparisons)


#glm for choosing high info in unequal info treatments
uneq_mod = glm(mean_HighInfoChoice ~ NewConcentrationDiff + as.factor(Horizon), data = unequal)
summary(uneq_mod)



#Testing choice of high or low value against horizon in equal 
chisq.test(table(equal$Choice1HV,equal$Horizon))

#Plots!!
#UNEQUAL
#Chosing high info vs Horizon
prop_data_unequal <- unequal %>%
  group_by(Horizon, HighInfoChoice) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Horizon) %>%
  mutate(Proportion = Count / sum(Count))  # Normalize to 100%
ggplot(prop_data_unequal, aes(x = as.factor(Horizon), y = Proportion, fill = as.factor(HighInfoChoice))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "black") +  # Add dashed line at 0.50
  scale_fill_manual(values = c("#a6bddb", "#3690c0"), labels = c("No", "Yes")) +  # Muted blue tones
  labs(x = "Horizon", y = "Proportion of Choices", fill = "Chose High Info") +
  theme_minimal()
#Boxplot with horizon and conc diff
unequal$HighInfoChoice <- as.factor(unequal$HighInfoChoice)
unequal$Horizon <- as.factor(unequal$Horizon)
ggplot(unequal, aes(x = NewConcentrationDiff, y = HighInfoChoice, fill = Horizon)) +
  geom_boxplot() +
  labs(x = "Concentration Difference", y = "Chose Informative", fill = "Horizon") +
  theme_minimal()

#EQUAL
#chose high val vs horizon
prop_data <- equal %>%
  group_by(Horizon, Choice1HV) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Horizon) %>%
  mutate(Proportion = Count / sum(Count))  # Normalize to 100%
ggplot(prop_data, aes(x = as.factor(Horizon), y = Proportion, fill = as.factor(Choice1HV))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "black") +  # Add dashed line at 0.50
  scale_fill_manual(values = c("#a6bddb", "#3690c0"), labels = c("No", "Yes")) +  # Muted blue tones
  labs(x = "Horizon", y = "Proportion of Choices", fill = "Chose High Value") +
  theme_minimal()
