library(dplyr)
library(tidyverse)
library(lme4)
library(pscl)
library(emmeans)
library(ggeffects)
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
  mutate(chose_R_numeric = as.numeric(chose_R))
LRmod = glmer(chose_R_numeric~ RightConcentrationDiff + RightInfo + Horizon + (1|Bee), family = binomial, data = LRdata)
summary(LRmod)


#glm for choosing high info in unequal info treatments
uneq_mod = glm(as.numeric(HighInfoChoice) ~ NewConcentrationDiff + as.factor(Horizon), family = binomial, data = unequal)
summary(uneq_mod)


#Plots!!
LRdata_clean <- LRdata %>%
  filter(!is.na(RightConcentrationDiff), !is.na(chose_R_numeric)) %>%
  mutate(
    Horizon = as.factor(Horizon),
    Info_Treatment = factor(Info_Treatment, levels = c("HH", "HL", "EQ"))
  )
# Plot
#raw data
ggplot(LRdata_clean, aes(x = RightConcentrationDiff, y = chose_R_numeric, color = Horizon)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.4, size = 1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 0.4) +
  facet_wrap(~ RightInfo) +
  labs(
    x = "Relative Value of Right Flower",
    y = "Probability of Choosing Right Flower",
    color = "Horizon"
  ) +
  theme_minimal(base_size = 14)
#model based predictions
mod <- glm(chose_R_numeric ~ RightConcentrationDiff * Horizon + RightInfo, 
           data = LRdata_clean, family = binomial)

preds <- ggpredict(mod, terms = c("RightConcentrationDiff", "Horizon", "RightInfo"))

ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  facet_wrap(~ facet) +
  labs(x = "Relative Value of Right Flower",
       y = "Probability of Choosing Right Flower",
       color = "Horizon", fill = "Horizon") +
  theme_minimal(base_size = 14)


#Chosing high info vs Horizon
preds_inf <- ggpredict(uneq_mod, terms = c("NewConcentrationDiff", "Horizon"))
ggplot(preds_inf, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Relative Value of Informative Flower",
    y = "Probability of Choosing Informative Flower",
    color = "Horizon",
    fill = "Horizon"
  ) +
  theme_minimal(base_size = 14)

#Boxplot with horizon and conc diff
unequal$HighInfoChoice <- as.factor(unequal$HighInfoChoice)
unequal$Horizon <- as.factor(unequal$Horizon)
ggplot(unequal, aes(x = NewConcentrationDiff, y = HighInfoChoice, fill = Horizon)) +
  geom_boxplot() +
  labs(x = "Concentration Difference", y = "Chose Informative", fill = "Horizon") +
  theme_minimal()
