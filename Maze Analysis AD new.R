## Power analysis?
## Simulated data


# Shannon McWaters, Jack-Morgan Mizell, Ren Calabro, Kiara Alexis Casas, Robert C Wilson, Anna Dornhaus
# (c) 2020-2026
# Directed Exploration in Bumble bees
# Data analysis & graphs 

# Packages used ---------------------
library(tidyverse) # Data handling/converting
library(lme4) # Linear models
library(ggeffects)
library(patchwork) # For combining graphs
library(sjPlot) # For nice table output
library(viridis)

colorsbees <- viridis(length(unique(MazeData$Bee)), begin = 0.2, alpha = 0.5)
colorsconc <- c("seagreen", "lightgreen", "white")


# Importing data for experiment 1 directly from github -----------------------
MazeData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/refs/heads/main/Maze%20Data%20Raw")

# Data formatting ---------------------------------

# Adding a column 'HighInfoChoice' indicating which choice (flower type) the bees
# had more information about. In the 'EQ' treatment, bees have the same amount of
# information about both types. 
####!!!!!! Is HighInfo 'High information' or 'High informativeness'? These are opposites ------------
MazeData <- MazeData %>%
  mutate(HighInfoChoice = case_when(
    Info_Treatment == "HH" & Choice1HV == "0" ~ "0",
    Info_Treatment == "HH" & Choice1HV == "1" ~ "1",
    Info_Treatment == "HL" & Choice1HV == "0" ~ "1",
    Info_Treatment == "HL" & Choice1HV == "1" ~ "0",
    Info_Treatment == "EQ" & Choice1HV == "0" ~ "EQ",
    Info_Treatment == "EQ" & Choice1HV == "1" ~ "EQ"
  ))

# When bees have more information about one flower type, we define the concentration
# difference as (High Information - Low Information) 
MazeData <- MazeData %>%
    mutate(HighInfoConcentrationDiff = ifelse(Info_Treatment == "HL", -ConcentrationDifference, ifelse(Info_Treatment == "EG", NA, ConcentrationDifference)))

# We also calculate the concentration difference relative to the flower on the right,
# as well as the information they have about the flower on the right
MazeData <- MazeData %>%
  mutate(RightConcentrationDiff = ifelse(Choice1HV == "1" & Side == "Right" | Choice1HV == "0" & Side == "Left", ConcentrationDifference, -ConcentrationDifference)) %>%
  mutate(chose_R = ifelse(Side == "Right", "1", "0")) %>%
  mutate(RightInfo = ifelse(HighInfoChoice == "EQ", "EQ", ifelse(HighInfoChoice == "1"&Side == "Right" | HighInfoChoice == "0"& Side =="Left","HighInfo","LowInfo")))
MazeData <- MazeData %>%
  mutate(
    Familiarity = case_when(
      RightInfo == "EQ" ~ 0,
      RightInfo == "HighInfo" ~ -0.5,
      RightInfo == "LowInfo" ~ 0.5,
      TRUE ~ NA_real_  # for safety in case of unexpected values
    )
  )
# Recoding 'High Informativeness' as 'Low Familiarity' and vice versa
MazeData <- MazeData %>%
  mutate(chose_R_numeric = as.numeric(chose_R)) %>%
  mutate(RightFamiliarity = factor(RightInfo,
                       levels = c("HighInfo", "EQ", "LowInfo"),
                       labels = c("Low Familiarity", "Equal", "High Familiarity"))
  )

# Plot A: Logistic regression curves by familiarity
# p1 <- 
ggplot(MazeData, aes(x = RightConcentrationDiff, y = chose_R_numeric)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4, size = 2.5) +  # increased size from 1 to 2.5
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_wrap(~ RightFamiliarity) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.4) +
  labs(
    x = "Relative Value of Right Flower",
    y = "Probability of Going Right"
  ) +
  theme_minimal(base_size = 14)

# Experimenting with distinguishing by bee and/or adding bee averages
# x-axis values can only be -1.75, -0.75, + 0.75, or +1.75
plot(jitter(chose_R_numeric, factor = 0.2) ~ jitter(RightConcentrationDiff, factor = 0.2)
     , data = MazeData
     , col = colorsbees[as.factor(MazeData$Bee)]
     , pch = 19)

# Reverse plot
ggplot(MazeData, aes(x = as.numeric(RightFamiliarity), y = chose_R_numeric)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4, size = 2.5) +  # increased size from 1 to 2.5
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_wrap(~ RightConcentrationDiff) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.4) +
  labs(
    x = "Relative Familiarity of Right Flower",
    y = "Probability of Going Right"
  ) +
  theme_minimal(base_size = 14)

# Summarize per-bee average choice
bee_summary <- MazeData %>%
  group_by(Bee) %>%
  summarise(
    mean_chose_R = mean(chose_R_numeric, na.rm = TRUE)
    #, add an equal vs unequal column?
  )

# Plot B: Boxplot of mean right choices per bee
p2 <- ggplot(bee_summary, aes(x = Familiarity, y = mean_chose_R)) +
  geom_boxplot(fill = "#a6bddb", alpha = 0.8, width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  labs(
    x = "Familiarity of Right Flower",
    y = "Proportion of Choices to the Right"
  ) +
  theme_minimal(base_size = 14)

# Combine plots side by side
p1 
/ p2


LRmod <- glmer(chose_R_numeric~ RightConcentrationDiff + RightInfo + (1|Bee), family = binomial, data = MazeData)
summary(LRmod)

LRmod_int <- glmer(chose_R_numeric~ RightConcentrationDiff * Horizon + RightInfo + (1|Bee), family = binomial, data = MazeData)
summary(LRmod_int)
LRmod <- glmer(chose_R_numeric~ RightInfo + RightConcentrationDiff * as.factor(Horizon)+ (1|Bee), family = binomial, data = MazeData)
summary(LRmod)

#glm for choosing high info in unequal info treatments
uneq_mod = glmer(as.numeric(HighInfoChoice) ~ HighInfoConcentrationDiff + as.factor(Horizon) + (1|Bee), family = binomial, data = MazeData)
summary(uneq_mod)








LRdata_clean <- LRdata %>%
  filter(!is.na(RightConcentrationDiff), !is.na(chose_R_numeric)) %>%
  mutate(
    Horizon = as.factor(Horizon),
    Info_Treatment = factor(Info_Treatment, levels = c("HH", "HL", "EQ"))
  )
# Plot
#raw data
ggplot(LRdata_clean, aes(x = RightConcentrationDiff, y = chose_R_numeric)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.4, size = 1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 0.4) +
  facet_wrap(~ RightInfo) +
  labs(
    x = "Relative Value of Right Flower",
    y = "Probability of Choosing Right Flower"
  ) +
  theme_minimal(base_size = 14)
#model based predictions
mod <- glm(chose_R_numeric ~ RightConcentrationDiff + Horizon + RightInfo, 
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
ggplot(preds_inf, aes(x = -x, y = 1 - predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - conf.high, ymax = 1 - conf.low, fill = group), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Relative Value of the Familiar Flower",
    y = "Probability of Choosing the Familiar Flower",
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


# Use tab_model to produce a well-formatted HTML/Word/LaTeX table
tab_model(LRmod,
          show.re.var = TRUE,
          pred.labels = c("Intercept",
                          "Low Familiarity (Right flower)",
                          "High Familiarity (Right flower)",
                          "Relative Reward of Right Flower",
                          "Long Horizon",
                          "Reward × Horizon Interaction"),
          dv.labels = "Maze Task – Choosing Right Flower")
tab_model(LRmod_int,
          show.re.var = TRUE,
          pred.labels = c("Intercept",
                          "Low Familiarity (Right flower)",
                          "High Familiarity (Right flower)",
                          "Relative Reward of Right Flower",
                          "Long Horizon",
                          "Reward × Horizon Interaction"),
          dv.labels = "Maze Task – Choosing Right Flower")


tab_model(
  uneq_mod,  
  show.re.var = TRUE,
  pred.labels = c(
    "Intercept",
    "Relative Reward of Informative Flower",
    "Long Horizon"
  ),
  dv.labels = "Maze Task – Choice of Informative Flower (Unequal Info)"
)


