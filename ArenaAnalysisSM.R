library(dplyr)
library(tidyverse)
library(ggplot2)
ArenaData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/refs/heads/main/Arena%20Data%20raw")

#####################################################################
#
#
#STOP HERE: 
# Consider the following options, which affect all subsequent calculations
# and graphs.
# Should 'problematic' bees be excluded? 
ArenaData2 <- subset(ArenaData, Bee!="Yellow19" & Bee!="Green83" & Bee!="Yellow46" & Bee!="Green57")
# Alternatively see section D.XIII.
#
# How should revisits to the same individual be counted?
#revisitoption <- "no reward" # counted as a visit with 0 reward
# revisitoption <- "not counted" # not counted as visit at all
# revisitoption <- "full" # counted as visit with full (intended) reward
#
#### data wrangling ####
#Filter for columns we will actually use here
FilteredArena <- ArenaData2 %>%
  select(Bee,Horizon,Order,Session,Condition,VideoReviewedChoices,Rewards)
#Make long version of data where each choice is a row
Choice <- strsplit(as.character(FilteredArena$VideoReviewedChoices), split=",")
Choices<-unlist(Choice)
Order <- rep(FilteredArena$Order, sapply(Choice, length))
Flowervalue <- strsplit(FilteredArena$Rewards, split=",")
Reward <- unlist(Flowervalue)
Bee <- rep(FilteredArena$Bee, sapply(Choice, length))
Horizon <- rep(FilteredArena$Horizon, sapply(Choice, length))
Session <- rep(FilteredArena$Session, sapply(Choice, length))
Condition <- rep(FilteredArena$Condition, sapply(Choice, length))

#Combine to form new dataset
ArenaDataLong <- data.frame(Bee,Horizon,Order,Session,Condition,Choices,Reward)


# Add new column for Flower A vs. B
ArenaDataLong <- ArenaDataLong %>%
  mutate(
    FlowerID = case_when(
      Session == "Test" & Choices %in% c("1", "3", "6", "8", "14", "16") ~ "A",
      Session == "Sample" & Choices %in% c("1", "2", "4", "5") ~ "A",
      TRUE ~ "B"
    )
  )
#Add a column that marks the number choice per bout
ArenaDataLong <- ArenaDataLong %>%
  group_by(Bee,Order, Session) %>%
  mutate(ChoiceNumber = row_number()) %>%
  ungroup()
#Marks choice number per horizon
ArenaDataLong <- ArenaDataLong %>%
  arrange(Bee, Order, Session, ChoiceNumber) %>%  # Ensure data is ordered correctly
  group_by(Bee, Order) %>%
  mutate(HorizonChoiceNum = row_number()) %>%       # Generate a continuous choice number within each Horizon
  ungroup()
# Create the RewardRepeat column
ArenaDataLong <- ArenaDataLong %>%
  group_by(Bee, Order, Session, Choices) %>%
  mutate(
    RewardRepeat = ifelse(row_number() == 1, Reward, 0) # Reward only for the first visit to each flower
  ) %>%
  ungroup()

ArenaDataLong <- ArenaDataLong %>%
  mutate(
    RewardRepeat = as.numeric(RewardRepeat)   # Convert Informative to numeric
  )
ArenaDataLong <- ArenaDataLong %>%
  mutate(
    Reward = na_if(Reward, ""),           # Convert empty strings to NA
    Reward = na_if(Reward, "NA"),         # Convert "NA" strings to actual NA
    Reward = as.numeric(Reward)           # Convert to numeric
  )
ArenaDataLong <- ArenaDataLong %>%
  arrange(Bee, Order, HorizonChoiceNum) %>%
  
  # Step 1: Identify first visits to each flower
  group_by(Bee, Order, FlowerID) %>%
  mutate(FirstVisit = !duplicated(HorizonChoiceNum)) %>%
  ungroup() %>%
  
  # Step 2: Count unique visits to each flower type
  group_by(Bee, Order) %>%
  mutate(
    UniqueVisits_A = cumsum((FlowerID == "A") * FirstVisit),
    UniqueVisits_B = cumsum((FlowerID == "B") * FirstVisit)
  ) %>%
  ungroup() %>%
  select(-FirstVisit) %>%
  
  # Step 3: Compute running reward averages based on unique visits only
  group_by(Bee, Order) %>%
  mutate(
    UniqueRunSum_A = lag(cumsum((Reward * (FlowerID == "A")) * (UniqueVisits_A > lag(UniqueVisits_A, default = 0))), default = 0),
    UniqueRunCount_A = lag(UniqueVisits_A, default = 0),
    UniqueRunAvg_A = ifelse(UniqueRunCount_A > 0, UniqueRunSum_A / UniqueRunCount_A, NA),
    
    UniqueRunSum_B = lag(cumsum((Reward * (FlowerID == "B")) * (UniqueVisits_B > lag(UniqueVisits_B, default = 0))), default = 0),
    UniqueRunCount_B = lag(UniqueVisits_B, default = 0),
    UniqueRunAvg_B = ifelse(UniqueRunCount_B > 0, UniqueRunSum_B / UniqueRunCount_B, NA)
  ) %>%
  ungroup() %>%
  select(-UniqueRunSum_A, -UniqueRunCount_A, -UniqueRunSum_B, -UniqueRunCount_B)

ArenaDataLong <- ArenaDataLong %>%
  arrange(Bee, Order, HorizonChoiceNum) %>%
  group_by(Bee, Order) %>%
  mutate(
    FlowerA_Visits = cumsum(FlowerID == "A"),
    FlowerB_Visits = cumsum(FlowerID == "B")
  ) %>%
  ungroup()

FirstChoice <- ArenaDataLong %>%
  filter(Session == "Test", ChoiceNumber == 1)
FirstChoice <- FirstChoice %>%
  mutate(TrueInformative = case_when(
    FlowerA_Visits > FlowerB_Visits ~ "B",     # More visits to A → B is less familiar
    FlowerA_Visits < FlowerB_Visits ~ "A",     # More visits to B → A is less familiar
    FlowerA_Visits == FlowerB_Visits ~ "Equal" # Equal visits → neither is more informative
  ))
FirstChoice <- FirstChoice %>%
  mutate(HighValueFlower = case_when(
    UniqueRunAvg_A > UniqueRunAvg_B ~ "A",
    UniqueRunAvg_A < UniqueRunAvg_B ~ "B",
    UniqueRunAvg_A == UniqueRunAvg_B ~ "Equal"
  ))
FirstChoice$chose_flowerA<- ifelse(FirstChoice$FlowerID == "A", 1, 0)
FirstChoice <- FirstChoice %>%
  mutate(
    # Proportional value difference from Flower A’s perspective
    PropValDiff_FlowerA = (UniqueRunAvg_A - UniqueRunAvg_B) / (UniqueRunAvg_A + UniqueRunAvg_B),
    
    # Proportional visit difference from Flower A’s perspective
    RelativeFamiliarity_FlowerA = (FlowerA_Visits - FlowerB_Visits) / (FlowerA_Visits + FlowerB_Visits)
  )
FirstChoice <- FirstChoice %>%
  mutate(
    # Value difference from informative flower's perspective
    PropValDiff = case_when(
      TrueInformative == "A" ~ (UniqueRunAvg_A - UniqueRunAvg_B) / (UniqueRunAvg_A + UniqueRunAvg_B),
      TrueInformative == "B" ~ (UniqueRunAvg_B - UniqueRunAvg_A) / (UniqueRunAvg_A + UniqueRunAvg_B),
      TrueInformative == "Equal" ~ 0,
      TRUE ~ NA_real_
    ),
    # Proportion of visits to informative flower
    RelativeFamiliarity = case_when(
      TrueInformative == "A" ~ (FlowerA_Visits - FlowerB_Visits) / (FlowerA_Visits + FlowerB_Visits),
      TrueInformative == "B" ~ (FlowerB_Visits - FlowerA_Visits) / (FlowerA_Visits + FlowerB_Visits),
      TrueInformative == "Equal" ~ 0,
      TRUE ~ NA_real_
    ),
    # Value difference from high-value flower's perspective
    PropValDiff_Value = case_when(
      HighValueFlower == "A" ~ (UniqueRunAvg_A - UniqueRunAvg_B) / (UniqueRunAvg_A + UniqueRunAvg_B),
      HighValueFlower == "B" ~ (UniqueRunAvg_B - UniqueRunAvg_A) / (UniqueRunAvg_A + UniqueRunAvg_B),
      HighValueFlower == "Equal" ~ 0,
      TRUE ~ NA_real_
    ),
    # Proportion of visits to high-value flower
    RelativeFamiliarity_Value = case_when(
      HighValueFlower == "A" ~ (FlowerA_Visits - FlowerB_Visits) / (FlowerA_Visits + FlowerB_Visits),
      HighValueFlower == "B" ~ (FlowerB_Visits - FlowerA_Visits) / (FlowerA_Visits + FlowerB_Visits),
      HighValueFlower == "Equal" ~ 0,
      TRUE ~ NA_real_
    )
  )

FirstChoice <- FirstChoice %>%
  mutate(
    ChoseInformative = ifelse(FlowerID == TrueInformative, 1, 0),
    ChoseHighValue = ifelse(FlowerID == HighValueFlower, 1, 0)
  )
####MAIN TEST: Do bees care about information difference?####

# In their first choice in the test phase, is the bee's choice
# predicted at all by information, or only by value difference?
FlowerAmod = glm(chose_flowerA ~ PropValDiff_FlowerA + RelativeFamiliarity_FlowerA + as.factor(Horizon), family = binomial, data = FirstChoice)
summary(FlowerAmod)
#For info choice - use unequal info only
FirstChoice_unequal <- FirstChoice %>%
  filter(TrueInformative != "Equal") %>%
  mutate(
    Horizon = factor(Horizon),
    Order = factor(Order)
  )
infomod = glm(ChoseInformative ~ Horizon + Order + PropValDiff, family = binomial, data = FirstChoice_unequal)
summary(infomod)
valuemod = glm(ChoseHighValue ~ Horizon + Order +RelativeFamiliarity_Value, family = binomial, data = FirstChoice)
summary(valuemod)

####Plots####
#Goal: Show how value difference and informational difference predict preference for Flower A.
ggplot(FirstChoice, aes(x = RelativeFamiliarity_FlowerA, y = chose_flowerA, color = as.factor(Horizon))) +
  geom_jitter(height = 0.05, width = 0.02, alpha = 0.4) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  geom_vline(xintercept = 0.0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.4) +
  labs(x = "Relative Familiarity of flower Flower A",
       y = "Probability of Choosing Flower A",
       color = "Horizon") +
  theme_minimal()

#Show how ChoseInformative varies across Horizon, Order, and  Value Difference.
ggplot(FirstChoice_unequal, aes(x = as.factor(Order), y = ChoseInformative, fill = as.factor(Horizon))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.6, position = position_dodge(width = 0.7)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.7),
              shape = 21, alpha = 0.5, size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(x = "Test Bout (Order)", y = "Chose Informative Flower", fill = "Horizon") +
  theme_minimal()

summary_bar <- FirstChoice_unequal %>%
  mutate(InformativeChoice = ifelse(ChoseInformative == 1, "Informative", "Uninformative")) %>%
  group_by(Order, Horizon, InformativeChoice) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Order, Horizon) %>%
  mutate(prop = n / sum(n))
ggplot(summary_bar, aes(x = factor(Order), y = prop, fill = InformativeChoice)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Horizon, labeller = label_both) +  # Adds "Horizon = 2", etc.
  scale_fill_manual(values = c("Informative" = "#3690c0", "Uninformative" = "#a6bddb")) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  labs(
    x = "Test Bout (Order)",
    y = "Proportion of Choices",
    fill = "Choice"
  ) +
  theme_minimal(base_size = 14)
