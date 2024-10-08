library(dplyr)
library(tidyverse)
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
    select(Bee,Horizon,Order,Session,Condition,InformativeColor,UninformativeColor,VideoReviewedChoices,Rewards,InformativeChoice..0.No.1.Yes.)
#Make long version of data where each choice is a row
Choice <- strsplit(as.character(FilteredArena$VideoReviewedChoices), split=",")
Choices<-unlist(Choice)
Order <- rep(FilteredArena$Order, sapply(Choice, length))
Flowervalue <- strsplit(FilteredArena$Rewards, split=",")
Reward <- unlist(Flowervalue)
InformativeChoice <- strsplit(FilteredArena$InformativeChoice..0.No.1.Yes., split=",")
Informative <- unlist(InformativeChoice)
Bee <- rep(FilteredArena$Bee, sapply(Choice, length))
Horizon <- rep(FilteredArena$Horizon, sapply(Choice, length))
Session <- rep(FilteredArena$Session, sapply(Choice, length))
Condition <- rep(FilteredArena$Condition, sapply(Choice, length))
InformativeColor <- rep(FilteredArena$InformativeColor, sapply(Choice, length))
UninformativeColor <- rep(FilteredArena$UninformativeColor, sapply(Choice, length))
#Combine to form new dataset
ArenaDataLong <- data.frame(Bee,Horizon,Order,Session,Condition,InformativeColor,UninformativeColor,Choices,Reward,Informative)
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
    RewardRepeat = as.numeric(RewardRepeat),    # Convert RewardRepeat to numeric
    Informative = as.numeric(Informative)       # Convert Informative to numeric
  )

# Now calculate RunningAvg1 and RunningAvg2
ArenaDataLong <- ArenaDataLong %>%
  arrange(Bee, Order, HorizonChoiceNum) %>%   # Ensure data is ordered properly
  group_by(Bee, Order) %>%                    # Group by Bee and Horizon (treatment)
  mutate(
    RunningSum1 = lag(cumsum(RewardRepeat * (Informative == 1)), default = 0),  # Running sum for flower type 1
    RunningCount1 = lag(cumsum(Informative == 1), default = 0),                 # Running count for flower type 1
    RunningAvg1 = ifelse(RunningCount1 > 0, RunningSum1 / RunningCount1, NA),   # Running avg for flower type 1
    RunningSum2 = lag(cumsum(RewardRepeat * (Informative == 0)), default = 0),  # Running sum for flower type 0
    RunningCount2 = lag(cumsum(Informative == 0), default = 0),                 # Running count for flower type 0
    RunningAvg2 = ifelse(RunningCount2 > 0, RunningSum2 / RunningCount2, NA)    # Running avg for flower type 0
  ) %>%
  ungroup() %>%
  select(-RunningSum1, -RunningCount1, -RunningSum2, -RunningCount2) # Remove intermediate columns

#Running averages but allowing repeats to count
ArenaDataLong <- ArenaDataLong %>%
  mutate(
    Reward = as.numeric(Reward),
    Informative = as.numeric(Informative))  %>%
  arrange(Bee, Order, HorizonChoiceNum) %>%   # Ensure data is ordered properly
  group_by(Bee, Order) %>%                    # Group by Bee and Horizon (treatment)
  mutate(
    RunSum1 = lag(cumsum(Reward * (Informative == 1)), default = 0),  # Running sum for flower type 1
    RunCount1 = lag(cumsum(Informative == 1), default = 0),                 # Running count for flower type 1
    RepeatRunAvg1 = ifelse(RunCount1 > 0, RunSum1 / RunCount1, NA),   # Running avg for flower type 1
    RunSum2 = lag(cumsum(Reward * (Informative == 0)), default = 0),  # Running sum for flower type 0
    RunCount2 = lag(cumsum(Informative == 0), default = 0),                 # Running count for flower type 0
    RepeatRunAvg2 = ifelse(RunCount2 > 0, RunSum2 / RunCount2, NA)    # Running avg for flower type 0
  ) %>%
  ungroup() %>%
  select(-RunSum1, -RunCount1, -RunSum2, -RunCount2) # Remove intermediate columns


#Add color chosen an color ignored
ArenaDataLong <- ArenaDataLong %>%
  mutate(
    ColorChosen = ifelse(Informative == 1, InformativeColor, UninformativeColor),
    ColorNotChosen = ifelse(Informative == 1, UninformativeColor, InformativeColor)
  )
#calculate true information for each flower type
ArenaDataLong <- ArenaDataLong %>%
  arrange(Bee, Order, HorizonChoiceNum) %>%  # Ensure the data is ordered correctly (Sample before Test)
  group_by(Bee, Order) %>%
  mutate(
    FlowerOneVisits = cumsum(Informative == 1),  # Cumulative sum of visits where Informative is 1
    FlowerTwoVisits = cumsum(Informative == 0)   # Cumulative sum of visits where Informative is 0
  ) %>%
  ungroup()

# Add the new column "FlowerChoice" based on the value of the "Informative" column
ArenaDataLong <- ArenaDataLong %>%
  mutate(FlowerChoice = case_when(
    Informative == 1 ~ "One",  # If Informative is 1
    Informative == 0 ~ "Two"   # If Informative is 0
  ))
####Analysis####

#MAIN TEST: Do bees care about information difference?

# In their first choice in the test phase, is the bee's choice
# predicted at all by information, or only by value difference?
#subset data fram so it's only first visits in test phase
FirstChoice <- ArenaDataLong %>%
  filter(Session == "Test", ChoiceNumber == 1)
#Make a row that says what flower is more informative - 
FirstChoice <- FirstChoice %>%
  mutate(TrueInformative = case_when(
    FlowerOneVisits > FlowerTwoVisits ~ "Two",  #More visits to FlowerOne
    FlowerOneVisits < FlowerTwoVisits ~ "One",  # More visits to FlowerTwo
    FlowerOneVisits == FlowerTwoVisits ~ "Equal"  # Equal visits to both
  ))


#Now for which flower is the true high value for REPEAT =0reward
FirstChoice <- FirstChoice %>%
  mutate(HighValueFlower = case_when(
    RunningAvg1 > RunningAvg2 ~ "One",    # If RunningAvg1 is greater than RunningAvg2
    RunningAvg1 < RunningAvg2 ~ "Two",    # If RunningAvg1 is less than RunningAvg2
    RunningAvg1 == RunningAvg2 ~ "Equal"  # If RunningAvg1 is equal to RunningAvg2
  ))
# Compute PropInfDiff and PropValDiff but make sure it represents the proportion change from the true informative option 
FirstChoice <- FirstChoice %>%
  mutate(
    # Compute PropValDiff 
    PropValDiff = case_when(
      TrueInformative == "One" ~ (RunningAvg1 - RunningAvg2) / RunningAvg1,
      TrueInformative == "Two" ~ (RunningAvg2 - RunningAvg1) / RunningAvg2,
      TrueInformative == "Equal" ~ 0,
      TRUE ~ NA_real_  # In case there are any unexpected values
    ),
    
    # Compute PropInfDiff if neg - they know more about the low info flower
    PropInfDiff = case_when(
      TrueInformative == "One" ~ (FlowerOneVisits - FlowerTwoVisits) / FlowerOneVisits,
      TrueInformative == "Two" ~ (FlowerTwoVisits - FlowerOneVisits) / FlowerTwoVisits,
      TrueInformative == "Equal" ~ 0,
      TRUE ~ NA_real_  # In case there are any unexpected values
    )
  )
#same thing but for value
FirstChoice <- FirstChoice %>%
  mutate(
    # Compute PropInfDiff
    PropValDiff_Value = case_when(
      HighValueFlower == "One" ~ (RunningAvg1 - RunningAvg2) / RunningAvg1,
      HighValueFlower == "Two" ~ (RunningAvg2 - RunningAvg1) / RunningAvg2,
      HighValueFlower == "Equal" ~ 0,
      TRUE ~ NA_real_  # In case there are any unexpected values
    ),
    
    # Compute PropValDiff if neg - they know more about the low value flower
    PropInfDiff_Value = case_when(
      HighValueFlower == "One" ~ (FlowerOneVisits - FlowerTwoVisits) / FlowerOneVisits,
      HighValueFlower == "Two" ~ (FlowerTwoVisits - FlowerOneVisits) / FlowerTwoVisits,
      HighValueFlower == "Equal" ~ 0,
      TRUE ~ NA_real_  # In case there are any unexpected values
    )
  )

#clean up some columns we no longer need 
FirstChoice_subset <- FirstChoice %>%
  select(Bee, Horizon, Order, FlowerChoice, ColorChosen, TrueInformative, 
         HighValueFlower, PropValDiff, PropInfDiff, PropInfDiff_Value, PropValDiff_Value,FlowerChoice)
#FOR NOW: remove equal info rows and make a new correct informative column (1 = chose informative option)
FirstChoice_subset <- FirstChoice_subset %>%
  filter(TrueInformative != "Equal") %>%  # Remove rows where TrueInformative is "Equal"
  mutate(ChoseInformative = ifelse(FlowerChoice == TrueInformative, 1, 0)) %>%
  mutate(Value = ifelse(FlowerChoice == HighValueFlower, 1, 0)) %>%
  mutate(HighInfoHighValue = ifelse(TrueInformative == HighValueFlower,1,0)) 
  
  

#Model of first choice in the test phase
firstchoicemod = glm(ChoseInformative ~ as.factor(Horizon) + as.factor(Order) + PropInfDiff + PropValDiff, family = binomial, data = FirstChoice_subset)
summary(firstchoicemod)
valuemod = glm(Value ~ as.factor(Horizon) + PropInfDiff_Value + PropValDiff_Value + as.factor(Order), family = binomial, data = FirstChoice_subset)
summary(valuemod)


#Plot results INFO
FirstChoice_subset$predicted_prob <- predict(firstchoicemod, newdata = FirstChoice_subset, type = "response")
ggplot(FirstChoice_subset, aes(x = PropInfDiff, y = predicted_prob, color = factor(Horizon))) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.6) +
  stat_smooth(method = "glm",se = FALSE) +
  labs(x = "Proportional Information Difference", y = "Predicted Probability of Choosing Informative Flower") +
  theme_minimal() 
#same plot as above but value diff on x instead of info
ggplot(FirstChoice_subset, aes(x = PropValDiff, y = predicted_prob, color = factor(Horizon))) +
  geom_jitter(width = 0, height = 0) +
  stat_smooth(method = "glm", se = FALSE) +
  labs(x = "Proportional Value Difference", y = "Predicted Probability of Choosing Informative Flower") +
  theme_minimal()
ggplot(FirstChoice_subset, aes(x = factor(Horizon), y = predicted_prob, fill = factor(HighInfoHighValue))) +
  geom_boxplot() +
  labs(x = "Horizon", y = "Predicted Probability of Choosing Informative Flower") +
  theme_minimal()
# Create the boxplot
ggplot(FirstChoice_subset, aes(x = factor(Order), y = predicted_prob, fill = factor(Horizon))) +
  geom_boxplot() +
  labs(x = "Order in which treatment was received", y = "Probability of Choosing Informative Flower") +
  theme_minimal()
#plots for VALUE
FirstChoice_subset$predicted_prob_value <- predict(valuemod, newdata = FirstChoice_subset, type = "response")
ggplot(FirstChoice_subset, aes(x = factor(HighInfoHighValue), y = predicted_prob_value, color = factor(Horizon))) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.6) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Proportional Information Difference", y = "Predicted Probability of Choosing High Value Flower") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) 


##############Let's assume they don't deplete flowers and so thye experience the nectar even on repeat visits
FirstChoiceRep <- ArenaDataLong %>%
  filter(Session == "Test", ChoiceNumber == 1)
#Make a row that says what flower is more informative - 
FirstChoiceRep <- FirstChoiceRep %>%
  mutate(TrueInformative = case_when(
    FlowerOneVisits > FlowerTwoVisits ~ "Two",  #More visits to FlowerOne
    FlowerOneVisits < FlowerTwoVisits ~ "One",  # More visits to FlowerTwo
    FlowerOneVisits == FlowerTwoVisits ~ "Equal"  # Equal visits to both
  ))


#Now for which flower is the true high value for REPEAT =0reward
FirstChoiceRep <- FirstChoiceRep %>%
  mutate(HighValueFlower = case_when(
    RepeatRunAvg1 > RepeatRunAvg2 ~ "One",    # If RunningAvg1 is greater than RunningAvg2
    RepeatRunAvg1 < RepeatRunAvg2 ~ "Two",    # If RunningAvg1 is less than RunningAvg2
    RepeatRunAvg1 == RepeatRunAvg2 ~ "Equal"  # If RunningAvg1 is equal to RunningAvg2
  ))
# Compute PropInfDiff and PropValDiff but make sure it represents the proportion change from the true informative option 
FirstChoiceRep <- FirstChoiceRep %>%
  mutate(
    # Compute PropValDiff 
    PropValDiff = case_when(
      TrueInformative == "One" ~ (RepeatRunAvg1 - RepeatRunAvg2) / RepeatRunAvg1,
      TrueInformative == "Two" ~ (RepeatRunAvg2 - RepeatRunAvg1) / RepeatRunAvg2,
      TrueInformative == "Equal" ~ 0,
      TRUE ~ NA_real_  # In case there are any unexpected values
    ),
    
    # Compute PropInfDiff if neg - they know more about the low info flower
    PropInfDiff = case_when(
      TrueInformative == "One" ~ (FlowerOneVisits - FlowerTwoVisits) / FlowerOneVisits,
      TrueInformative == "Two" ~ (FlowerTwoVisits - FlowerOneVisits) / FlowerTwoVisits,
      TrueInformative == "Equal" ~ 0,
      TRUE ~ NA_real_  # In case there are any unexpected values
    )
  )
#same thing but for value
FirstChoiceRep <- FirstChoiceRep %>%
  mutate(
    # Compute PropInfDiff
    PropValDiff_Value = case_when(
      HighValueFlower == "One" ~ (RepeatRunAvg1 - RepeatRunAvg2) / RepeatRunAvg1,
      HighValueFlower == "Two" ~ (RepeatRunAvg2 - RepeatRunAvg1) / RepeatRunAvg2,
      HighValueFlower == "Equal" ~ 0,
      TRUE ~ NA_real_  # In case there are any unexpected values
    ),
    
    # Compute PropValDiff if neg - they know more about the low value flower
    PropInfDiff_Value = case_when(
      HighValueFlower == "One" ~ (FlowerOneVisits - FlowerTwoVisits) / FlowerOneVisits,
      HighValueFlower == "Two" ~ (FlowerTwoVisits - FlowerOneVisits) / FlowerTwoVisits,
      HighValueFlower == "Equal" ~ 0,
      TRUE ~ NA_real_  # In case there are any unexpected values
    )
  )

#clean up some columns we no longer need 
FirstChoiceRep_subset <- FirstChoiceRep %>%
  select(Bee, Horizon, Order, FlowerChoice, ColorChosen, TrueInformative, 
         HighValueFlower, PropValDiff, PropInfDiff, PropInfDiff_Value, PropValDiff_Value,FlowerChoice)
#FOR NOW: remove equal info rows and make a new correct informative column (1 = chose informative option)
FirstChoiceRep_subset <- FirstChoiceRep_subset %>%
  filter(TrueInformative != "Equal") %>%  # Remove rows where TrueInformative is "Equal"
  mutate(ChoseInformative = ifelse(FlowerChoice == TrueInformative, 1, 0)) %>%
  mutate(Value = ifelse(FlowerChoice == HighValueFlower, 1, 0)) %>%
  mutate(HighInfoHighValue = ifelse(TrueInformative == HighValueFlower,1,0)) 



#Model of first choice in the test phase
FirstChoiceRepmod = glm(ChoseInformative ~ as.factor(Horizon) + Order + PropInfDiff + PropValDiff, family = binomial, data = FirstChoiceRep_subset)
summary(FirstChoiceRepmod)
valueRepmod = glm(Value ~ as.factor(Horizon) + as.factor(HighInfoHighValue) + PropInfDiff + Order, family = binomial, data = FirstChoiceRep_subset)
summary(valueRepmod)


FirstChoiceRep_subset$predicted_prob <- predict(FirstChoiceRepmod, newdata = FirstChoiceRep_subset, type = "response")

ggplot(FirstChoiceRep_subset, aes(x = PropInfDiff, y = predicted_prob, color = factor(Horizon))) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.6) +
  stat_smooth(method = "glm",se = FALSE) +
  labs(x = "Proportional Information Difference", y = "Predicted Probability of Choosing Informative Flower") +
  theme_minimal() 
#same plot as above but value diff on x instead of info
ggplot(FirstChoiceRep_subset, aes(x = PropValDiff, y = predicted_prob, color = factor(Horizon))) +
  geom_jitter(width = 0, height = 0) +
  stat_smooth(method = "glm", se = FALSE) +
  labs(x = "Proportional Value Difference", y = "Predicted Probability of Choosing Informative Flower") +
  theme_minimal()
ggplot(FirstChoiceRep_subset, aes(x = factor(Horizon), y = predicted_prob)) +
  geom_boxplot() +
  labs(x = "Horizon", y = "Predicted Probability of Choosing Informative Flower") +
  theme_minimal()
# Create the boxplot
ggplot(FirstChoiceRep_subset, aes(x = factor(Order), y = predicted_prob, fill = factor(Horizon))) +
  geom_boxplot() +
  labs(x = "Order in which treatment was received", y = "Probability of Choosing Informative Flower") +
  theme_minimal()
#plots for VALUE
FirstChoiceRep_subset$predicted_prob_value <- predict(valueRepmod, newdata = FirstChoiceRep_subset, type = "response")
ggplot(FirstChoiceRep_subset, aes(x = PropInfDiff_Value, y = predicted_prob_value, color = factor(Horizon))) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.6) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Proportional Information Difference", y = "Predicted Probability of Choosing High Value Flower") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) 
ggplot(FirstChoiceRep_subset, aes(x = as.factor(HighInfoHighValue), y = predicted_prob_value, fill = factor(Horizon))) +
  geom_boxplot() +
  labs(x = "Treatment", y = "Probability of Choosing High Value Flower") +
  theme_minimal()
