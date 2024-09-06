library(dplyr)
ArenaData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/main/BeeHorizonArena%20MASTER%20DATA%20-%20Sheet1%20(4).csv")

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

#Filter for columns we will actually use here
FilteredArena <- ArenaData2 %>%
    select(Bee,Horizon,Session,Condition,InformativeColor,UninformativeColor,VideoReviewedChoices,Rewards,InformativeChoice..0.No.1.Yes.)
#Make long version of data where each choice is a row
Choice <- strsplit(as.character(FilteredArena$VideoReviewedChoices), split=",")
Choices<-unlist(Choice)
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
ArenaDataLong <- data.frame(Bee,Horizon,Session,Condition,InformativeColor,UninformativeColor,Choices,Reward,Informative)
#Add a column that marks the number choice
ArenaDataLong <- ArenaDataLong %>%
  group_by(Bee,Horizon, Session) %>%
  mutate(ChoiceNumber = row_number()) %>%
  ungroup()

# Create the RewardRepeat column
ArenaDataLong <- ArenaDataLong %>%
  group_by(Bee, Horizon, Session, Choices) %>%
  mutate(
    RewardRepeat = ifelse(row_number() == 1, Reward, 0) # Reward only for the first visit to each flower
  ) %>%
  ungroup()

# Calculate the average RewardRepeat for each flower type (Informative) within each Bee/Horizon/Session combo
ArenaDataLong <- ArenaDataLong %>%
  group_by(Bee, Horizon, Session, Informative) %>%
  mutate(
    AvgRewardRepeat = mean(as.numeric(RewardRepeat))
  ) %>%
  ungroup()

# Step 1: Create running averages for both flower types
ArenaDataLong <- ArenaDataLong %>%
  group_by(Bee, Horizon, Session, Informative) %>%
  mutate(
    RunningAvgInformative1 = ifelse(Informative == 1, cummean(Reward), NA),
    RunningAvgInformative0 = ifelse(Informative == 0, cummean(Reward), NA)
  ) %>%
  ungroup()

# Step 2: Use fill() to propagate the running averages across rows for each bee and treatment
ArenaDataLong <- ArenaDataLong %>%
  group_by(Bee, Horizon, Session) %>%
  fill(RunningAvgInformative1, RunningAvgInformative0, .direction = "down") %>%
  ungroup()

# View the updated data
head(ArenaDataLong)