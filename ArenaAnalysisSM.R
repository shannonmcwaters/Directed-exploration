library(dplyr)
ArenaData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/main/BeeHorizonArena%20MASTER%20DATA%20-%20Sheet1.csv")

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
  revisitoption <- "no reward" # counted as a visit with 0 reward
  # revisitoption <- "not counted" # not counted as visit at all
  # revisitoption <- "full" # counted as visit with full (intended) reward
  #

#Filter for columns we will actually use here
FilteredArena <- ArenaData2 %>%
    select(Bee,Horizon,Session,Condition,InformativeColor,UninformativeColor,VideoReviewedChoices,Rewards,InformativeChoice..0.No.1.Yes.)
#Make long version of data where each choice is a row
Choice = strsplit(as.character(FilteredArena$VideoReviewedChoices), split=",")
Choices = unlist(Choice)
Flowervalue = strsplit(FilteredArena$Rewards, split=",")
Reward = unlist(Flowervalue)
InformativeChoice = strsplit(FilteredArena$InformativeChoice..0.No.1.Yes., split=",")
Informative = unlist(InformativeChoice)
Bee = rep(FilteredArena$Bee, sapply(Choice, length))
Horizon = rep(FilteredArena$Horizon, sapply(Choice, length))
Session = rep(FilteredArena$Session, sapply(Choice, length))
Condition = rep(FilteredArena$Condition, sapply(Choice, length))
InformativeColor = rep(FilteredArena$InformativeColor, sapply(Choice, length))
UninformativeColor = rep(FilteredArena$UninformativeColor, sapply(Choice, length))
ArenaDataLong = data.frame(Bee,Horizon,Session,Condition,InformativeColor,UninformativeColor,Choices,Reward,Informative)

#New data frame to use for second analysis:
LandingDataNew = data.frame(ColonyID,BeeID,ChoiceNumber,Choices, Successes, ThoraxWidth)
LandingDataNew$Successes= ifelse(LandingDataNew$Successes == "S",1,0)