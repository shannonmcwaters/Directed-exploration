# Shannon McWaters, JM Mizell, Anna Dornhaus
# (c) 2020-2023
# Directed Exploration in Bumble bees, Data analysis script
# Manuscript is here: https://docs.google.com/document/d/1FI106_DvCBktyNP5cmxmQUfszkM7z-Wrtcp7nT1NsuE/edit

#########################
# Outline of code:
#########################
# PART A: Import/look up/format data to get a single table with information 
# about each choice, including flower color and reward
# I. Import (data wide format)
# NOTE: !!!! Here some script execution options get defined !!!
# II. Bee Choices: extract number values from strings (data wide format)
#         NOTE: some redundancy in original data sheet, only 'VideoReviewedChoices'
#               used as bee choice data.
# III. Flower Info: extract flower traits into separate table (data wide format)
# IV. Convert data to long format (each bee choice is one row)
# V. Look up flower information and add (long data format)
# VI. Convert back to wide format (wide data format)
#########################
# PART B: Calculations of additional information 
# VII. Clean table of just rewards and one of flower types
# VIII. Calculate running totals at each choice and deal with revisits
# IX. Add the end results from matching sample phases to test phases
# X. Put information back into long format data table, add all 'running' 
#    information and value, and delete all intermediate variables so we are
#    left with just one final data table. 
#########################
# PART C: Actual analysis and graphing 
# XI. Some data inspection first
# XII. MAIN TEST: Do bees care about information difference?
# XIII. Illustrate this
#########################
# Part D: Alternative factors and additional analyses
# XIII. Excluding problematic data points
# XIV. Including Horizon as factor
# XV. Including Order as factor
# XVI. Different ways to treat revisits
# XVII. Other?
#########################
# Part E. Analyze all choices rather than just first choice
#########################

# Libraries required
library(beepr) # just to be able to hear a warning sound
library(tidyverse)
# Includes several packages, e.g. 
#library(tidy) # For reshaping wide to long format
#library(dplyr) # For bind_rows
library(sjPlot) # to use plot_model() at the end
library(reshape2) #for a single use of melt()

# Remove all current objects from the workspace (i.e. variables in 'Environment')
rm(list = ls(all.names = TRUE))

# Save the original graphics parameters
opar <- par()


#########################
# PART A: Import/look up/format data to get a single table with information 
#########################
# I. Import (data wide format)
#read data in
OriginalBeeHorizonArena <- read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/main/BeeHorizonArena%20MASTER%20DATA%20(2).csv")
# Note this is the slightly older version with color information
# Original datasheets are here: https://drive.google.com/drive/folders/1zpLofiK94Yivnq1wPqxEDhMRsQ5kj-Bn

#####################################################################
#
#
# STOP HERE: 
# Consider the following options, which affect all subsequent calculations
# and graphs.
# Should 'problematic' bees be excluded? 
OriginalBeeHorizonArena <- subset(OriginalBeeHorizonArena, Bee!="Yellow19" & Bee!="Green83" & Bee!="Yellow46" & Bee!="Green57")
# Alternatively see section D.XIII.
#
# How should revisits to the same individual be counted?
revisitoption <- "no reward" # counted as a visit with 0 reward
# revisitoption <- "not counted" # not counted as visit at all
# revisitoption <- "full" # counted as visit with full (intended) reward
#
beep()
Sys.sleep(2)
#
# Should variables used for calculations be removed by the end of the script?
variablecleanup <- TRUE
#
# Do you want to define the color scheme? 
colorsphases <- c("darkseagreen2", "darkseagreen4") # Sample, Test phases
colorsflowertypes <- c("tomato", "slateblue1") # High, Low sample flower types
colorsactualflowers <- c("lightgreen", "darkgreen", "darkgrey", "orange")
bees <- length(unique(OriginalBeeHorizonArena$Bee))
colorsbees <- colorRampPalette(c("blue", "red"))(bees)
#
#
#####################################################################


# Generate a column that uniquely identifies each arena setup (with same flower set)
OriginalBeeHorizonArena$ArenaCode <- paste(OriginalBeeHorizonArena$Bee, OriginalBeeHorizonArena$Order, OriginalBeeHorizonArena$Horizon, sep="_")

# II. Bee Choices: extract number values from strings (data wide format)
#         NOTE: some redundancy in original data sheet, only 'VideoReviewedChoices'
#               used as bee choice data.

#extract choice information as a list of lists
ListOfListsChoices <-strsplit(OriginalBeeHorizonArena$VideoReviewedChoices, split=",")
#extract flower information as a list of lists
ListOfListsFlowerRewards <- strsplit(OriginalBeeHorizonArena$FlowerRewards, split=",")
ListOfListsFlowerColors <- strsplit(OriginalBeeHorizonArena$FlowerColors, split=",")

##reconstitute a 'wide format' data frame from data entered as list

# Find out the maximum number of choices made by any bee
maxChoices <- 0
totalchoices <- 0
for (i in 1:length(ListOfListsChoices)) {
  totalchoices <- totalchoices + length(ListOfListsChoices[[i]])
  maxChoices <- max(c(maxChoices, length(ListOfListsChoices[[i]]))) 
}

#extract choices into individual columns
TempList <- data.frame()
for (i_row in 1:length(ListOfListsChoices)) {
  NewRow <- data.frame(matrix(ncol=0, nrow=1))
  TempList <- ListOfListsChoices[[i_row]]
  NewRow$ArenaCode <- OriginalBeeHorizonArena$ArenaCode[i_row]
  NewRow$BeeID <- OriginalBeeHorizonArena$Bee[i_row]
  NewRow$HorizonInTest <- OriginalBeeHorizonArena$Horizon[i_row]
  NewRow$HorizonOrder <- OriginalBeeHorizonArena$Order[i_row]
  NewRow$Session <- OriginalBeeHorizonArena$Session[i_row]
  NewRow$Treatment <- OriginalBeeHorizonArena$Condition[i_row]
  # Note 'HighSample', referring to flower type, is the color of which 
  # more flowers are available in the sample phase (in the test phase an
  # equal number are available). See original data sheets for visualization
  # at https://drive.google.com/drive/folders/1zpLofiK94Yivnq1wPqxEDhMRsQ5kj-Bn
  NewRow$LowSampleColor <- OriginalBeeHorizonArena$InformativeColor[i_row]
  NewRow$HighSampleColor <- OriginalBeeHorizonArena$UninformativeColor[i_row]
  NewRow$NoofChoices <- length(TempList)
  for (j_choice in 1:length(TempList)) {
    namevar <- paste("Choice", as.character(j_choice), sep="_")
    NewRow[namevar] <- as.numeric(TempList[j_choice])
  }
  if(i_row == 1) {
    NewBeesWide <- NewRow
  } else {
    NewBeesWide <- bind_rows(NewBeesWide, NewRow) 
  }
}
# this column I think gets made if there is a bee
# with zero choices (which should not happen)
# NewBeesWide <- subset(NewBeesWide, select = -c(Choice_0))

NewBeesWide <- NewBeesWide[order(NewBeesWide$ArenaCode),]

# Some tests/examining data:
print("Horizon conditions:")
summary(as.factor(NewBeesWide$HorizonInTest))
print("Bees:")
summary(as.factor(NewBeesWide$BeeID))
print("Choices per arena:")
summary(NewBeesWide$NoofChoices)

## Problems:
## Yellow 46 only has two arenas
## as does Green 57 

plot(NoofChoices ~ as.factor(HorizonInTest)
     , data = NewBeesWide)
print("Colors for 'low':")
summary(as.factor(NewBeesWide$LowSampleColor))
print("Colors for 'high':")
summary(as.factor(NewBeesWide$HighSampleColor))
plot(NoofChoices ~ as.factor(LowSampleColor)
     , data = NewBeesWide)
plot(NoofChoices ~ as.factor(HighSampleColor)
     , data = NewBeesWide)
bymedian <- with(NewBeesWide, reorder(BeeID, NoofChoices, median))
plot(NoofChoices ~ bymedian
     , data = NewBeesWide)
#points(NoofChoices ~ bymedian
#       , data = NewBeesWide)

# III. Flower Info: extract flower traits into separate table (data wide format)

# Make new data sheet to look up flower values, type, and color
TempListColors <- data.frame()
TempListRewards <- data.frame()
for (i_row in 1:length(ListOfListsFlowerColors)) {
  NewRowFlowers <- data.frame(matrix(ncol=0, nrow=1))
  TempListColors <- ListOfListsFlowerColors[[i_row]]
  TempListRewards <- ListOfListsFlowerRewards[[i_row]]
  NewRowFlowers$ArenaCode <- paste(OriginalBeeHorizonArena$Bee[i_row], OriginalBeeHorizonArena$Order[i_row], OriginalBeeHorizonArena$Horizon[i_row], sep="_")
  NewRowFlowers$LowSampleColor <- OriginalBeeHorizonArena$InformativeColor[i_row]
  NewRowFlowers$HighSampleColor <- OriginalBeeHorizonArena$UninformativeColor[i_row]
  NewRowFlowers$ErrorCheck <- paste(OriginalBeeHorizonArena$Horizon[i_row], length(TempListColors), length(TempListRewards), sep="_")
  for (j_flowerID in 1:length(TempListColors)) {
    namevar <- paste("Flower", as.character(j_flowerID), sep="_")
    NewRowFlowers[namevar] <- paste(TempListRewards[j_flowerID], TempListColors[j_flowerID], sep="_")
  }
  if(i_row == 1) {
    NewFlowersWide <- NewRowFlowers
  } else {
    NewFlowersWide <- bind_rows(NewFlowersWide, NewRowFlowers) 
  }
}

# IV. Convert data to long format (each bee choice is one row)

##convert into a 'long format' data frame
BeeChoicesLong <- gather(NewBeesWide, ChoiceNo, FlowerID, "Choice_1":paste("Choice", as.character(maxChoices), sep="_"), factor_key=TRUE)
FlowerInfoLong <- gather(NewFlowersWide, FlowerID, Reward_Color, "Flower_1":"Flower_16", factor_key=TRUE)

# Clear the decks a bit
if(variablecleanup) {
  rm(ListOfListsFlowerColors, ListOfListsFlowerRewards)
  rm(NewRow, NewRowFlowers, NewFlowersWide)
  rm(TempList, TempListColors, TempListRewards)
  rm(i, i_row, j_choice, j_flowerID, namevar)
}

#Clean up and look up information in the long format data
#delete the rows for flowers not chosen or not existing (since not all treatments have 16 flowers)
FlowerInfoLong <- drop_na(FlowerInfoLong)
BeeChoicesLong <- drop_na(BeeChoicesLong)
#convert reward and color back into separate columns
FlowerInfoLong <- separate(FlowerInfoLong, Reward_Color, c("Reward", "Color"), sep = "_", remove = TRUE, convert = TRUE)

# Some data examination:
summary(as.factor(BeeChoicesLong$FlowerID))
summary(as.factor(FlowerInfoLong$ErrorCheck))

# V. Look up flower information and add (long data format)
#look up which flowers were in the 'low sample' ('informative') category
FlowerInfoLong$FlowerType[FlowerInfoLong$Color == FlowerInfoLong$LowSampleColor] <- "LowSample"
FlowerInfoLong$FlowerType[FlowerInfoLong$Color == FlowerInfoLong$HighSampleColor] <- "HighSample"
#remove the characters in the compound columns FlowerID and Choice
#BeeChoicesLong <- separate(BeeChoicesLong, ChoiceNo, c("tempname", "ChoiceNo"), sep = "_", remove = TRUE, convert = TRUE)
#BeeChoicesLong$tempname <- NULL
FlowerInfoLong <- separate(FlowerInfoLong, FlowerID, c("tempname", "FlowerID"), sep = "_", remove = TRUE, convert = TRUE)
FlowerInfoLong$tempname <- NULL

# Look up the information about the flowers chosen by the bees
# For this, we temporarily unite the ArenaCode and FlowerID info to make a unique identifier for each flower
BeeChoicesLong <- unite(BeeChoicesLong, col='UniqueFlowerCode', c('ArenaCode', 'FlowerID') , sep = "_", remove = FALSE)
FlowerInfoLong <- unite(FlowerInfoLong, col='UniqueFlowerCode', c('ArenaCode', 'FlowerID') , sep = "_", remove = FALSE)
BeeChoicesLong$PossibleReward <- FlowerInfoLong$Reward[
  match(BeeChoicesLong$UniqueFlowerCode, FlowerInfoLong$UniqueFlowerCode)
]
BeeChoicesLong$Color <- FlowerInfoLong$Color[
  match(BeeChoicesLong$UniqueFlowerCode, FlowerInfoLong$UniqueFlowerCode)
]
BeeChoicesLong$FlowerType <- FlowerInfoLong$FlowerType[
  match(BeeChoicesLong$UniqueFlowerCode, FlowerInfoLong$UniqueFlowerCode)
]
## Anna: possibly the last three commands could have been replaced by
## merge(BeeChoicesLong, FlowerInfoLong, by="UniqueFlowerCode")
## but I am not sure what will happen if either flowers are missing or revisited
## (appear twice in the BeeChoices dataset).

#BeeChoicesLong <- BeeChoicesLong[order(BeeChoicesLong$ArenaCode),]
BeeChoicesLong[!complete.cases(BeeChoicesLong), 1]
# The output from above should be character(0) if there are
# no NAs (i.e. no incomplete cases)
# Optional: there should not be any NAs
BeeChoicesLong <- drop_na(BeeChoicesLong)

# VI. Convert back to wide format (wide data format)
BeeChoicesLongPlus <- unite(BeeChoicesLong, col='FlowerTraits', c('FlowerID', 'PossibleReward', 'Color', 'FlowerType') , sep = "_", remove = TRUE)
BeeChoicesLongPlus <- subset(BeeChoicesLongPlus, select = -c(UniqueFlowerCode))
BeeChoicesLongPlus <- BeeChoicesLongPlus[order(BeeChoicesLongPlus$ArenaCode),]
NewBeesWidePlus <- spread(BeeChoicesLongPlus, ChoiceNo, FlowerTraits)
NewBeesWidePlus <- NewBeesWidePlus[order(NewBeesWidePlus$ArenaCode),]

# Add/update how many useable data we have: total number of bee choices
# as well as the maximum number made by a bee in an arena.
arenas <- nrow(NewBeesWidePlus)
maxChoicesPlus <- 0 #ncol(NewBeesWidePlus) - 9
totalchoicesPlus <- 0
for (i in 1:arenas) {
  currentchoices <- NewBeesWidePlus[i, -1:-9]
  currentnochoices <- sum(!is.na(currentchoices))
  totalchoicesPlus <- totalchoicesPlus + currentnochoices
  maxChoicesPlus <- max(c(maxChoicesPlus, currentnochoices)) 
}

# Some data examination:
# This should be a straight 1:1 line:
plot(NewBeesWide$NoofChoices ~ NewBeesWidePlus$NoofChoices)
# This should be all zeros:
summary(NewBeesWide$HorizonInTest - NewBeesWidePlus$HorizonInTest)

if(variablecleanup) {
  # Clear the decks a bit
  rm(NewBeesWide)
  rm(BeeChoicesLong)
  rm(currentnochoices, i, ListOfListsChoices, currentchoices)
}
# Voila, information about each choice, including flower color and reward, has been added.
####

#########################
# PART B: Calculations of additional information 
#########################
# What we want is to calculate the ongoing perception the bees have about the value
# of each color; how many samples the bees have taken, at each decision, of each
# color; and we want to take into account that revisits to the exact same flower 
# do not in fact provide additional reward.

# VII. Clean table of just rewards and one of flower types
## Making a table of just reward values for quick and easy lookup
## and a table of flower types
RewardsTable <- data.frame(matrix(ncol=maxChoicesPlus, nrow=arenas))
FlowerTypeTable <- data.frame(matrix(ncol=maxChoicesPlus, nrow=arenas))
FlowerIDTable <- data.frame(matrix(ncol=maxChoicesPlus, nrow=arenas))
RewardsNoRevisitsTable <- data.frame(matrix(ncol=maxChoicesPlus, nrow=arenas))
for (j_choice in 1:maxChoicesPlus) {
  TempColumn <- NewBeesWidePlus[paste("Choice", j_choice, sep="_")]
  colnames(RewardsTable)[j_choice] = paste("Reward", j_choice, sep = "_")
  colnames(RewardsNoRevisitsTable)[j_choice] = paste("Reward", j_choice, sep = "_")
  colnames(FlowerTypeTable)[j_choice] = paste("FlowerType", j_choice, sep = "_")
  for (i_row in 1:arenas) {
    TempFlowerTraits <- TempColumn[i_row,1]
    TempFlowerTraits <- strsplit(TempFlowerTraits, split="_")
    FlowerIDTable[i_row, j_choice] <- as.numeric(TempFlowerTraits[[1]][1])
    if (!anyNA(FlowerIDTable[i_row, 1:j_choice])) #if this choice exists
      # and all before
    {
      RewardsTable[i_row, j_choice] <- as.numeric(TempFlowerTraits[[1]][2])
      FlowerTypeTable[i_row, j_choice] <- TempFlowerTraits[[1]][4]
      RewardsNoRevisitsTable[i_row, j_choice] <- RewardsTable[i_row, j_choice]
      # Any revisits to the same exact flower are presumably unrewarded; so we'll count that as 0 reward.
      if (any(FlowerIDTable[i_row, 1:(j_choice-1)]==FlowerIDTable[i_row, j_choice])
          & j_choice > 1)
        #it's at least the second visit AND the same flower ID occurs
        #previously in the same row
      {
        RewardsNoRevisitsTable[i_row, j_choice] <- 0 
      } 
    }
  }
}

# Some data examination:
# Maximum and minimum flower number should be 16 and 1 
max(FlowerIDTable, na.rm = TRUE)
min(FlowerIDTable, na.rm = TRUE)
# Values in FlowerTypeTable should be only NA or 
# 'HighSample' or 'LowerSample'
unique(FlowerTypeTable$FlowerType_1)

# The following gives the total amount of reward 'lost'
# or discounted because of revisits for each arena 
MissedRewards <- t(RewardsNoRevisitsTable) - t(RewardsTable)
RevisitsTable <- t(MissedRewards)
RevisitsTable[RevisitsTable<0] <- 1
colSums(MissedRewards, na.rm = TRUE)

# Proportion of revisits
Visits <- t(RewardsTable)
Visits[Visits>0] <- 1
Visits[is.na(Visits)] <- 0
PropUniqueVisitsPerArena <- colSums(MissedRewards==0, na.rm = TRUE) / colSums(Visits)
PropRevisitsPerChoice <- colSums(RewardsNoRevisitsTable==0, na.rm = TRUE) / colSums(RewardsTable, na.rm = TRUE)

# Plotting the proportion of unique vs revisits and
# amount of reward discounted
MissedRewardsLong <- melt(MissedRewards)
colnames(MissedRewardsLong) <- c("Choice", "Arena", "Reward")
levels(MissedRewardsLong$Choice) <- seq(from=1, to=maxChoicesPlus)
Nice_Plot <- boxplot(Reward ~ Choice
                     , data = MissedRewardsLong
                     , main = "Proportion of revisits and amount of rewards lost"
                     , xlab = "Choice number"
                     , ylab = "Concentration of reward not gained bc of revisit"
                     , ylim = c(-2.5, 1)
)
# Adding (blue dots) proportion of revisits for each choice
points(PropRevisitsPerChoice
       , col = "blue"
)
# Adding total number of arenas with this number of choices
nbGroup <- nlevels(as.factor(Nice_Plot$names))
text( 
  x=c(1:nbGroup), 
  y=1.05,
  cex = 0.3,
  col = "blue",
  paste(" ", Nice_Plot$n)  
)
##Note that this plot lumps together arenas that only have 2 flowers
##with those that have 6 or 16 flowers. 

#Cleanup
if (variablecleanup) {
  rm(MissedRewards, MissedRewardsLong)
}

##
# VIII. Calculate running totals at each choice and deal with revisits
## Now looping through to calculate running totals to estimate information 
# bees actually have at each choice point.
RunningRewardTotalsLowSample <- data.frame(matrix(0, ncol=maxChoicesPlus, nrow=arenas))
RunningRewardTotalsHighSample <- data.frame(matrix(0, ncol=maxChoicesPlus, nrow=arenas))
RunningChoiceTotalsLowSample <- data.frame(matrix(0, ncol=maxChoicesPlus, nrow=arenas))
RunningChoiceTotalsHighSample <- data.frame(matrix(0, ncol=maxChoicesPlus, nrow=arenas))
FinalAvgRewardHigh <- data.frame(matrix(0, ncol=1, nrow=arenas))
FinalAvgRewardLow <- data.frame(matrix(0, ncol=1, nrow=arenas))
FinalTotalChoicesHigh <- data.frame(matrix(0, ncol=1, nrow=arenas))
FinalTotalChoicesLow <- data.frame(matrix(0, ncol=1, nrow=arenas))
#
# Remember that in this section, there is a key option that was defined at the
# beginning of this script, about what to do about revisits.#
# "no reward" means revisits are counted as a visit with 0 reward
# "not counted" means revisits are not counted as visit at all
# "full" means revisits are counted as visit with full (intended but possibly
#        already drained) reward

for (i_row in 1:arenas) {
  RunningTotalHigh <- 0
  RunningTotalRewardHigh <- 0
  RunningTotalLow <- 0
  RunningTotalRewardLow <- 0
  for (j_choice in 1:maxChoicesPlus) {
    if (is.na(FlowerTypeTable[i_row, j_choice])) {
      # If this choice didn't exist we do nothing
    } else {
      if (FlowerTypeTable[i_row, j_choice] == "HighSample") {
        # If the choice was for the 'High' type flower, we
        # add to the High total and add the reward
        if (revisitoption=="no reward") {
          RunningTotalHigh <- RunningTotalHigh + 1
          RunningTotalRewardHigh <- RunningTotalRewardHigh + RewardsNoRevisitsTable[i_row, j_choice]
        } else {
          if (revisitoption=="full") {
            RunningTotalHigh <- RunningTotalHigh + 1
            RunningTotalRewardHigh <- RunningTotalRewardHigh + RewardsTable[i_row, j_choice]
          } else {
            if (revisitoption=="not counted") {
              if (RevisitsTable[i_row, j_choice]==0) {
                RunningTotalHigh <- RunningTotalHigh + 1
                RunningTotalRewardHigh <- RunningTotalRewardHigh + RewardsTable[i_row, j_choice]
              }
            }
          }
        }
      } else {
        if (FlowerTypeTable[i_row, j_choice] == "LowSample") {
          # Equivalent if the choice was 'Low'
          if (revisitoption=="no reward") {
            RunningTotalLow <- RunningTotalLow + 1
            RunningTotalRewardLow <- RunningTotalRewardLow + RewardsNoRevisitsTable[i_row, j_choice]
          } else {
            if (revisitoption=="full") {
              RunningTotalLow <- RunningTotalLow + 1
              RunningTotalRewardLow <- RunningTotalRewardLow + RewardsTable[i_row, j_choice]
            } else {
              if (revisitoption=="not counted") {
                if (RevisitsTable[i_row, j_choice]==0) {
                  RunningTotalLow <- RunningTotalLow + 1
                  RunningTotalRewardLow <- RunningTotalRewardLow + RewardsTable[i_row, j_choice]
                }
              }
            }
          }
        } else {
          # We should never get here
          print("Choice", j_choice, ", in", NewBeesWidePlus$UniqueFlowerCode[i_row], "not high or low")
        }
      }
    }
    # For each choice, we collect the result in tables
    RunningRewardTotalsHighSample[i_row, j_choice] <- RunningTotalRewardHigh
    RunningChoiceTotalsHighSample[i_row, j_choice] <- RunningTotalHigh
    RunningRewardTotalsLowSample[i_row, j_choice] <- RunningTotalRewardLow
    RunningChoiceTotalsLowSample[i_row, j_choice] <- RunningTotalLow
  }
  # And at the end of each row, we collect the final results
  FinalAvgRewardHigh[i_row,1] <- RunningTotalRewardHigh / RunningTotalHigh
  FinalAvgRewardLow[i_row,1] <- RunningTotalRewardLow / RunningTotalLow
  FinalTotalChoicesHigh[i_row,1] <- RunningTotalHigh
  FinalTotalChoicesLow[i_row,1] <- RunningTotalLow
}


## Now NewBeesWidePlus, which has a row for each bee trip/arena, has summaries for that phase at the end
NewBeesWidePlus$FinalAvgRewardHigh <- FinalAvgRewardHigh[,1]
NewBeesWidePlus$FinalAvgRewardLow <- FinalAvgRewardLow[,1]
NewBeesWidePlus$FinalTotalChoicesHigh <- FinalTotalChoicesHigh[,1]
NewBeesWidePlus$FinalTotalChoicesLow <- FinalTotalChoicesLow[,1]
# Anna: so this is cumbersome; I defined these as matrices with 1 column earlier, 
# but if they are used here without the [,1] then the column name becomes
# [intended column name]$[column name in the temp matrix]. 
# There must be a more elegant way - perhaps not initializing them as 
# matrix to begin with...


# Some data checking:
str(NewBeesWidePlus[,c("FinalAvgRewardHigh", "FinalAvgRewardLow", "FinalTotalChoicesHigh", "FinalTotalChoicesLow")])
colSums(NewBeesWidePlus[,c("FinalTotalChoicesHigh", "FinalTotalChoicesLow")], na.rm = TRUE)
# These two numbers should sum to the number of rows in BeeChoicesLongPlus:
# checks out

# What rewards to bees get from the 'high sample' vs 
# the 'low sample' flowers?
FinalAvgRew <- NewBeesWidePlus[,c("FinalAvgRewardHigh", "FinalAvgRewardLow")]
colnames(FinalAvgRew) <- c("High Sample", "Low Sample")
Nice_PLot <- boxplot(FinalAvgRew
                     , xlab = "Flower type"
                     , ylab = "Final average reward per arena"
                     , col = colorsflowertypes
)
# about the same, as it should be
stripchart(FinalAvgRew,              # Data
           method = "jitter", # Random noise
           pch = 1,          # Pch symbols
           #           col = colorsflowertypes,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)        # Add it over
# Anna: What I'd like to have here is the points in each 
# group connecte with their corresponding paired data point,
# but I can't figure out how without using ggplot.

# Each of the following should be monotonically increasing, 
# saturating functions
boxplot(RunningRewardTotalsHighSample
        , xlab = "Choice No"
        , ylab = "RunningRewardTotal"
        , col = colorsflowertypes[1]
        , range = 0)
boxplot(RunningRewardTotalsLowSample
        , xlab = "Choice No"
        , ylab = "RunningRewardTotal"
        , col = colorsflowertypes[2]
        , range = 0)
# Or for another version each arena as a line
matplot(t(RunningRewardTotalsLowSample)
        , type="l"
        , xlab = "Choice No")
matplot(t(RunningRewardTotalsHighSample)
        , type="l"
        , xlab = "Choice No")

if (variablecleanup) {
  # Clear the decks a bit
  rm(FinalAvgRewardHigh, FinalAvgRewardLow, FinalTotalChoicesHigh, FinalTotalChoicesLow)
  rm(RunningTotalHigh, RunningTotalLow, RunningTotalRewardHigh, RunningTotalRewardLow)
  rm(TempColumn, TempFlowerTraits, i_row, j_choice)
  rm(FinalAvgRew)
  rm(Nice_Plot)
  rm(Visits)
}

# IX. Add the end results from matching sample phases to test phases
# and calculate differences between High and Low flower type
NewBeesWidePlus$TestCode <- paste(NewBeesWidePlus$Bee, NewBeesWidePlus$HorizonOrder, sep="_")
NewBeesWidePlusSamplePhase <- subset(NewBeesWidePlus, NewBeesWidePlus$Session=="Sample")
NewBeesWidePlusTestPhase <- subset(NewBeesWidePlus, NewBeesWidePlus$Session=="Test")

NewBeesWidePlusTestPhase$SamplePhaseAvgRewardHigh <- NewBeesWidePlusSamplePhase$FinalAvgRewardHigh[
  match(NewBeesWidePlusTestPhase$TestCode, NewBeesWidePlusSamplePhase$TestCode)]
NewBeesWidePlusTestPhase$SamplePhaseAvgRewardLow <- NewBeesWidePlusSamplePhase$FinalAvgRewardLow[
  match(NewBeesWidePlusTestPhase$TestCode, NewBeesWidePlusSamplePhase$TestCode)]
NewBeesWidePlusTestPhase$SamplePhaseTotalChoicesHigh <- NewBeesWidePlusSamplePhase$FinalTotalChoicesHigh[
  match(NewBeesWidePlusTestPhase$TestCode, NewBeesWidePlusSamplePhase$TestCode)]
NewBeesWidePlusTestPhase$SamplePhaseTotalChoicesLow <- NewBeesWidePlusSamplePhase$FinalTotalChoicesLow[
  match(NewBeesWidePlusTestPhase$TestCode, NewBeesWidePlusSamplePhase$TestCode)]

# perceived value difference based on sample phase
# Anna: Should these be proportional differences?! Right now it is just 
# absolute difference
NewBeesWidePlusTestPhase$PerceivedValueDiff <- NewBeesWidePlusTestPhase$SamplePhaseAvgRewardHigh - NewBeesWidePlusTestPhase$SamplePhaseAvgRewardLow
NewBeesWidePlusTestPhase$InformationDiff <- NewBeesWidePlusTestPhase$SamplePhaseTotalChoicesHigh - NewBeesWidePlusTestPhase$SamplePhaseTotalChoicesLow
# Put it back together to keep it in one place
NewBeesWidePlus <- bind_rows(NewBeesWidePlusTestPhase, NewBeesWidePlusSamplePhase)
NewBeesWidePlus <- NewBeesWidePlus[order(NewBeesWidePlus$ArenaCode),]
rownames(NewBeesWidePlus) <- 1:nrow(NewBeesWidePlus) 

# Some examination
ColumnsToPlot <- NewBeesWidePlusTestPhase[,c("PerceivedValueDiff", "InformationDiff")]
Nice_Plot <- boxplot(ColumnsToPlot
                     , ylab = "[High Sample] - [Low Sample]")
# Good, across all trials, same value but bees have more information 
# about high-sample flowers, that's what we want. 


# X. Put information back into long format data table
# Put this in BeeChoicesLongPlus
BeeChoicesLongPlus <- gather(NewBeesWidePlus, ChoiceNo, FlowerTraits, "Choice_1":paste("Choice", as.character(maxChoices), sep="_"), factor_key=TRUE)
BeeChoicesLongPlus <- separate(BeeChoicesLongPlus, FlowerTraits, c("FlowerID", "PossibleReward", "Color", "FlowerType"), sep = "_", remove = FALSE, convert = TRUE)
BeeChoicesLongPlus <- separate(BeeChoicesLongPlus, ChoiceNo, c("label", "ChoiceNo"), sep = "_", remove = FALSE, convert = TRUE)
BeeChoicesLongPlus <- subset(BeeChoicesLongPlus, select = -c(label))
# Delete all the lines that don't have choices in them
BeeChoicesLongPlus <- BeeChoicesLongPlus[!is.na(BeeChoicesLongPlus$FlowerID),]
RowsToDelete <- vector()
if (revisitoption=="not counted") {
  for (i_row in 1:nrow(BeeChoicesLongPlus)) {
    arena <- match(BeeChoicesLongPlus$ArenaCode[i_row], NewBeesWidePlus$ArenaCode)
    j_choice <- BeeChoicesLongPlus$ChoiceNo[i_row] 
    if (RevisitsTable[arena, j_choice]==1) {
      RowsToDelete <- c(RowsToDelete, i_row)
    }
  }
  BeeChoicesLongPlus <- BeeChoicesLongPlus[-RowsToDelete,]
}

# add info for each choice
for (i_row in 1:nrow(BeeChoicesLongPlus)) {
  # 'arena' gives us the row number of this arena code in NewBeeChoicesWidePlus
  # and thus in the wide-format RunningTotals tables
  arena <- match(BeeChoicesLongPlus$ArenaCode[i_row], NewBeesWidePlus$ArenaCode)
  # j_choice gives us the column in the 'wide' tables
  j_choice <- BeeChoicesLongPlus$ChoiceNo[i_row] 
  BeeChoicesLongPlus$RunningTotalThisPhaseHighSamples[i_row] <- 
    RunningChoiceTotalsHighSample[arena, j_choice]
  BeeChoicesLongPlus$RunningTotalThisPhaseLowSamples[i_row] <- 
    RunningChoiceTotalsLowSample[arena, j_choice]
  BeeChoicesLongPlus$RunningAvgRewardThisPhaseHigh[i_row] <- 
    RunningRewardTotalsHighSample[arena, j_choice] / RunningChoiceTotalsHighSample[arena, j_choice]
  BeeChoicesLongPlus$RunningAvgRewardThisPhaseLow[i_row] <- 
    RunningRewardTotalsLowSample[arena, j_choice] / RunningChoiceTotalsLowSample[arena, j_choice]
  # and then including possible prior sample phase for all 4:
  # ... if this is actually test phase
  if (BeeChoicesLongPlus$Session[i_row]=="Test") 
  {
    BeeChoicesLongPlus$RunningTotalAllHigh[i_row] <- 
      BeeChoicesLongPlus$RunningTotalThisPhaseHighSamples[i_row] + BeeChoicesLongPlus$SamplePhaseTotalChoicesHigh[i_row]
    BeeChoicesLongPlus$RunningTotalAllLow[i_row] <- 
      BeeChoicesLongPlus$RunningTotalThisPhaseLowSamples[i_row] + BeeChoicesLongPlus$SamplePhaseTotalChoicesLow[i_row]
    BeeChoicesLongPlus$RunningTotalAllAvgRewardHigh[i_row] <- 
      (BeeChoicesLongPlus$RunningAvgRewardThisPhaseHigh[i_row]*BeeChoicesLongPlus$RunningTotalThisPhaseHighSamples[i_row] + BeeChoicesLongPlus$SamplePhaseAvgRewardHigh[i_row]*BeeChoicesLongPlus$SamplePhaseTotalChoicesHigh[i_row]) / (BeeChoicesLongPlus$SamplePhaseTotalChoicesHigh[i_row]+BeeChoicesLongPlus$RunningTotalThisPhaseHighSamples[i_row]) 
    BeeChoicesLongPlus$RunningTotalAllAvgRewardLow[i_row] <- 
      (BeeChoicesLongPlus$RunningAvgRewardThisPhaseLow[i_row]*BeeChoicesLongPlus$RunningTotalThisPhaseLowSamples[i_row] + BeeChoicesLongPlus$SamplePhaseAvgRewardLow[i_row]*BeeChoicesLongPlus$SamplePhaseTotalChoicesLow[i_row]) / (BeeChoicesLongPlus$SamplePhaseTotalChoicesLow[i_row]+BeeChoicesLongPlus$RunningTotalThisPhaseLowSamples[i_row]) 
  } else {
    # if not, we don't need to include a prior phase
    BeeChoicesLongPlus$RunningTotalAllHigh[i_row] <- BeeChoicesLongPlus$RunningTotalThisPhaseHighSamples[i_row]
    BeeChoicesLongPlus$RunningTotalAllLow[i_row] <- BeeChoicesLongPlus$RunningTotalThisPhaseLowSamples[i_row]
    BeeChoicesLongPlus$RunningTotalAllAvgRewardHigh[i_row] <- BeeChoicesLongPlus$RunningAvgRewardThisPhaseHigh[i_row]
    BeeChoicesLongPlus$RunningTotalAllAvgRewardLow[i_row] <- BeeChoicesLongPlus$RunningAvgRewardThisPhaseLow[i_row]
  }
}
# Anna: instead of looping over rows, there may be a vector-way of doing this more elegantly.

# then calculating difference in value and information
# Note this is also absolute differences...
BeeChoicesLongPlus$CurrentPerceivedValueDiff <- 
  BeeChoicesLongPlus$RunningTotalAllAvgRewardHigh - BeeChoicesLongPlus$RunningTotalAllAvgRewardLow
BeeChoicesLongPlus$CurrentInformationDiff <- 
  BeeChoicesLongPlus$RunningTotalAllHigh - BeeChoicesLongPlus$RunningTotalAllLow

# This is the table with complete information that we want:
DataByChoice <- BeeChoicesLongPlus
DataByArena <- NewBeesWidePlus
# Note that even with "not counted" option for revisits, these are 
# still listed in the NewBeesWidePlus dataset.

if (variablecleanup) {
  # Clear everything else
  rm(BeeChoicesLongPlus)
  rm(RewardsTable, FlowerTypeTable, FlowerInfoLong)
  rm(NewBeesWidePlus, NewBeesWidePlusSamplePhase, NewBeesWidePlusTestPhase)
  rm(RunningChoiceTotalsHighSample, RunningChoiceTotalsLowSample, RunningRewardTotalsHighSample, RunningRewardTotalsLowSample)
  rm(RewardsNoRevisitsTable)
  rm(Nice_Plot, ColumnsToPlot, Revisits)
}

TestData <- subset(DataByChoice, 
                   ChoiceNo=="1" 
                   & Session=="Test")

#########################
# PART C: Actual analysis and graphing 
#########################
# XI. Some data inspection first
########################## Data Inspection
plot(jitter(FinalTotalChoicesHigh) ~ jitter(SamplePhaseTotalChoicesHigh)
     , data = DataByArena
     , pch = 1
     , col = colorsflowertypes[1]
     , xlab = "No of choices Sample Phase"
     , ylab = "No of choices Test Phase"
)
points(jitter(FinalTotalChoicesLow) ~ jitter(SamplePhaseTotalChoicesLow)
       , data = DataByArena
       , pch = 1
       , col = colorsflowertypes[2]
)
text(DataByArena$SamplePhaseTotalChoicesHigh + 0.2
     , DataByArena$FinalTotalChoicesHigh + 0.8
     , labels = DataByArena$BeeID
     , col = colorsflowertypes[1]
     , cex = 0.3
)
text(DataByArena$SamplePhaseTotalChoicesLow + 0.2
     , DataByArena$FinalTotalChoicesLow + 0.8
     , labels = DataByArena$BeeID
     , col = colorsflowertypes[2]
     , cex = 0.3
)

plot(FinalTotalChoicesHigh/NoofChoices ~ PerceivedValueDiff
     , data = DataByArena
     , pch = 1
     , col = colorsflowertypes[1]
     , ylim = c(0, 1.1)
)
points(FinalTotalChoicesLow/NoofChoices ~ PerceivedValueDiff
       , data = DataByArena
       , pch = 1
       , col = colorsflowertypes[2]
)
text(DataByArena$PerceivedValueDiff + 0.05
     , (DataByArena$FinalTotalChoicesHigh/DataByArena$NoofChoices) + 0.05
     , labels = DataByArena$BeeID
     , col = colorsflowertypes[1]
     , cex = 0.3
)
text(DataByArena$PerceivedValueDiff + 0.05
     , (DataByArena$FinalTotalChoicesLow/DataByArena$NoofChoices) + 0.05
     , labels = DataByArena$BeeID
     , col = colorsflowertypes[2]
     , cex = 0.3
)

plot(FinalTotalChoicesHigh/NoofChoices ~ InformationDiff
     , data = DataByArena
     , pch = 1
     , col = colorsflowertypes[1]
)
points(FinalTotalChoicesLow/NoofChoices ~ InformationDiff
       , data = DataByArena
       , pch = 1
       , col = colorsflowertypes[2]
)
text(DataByArena$InformationDiff + 0.05
     , (DataByArena$FinalTotalChoicesHigh/DataByArena$NoofChoices) + 0.05
     , labels = DataByArena$BeeID
     , col = colorsflowertypes[1]
     , cex = 0.3
)
text(DataByArena$InformationDiff + 0.05
     , (DataByArena$FinalTotalChoicesLow/DataByArena$NoofChoices) + 0.05
     , labels = DataByArena$BeeID
     , col = colorsflowertypes[2]
     , cex = 0.3
)
#The two outliers are Yellow19. Why?
# Either way these should probably be excluded, as
# the information difference goes the wrong way.

par(mfrow = c(1,2), mar= c(5.1, 4.1, 1, 1))    
plot(RunningTotalAllHigh ~ ChoiceNo
     , data = subset(DataByChoice, Session == "Sample")
     , col = colorsphases[1]
     , pch = 16
     , ylim = c(0, maxChoices)
     , xlim = c(0, maxChoices)
)
legend("topright", legend = levels(as.factor(DataByChoice$Session)), col = colorsphases, pch = 16, cex = 0.8)
par(mar= c(5.1, 1, 1, 4.1))    
plot(RunningTotalAllHigh ~ ChoiceNo
     , data = subset(DataByChoice, Session == "Test")
     , col = colorsphases[2]
     , pch = 16
     , ylim = c(0, maxChoices)
     , ylab="" 
     , xlim = c(0, maxChoices)
     , yaxt="n"
)
par <- opar

par(mfrow = c(1,2), mar= c(5.1, 4.1, 1, 1))    
plot(RunningTotalAllLow ~ ChoiceNo
     , data = subset(DataByChoice, Session == "Sample")
     , col = colorsphases[1]
     , pch = 16
     , ylim = c(0, maxChoices)
     , xlim = c(0, maxChoices)
)
legend("topright", legend = levels(as.factor(DataByChoice$Session)), col = colorsphases, pch = 16, cex = 0.8)
par(mar= c(5.1, 1, 1, 4.1))    
plot(RunningTotalAllLow ~ ChoiceNo
     , data = subset(DataByChoice, Session == "Test")
     , col = colorsphases[2]
     , pch = 16
     , ylim = c(0, maxChoices)
     , ylab = "" 
     , xlim = c(0, maxChoices)
     , yaxt = "n"
)
par <- opar

par(mfrow = c(1,2), mar= c(5.1, 4.1, 1, 1))    
plot(RunningTotalAllAvgRewardHigh ~ ChoiceNo
     , data = DataByChoice
     , col = colorsphases[as.factor(Session)]
     , pch = 16
     , xlim = c(0, maxChoices)
)
par(mar= c(5.1, 1, 1, 4.1))    
plot(RunningTotalAllAvgRewardLow ~ ChoiceNo
     , data = DataByChoice
     , col = colorsphases[as.factor(Session)]
     , pch = 16
     , ylab = "" 
     , xlim = c(0, maxChoices)
     , yaxt = "n"
)
legend("topright", legend = levels(as.factor(DataByChoice$Session)), col = colorsphases, pch = 16, cex = 0.8)
par <- opar

plot(CurrentPerceivedValueDiff ~ ChoiceNo
     , data = subset(DataByChoice, Session == "Test")
     , col = colorsbees[as.factor(BeeID)]
     , pch = c(1,16)[as.factor(FlowerType)]
     , xlim = c(0, maxChoices)
)
plot(CurrentInformationDiff ~ ChoiceNo
     , data = subset(DataByChoice, Session == "Test")
     , col = colorsbees[as.factor(BeeID)]
     , pch = c(1,16)[as.factor(FlowerType)]
     , xlim = c(0, maxChoices)
)

################################################################
### PROBLEMS
# In the graph of RunningTotalAllLow, it seems the across-phase totals
# in one case start with 22, even though no sample phase ends with that many choices.

# In CurrentInformationDiff, how can it ever stay flat for the same bee?




##########################
# XII. MAIN TEST: Do bees care about information difference?

# In their first choice in the test phase, is the bee's choice
# predicted at all by information, or only by value difference?
mylogit<- glm(data = TestData, as.factor(FlowerType) ~ InformationDiff + PerceivedValueDiff,
              family = "binomial")
#family = "binomial")
summary(mylogit)
plot_model(mylogit)
# Same but including color and interaction
mylogit<- glm(data = TestData, as.factor(FlowerType) ~ InformationDiff + PerceivedValueDiff + Color + InformationDiff*PerceivedValueDiff,
              family = "binomial")
#family = "binomial")
summary(mylogit)
plot_model(mylogit)
# Note: current data seem to indicate information does not 
# make a difference.
# It also indicates that color makes a big difference, possibly more than reward difference. 
# This makes the fact that colors are not really symmetric across treatments a bit problematic. 
summary(as.factor(TestData$LowSampleColor))
summary(as.factor(TestData$HighSampleColor))


############################
# XIII. Illustrate this
# Plot identity of first choice against perceived value and information differences,
# x = perceived value difference
# y = information difference
# color = choice
plot(InformationDiff ~ PerceivedValueDiff
     , data = TestData
     , col = colorsflowertypes[as.factor(FlowerType)]
)
modelhigh <- lm(InformationDiff ~ PerceivedValueDiff, data = subset(TestData, FlowerType=="HighSample"))
modellow <- lm(InformationDiff ~ PerceivedValueDiff, data = subset(TestData, FlowerType=="LowSample"))
abline(modelhigh, col = colorsflowertypes[1])
abline(modellow, col = colorsflowertypes[2])
summary(modelhigh)
summary(modellow)

# Plot as boxplot the information difference for bees who end up choosing the
# 'high sample' flower type vs those that chose the 'low sample' flower type.
Nice_Plot <- boxplot(InformationDiff ~ FlowerType
                     , data = TestData
                     , col = colorsflowertypes
                     , range = 0
                     , xlab = "Flower Type chosen in Test"
                     , ylab = "Information difference from Sample Phase"
                     # Perhaps all these plots should be horizontal?        , horizontal = TRUE
)
nbGroup <- nlevels(as.factor(Nice_Plot$names))
text( 
  x=c(1:nbGroup), 
  y=Nice_Plot$stats[3,] + 1,
  cex = 0.7,
  paste("n=", Nice_Plot$n)  
)
Nice_Plot <- boxplot(PerceivedValueDiff ~ FlowerType
                     , data = TestData
                     , col = colorsflowertypes
                     , range = 0
                     , xlab = "Flower Type chosen in Test"
                     , ylab = "Perceived value difference from Sample Phase"
)
nbGroup <- nlevels(as.factor(Nice_Plot$names))
text( 
  x=c(1:nbGroup), 
  y=Nice_Plot$stats[3,] + 0.1,
  cex = 0.7,
  paste("n=", Nice_Plot$n)  
)

# Same plot type, but now separating out the colors, with those that were
# presented simultaneously next to each other.
TestData <- unite(TestData, col="CategoriesForPlot", c('FlowerType', 'Color'), sep = ":", remove = FALSE)
TestData$CategoriesForPlot <- factor(TestData$CategoriesForPlot, levels=
                                       c("HighSample:B", "LowSample:DG", 
                                         "HighSample:DG", "LowSample:B",
                                         "HighSample:LG", "LowSample:O",
                                         "HighSample:O", "LowSample:LG"))
par(mar = c(9, 4.1, 1, 1), mgp=c(3, 1, 0), las=2)
Nice_Plot <- boxplot(PerceivedValueDiff ~ CategoriesForPlot
                     , data = TestData
                     , col = colorsflowertypes
                     , range = 0
                     , cex.axis = 0.8
                     , las=2
                     , xlab = ""
                     , ylab = "Perceived Value Difference from Sample Phase [H-L]"
)
title(xlab = "Flower Type and Color chosen in Test", mgp = c(6, 1, 0))
nbGroup <- nlevels(as.factor(Nice_Plot$names))
text( 
  x=c(1:nbGroup), 
  y=Nice_Plot$stats[3,] + 0.5,
  cex = 0.7,
  paste("n=", Nice_Plot$n)  
)
par(opar)

par(mar = c(9, 4.1, 1, 1), mgp=c(3, 1, 0), las=2)
Nice_Plot <- boxplot(InformationDiff ~ CategoriesForPlot
                     , data = TestData
                     , col = colorsflowertypes
                     , range = 0
                     , cex.axis = 0.8
                     , las=2
                     , xlab = ""
                     , ylab = "Information Difference from Sample Phase [H-L]"
)
title(xlab = "Flower Type and Color chosen in Test", mgp = c(6, 1, 0))
nbGroup <- nlevels(as.factor(Nice_Plot$names))
text( 
  x=c(1:nbGroup), 
  y=Nice_Plot$stats[3,] + 0.5,
  cex = 0.7,
  paste("n=", Nice_Plot$n)  
)
par(opar)
# What's interesting here is that in each pairing, although not significant,
# the bees that ended up choosing the high sample flower type actually already
# had more information on that flower type -
# i.e. opposite to directed exploration, they actually stuck to the color they visited
# most in the sample phase.


#########################
# Part D: Alternative factors and additional analyses
################################################################
## Some possible alternatives worth examining.

# XIII. Excluding problematic data points

## Should the following data be excluded?
# Something strange is going on with Yellow19 in terms of information difference.
# Two bees are not balanced for all treatments (Yellow46 and Green57).
# One bee (Green83) has the color pairing switched (for one treatment).
str(unique(TestData$BeeID))
#save the old version
DBArena <- DataByArena
DBChoice <- DataByChoice
# exclude problematic bees
DataByArena <- subset(DataByArena, BeeID!="Yellow19" & BeeID!="Green83" & BeeID!="Yellow46" & BeeID!="Green57")
DataByChoice <- subset(DataByChoice, BeeID!="Yellow19" & BeeID!="Green83" & BeeID!="Yellow46" & BeeID!="Green57")
TestData <- subset(DataByChoice, 
                   ChoiceNo=="1" 
                   & Session=="Test")
# After running the code above to exclude these bees, you can just run all the 
# analyses again (code above in 'Part C').
# The results are the same as before, i.e. nothing significant except color, and 
# for information the direction of the effect seems opposite to directed exploration
# (i.e. bees who already visited a flower more kept chosing that same flower).
# restore previous versions if needed
#DataByArena <- DBArena
#DataByChoice <- DBChoice
#TestData <- subset(DataByChoice, 
#                   ChoiceNo=="1" 
#                   & Session=="Test")


########
# XIV. Including Horizon as factor
# Currently the horizon 2 and horizon 16 are treated the same
# (pooled). Does horizon affect the degree of directed exploration?
mylogit<- glm(data = TestData, as.factor(FlowerType) ~ InformationDiff + PerceivedValueDiff + HorizonInTest + Color + InformationDiff:HorizonInTest,
              family = "binomial")
summary(mylogit)
plot_model(mylogit)
# Color in graph shows positive vs negative effect, not significance!
mylogit<- glm(data = TestData, as.factor(FlowerType) ~ InformationDiff + PerceivedValueDiff + HorizonInTest + InformationDiff:HorizonInTest,
              family = "binomial")
summary(mylogit)
plot_model(mylogit)
# If including horizon and horizon*infor, 
# and not including color, the horizon*infor interaction actually has the 
# lowest p-value (0.09). Still not significant but with that number of factors
# and the color imbalance, that is also an argument for collecting 
# more data. 
Nice_Plot <- boxplot(InformationDiff ~ FlowerType + HorizonInTest
                     , data = TestData
                     , col = colorsflowertypes
                     , range = 0
                     , xlab = "Flower Type chosen in Test"
                     , ylab = "Information difference from Sample Phase"
)
nbGroup <- nlevels(as.factor(Nice_Plot$names))
text( 
  x=c(1:nbGroup), 
  y=Nice_Plot$stats[3,] + 1,
  cex = 0.7,
  paste("n=", Nice_Plot$n)  
)
Nice_Plot <- boxplot(PerceivedValueDiff ~ FlowerType + HorizonInTest
                     , data = TestData
                     , col = colorsflowertypes
                     , range = 0
                     , xlab = "Flower Type chosen in Test"
                     , ylab = "Perceived value difference from Sample Phase"
)
nbGroup <- nlevels(as.factor(Nice_Plot$names))
text( 
  x=c(1:nbGroup), 
  y=Nice_Plot$stats[3,] + 0.1,
  cex = 0.7,
  paste("n=", Nice_Plot$n)  
)
# Ok this is interesting. First, a lot of variation seems explained by horizon -
# this is not significant but plausibly a result of low sample size.
# The fact that horizon matters in itself would be a more surprising result than 
# everything else we have. 
# My intuition that is MAY in fact be a real result is that both for info and 
# value, including horizon reduces variation and generates consistent direction
# for possible differences between low and high sample. 
# What is a bit confusing to me is the effect of perceived value. I want us to make
# double and triple sure that the prediction is in the direction I think, 
# namely that higher value for 'Perceived value difference' means they should
# prefer the 'high sample' flower and the blue medians should be higher. 
# Because right now it seems the effect is in the opposite direction. 
# I also wonder if a non-parametric test on the specific comparisons we care about
# isn't more powerful than a logistic model that has everything thrown in.
wilcox.test(InformationDiff ~ FlowerType, 
            data = subset(TestData, HorizonInTest==2),
            exact = FALSE)
wilcox.test(InformationDiff ~ FlowerType, 
            data = subset(TestData, HorizonInTest==16),
            exact = FALSE)
wilcox.test(PerceivedValueDiff ~ FlowerType, 
            data = subset(TestData, HorizonInTest==2),
            exact = FALSE)
wilcox.test(PerceivedValueDiff ~ FlowerType, 
            data = subset(TestData, HorizonInTest==16),
            exact = FALSE)
# Certainly this is easier to interpret I think. Result is the same though
# (not significant).

#############
# XV. Including Order as factor
## Order effects / effects of training.
# We are not currently testing for an effect of order of horizon phases, 
# or generally of more bee experience on choices. 
# E.g. do they become better at matching choice to value difference in the second
# set of sample+test phase? 
Nice_Plot <- boxplot(InformationDiff ~ FlowerType + HorizonOrder
                     , data = TestData
                     , col = colorsflowertypes
                     , range = 0
                     , xlab = "Flower Type chosen in Test"
                     , ylab = "Information difference from Sample Phase"
)
nbGroup <- nlevels(as.factor(Nice_Plot$names))
text( 
  x=c(1:nbGroup), 
  y=Nice_Plot$stats[3,] + 1,
  cex = 0.7,
  paste("n=", Nice_Plot$n)  
)
Nice_Plot <- boxplot(PerceivedValueDiff ~ FlowerType + HorizonOrder
                     , data = TestData
                     , col = colorsflowertypes
                     , range = 0
                     , xlab = "Flower Type chosen in Test"
                     , ylab = "Perceived value difference from Sample Phase"
)
nbGroup <- nlevels(as.factor(Nice_Plot$names))
text( 
  x=c(1:nbGroup), 
  y=Nice_Plot$stats[3,] + 0.1,
  cex = 0.7,
  paste("n=", Nice_Plot$n)  
)
mylogit<- glm(data = TestData, as.factor(FlowerType) ~ InformationDiff + PerceivedValueDiff + HorizonOrder + Color + InformationDiff:HorizonOrder + PerceivedValueDiff:HorizonOrder,
              family = "binomial")
summary(mylogit)
plot_model(mylogit)
# Color in graph shows positive vs negative effect, not significance!
mylogit<- glm(data = TestData, as.factor(FlowerType) ~ InformationDiff + PerceivedValueDiff + HorizonOrder + InformationDiff:HorizonOrder + PerceivedValueDiff:HorizonOrder,
              family = "binomial")
summary(mylogit)
plot_model(mylogit)
# Hah. Including order actually makes all the other factors have stronger effects,
# and order itself also has a decent odds ratio. Perhaps surprising, perhaps not.
# In any case, like color, it means we can't in fact easily ignore it, especially
# given low sample size. We need more data. 
# My instinct, given the boxplot patterns, is that bees learn to ignore 'information'
# (because that's actually just innate color bias) and instead pay attention to 'value'.
# This is why the effect of 'information' is less in the second round, and the
# effect of value is more in the second round (although still not sure about direction
# of value effect). 


#####################
# XVI. Different ways to treat revisits

## Revisits: Current analysis counts them as 0 reward.
# Alternatives are
# counting them as full reward or not
# counting them as visits at all. 


# This is now and option that can be set in the very beginning of the script.

################################
# XVII. Other?

## Collecting more data?
# The colors are not balanced by treatments. 
# If analyzing by color, some groups have really low 
# sample size; there is n=1 bee who chose a high-sample O flower.

## Space effects.
# We're also not testing this but I consider it low priority; I feel our
# arrangement is pretty well balanced against this affecting
# anything else.


#########################
# Part E. Analyze all choices rather than just first choice
##################################################



##################################################
###################################################
##################################################
# XII. Analysis as above but for all choices during test and sample phases

plot(CurrentInformationDiff ~ CurrentPerceivedValueDiff
     , data = DataByChoice
     , col = colorsflowertypes[as.factor(FlowerType)]
     , main = ""
)
modelhigh <- lm(CurrentInformationDiff ~ CurrentPerceivedValueDiff, data = subset(DataByChoice, FlowerType=="HighSample"))
modellow <- lm(InformationDiff ~ PerceivedValueDiff, data = subset(DataByChoice, FlowerType=="LowSample"))
abline(modelhigh, col = colorsflowertypes[1])
abline(modellow, col = colorsflowertypes[2])
summary(modelhigh)
summary(modellow)


mylogit<- glm(data = DataByChoice, as.factor(FlowerType) ~ CurrentInformationDiff + CurrentPerceivedValueDiff + HorizonOrder + Color + CurrentInformationDiff:HorizonOrder + CurrentPerceivedValueDiff:HorizonOrder,
              family = "binomial")
summary(mylogit)
plot_model(mylogit)

mylogit<- glm(data = DataByChoice, as.factor(FlowerType) ~ CurrentInformationDiff + CurrentPerceivedValueDiff + HorizonInTest + Color + CurrentInformationDiff:HorizonInTest + CurrentPerceivedValueDiff:HorizonInTest,
              family = "binomial")
summary(mylogit)
plot_model(mylogit)


Nice_Plot <- boxplot(CurrentInformationDiff ~ FlowerType
                     , data = DataByChoice
                     , col = colorsflowertypes
                     , range = 0
)
nbGroup <- nlevels(as.factor(Nice_Plot$names))
text( 
  x=c(1:nbGroup), 
  y=Nice_Plot$stats[3,] + 2,
  cex = 0.7,
  paste("n=", Nice_Plot$n)  
)
wilcox.test(CurrentInformationDiff ~ FlowerType, data = DataByChoice)


Nice_Plot <- boxplot(CurrentPerceivedValueDiff ~ FlowerType
                     , data = DataByChoice
                     , col = colorsflowertypes
                     , range = 0
)
nbGroup <- nlevels(as.factor(Nice_Plot$names))
text( 
  x=c(1:nbGroup), 
  y=Nice_Plot$stats[3,] + 0.1,
  cex = 0.7,
  paste("n=", Nice_Plot$n)  
)
wilcox.test(CurrentPerceivedValueDiff ~ FlowerType, data = DataByChoice)

############################




## More questions for analysis listed here
#https://docs.google.com/document/d/1dMMAm4RWYOGBEoNnYUuRUHbzfe3bHLatQqoLNH84TiA/edit

#############################

