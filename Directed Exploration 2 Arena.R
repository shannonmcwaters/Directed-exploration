# Shannon McWaters, Jack-Morgan Mizell, Ren Calabro, Kiara Alexis Casas, Robert C Wilson, Anna Dornhaus
# (c) 2020-2026
# Directed Exploration in Bumble bees
# Data analysis & graphs - part 2: ARENA

# Packages used ---------------------
library(tidyverse) # Data handling/converting
library(sjPlot) # Produce model output tables for print
library(viridis)

# Colors & options -----------------------------
colorsfam <- viridis(3, begin = 0.2, end = 0.8)
colorshor <- inferno(2, begin = 0.2, end = 0.8)
colorsparameters <- inferno(4, alpha = 0.8, begin = 0.2, end = 0.8)
# This is how many lines we plot when illustrating uncertainty around fits:
n_uncertainty <- 500
showsim <- FALSE
# How should revisits to the same individual be counted?
revisitoption <- "no reward" # counted as a visit with 0 reward
# revisitoption <- "not counted" # not counted as visit at all
# revisitoption <- "full" # counted as visit with full (intended) reward

# Importing data for experiment 2 directly from github -----------------------
ArenaData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/refs/heads/main/Arena%20Data%20raw")
# Should 'problematic' bees be excluded? 
ArenaData2 <- subset(ArenaData, Bee!="Yellow19" & Bee!="Green83" 
                     & Bee!="Yellow46" & Bee!="Green57")
# Yellow46 has only 2 rows order 1 (no horizon 2)
# Green57 has 2 rows order 1 (no horizon 16)
# Green83 - some error occurred in color recording

# What's wrong with Yellow19?

#####################################################################
# Data formatting & calculations --------------------------------------
# Filter for columns we will actually use here
FilteredArena <- ArenaData2 %>% dplyr::select(Bee,Horizon,Order,Session,Condition,VideoReviewedChoices,Rewards)
# Make long version of data where each choice is a row
Choice <- strsplit(as.character(FilteredArena$VideoReviewedChoices), split=",")
Choices<- unlist(Choice)
Flowervalue <- strsplit(FilteredArena$Rewards, split=",")
Reward <- unlist(Flowervalue)
Bee <- rep(FilteredArena$Bee, sapply(Choice, length))
Horizon <- rep(FilteredArena$Horizon, sapply(Choice, length))
Order <- rep(FilteredArena$Order, sapply(Choice, length))
Session <- rep(FilteredArena$Session, sapply(Choice, length))
Condition <- rep(FilteredArena$Condition, sapply(Choice, length))

#Combine to form new dataset
ArenaDataLong <- data.frame(Bee,Horizon,Order,Session,Condition,Choices,Reward)

# Make sure Order is numeric so filtering and min(Order) works later
ArenaDataLong$Order <- as.numeric(as.character(ArenaDataLong$Order))

# Assign ChoiceNumber correctly — reorder before numbering
ArenaDataLong <- ArenaDataLong %>%
  arrange(Bee, Order, Session) %>%
  group_by(Bee, Order, Session) %>%
  mutate(ChoiceNumber = row_number()) %>%
  ungroup()

# Add new column randomizing the assignment of Flower A and B
# Step 1: Create a random flip assignment per bee
#set.seed(123)  # Remove this if you want different randomization each time
bee_map <- ArenaDataLong %>%
  distinct(Bee, Order) %>% # This gets all unique color pairs (which are repeated across
  # horizon 6 and whatever the Test horizon is)
  mutate(flip_A = sample(c(TRUE, FALSE), n(), replace = TRUE))
# Step 2: Join it back to dataset
ArenaDataLong <- ArenaDataLong %>%
  left_join(bee_map, by = c("Bee", "Order"))
# Step 3: Apply random A/B mapping using ifelse logic
ArenaDataLong <- ArenaDataLong %>%
  mutate(
    FlowerType = case_when(
      flip_A & Session == "Test" & Choices %in% c("1", "3", "6", "8", "14", "16") ~ "A",
      flip_A & Session == "Sample" & Choices %in% c("1", "2", "4", "5") ~ "A",
      !flip_A & Session == "Test" & Choices %in% c("1", "3", "6", "8", "14", "16") ~ "B",
      !flip_A & Session == "Sample" & Choices %in% c("1", "2", "4", "5") ~ "B",
      TRUE ~ ifelse(flip_A, "B", "A")  # everything else gets the opposite
    )
  )

# Marks choice number per horizon
ArenaDataLong <- ArenaDataLong %>%
  arrange(Bee, Order, Session, ChoiceNumber) %>%  # Ensure data is ordered correctly
  group_by(Bee, Order) %>%
  mutate(HorizonChoiceNum = row_number()) %>%       # Generate a continuous choice number within each Horizon
  ungroup()

ArenaDataLong <- ArenaDataLong %>%
  mutate(
    Reward = na_if(Reward, ""),           # Convert empty strings to NA
    Reward = na_if(Reward, "NA"),         # Convert "NA" strings to actual NA
    Reward = as.numeric(Reward)           # Convert to numeric
  )

# Create the RewardRepeat column
# revisitoption <- "no reward" # counted as a visit with 0 reward
# revisitoption <- "not counted" # not counted as visit at all
# revisitoption <- "full" # counted as visit with full (intended) reward
revisitreward <- case_when(
  revisitoption == "no reward" ~ 0
  , revisitoption == "not counted" ~ NA
  , revisitoption == "full" ~ -99
)
ArenaDataLong <- ArenaDataLong %>%
  group_by(Bee, Order, Session, Choices) %>%
  mutate(FirstVisitToFlower = ifelse(row_number() == 1, TRUE, FALSE)) %>%
  mutate(
    CountedVisit = ifelse(revisitoption=="not counted", 
                          ifelse(FirstVisitToFlower, TRUE, FALSE)
                          , TRUE),
    RewardRepeat = ifelse(FirstVisitToFlower, Reward
                               , ifelse(revisitoption=="full", Reward
                                        , revisitreward))) %>%
  ungroup()

ArenaDataLong$RewardRepeat <- as.numeric(ArenaDataLong$RewardRepeat)

# Calculate running reward averages for each flower type
ArenaDataLong <- ArenaDataLong %>%
  arrange(Bee, Order, HorizonChoiceNum) %>%
  # This grouping includes all the bee's experience with this set of colors/
  # flower types
  group_by(Bee, Order) %>%
  mutate(
    # This is the number of visits and rewards *up to this point*, i.e. BEFORE
    # the bee is making the current choice.
    Visits_A = dplyr::lag(cumsum((FlowerType == "A") * CountedVisit)),
    Visits_B = dplyr::lag(cumsum((FlowerType == "B") * CountedVisit)),
    RewardSum_A = dplyr::lag(cumsum((Reward * (FlowerType == "A") * CountedVisit))),
    RewardSum_B = dplyr::lag(cumsum((Reward * (FlowerType == "B") * CountedVisit))),
    RewAvg_A = RewardSum_A/Visits_A,
    RewAvg_B = RewardSum_B/Visits_B,
    TrueInformative = case_when(
      Visits_A >  Visits_B ~ "B",     # More visits to A → B is less familiar
      Visits_A <  Visits_B ~ "A",     # More visits to B → A is less familiar
      Visits_A ==  Visits_B ~ "Equal" # Equal visits → neither is more informative
    ),
    HighValueFlower = case_when(
      RewAvg_A > RewAvg_B ~ "A",
      RewAvg_A < RewAvg_B ~ "B",
      RewAvg_A == RewAvg_B ~ "Equal"
    ),
    PropValDiff_FlowerA = (RewAvg_A - RewAvg_B) / (RewAvg_A + RewAvg_B),
    RelativeFamiliarity_FlowerA = (Visits_A - Visits_B) / (Visits_A + Visits_B),
    PropValDiff_FlowerB = (RewAvg_B - RewAvg_A) / (RewAvg_A + RewAvg_B),
    RelativeFamiliarity_FlowerB = (Visits_B - Visits_A) / (Visits_A + Visits_B),
    # Value difference from informative flower's perspective
    PropValDiff_Fam = case_when(
      TrueInformative == "A" ~ PropValDiff_FlowerA,
      TrueInformative == "B" ~  PropValDiff_FlowerB,
      TrueInformative == "Equal" ~ 0,
      TRUE ~ NA_real_
    ),
    # Proportion of visits to informative flower
    RelativeFamiliarity_Fam = case_when(
      TrueInformative == "A" ~ RelativeFamiliarity_FlowerA,
      TrueInformative == "B" ~ RelativeFamiliarity_FlowerB,
      TrueInformative == "Equal" ~ 0,
      TRUE ~ NA_real_
    ),
    # Value difference from high-value flower's perspective
    PropValDiff_Value = case_when(
      HighValueFlower == "A" ~ PropValDiff_FlowerA,
      HighValueFlower == "B" ~  PropValDiff_FlowerB,
      HighValueFlower == "Equal" ~ 0,
      TRUE ~ NA_real_
    ),
    # Proportion of visits to high-value flower
    RelativeFamiliarity_Value = case_when(
      HighValueFlower == "A" ~ RelativeFamiliarity_FlowerA,
      HighValueFlower == "B" ~ RelativeFamiliarity_FlowerB,
      HighValueFlower == "Equal" ~ 0,
      TRUE ~ NA_real_
    )
    
  ) %>%
  ungroup()

# Filter out the first choice in the Test trial, which is the key choice we are
# evaluating.
TestChoice <- ArenaDataLong %>%
  filter(Session == "Test", ChoiceNumber == 1)

TestChoice$chose_flowerA <- ifelse(TestChoice$FlowerType == "A", 1, 0)
TestChoice$choseHighValue = ifelse(TestChoice$FlowerType == TestChoice$HighValueFlower, 1, 0)
TestChoice$choseInformative = ifelse(TestChoice$FlowerType == TestChoice$TrueInformative, 1, 0)
#####################################################################

#### SIMULATED DATA



## Generative model and simulated data -------------------------
ifelse(showsim
       , dat <- simdata
       , dat <- TestChoice
)
## end sim ---------------------------


#####################################################################
#### Bayesian model setup


# I. GLM full
chooseAmod_full <- glm(chose_flowerA ~ PropValDiff_FlowerA * RelativeFamiliarity_FlowerA + Order, family = binomial, data = dat)
summary(chooseAmod_full)
## Output table GLM for paper -------------------------
tab_model(chooseAmod_full
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Prop. Reward Difference (conc. A - B/conc. A + B)"
                            , "Prop. Familiarity Difference (A-B/A+B)"
                            , "Order"
                            , "Interaction Prop. Rew Diff x Prop. Fam Diff"
          )
          , dv.labels = "Effect on probability of choosing flower type A"
)

## Illustrate effects of familiarity and value (and interaction?) 
# x - Prop reward diff
# y - choose flower A
# 3 panels, fam <0, fam = 0, fam >0

# II. GLM full

rndExploration_modA <- glm(chose_flowerA ~ PropValDiff_FlowerA * Horizon + Order, family = binomial, data = dat)
summary(rndExploration_modA)
tab_model(rndExploration_modA
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward Difference (right - left)"
                            , "Horizon"
                            , "Order"
                            , "Interaction Rew Diff x Horizon"
          )
          , dv.labels = "Effect on probability of choosing flower A"
)

dirExpl_HI_modA <- glm(choseInformative ~ PropValDiff_Fam * Horizon + Order, family = binomial, data = dat)
summary(dirExpl_HI_modA)
tab_model(dirExpl_HI_modA
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward Difference (unfamiliar - familiar)"
                            , "Horizon"
                            , "Order"
                            , "Interaction Rew Diff x Horizon"
          )
          , dv.labels = "Effect on probability of choosing less familiar flower"
)

# Plot 
# x value of informative
# y chose informative
# panels order 1 order 2
# horizon 2 vs 16 as different colors


## Barplot of the same
summary_bar <- TestChoice_unequal %>%
  mutate(InformativeChoice = ifelse(choseInformative == 1, "Informative", "Uninformative")) %>%
  group_by(Horizon,Order, InformativeChoice) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Horizon, Order) %>%
  mutate(prop = n / sum(n))
ggplot(summary_bar, aes(x = factor(Horizon), y = prop, fill = InformativeChoice)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Order, labeller = label_both) +
  scale_fill_manual(
    values = c("Informative" = "#3690c0", "Uninformative" = "#a6bddb"),
    labels = c("Informative" = "Less Familiar", "Uninformative" = "More Familiar")
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  labs(
    x = "Horizon",
    y = "Proportion of Choices",
    fill = "Choice"
  ) +
  theme_minimal(base_size = 14)


