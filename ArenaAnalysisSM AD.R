# Shannon McWaters, Jack-Morgan Mizell, Ren Calabro, Kiara Alexis Casas, Robert C Wilson, Anna Dornhaus
# (c) 2020-2026
# Directed Exploration in Bumble bees
# Data analysis & graphs - part 2: ARENA

# Packages used ---------------------
library(tidyverse) # Data handling/converting
library(patchwork) # Arrangement of plots
library(ggeffects) # Fine-tuning axes of ggplot
#library(broom.mixed)
library(sjPlot) # Produce model output tables for print
library(viridis)

#####################################################################
# Importing data for experiment 2 directly from github -----------------------
ArenaData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/refs/heads/main/Arena%20Data%20raw")
# Should 'problematic' bees be excluded? 
ArenaData2 <- subset(ArenaData, Bee!="Yellow19" & Bee!="Green83" 
                     & Bee!="Yellow46" & Bee!="Green57")
# Yellow46 has only 2 rows order 1 (no horizon 2)
# Green57 has 2 rows order 1 (no horizon 16)
# Green83 - some error occurred in color recording
# What's wrong with Yellow19?

# How should revisits to the same individual be counted?
revisitoption <- "no reward" # counted as a visit with 0 reward
# revisitoption <- "not counted" # not counted as visit at all
# revisitoption <- "full" # counted as visit with full (intended) reward

showsim <- FALSE

# Colors -----------------------------
colorsfam <- viridis(3, begin = 0.2, end = 0.8)
colorshor <- inferno(2, begin = 0.2, end = 0.8)
colorsparameters <- inferno(4, alpha = 0.8, begin = 0.2, end = 0.8)
# This is how many lines we plot when illustrating uncertainty around fits:
n_uncertainty <- 500

#####################################################################
#### Data wrangling --------------------------------------
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
    Visits_A = cumsum((FlowerType == "A") * CountedVisit),
    Visits_B = cumsum((FlowerType == "B") * CountedVisit),
    RewardSum_A = cumsum((Reward * (FlowerType == "A") * CountedVisit)),
    RewardSum_B = cumsum((Reward * (FlowerType == "B") * CountedVisit)),
    RewAvg_A = RewardSum_A/Visits_A,
    RewAvg_B = RewardSum_B/Visits_B
  ) %>%
  ungroup()


# Filter out the first choice in the Test trial, which is the key choice we are
# evaluating.
TestChoice <- ArenaDataLong %>%
  filter(Session == "Test", ChoiceNumber == 1)
FirstChoice <- FirstChoice %>%
  mutate(TrueInformative = case_when(
    UniqueVisits_A > UniqueVisits_B ~ "B",     # More visits to A → B is less familiar
    UniqueVisits_A < UniqueVisits_B ~ "A",     # More visits to B → A is less familiar
    UniqueVisits_A == UniqueVisits_B ~ "Equal" # Equal visits → neither is more informative
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
    RelativeFamiliarity_FlowerA = (UniqueVisits_A - UniqueVisits_B) / (UniqueVisits_A + UniqueVisits_B)
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
      TrueInformative == "A" ~ (UniqueVisits_A - UniqueVisits_B) / (UniqueVisits_A + UniqueVisits_B),
      TrueInformative == "B" ~ (UniqueVisits_B - UniqueVisits_A) / (UniqueVisits_A + UniqueVisits_B),
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
      HighValueFlower == "A" ~ (UniqueVisits_A - UniqueVisits_B) / (UniqueVisits_A + UniqueVisits_B),
      HighValueFlower == "B" ~ (UniqueVisits_B - UniqueVisits_A) / (UniqueVisits_A + UniqueVisits_B),
      HighValueFlower == "Equal" ~ 0,
      TRUE ~ NA_real_
    )
  )
FirstChoice <- FirstChoice %>%
  mutate(
    Horizon = factor(Horizon),
    Order = factor(Order)
  )
FirstChoice <- FirstChoice %>%
  mutate(
    ChoseHighValue = ifelse(FlowerID == HighValueFlower, 1, 0)
  )
FirstChoice_unequal <- FirstChoice %>%
  filter(TrueInformative != "Equal") %>%
  mutate(ChoseInformative = ifelse(FlowerID == TrueInformative,1,0))
#####################################################################


## Generative model and simulated data -------------------------
ifelse(showsim
       , dat <- simdata
       , dat <- ArenaData
)
## end sim ---------------------------


#####################################################################
####MAIN TEST: Do bees care about information difference?####

# In their first choice in the test phase, is the bee's choice
# predicted at all by information, or only by value difference?
FlowerAmod = glm(chose_flowerA ~ PropValDiff_FlowerA * Horizon + RelativeFamiliarity_FlowerA, family = binomial, data = FirstChoice)
summary(FlowerAmod)


# For info choice - use unequal info only

infomodarena = glm(ChoseInformative ~ PropValDiff + Horizon + Order, family = binomial, data = FirstChoice_unequal)
summary(infomodarena)

valuemod = glm(ChoseHighValue ~ as.factor(Horizon)+ RelativeFamiliarity_Value + Order, family = binomial, data = FirstChoice)
summary(valuemod)

####Plots####
#Goal: Show how value difference and informational difference predict preference for Flower A.

# Plot 1: Relative Familiarity of Flower A
p1 <- ggplot(FirstChoice, aes(x = RelativeFamiliarity_FlowerA, y = chose_flowerA)) +
  geom_jitter(height = 0, width = 0, alpha = 0.4) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  geom_vline(xintercept = 0.0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.4) +
  labs(x = "Relative Familiarity of Flower A",
       y = "Probability of Choosing Flower A") +
  theme_minimal()

# Plot 2: Relative Value of Flower A
p2 <- ggplot(FirstChoice, aes(x = PropValDiff_FlowerA, y = chose_flowerA)) +
  geom_jitter(height = 0, width = 0, alpha = 0.4) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  geom_vline(xintercept = 0.0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.4) +
  labs(x = "Relative Value of Flower A",
       y = NULL) +
  theme_minimal()

# Combine side by side
p1 + p2


# Plot based on model predictions
infomod <- glm(ChoseInformative ~ Horizon * Order + PropValDiff,
               family = binomial, data = FirstChoice_unequal)

preds <- ggpredict(infomod, terms = c("PropValDiff", "Horizon", "Order"))
ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, color = NA) +
  facet_wrap(~ facet, labeller = labeller(facet = function(x) paste("Order", x))) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Relative Value of Informative Flower",
    y = "Predicted Probability of Choosing Informative Flower",
    color = "Horizon",
    fill = "Horizon"
  ) +
  theme_minimal(base_size = 14)
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


preds_box <- ggpredict(infomod, terms = c("Order", "Horizon"))
# Add predicted probabilities to each row
FirstChoice_unequal$predicted_prob <- predict(infomod, type = "response")

# Now plot predicted probabilities as boxplots grouped by Order & Horizon

ggplot(FirstChoice_unequal, aes(x = factor(Order), y = predicted_prob, fill = factor(Horizon))) +
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.7, outlier.shape = NA) +
  labs(
    x = "Test Bout (Order)",
    y = "Predicted Probability of Choosing Informative Flower",
    fill = "Horizon"
  ) +
  theme_minimal(base_size = 14)



tab_model(
  FlowerAmod,
  show.re.var = FALSE,
  pred.labels = c(
    "Intercept",
    "Relative Reward of Flower A",
    "Long Horizon",
    "Relative Familiarity (Flower A)",
    "Reward × Horizon Interaction"
  ),
  dv.labels = "Arena Task – Choice of Flower A"
)

tab_model(
  infomodarena, 
  show.re.var = FALSE,
  pred.labels = c(
    "Intercept",
    "Relative Value Difference (Informative Flower)",
    "Long Horizon",
    "Test Order"
  ),
  dv.labels = "Arena Task – Choice of Informative Flower (Unequal Info)"
)
