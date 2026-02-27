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

# Make sure Order is numeric so filtering and min(Order) works later
ArenaDataLong$Order <- as.numeric(as.character(ArenaDataLong$Order))

# Assign ChoiceNumber correctly — reorder before numbering
ArenaDataLong <- ArenaDataLong %>%
  arrange(Bee, Order, Session) %>%
  group_by(Bee, Order, Session) %>%
  mutate(ChoiceNumber = row_number()) %>%
  ungroup()
# Add new column randomizing the assingment of Flower A and B
# Step 1: Create a random flip assignment per bee
set.seed(123)  # Remove this if you want different randomization each time

bee_map <- ArenaDataLong %>%
  distinct(Bee, Order) %>%
  mutate(flip_A = sample(c(TRUE, FALSE), n(), replace = TRUE))

# Step 2: Join it back to dataset
ArenaDataLong <- ArenaDataLong %>%
  left_join(bee_map, by = c("Bee", "Order"))


# Step 3: Apply random A/B mapping using ifelse logic
ArenaDataLong <- ArenaDataLong %>%
  mutate(
    FlowerID = case_when(
      flip_A & Session == "Test" & Choices %in% c("1", "3", "6", "8", "14", "16") ~ "A",
      flip_A & Session == "Sample" & Choices %in% c("1", "2", "4", "5") ~ "A",
      !flip_A & Session == "Test" & Choices %in% c("1", "3", "6", "8", "14", "16") ~ "B",
      !flip_A & Session == "Sample" & Choices %in% c("1", "2", "4", "5") ~ "B",
      TRUE ~ ifelse(flip_A, "B", "A")  # everything else gets the opposite
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
  mutate(FirstVisit = !duplicated(Choices)) %>%
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

####MAIN TEST: Do bees care about information difference?####

# In their first choice in the test phase, is the bee's choice
# predicted at all by information, or only by value difference?
FlowerAmod = glm(chose_flowerA ~ PropValDiff_FlowerA * Horizon + RelativeFamiliarity_FlowerA, family = binomial, data = FirstChoice)
summary(FlowerAmod)


#For info choice - use unequal info only

infomodarena = glm(ChoseInformative ~ PropValDiff + Horizon + Order, family = binomial, data = FirstChoice_unequal)
summary(infomodarena)

valuemod = glm(ChoseHighValue ~ as.factor(Horizon)+ RelativeFamiliarity_Value + Order, family = binomial, data = FirstChoice)
summary(valuemod)

####Plots####
#Goal: Show how value difference and informational difference predict preference for Flower A.
library(patchwork)

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
#plot based on model predictions
library(ggeffects)
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
library(ggplot2)

ggplot(FirstChoice_unequal, aes(x = factor(Order), y = predicted_prob, fill = factor(Horizon))) +
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.7, outlier.shape = NA) +
  labs(
    x = "Test Bout (Order)",
    y = "Predicted Probability of Choosing Informative Flower",
    fill = "Horizon"
  ) +
  theme_minimal(base_size = 14)


library(broom.mixed)
library(sjPlot)

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
