# Shannon McWaters, Jack-Morgan Mizell, Ren Calabro, Kiara Alexis Casas, Robert C Wilson, Anna Dornhaus
# (c) 2020-2026
# Directed Exploration in Bumble bees
# Data analysis & graphs - part 2: ARENA

## ARENA Exp
# Data problems:
# - FlowerColor has only 630 entries, choices had 662
# - Yellow19 and Green83: 
# number of choices is 759; rewards is 707, chosen color 690, informCh 707
# Analysis problems:
# - the A-B choices graph should really use absolute concentration on flower A, 
# not concentration difference; or if we only have difference, we should focus on 
# familiarity difference not whether it's A or B or right or left

# SET THESE OPTIONS BEFORE RUNNING SCRIPT -----------------
showsim <- FALSE
showBayes <- TRUE
showDetail <- FALSE
saveFigs <- FALSE
countOnlyFirstChoice <- TRUE
# How should revisits to the same individual be counted?
revisitoption <- "no reward" # counted as a visit with 0 reward
# revisitoption <- "not counted" # not counted as visit at all
# revisitoption <- "full" # counted as visit with full (intended) reward

# Packages used ---------------------
library(tidyverse) # Data handling/converting
library(sjPlot) # Produce model output tables for print
library(viridis) # Color scales

# Colors & options -----------------------------
colorsfam <- viridis(3, begin = 0.2, end = 0.8)
colorshor <- inferno(2, begin = 0.2, end = 0.8)
colorsparameters <- inferno(4, alpha = 0.8, begin = 0.2, end = 0.8)
transparency_glm_uncertainty <- 0.05 #0.01 #0.1
transparency_Bay_uncertainty <- 0.15 #0.018 #0.15
colorassumption <- "darkred"
colorprior <- "slateblue"
# This is how many lines we plot when illustrating uncertainty around fits:
n_uncertainty <- 300
n_ppp_per_set <- 200
N_sim <- 1000

# Importing data for experiment 2 directly from github -----------------------
ArenaData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/refs/heads/main/Arena%20Data%20raw")
# Should 'problematic' bees be excluded? 
ArenaData2 <- subset(ArenaData, Bee!="Yellow46" & Bee!="Green57" & Bee!="Yellow19" & Bee!="Green83")
# Yellow46 has only 2 rows order 1 (no horizon 2)
# Green57 has 2 rows order 1 (no horizon 16)

#####################################################################
# Data formatting & calculations --------------------------------------
# Filter for columns we will actually use here
FilteredArena <- ArenaData2 %>% dplyr::select(Bee,Horizon,Order,Session,Condition,VideoReviewedChoices,Rewards, InformativeColor, InformativeChoice..0.No.1.Yes.)
# Make long version of data where each choice is a row
Choice <- strsplit(as.character(FilteredArena$VideoReviewedChoices), split=",")
Choices<- unlist(Choice)
InformChoice <- strsplit(FilteredArena$InformativeChoice..0.No.1.Yes., split=",")
InformChoices <- unlist(InformChoice)
Flowervalue <- strsplit(FilteredArena$Rewards, split=",")
Reward <- unlist(Flowervalue)
Bee <- rep(FilteredArena$Bee, sapply(Choice, length))
Horizon <- rep(FilteredArena$Horizon, sapply(Choice, length))
Order <- rep(FilteredArena$Order, sapply(Choice, length))
Session <- rep(FilteredArena$Session, sapply(Choice, length))
Condition <- rep(FilteredArena$Condition, sapply(Choice, length))
InformativeColor <- rep(FilteredArena$InformativeColor, sapply(Choice, length))
# Colors
infcol <- FilteredArena$InformativeColor
infchoi <- as.numeric(substr(FilteredArena$InformativeChoice..0.No.1.Yes., start = 1, stop = 1))
FilteredArena$ColorFirstChoice <- ifelse(infchoi==1, infcol, 
                case_when(
                  infcol=="B" ~ "DG",
                  infcol=="DG" ~ "B",
                  infcol=="LG" ~ "O",
                  infcol=="O" ~ "LG"
                  )
                )
ColorFirstChoice <- rep(FilteredArena$ColorFirstChoice, sapply(Choice, length))
Horizon <- as.factor(Horizon)
Phase <- ifelse(Horizon==6, "Training", "Test")
Horizon[Horizon=="6"] <- NA
Horizon <- droplevels(Horizon)
#Order <- as.factor(Order)

#Combine to form new dataset
ArenaDataLong <- data.frame(Bee,Horizon,Order,Session,Condition,Choices,Reward,ColorFirstChoice, InformativeColor, InformChoices)

# Make sure Order is numeric 
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
    RewardSum_A = dplyr::lag(cumsum(ifelse(is.na((RewardRepeat * (FlowerType == "A") * CountedVisit)), 0, (RewardRepeat * (FlowerType == "A") * CountedVisit)))),
    RewardSum_B = dplyr::lag(cumsum(ifelse(is.na((RewardRepeat * (FlowerType == "B") * CountedVisit)), 0, (RewardRepeat * (FlowerType == "A") * CountedVisit)))),
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
    ),
    TypeChoice = ifelse(CountedVisit, FlowerType, NA),
    chose_flowerA = ifelse(!is.na(TypeChoice), ifelse(TypeChoice == "A", 1, 0), NA),
    choseHighValue = ifelse(!is.na(TypeChoice), ifelse(TypeChoice == HighValueFlower, 1, 0), NA),
    choseInformative = ifelse(!is.na(TypeChoice), ifelse(TypeChoice == TrueInformative, 1, 0), NA)
  ) %>%
  ungroup()

# Filter out the first choice in the Test trial, which is the key choice we are
# evaluating.
TestChoice <- ArenaDataLong %>%
  filter(Session == "Test", ChoiceNumber == 1)

ColorLookup <- TestChoice %>% dplyr::select(Bee, Order, TypeChoice, ColorFirstChoice)

ArenaDataLong <- ArenaDataLong %>%
  left_join(ColorLookup, by = c("Bee", "Order"), suffix = c("_dat", "_first")) %>%
  mutate(ArenaDataLong, ColorChosen = 
           ifelse(TypeChoice_dat==TypeChoice_first, ColorFirstChoice_first,
                  case_when(
                    ColorFirstChoice_first=="B" ~ "DG",
                    ColorFirstChoice_first=="DG" ~ "B",
                    ColorFirstChoice_first=="LG" ~ "O",
                    ColorFirstChoice_first=="O" ~ "LG"
                  )))
  
ArenaDataLong$TreatmentInformative <- ifelse(ArenaDataLong$ColorChosen==ArenaDataLong$InformativeColor, TRUE, FALSE)
ArenaDataLong$TreatInfType <- ifelse(ArenaDataLong$TreatmentInformative, ArenaDataLong$TypeChoice, 
                                     ifelse(ArenaDataLong$TypeChoice=="A", "B", "A"))
ArenaDataLong$FamiliarityA <- ifelse(ArenaDataLong$TreatInfType=="A", 0, 1)
ArenaDataLong$ColorChosen <- factor(ArenaDataLong$ColorChosen)
ArenaDataLong$ColorNotChosen <- case_when(
  ArenaDataLong$ColorChosen=="B" ~ "DG",
  ArenaDataLong$ColorChosen=="DG" ~ "B",
  ArenaDataLong$ColorChosen=="LG" ~ "O",
  ArenaDataLong$ColorChosen=="O" ~ "LG")

colpref <- table(ArenaDataLong$ColorChosen) / (table(ArenaDataLong$ColorChosen) + table(ArenaDataLong$ColorNotChosen))
ArenaDataLong$ColorPref <- colpref[ArenaDataLong$ColorChosen]
ArenaDataLong$ColorPref <- as.vector(colpref[ArenaDataLong$ColorChosen])
ArenaDataLong$ColorPref[is.na(ArenaDataLong$ColorPref)] <- 0.5

ArenaDataLong <- ArenaDataLong %>% dplyr::select(-TypeChoice_first, -ColorFirstChoice_first, -ColorFirstChoice_dat) 

# Value difference from informative flower's perspective
ArenaDataLong$PropValDiff_Treat <- case_when(
  ArenaDataLong$TreatInfType == "A" ~ ArenaDataLong$PropValDiff_FlowerA,
  ArenaDataLong$TreatInfType == "B" ~  ArenaDataLong$PropValDiff_FlowerB,
  ArenaDataLong$TreatInfType == "Equal" ~ 0,
  TRUE ~ NA_real_
)
ArenaDataLong$choseTreatInformative <- as.numeric(ArenaDataLong$TreatmentInformative)
ArenaDataLong$Horizon_fac <- as.numeric(ArenaDataLong$Horizon) -1
ArenaDataLong$Order_fac <- ArenaDataLong$Order - 1

# Filter out the first choice in the Test trial, which is the key choice we are
# evaluating.
TestChoice <- ArenaDataLong %>%
  filter(Session == "Test", ChoiceNumber == 1)
## Color preferences Fig. -----------------------------------
colpref_first <- table(TestChoice$ColorChosen) / (table(TestChoice$ColorChosen) + table(TestChoice$ColorNotChosen))
par(mfrow=c(1,2), mar=c(4,4,0,0), oma=c(0,0,0,0))
colorlist_colors <- c("black", "green4", "green1", "orange")
spacing <- c(0.1, 0, 0.1, 0)
textpos <- cumsum(spacing)

Nice_plot <- barplot(colpref_first
                     , ylim = c(0,1)
                     , ylab = "Proportion of choices out of available trials"
                     , xlab = "Color pref. in test choice"
                     , col = colorlist_colors
                     , space = spacing
                     , cex.names = 0.8
                     , names.arg = c("Black", "Dark Green", "Light Green", "Orange")
)
text((1:4)+textpos-0.5, 0.1, paste("n=",table(TestChoice$ColorChosen), sep=""), col = c("white", "black","black","black"))

Nice_plot <- barplot(colpref
                     , ylim = c(0,1)
                     , ylab = "Proportion of choices out of available trials"
                     , xlab = "Color pref. across all choices"
                     , col = colorlist_colors
                     , space = spacing
                     , cex.names = 0.8
                     , names.arg = c("Black", "Dark Green", "Light Green", "Orange")
)
text((1:4)+textpos-0.5, 0.1, paste("n=",table(ArenaDataLong$ColorChosen), sep=""), col = c("white", "black","black","black"))


#####################################################################

#######################################################
# MODELING PROBABILITY OF CHOOSING FLOWER 'A' AS REWARD x FAMILIARITY ------
# First, we define some helper functions --------------------------
# Let's say that bees choose a probability for selecting the left or right flower
# based on their estimated concentration difference as well as the difference in familiarity.
probs_from_pars <- function(factor1, factor2, factor3, intercept = 0, slope1 = 0, slope2 = 0, slope1x2 = 0, slope3 = 0) {
  samplesize <- length(factor1)
  linear_predictor <- intercept + slope1 * factor1 + slope2 * factor2 + slope1x2 * factor1 * factor2 + slope3 * factor3
  probability_right <- 1/(1+exp(-linear_predictor))
  return(probability_right)
}
# This function just picks actual choices from probabilities
sim_choiceR <- function(probabilities) {
  samplesize <- length(probabilities)
  choice_roll <- runif(samplesize, 0, 1)
  choices <- ifelse(choice_roll<probabilities, "right", "left")
  return(choices)
}
# Function to make figure for comparing odds ratios (effect sizes) for both models -------------------------
model_comp <- function(posterior, model_fit, par_labels, no_pars) {
  pars_Bayes <- precis(posterior)[,1]
  pars_GLM <- coef(model_fit)
  or_Bayes <- exp(pars_Bayes)
  or_GLM <- exp(pars_GLM)
  se_Bayes <- precis(posterior)[,2]
  se_GLM <- summary(model_fit)$coefficients[, 2]
  upper_Bayes <- exp(pars_Bayes+se_Bayes)
  lower_Bayes <- exp(pars_Bayes-se_Bayes)
  upper_GLM <- exp(pars_GLM+se_GLM)
  lower_GLM <- exp(pars_GLM-se_GLM)
  par(mfrow=c(1,2))
  par(mar=c(4, 1, 0.5, 0.5), oma=c(1, 9, 0, 0))
  plot(NULL
       , xlim=c(0,3)
       , ylim=c(0.5, no_pars+0.5)
       , ylab=""
       , yaxt = "n"
       , xlab = "Odds ratio fits from Bayesian model"
  )
  axis(side = 2, at = 1:no_pars, labels = par_labels, las = 1, cex = 0.8)
  abline(v=1, lty = 2, col = "grey")
  for(i in 1:no_pars) {
    abline(h=i, lty = 2, col = "grey")
  }
  segments(x0 = lower_Bayes, y0 = no_pars:1, 
           x1 = upper_Bayes, y1 = no_pars:1, 
           lwd = 4, col = "darkgrey")
  points(x = or_Bayes, y = no_pars:1, 
         pch = 19, cex = 1.5, col = "black")
  #mtext("Factor", side = 2, outer = TRUE, line = 5)
  plot(NULL
       , xlim=c(0,3)
       , ylim=c(0.5, no_pars+0.5)
       , ylab=""
       , yaxt = "n"
       , xlab = "Odds ratio fits from GLM"
  )
  abline(v=1, lty = 2, col = "grey")
  for(i in 1:no_pars) {
    abline(h=i, lty = 2, col = "grey")
  }
  segments(x0 = lower_GLM, y0 = no_pars:1, 
           x1 = upper_GLM, y1 = no_pars:1, 
           lwd = 4, col = "darkgrey")
  points(x = or_GLM, y = no_pars:1, 
         pch = 19, cex = 1.5, col = "black")
}

# Choose dataset to use -------------------------
if(showsim) {
  dat <- simdata
} else {
  if(countOnlyFirstChoice) {
    dat <- TestChoice
  } else {
    dat <- ArenaDataLong
  }
}
dat <- dat[!is.na(dat$PropValDiff_FlowerA),]
# Bayesian analysis with quap ----------------------
## First, define priors -------------------
# Priors involve the probability distributions of all the parameters, so typically
# means and standard deviations.
intercept_prior_mean <- 0.5
intercept_prior_sd <- 0.3
slopeval_prior_mean <- 1
slopeval_prior_sd <- 0.5
slopefam_prior_mean <- 0
slopefam_prior_sd <- 2
slopevf_prior_mean <- 0
slopevf_prior_sd <- 2
slopecol_prior_mean <- 0.5
slopecol_prior_sd <- 0.1

## BAYESIAN MODEL FORMUAL and assumption list ---------------
## The model is specified in the assumptions, and should match what we previously
## decided as the 'generative' model, i.e. the one used to make the simdata. 
## I am using the functions & format for the Bayesian model from Richard McElreath's
## 'Statistical Rethinking' book and the accompanying package 'rethinking'.
list_of_assumptions <- alist(
  # Actual output is list of choices CL
  # Analogous to picking points based on a model and error term, here we pick actual
  # choices based on the probability (~model). There is no 'error term'.
  # Note all the dbinom, dnorm functions describe the probability density functions.
  CL ~ dbinom(1, prob), 
  # This is the core model formula
  logit(prob) <- intpt + pval*val + pfam*fam + pcol * col + pvxf*val*fam, 
  # And the following describes the priors for the parameters
  intpt ~ dnorm(intercept_prior_mean, intercept_prior_sd),
  pval ~ dnorm(slopeval_prior_mean, slopeval_prior_sd), 
  pfam ~ dnorm(slopefam_prior_mean, slopefam_prior_sd),
  pcol ~ dnorm(slopecol_prior_mean, slopecol_prior_sd),
  pvxf ~ dnorm(slopevf_prior_mean, slopevf_prior_sd)
)
# We may or may not need 'start' to help the model converge on a solution. 
# This is not a prior and should not affect the actual resulting estimates.
start <- function(dat) {
  return(
    list(
      intpt = intercept_prior_mean
      , pval = slopeval_prior_mean
      , pfam = slopefam_prior_mean
      , pcol = slopecol_prior_mean
      , pvxf = slopevf_prior_mean
    )
  )
}

## Bayesian estimation function ------------------
# We're going to use quap() for the actual estimation. 
# CL stands for choice list, val is the value or reward difference between the options,
# fam is the familiarity difference between the options. 
BayesChooseA <- function(dat) {
  quap(
    list_of_assumptions
    , data = list(CL = dat$chose_flowerA, val = dat$PropValDiff_FlowerA, fam = dat$FamiliarityA, col = dat$ColorPref)
    , start = start(dat)
  )
}

# I. GLM & Bayes  choose right ------------------------
chooseA <- glm(chose_flowerA ~ PropValDiff_FlowerA * FamiliarityA + ColorPref, family = binomial, data = dat[!is.na(dat$PropValDiff_FlowerA),])

# Bayesian model for choosing right flower
posterior_bees <- BayesChooseA(dat[!is.na(dat$PropValDiff_FlowerA),])
# Done!

## Table I. GLM  -------------------------
tab_model(chooseA
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward Difference (concentration right - left)"
                            , "Familiarity Difference (right - left)"
                            , "Color Bias"
                            , "Interaction Rew Diff x Fam Diff"
          )
          , dv.labels = "Effect on probability of choosing right flower"
)
#######################################################
## Effect sizes figure - Fig S2 to go with Fig. 3 -------------------------
lab <- c("Interaction Val x Fam", "Color", "Familiarity", "Value", "Intercept")
model_comp(posterior_bees, chooseA, lab, 5)


#######################################################
# MODELING EFFECT OF HORIZON TWO WAYS: on 'right' choices and on choosing 'unfamiliar' -----------------
#######################################################
# II. Modeling choice of flower on right with random and directed exploration ------------
# Bayesian analysis with quap ----------------------
## Priors -------------------
# Priors involve the probability distributions of all the parameters, so typically
# means and standard deviations.
# We'll use the same priors as before for all the parameters we had before. 
# For the new ones:
rnd_explor_prior_mean <- 0
rnd_explor_prior_sd <- 2
dir_explor_prior_mean <- 0
dir_explor_prior_sd <- 2

## Model formula and assumption list ---------------
## The model is specified in the assumptions, and should match what we previously
## decided as the 'generative' model, i.e. the one used to make the simdata. 
## I am using the functions & format for the Bayesian model from Richard McElreath's
## 'Statistical Rethinking' book and the accompanying package 'rethinking'.
list_of_assumptions <- alist(
  # Actual output is list of choices CL
  # Analogous to picking points based on a model and error term, here we pick actual
  # choices based on the probability (~model). There is no 'error term'.
  # Note all the dbinom, dnorm functions describe the probability density functions.
  CL ~ dbinom(1, prob), 
  # This is the core model formula
  #RightConcentrationDiff:Horizon + famdiffs:Horizon
  logit(prob) <- intpt + pcol*col + pval*val + pvxf*val*fam + rnd*hor*val + dir*hor*fam, 
  # And the following describes the priors for the parameters
  intpt ~ dnorm(intercept_prior_mean, intercept_prior_sd),
  pval ~ dnorm(slopeval_prior_mean, slopeval_prior_sd), 
  pcol ~ dnorm(slopecol_prior_mean, slopecol_prior_sd),
  pvxf ~ dnorm(slopevf_prior_mean, slopevf_prior_sd),
  rnd ~ dnorm(rnd_explor_prior_mean, rnd_explor_prior_sd),
  dir ~ dnorm(dir_explor_prior_mean, dir_explor_prior_sd)
)
# We may or may not need 'start' to help the model converge on a solution. 
# This is not a prior and should not affect the actual resulting estimates.
start <- function(dat) {
  return(
    list(
      intpt = intercept_prior_mean
      , pval = slopeval_prior_mean
      , pcol = slopecol_prior_mean
      , pvxf = slopevf_prior_mean
      , rnd = rnd_explor_prior_mean
      , dir = dir_explor_prior_mean
    )
  )
}
## Bayesian estimation function ------------------
# We're going to use quap() for the actual estimation. 
# CL stands for choice list, val is the value or reward difference between the options,
# fam is the familiarity difference between the options. 
# For horizon we want hor=0 for 'Horizon 1' and hor=1 for 'Horizon 6'.
BayesExploreR <- function(dat) {
  quap(
    list_of_assumptions
    , data = list(CL = dat$chose_flowerA, val = dat$PropValDiff_FlowerA, fam = dat$FamiliarityA, col = dat$ColorPref, hor = (as.numeric(dat$Horizon)-1))
    , start = start(dat)
  )
}

## Bayesian fitting -----------------------
posterior_ExplA <- BayesExploreR(dat[!is.na(dat$PropValDiff_FlowerA),])
# GLM choice right flower' with exploration ------------------------------------
expl_A <- glm(chose_flowerA ~ ColorPref + PropValDiff_FlowerA + PropValDiff_FlowerA:FamiliarityA + PropValDiff_FlowerA:Horizon + FamiliarityA:Horizon, family = binomial, data = dat[!is.na(dat$PropValDiff_FlowerA),])
## Table II. Random exploration GLM ---------------------
tab_model(expl_A
          , show.re.var = TRUE
          , pred.labels = c("Preference for 'flower A' (Intercept)"
                            , "Color Bias"
                            , "Reward (Diff. A - B)"
                            , "Learning (interaction Rew Diff x Familiarity)"
                            , "Random exploration (interaction Rew Diff x Horizon)"
                            , "Directed exploration (interaction Familiarity x Horizon)"
          )
          , dv.labels = "Effect on prob. of choosing flower type 'A'"
)
## Effect sizes figure Fig S4 -------------------------
lab <- c("Directed exploration", "Random exploration", "Learning", "Reward", "Color Bias", "Right preference")
model_comp(posterior_ExplA, expl_A, lab, 6)
########################################################
# III. Modeling choice of unfamiliar option directly ------------
## GLM for directed exploration -----------------------
expl_UF_arena <- glm(choseTreatInformative ~ PropValDiff_Treat * Horizon_fac, family = binomial, data = dat)
## Table III. Directed exploration GLM --------------
tab_model(expl_UF_arena
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward Difference (unfamiliar - familiar)"
                            , "Horizon"
                            , "Interaction Rew Diff x Horizon"
          )
          , dv.labels = "Effect on probability of choosing less familiar flower"
)
## Bayesian model with prob of unfamiliar as response variable --------------
### Priors ---------------------------------
intercept_prior_mean <- 0.5
intercept_prior_sd <- 0.3
slopeval_prior_mean <- 1
slopeval_prior_sd <- 0.5
slopehor_prior_mean <- 0
slopehor_prior_sd <- 2
slopevh_prior_mean <- 0
slopevh_prior_sd <- 2

### Model structure and assumptions --------------------
list_of_assumptions <- alist(
  # Actual output is list of choices CL
  # Everything is analogous to the first Bayesian model, except we are
  # modeling the choice of the unfamiliar flower instead of the right side flower.
  CL ~ dbinom(1, prob), 
  # This is the core model formula
  logit(prob) <- a + b*val + c*hor + d*val*hor, 
  # And the following describes the priors for the parameters
  a ~ dnorm(intercept_prior_mean, intercept_prior_sd), 
  b ~ dnorm(slopeval_prior_mean, slopeval_prior_sd), 
  c ~ dnorm(slopehor_prior_mean, slopehor_prior_sd),
  d ~ dnorm(slopevh_prior_mean, slopevh_prior_sd)
)
start <- function(dat) {
  return(
    list(
      a = intercept_prior_mean
      , b = slopeval_prior_mean
      , c = slopehor_prior_mean
      , d = slopevh_prior_mean
    )
  )
}

### Bayesian estimation function -----------------------
# CL stands for choice list, val is the value or reward difference between the options,
# fam is the familiarity difference between the options. 
BayesExploreUnf_arena <- function(dat) {
  quap(
    list_of_assumptions
    , data = list(CL = dat$choseTreatInformative, val = dat$PropValDiff_Treat, hor = dat$Horizon_fac)
    , start = start(dat)
  )
}
### Running the Bayesian analysis ----------------------
posterior_ExplUF_arena <- BayesExploreUnf_arena(dat)
## Effect sizes figure choosing unfamiliar flower -- Fig S4 to go with Fig. 4 -------------------------
lab <- c("Interaction Val x Hor", "Horizon", "Value", "Intercept")
model_comp(posterior_ExplUF_arena, expl_UF_arena, lab, 4)

## Results Fig 4 - choice of unfamiliar flower w GLM & Bayes II -----------
# 2-panel Base R Plot to illustrate binomial fit and uncertainty
if(saveFigs) {
  setwd(figs_path)
  pdf(file = "Fig4 Maze chose unfamiliar.pdf", width = 10, height = 6)
}
# Layout
par(mfrow=c(1,2))
par(oma = c(4,4,0,0), mar = c(1,1,3,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
# Each panel doing its own modeling separately for glm
# Extracting values from Bayesian model for graphing
samples_of_post <- extract.samples(posterior_ExplUF_arena, n=n_uncertainty)
intercepts_post <- samples_of_post$a
slopes_val <- samples_of_post$b
slopes_hor <- samples_of_post$c
slopes_valhor <- samples_of_post$d

### Horizon 2 - model and graph ----------------
d_graph <- subset(dat, dat$Horizon_fac == 0)
# Plot frame
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(-1, 1)
)
axis(1, at = c(-1, -0.5, 0, 0.5, 1)) # Define where x-axis tick marks are
mtext("Horizon 2", 3, 1)
# Y axis label for all panels
mtext("Chose Low Familiarity Option", 2, 3)
# Gridlines
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
# GLM - model and extracting estimates
H1_Expl_mod <- glm(choseTreatInformative ~ PropValDiff_Treat, family = binomial, data = d_graph)
interc <- H1_Expl_mod$coefficients[[1]]
par1 <- H1_Expl_mod$coefficients[[2]]
ose <- round(exp(par1),1)
ose_sd <- round(exp(par1+summary(H1_Expl_mod)$coefficients[2,2]) - ose, 1)
pvalue <- round(summary(H1_Expl_mod)$coefficients[2,4], 3)
int <- round(probs_from_pars(0, 0, 0.5, interc, par1, 0, 0), 2)
int_sd <- round(probs_from_pars(0, 0, 0.5, interc+summary(H1_Expl_mod)$coefficients[1,2], par1, 0, 0) - int, 2)
# Extracting uncertainty
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(H1_Expl_mod))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 0, 0.5, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0), from = -2, to = 2
        , add = TRUE
        , col = alpha(colorshor[1], transparency_glm_uncertainty)
        , lwd = 3
  )
  if(showBayes) curve(probs_from_pars(x, 0, 0.5, intercepts_post[i], slopes_val[i], slopes_hor[i], slopes_valhor[i])
                      , from = -2, to = 2
                      , add = TRUE
                      , lwd = 3
                      , col = alpha("grey34", transparency_Bay_uncertainty)
                      , lty = 2
  )
}
# Original data points with slight jitter
points(jitter(choseTreatInformative, factor = 0.2) ~ jitter(PropValDiff_Treat, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorshor[1], 0.5)
       , cex = 1.5
)
# Plotting the estimated fit from the glm:
# (We're doing this last instead of earlier so it comes out on top for better
# visibility.)
curve(probs_from_pars(x, 0, 0.5, interc, par1, 0, 0), from = -2, to = 2
      , add = TRUE
      , col = colorshor[1]
      , lwd = 5)
# Plotting Bayesian fit line
if(showBayes) curve(probs_from_pars(x, 0, 0.5
                                    , precis(posterior_ExplUF_arena)[1,1]
                                    , precis(posterior_ExplUF_arena)[2,1]
                                    , precis(posterior_ExplUF_arena)[3,1]
                                    , precis(posterior_ExplUF_arena)[4,1])
                    , from = -2, to = 2
                    , add = TRUE
                    , lwd = 4
                    , col = "grey34"
                    , lty = 2
)
### Horizon 16 - model and graph ----------------------
d_graph <- subset(dat, dat$Horizon_fac == 1)
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , yaxt = 'n'
     , xaxt = 'n'
     , xlim = c(-1, 1)
)
axis(1, at = c(-1, -0.5, 0, 0.5, 1)) # Define where x-axis tick marks are
mtext("Horizon 16", 3, 1)
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
# GLM - model and extracting estimates
H6_Expl_mod <- glm(choseTreatInformative ~ PropValDiff_Treat, family = binomial, data = d_graph)
interc <- H6_Expl_mod$coefficients[[1]]
par1 <- H6_Expl_mod$coefficients[[2]]
ose <- round(exp(par1),1)
ose_sd <- round(exp(par1+summary(H6_Expl_mod)$coefficients[2,2]) - ose, 1)
pvalue <- round(summary(H6_Expl_mod)$coefficients[2,4], 3)
int <- round(probs_from_pars(0, 0, 0.5, interc, par1, 0, 0), 2)
int_sd <- round(probs_from_pars(0, 0, 0.5, interc+summary(H6_Expl_mod)$coefficients[1,2], par1, 0, 0) - int, 2)
# Bayesian model and extracting estimates
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(H6_Expl_mod))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, -1, 0.5, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0), from = -2, to = 2
        , add = TRUE
        , col = alpha(colorshor[2], transparency_glm_uncertainty)
        , lwd = 3
  )
  if(showBayes) curve(probs_from_pars(x, 1, 0.5, intercepts_post[i], slopes_val[i], slopes_hor[i], slopes_valhor[i])
                      , from = -2, to = 2
                      , add = TRUE
                      , lwd = 3
                      , col = alpha("grey34", transparency_Bay_uncertainty)
                      , lty = 2
  )
}
# Original data points with slight jitter
points(jitter(choseTreatInformative, factor = 0.2) ~ jitter(PropValDiff_Treat, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorshor[2], 0.5)
       , cex = 1.5
)
curve(probs_from_pars(x, 0, 0.5, interc, par1, 0, 0), from = -2, to = 2
      , add = TRUE
      , col = colorshor[2]
      , lwd = 5)
# Plotting Bayesian fit line
if(showBayes) curve(probs_from_pars(x, 1, 0.5
                                    , precis(posterior_ExplUF_arena)[1,1]
                                    , precis(posterior_ExplUF_arena)[2,1]
                                    , precis(posterior_ExplUF_arena)[3,1]
                                    , precis(posterior_ExplUF_arena)[4,1])
                    , from = -2, to = 2
                    , add = TRUE
                    , lwd = 4
                    , col = "grey34"
                    , lty = 2
)
### Axis label -------------
mtext("Proportional Concentration Difference (unfamiliar - familiar)/(unfamiliar+familiar)", 1, 2, outer = TRUE)
if(saveFigs) {dev.off()}
### end fig --------------------------------------































################################
################################
# OLD CODE --------------------


#### SIMULATE DATA -----------------------------------
# Functions to simulate data are essentially the same as before (with maze experiment).
probs_from_pars <- function(factor1, factor2, intercept, slope1, slope2, slope1x2) {
  samplesize <- length(factor1)
  linear_predictor <- intercept + slope1 * factor1 + slope2 * factor2 + slope1x2 * factor1 * factor2
  probability_right <- 1/(1+exp(-linear_predictor))
  return(probability_right)
}

# This function just picks actual choices from probabilities
sim_choice <- function(probabilities) {
  samplesize <- length(probabilities)
  choice_roll <- runif(samplesize, 0, 1)
  choices <- ifelse(choice_roll<probabilities, 1, 0)
  return(choices)
}

# Parameter values chosen for simulation; parameters don't mean quite the same here
slope_value <- 0.5
slope_familiarity <- 0
slope_valxfam <- 0.8
intercept <- 0
rnd_expl_simpar <- 0.7
dir_expl_simpar <- 0.4
#order_effect <- 0.1
# Note that the current code doesn't actually allow for an order effect in the simulated data. 

# There is no attempt here to simulate an actual experiment; we merely
# simulate a dataset that varies these parameters uniformly and independently, 
# and calculate the resulting 'choice' from the modeled function. 
rel_valuediffs <- runif(N_sim, min = -1, max = 1)
n_equal <- round(N_sim*0.2) # This is to make sure there are enough of the
# single value familiarity difference = 0 to make a graph panel. 
rel_famdiffs <- c(rep(0, times = n_equal), runif(N_sim-n_equal, min = -1, max = 1))
bees <- sample(c("onebee", "twobee"), N_sim, replace = TRUE)
order <- sample(c(1,2), N_sim, replace = TRUE)

# Horizon 2
choices_simdata <- sim_choice(probs_from_pars(rel_valuediffs, rel_famdiffs, intercept, slope_value, slope_familiarity, slope_valxfam))
# In this dataset, the 'choices' that count are the ones in column 'TypeChoice', and 
# value and familiarity differences are relative to total. 
simdata_h2 <- data.frame(chose_flowerA = choices_simdata, PropValDiff_FlowerA = rel_valuediffs, RelativeFamiliarity_FlowerA = rel_famdiffs, Bee = bees, Horizon = 2, Order = order)

# Horizon 16
choices_simdata <- sim_choice(probs_from_pars(rel_valuediffs, rel_famdiffs, intercept, slope_value+rnd_expl_simpar, slope_familiarity+dir_expl_simpar, slope_valxfam))
simdata_h16 <- data.frame(chose_flowerA = choices_simdata, PropValDiff_FlowerA = rel_valuediffs, RelativeFamiliarity_FlowerA = rel_famdiffs, Bee = bees, Horizon = 16, Order = order)

# Now joining the two datasets together
simdata <- rbind(simdata_h2, simdata_h16)

# Some data formatting to match empirical data
simdata$Horizon <- as.factor(simdata$Horizon)
simdata$choseInformative <- ifelse(simdata$RelativeFamiliarity_FlowerA < 0
                                   , simdata$chose_flowerA, 1-simdata$chose_flowerA
                                   )
simdata$PropValDiff_Fam <- ifelse(simdata$RelativeFamiliarity_FlowerA < 0
                                  , simdata$PropValDiff_FlowerA
                                  , ifelse(simdata$RelativeFamiliarity_FlowerA == 0
                                           , NA
                                           , -simdata$PropValDiff_FlowerA
                                           )
)




## end sim ---------------------------
#####################################################################
#### Bayesian model setup --------------------------------------
## First, define priors and other assumptions -------------------
# Priors involve the probability distributions of all the parameters, so typically
# means and standard deviations.
intercept_prior_mean <- 0
intercept_prior_SD <- 0.3
slopeval_prior_mean <- 1
slopeval_prior_sd <- 0.5
slopefam_prior_mean <- 0
slopefam_prior_sd <- 2
slopevf_prior_mean <- 0
slopevf_prior_sd <- 2
rnd_explor_prior_mean <- 0
rnd_explor_prior_sd <- 2
dir_explor_prior_mean <- 0
dir_explor_prior_sd <- 2
exp_prior_mean <- 0
exp_prior_sd <- 2

## Model assumption list ---------------
## The model is specified in the assumptions, and should match what we previously
## decided as the 'generative' model, i.e. the one used to make the simdata. 
## I am using the functions & format for the Bayesian model from Richard McElreath's
## 'Statistical Rethinking' book and the accompanying package 'rethinking'.
list_of_assumptions <- alist(
  # Actual output is list of choices CL
  # Analogous to picking points based on a model and error term, here we pick actual
  # choices based on the probability (~model). There is no 'error term'.
  # Note all the dbinom, dnorm functions describe the probability density functions.
  CL ~ dbinom(1, prob), 
  # This is the core model formula
  # Essentially this is the same as the way we simulated data, and also as the model for 
  # the maze experiment, with one difference: we also here allow for an order effect.
  # We're not modeling any interaction between order and exploration, value, etc.
  logit(prob) <- a + (b+rnd*hor)*val + (c+dir*hor)*fam + d*val*fam + exp * ord, 
  # And the following describes the priors for the parameters
  a ~ dnorm(intercept_prior_mean, intercept_prior_SD), 
  b ~ dnorm(slopeval_prior_mean, slopeval_prior_sd), 
  c ~ dnorm(slopefam_prior_mean, slopefam_prior_sd),
  d ~ dnorm(slopevf_prior_mean, slopevf_prior_sd),
  rnd ~ dnorm(rnd_explor_prior_mean, rnd_explor_prior_sd),
  dir ~ dnorm(dir_explor_prior_mean, dir_explor_prior_sd),
  exp ~ dnorm(exp_prior_mean, exp_prior_sd)
)

## Bayesian estimation function ------------------
# We're going to use quap() for the actual estimation. 
# We may or may not need 'start' to help the model converge on a solution. 
# This is not a prior and should not affect the actual resulting estimates.
start <- function(dat) {
  return(
    list(
        a = intercept_prior_mean
      , b = slopeval_prior_mean
      , c = slopefam_prior_mean
      , d = slopevf_prior_mean
      , rnd = rnd_explor_prior_mean
      , dir = dir_explor_prior_mean
      , exp = exp_prior_mean
    )
  )
}
# CL stands for choice list, val is the value or reward difference between the options,
# fam is the familiarity difference between the options. 
# Both for Horizon and Order, we want to change the numbers 1 vs 2 into 0 vs 1
ExplorModel <- function(dat) {
  quap(
    list_of_assumptions
    , data = list(CL = dat$chose_flowerA, val = dat$PropValDiff_FlowerA, fam = dat$RelativeFamiliarity_FlowerA, hor = (as.numeric(dat$Horizon)-1), ord = (as.numeric(dat$Order)-1))
    , start = start(dat)
  )
}


#####################################################################
## Barplot of bee choices based on familiarity ------------------------
summary_bar <- subset(dat, dat$RelativeFamiliarity_FlowerA != 0 & !is.na(chose_flowerA)) %>%
  mutate(InformativeChoice = ifelse(InformChoices==1, "Informative", "Uninformative")) %>%
  group_by(Horizon, Order, InformativeChoice) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Horizon, Order) %>%
  mutate(prop = n / sum(n))
levels(summary_bar$Horizon) <- c(levels(summary_bar$Horizon), "Training")
summary_bar$Horizon[is.na(summary_bar$Horizon)] <- "Training"
summary_bar$Horizon <- factor(summary_bar$Horizon, levels = c("Training", "2", "16"))
ggplot(summary_bar, aes(x = factor(Horizon), y = prop, fill = InformativeChoice)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Order, labeller = label_both) +
  scale_fill_manual(
    values = c("Informative" = "#3690c0", "Uninformative" = "#a6bddb"),
    labels = c("Informative" = "2 flowers in training", "Uninformative" = "4 flowers in training")
  ) +
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5)
    ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  labs(
    x = "Horizon",
    y = "Proportion of total Choices",
    fill = "Choice"
  ) +
  theme_minimal(base_size = 14)


#### GLMs  -------------------------------
#### - these are basically identical to the ones for the Maze Experiment,
#### just with 'Order' added as a factor.
dat$Order_fac <- dat$Order - 1
dat <- dat # for all choices, exclude training phase and not counted visits
# I. GLM Val x Fam ----------------------
chooseA <- glm(
  chose_flowerA ~ PropValDiff_FlowerA * TreatInfType + ColorPref 
  , family = binomial, data = dat)
tab_model(chooseA
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Prop. Reward Difference (conc. A - B/conc. A + B)"
                            , "Familiarity Treatment"
                            , "Color Bias"
                            , "Interaction Prop. Rew Diff x Fam"
          )
          , dv.labels = "Effect on probability of choosing flower type A"
)
# II. GLM Random Exploration ---------------------------
rndExplA <- glm(
  chose_flowerA ~ PropValDiff_FlowerA + PropValDiff_FlowerA:Horizon + ColorPref
  , family = binomial, data = dat)
tab_model(rndExplA
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Prop. Reward Difference (conc. A - B/conc. A + B)"
                            , "Color Bias"
                            , "Interaction Prop. Rew Diff x Horizon"
          )
          , dv.labels = "Effect on probability of choosing flower A"
)
rndExplA <- glm(
  chose_flowerA ~ PropValDiff_FlowerA + PropValDiff_FlowerA:Horizon
  , family = binomial, data = dat)
tab_model(rndExplA
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Prop. Reward Difference (conc. A - B/conc. A + B)"
                            , "Interaction Prop. Rew Diff x Horizon"
          )
          , dv.labels = "Effect on probability of choosing flower A"
)

# III. GLM Directed Exploration -----------------------
dat$Horizon_num <- as.numeric(Horizon)-1
dat$InformChoices <- as.numeric(dat$InformChoices)
dirExpl_HI_modA <- glm(
  InformChoices ~ PropValDiff_Treat * Horizon #+ PropValDiff_Treat * Order_fac
  , family = binomial, data = dat)
tab_model(dirExpl_HI_modA
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward Difference (unfamiliar - familiar)"
                            , "Horizon"
                            , "Interaction Rew Diff x Horizon"
          )
          , dv.labels = "Effect on probability of choosing less familiar flower"
)

#### Bayesian fits ----------------------------------------------
if(showBayes) {
  posterior_bees <- ExplorModel(dat)
  # Extracting values from Bayesian model for graphing
  samples_of_post <- extract.samples(posterior_bees, n=n_uncertainty)
  intercepts_post <- samples_of_post$a
  slopes_c <- samples_of_post$b
  slopes_f <- samples_of_post$c
  slopes_cf <- samples_of_post$d
  rnd_expl <- samples_of_post$rnd
  dir_expl <- samples_of_post$dir
  slope_ord <- samples_of_post$exp
}
## Effect size figures ---------------------------
## Comparing odds ratios for both models -------------------------
model_comp <- function(posterior, model_fit, par_labels, no_pars) {
  pars_Bayes <- precis(posterior_bees)[,1]
  #  pars_Bayes <- insert(precis(posterior_bees)[,1], ats=4, values=NA)
  pars_GLM <- c(coef(model_fit), NA)
  or_Bayes <- exp(pars_Bayes)
  or_GLM <- exp(pars_GLM)
  se_Bayes <- precis(posterior_bees)[,2]
#  se_Bayes <- insert(precis(posterior_bees)[,2], ats=4, values=NA)
  se_GLM <- c(summary(model_fit)$coefficients[, 2], NA)
  upper_Bayes <- exp(pars_Bayes+se_Bayes)
  lower_Bayes <- exp(pars_Bayes-se_Bayes)
  upper_GLM <- exp(pars_GLM+se_GLM)
  lower_GLM <- exp(pars_GLM-se_GLM)
  par(mfrow=c(1,2))
  par(mar=c(5, 1, 0.5, 0.5), oma=c(1, 9, 0, 0), mgp=c(4,1,0))
  plot(NULL
       , xlim=c(-4,5)
       , ylim=c(0.5, no_pars+0.5)
       , ylab=""
       , yaxt = "n"
       , xlab = "Odds ratio fits\nfrom Bayesian model"
  )
  axis(side = 2, at = 1:no_pars, labels = par_labels, las = 1, cex = 0.8)
  abline(v=1, lty = 2, col = "grey")
  for(i in 1:no_pars) {
    abline(h=i, lty = 2, col = "grey")
  }
  segments(x0 = lower_Bayes, y0 = no_pars:1, 
           x1 = upper_Bayes, y1 = no_pars:1, 
           lwd = 4, col = "darkgrey")
  points(x = or_Bayes, y = no_pars:1, 
         pch = 19, cex = 1.5, col = "black")
#  mtext("Factor", side = 2, outer = TRUE, line = 5)
  plot(NULL
       , xlim=c(-4,5)
       , ylim=c(0.5, no_pars+0.5)
       , ylab=""
       , yaxt = "n"
       , xlab = "Odds ratio fits\nfrom GLM"
  )
  abline(v=1, lty = 2, col = "grey")
  for(i in 1:no_pars) {
    abline(h=i, lty = 2, col = "grey")
  }
  segments(x0 = lower_GLM, y0 = no_pars:1, 
           x1 = upper_GLM, y1 = no_pars:1, 
           lwd = 4, col = "darkgrey")
  points(x = or_GLM, y = no_pars:1, 
         pch = 19, cex = 1.5, col = "black")
}
## Effect sizes to go with flower A as response -------------------------
lab <- c("7", "6", "5", "Interaction Val x Fam", "Familiarity", "Value", "Intercept")
model_comp(posterior_bees, chooseA, lab, 7)
## Fig S4 to go with Fig. 4 -------------------------
lab <- c("Interaction Val x Hor", "Color", "Horizon", "Value", "Intercept")
model_comp(posterior_bees_DirExp, dirExpl_HI_mod, lab, 5)


######################################################################
#### FIGURES ------------------------------
## Note that Fig 5 can come out significantly differently looking
## based on the low sample size and the randomization of what is 
## designated 'flower A'. The key is to remember that for 
## generalization to be valid, we should be looking at the p-value...

## Results Fig I. value x familiarity ---------------------
## Illustrate effects of familiarity and value (and interaction) 
# x - Prop reward diff
# y - choose flower A
# 3 panels, fam <0, fam = 0, fam >0
par(mfrow=c(1,3))
par(oma = c(4,4,0,0), mar = c(1,1,1,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
# Each panel doing its own modeling separately

### Panel 1: LOW FAMILIARITY - model and graph ----------------
d_graph <- subset(dat, dat$RelativeFamiliarity_FlowerA < 0)
# Plot frame
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(-1, 1)
)
axis(1, at = c(-1, -0.5, 0, 0.5, 1)) # Define where x-axis tick marks are
mtext("Chose 'Flower type A'", 2, 3)
# Two gridlines to show random choice and equal reward
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseAmod1 <- glm(chose_flowerA ~ PropValDiff_FlowerA, family = binomial, data = d_graph)
interc <- chooseAmod1$coefficients[[1]]
par1 <- chooseAmod1$coefficients[[2]]
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseAmod1))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, -1, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
        , from = -1, to = 1
        , add = TRUE
        , col = alpha(colorsfam[1], transparency_glm_uncertainty)
        , lwd = 3
  )
  if(showBayes) curve(probs_from_pars(x, -1, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
                      , from = -2, to = 2
                      , add = TRUE
                      , lwd = 3
                      , col = alpha("grey34", transparency_Bay_uncertainty)
                      , lty = 2
  )
}

# 'True' relationship if using simdata; note that this is just for familiarity
# difference -0.5 (whereas the simdata are uniformly distributed from -1 to 0).
if(showsim) 
  curve(probs_from_pars(x,  -0.5, intercept, slope_value, slope_familiarity, slope_valxfam)
        , from = -1, to = 1
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )

# Original data points with slight jitter
points(jitter(chose_flowerA, factor = 0.2) ~ PropValDiff_FlowerA
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[1], 0.5)
       , cex = 2
)

# Plotting Bayesian fit line
if(showBayes) curve(probs_from_pars(x, -1, precis(posterior_bees)[1,1]
                                    , precis(posterior_bees)[2,1]
                                    , precis(posterior_bees)[3,1]
                                    , precis(posterior_bees)[4,1])
                    , from = -2, to = 2
                    , add = TRUE
                    , lwd = 4
                    , col = "grey34"
                    , lty = 2
)

# Plotting the estimated fit from the glm:
curve(probs_from_pars(x, -1, interc, par1, 0, 0)
      , from = -2, to = 2
      , add = TRUE
      , col = colorsfam[1]
      , lwd = 5)

# Text labels for panel
# Converting parameters for labels:
ose <- round(exp(par1),1) # 'odds slope', how much the odds change with a change in x
int <- round(probs_from_pars(0, -1, interc, par1, 0, 0), 2) # probability at x=0
ose_sd <- round(exp(par1+summary(chooseAmod1)$coefficients[2,2]) - ose, 1)
int_sd <- round(probs_from_pars(0, -1, interc+summary(chooseAmod1)$coefficients[1,2], par1, 0, 0) - int, 2)
pvalue <- round(summary(chooseAmod1)$coefficients[2,4], 3)
# Just for better understanding and data checking, we'll also label with the
# sample size for the entire panel and the overall proportion of right choices.
overall <- round(mean(d_graph$chose_flowerA), 2)
# Now label with overall and per-level sample sizes, and add model result. 
samples <- length(d_graph$chose_flowerA)
text(x = -1, y = 1.13, labels = paste("N = ", samples, sep = ""), col = colorsfam[1], adj = 0)
text(x = -1, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue, sep = ""), cex = 1, col = colorsfam[1], adj = 0)
text(x = -1, y = 1.07, labels = paste("'A' pref (glm): ", int, " +/- ", int_sd, sep = ""), cex = 1, col = colorsfam[1], adj = 0)

### Panel 2: EQUAL FAMILIARITY - model and graph --------------------
# We follow all the same steps as for Panel 1 (hence no annotations)
d_graph <- subset(dat, dat$RelativeFamiliarity_FlowerA == 0)
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(-1, 1)
     , yaxt = 'n'
)
axis(1, at = c(-1, -0.5, 0, 0.5, 1)) # Define where x-axis tick marks are
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseAmod2 <- glm(chose_flowerA ~ PropValDiff_FlowerA, family = binomial, data = d_graph)
interc <- chooseAmod2$coefficients[[1]]
par1 <- chooseAmod2$coefficients[[2]]
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseAmod2))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
        , from = -1, to = 1
        , add = TRUE
        , col = alpha(colorsfam[2], transparency_glm_uncertainty)
        , lwd = 3
  )
  if(showBayes) curve(probs_from_pars(x, 0, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
                      , from = -1, to = 1
                      , add = TRUE
                      , lwd = 3
                      , col = alpha("grey34", transparency_Bay_uncertainty)
                      , lty = 2
  )
}
if(showsim) 
  curve(probs_from_pars(x,  0, intercept, slope_value, slope_familiarity, slope_valxfam)
        , from = -1, to = 1
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )
points(jitter(chose_flowerA, factor = 0.2) ~ PropValDiff_FlowerA
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[2], 0.5)
       , cex = 2
)
if(showBayes) curve(probs_from_pars(x, 0, precis(posterior_bees)[1,1]
                                    , precis(posterior_bees)[2,1]
                                    , precis(posterior_bees)[3,1]
                                    , precis(posterior_bees)[4,1])
                    , from = -1, to = 1
                    , add = TRUE
                    , lwd = 4
                    , col = "grey34"
                    , lty = 2
)
curve(probs_from_pars(x, 0, interc, par1, 0, 0)
      , from = -1, to = 1
      , add = TRUE
      , col = colorsfam[2]
      , lwd = 5)
ose <- round(exp(par1),1) # 'odds slope', how much the odds change with a change in x
int <- round(probs_from_pars(0, 0, interc, par1, 0, 0), 2) # probability at x=0
ose_sd <- round(exp(par1+summary(chooseAmod2)$coefficients[2,2]) - ose, 1)
int_sd <- round(probs_from_pars(0, 0, interc+summary(chooseAmod2)$coefficients[1,2], par1, 0, 0) - int, 2)
pvalue <- round(summary(chooseAmod2)$coefficients[2,4], 3)
overall <- round(mean(d_graph$chose_flowerA), 2)
samples <- length(d_graph$chose_flowerA)
text(x = -1, y = 1.13, labels = paste("N = ", samples, sep = ""), col = colorsfam[2], adj = 0)
text(x = -1, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue, sep = ""), cex = 1, col = colorsfam[2], adj = 0)
text(x = -1, y = 1.07, labels = paste("'A' pref (glm): ", int, " +/- ", int_sd, sep = ""), cex = 1, col = colorsfam[2], adj = 0)

### Panel 3: HIGH FAMILIARITY - model and graph ----------------------
d_graph <- subset(dat, dat$RelativeFamiliarity_FlowerA > 0)
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , yaxt = 'n'
     , xlim = c(-1, 1)
)
axis(1, at = c(-1, -0.5, 0, 0.5, 1)) # Define where x-axis tick marks are
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseAmod3 <- glm(chose_flowerA ~ PropValDiff_FlowerA, family = binomial, data = d_graph)
interc <- chooseAmod3$coefficients[[1]]
par1 <- chooseAmod3$coefficients[[2]]
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseAmod3))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 1, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
        , from = -1, to = 1
        , add = TRUE
        , col = alpha(colorsfam[3], transparency_glm_uncertainty)
        , lwd = 3
  )
  if(showBayes) curve(probs_from_pars(x, 1, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
                      , from = -1, to = 1
                      , add = TRUE
                      , lwd = 3
                      , col = alpha("grey34", transparency_Bay_uncertainty)
                      , lty = 2
  )
}
if(showsim) 
  curve(probs_from_pars(x,  1, intercept, slope_value, slope_familiarity, slope_valxfam)
        , from = -1, to = 1
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )
points(jitter(chose_flowerA, factor = 0.2) ~ PropValDiff_FlowerA
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[3], 0.5)
       , cex = 2
)
if(showBayes) curve(probs_from_pars(x, 1, precis(posterior_bees)[1,1]
                                    , precis(posterior_bees)[2,1]
                                    , precis(posterior_bees)[3,1]
                                    , precis(posterior_bees)[4,1])
                    , from = -1, to = 1
                    , add = TRUE
                    , lwd = 4
                    , col = "grey34"
                    , lty = 2
)
curve(probs_from_pars(x, 1, interc, par1, 0, 0)
      , from = -1, to = 1
      , add = TRUE
      , col = colorsfam[3]
      , lwd = 5)
ose <- round(exp(par1),1) # 'odds slope', how much the odds change with a change in x
int <- round(probs_from_pars(0, 1, interc, par1, 0, 0), 2) # probability at x=0
ose_sd <- round(exp(par1+summary(chooseAmod2)$coefficients[2,2]) - ose, 1)
int_sd <- round(probs_from_pars(0, 1, interc+summary(chooseAmod2)$coefficients[1,2], par1, 0, 0) - int, 2)
pvalue <- round(summary(chooseAmod2)$coefficients[2,4], 3)
overall <- round(mean(d_graph$chose_flowerA), 2)
samples <- length(d_graph$chose_flowerA)
text(x = -1, y = 1.13, labels = paste("N = ", samples, sep = ""), col = colorsfam[3], adj = 0)
text(x = -1, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue, sep = ""), cex = 1, col = colorsfam[3], adj = 0)
text(x = -1, y = 1.07, labels = paste("'A' pref (glm): ", int, " +/- ", int_sd, sep = ""), cex = 1, col = colorsfam[3], adj = 0)
### Joint x axis label for all panels: ---------------
mtext("Concentration Difference", 1, 2, outer = TRUE)
### end figure -------------------------



## Results Fig I. value x familiarity ---------------------
## Illustrate effects of familiarity and value (and interaction) 
# x - Prop reward diff
# y - choose flower A
# 3 panels, fam <0, fam = 0, fam >0
par(mfrow=c(1,3))
par(oma = c(4,4,0,0), mar = c(1,1,1,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
# Each panel doing its own modeling separately

### Panel 1: LOW FAMILIARITY - model and graph ----------------
d_graph <- subset(dat, dat$RelativeFamiliarity_FlowerA < 0)
# Plot frame
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(-1, 1)
)
axis(1, at = c(-1, -0.5, 0, 0.5, 1)) # Define where x-axis tick marks are
mtext("Chose 'Flower type A'", 2, 3)
# Two gridlines to show random choice and equal reward
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseAmod1 <- glm(chose_flowerA ~ PropValDiff_FlowerA, family = binomial, data = d_graph)
interc <- chooseAmod1$coefficients[[1]]
par1 <- chooseAmod1$coefficients[[2]]
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseAmod1))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, -1, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
        , from = -1, to = 1
        , add = TRUE
        , col = alpha(colorsfam[1], transparency_glm_uncertainty)
        , lwd = 3
  )
  if(showBayes) curve(probs_from_pars(x, -1, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
                      , from = -2, to = 2
                      , add = TRUE
                      , lwd = 3
                      , col = alpha("grey34", transparency_Bay_uncertainty)
                      , lty = 2
  )
}

# 'True' relationship if using simdata; note that this is just for familiarity
# difference -0.5 (whereas the simdata are uniformly distributed from -1 to 0).
if(showsim) 
  curve(probs_from_pars(x,  -0.5, intercept, slope_value, slope_familiarity, slope_valxfam)
        , from = -1, to = 1
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )

# Original data points with slight jitter
points(jitter(chose_flowerA, factor = 0.2) ~ PropValDiff_FlowerA
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[1], 0.5)
       , cex = 2
)

# Plotting Bayesian fit line
if(showBayes) curve(probs_from_pars(x, -1, precis(posterior_bees)[1,1]
                                    , precis(posterior_bees)[2,1]
                                    , precis(posterior_bees)[3,1]
                                    , precis(posterior_bees)[4,1])
                    , from = -2, to = 2
                    , add = TRUE
                    , lwd = 4
                    , col = "grey34"
                    , lty = 2
)

# Plotting the estimated fit from the glm:
curve(probs_from_pars(x, -1, interc, par1, 0, 0)
      , from = -2, to = 2
      , add = TRUE
      , col = colorsfam[1]
      , lwd = 5)

# Text labels for panel
# Converting parameters for labels:
ose <- round(exp(par1),1) # 'odds slope', how much the odds change with a change in x
int <- round(probs_from_pars(0, -1, interc, par1, 0, 0), 2) # probability at x=0
ose_sd <- round(exp(par1+summary(chooseAmod1)$coefficients[2,2]) - ose, 1)
int_sd <- round(probs_from_pars(0, -1, interc+summary(chooseAmod1)$coefficients[1,2], par1, 0, 0) - int, 2)
pvalue <- round(summary(chooseAmod1)$coefficients[2,4], 3)
# Just for better understanding and data checking, we'll also label with the
# sample size for the entire panel and the overall proportion of right choices.
overall <- round(mean(d_graph$chose_flowerA), 2)
# Now label with overall and per-level sample sizes, and add model result. 
samples <- length(d_graph$chose_flowerA)
text(x = -1, y = 1.13, labels = paste("N = ", samples, sep = ""), col = colorsfam[1], adj = 0)
text(x = -1, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue, sep = ""), cex = 1, col = colorsfam[1], adj = 0)
text(x = -1, y = 1.07, labels = paste("'A' pref (glm): ", int, " +/- ", int_sd, sep = ""), cex = 1, col = colorsfam[1], adj = 0)

### Panel 2: EQUAL FAMILIARITY - model and graph --------------------
# We follow all the same steps as for Panel 1 (hence no annotations)
d_graph <- subset(dat, dat$RelativeFamiliarity_FlowerA == 0)
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(-1, 1)
     , yaxt = 'n'
)
axis(1, at = c(-1, -0.5, 0, 0.5, 1)) # Define where x-axis tick marks are
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseAmod2 <- glm(chose_flowerA ~ PropValDiff_FlowerA, family = binomial, data = d_graph)
interc <- chooseAmod2$coefficients[[1]]
par1 <- chooseAmod2$coefficients[[2]]
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseAmod2))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
        , from = -1, to = 1
        , add = TRUE
        , col = alpha(colorsfam[2], transparency_glm_uncertainty)
        , lwd = 3
  )
  if(showBayes) curve(probs_from_pars(x, 0, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
                      , from = -1, to = 1
                      , add = TRUE
                      , lwd = 3
                      , col = alpha("grey34", transparency_Bay_uncertainty)
                      , lty = 2
  )
}
if(showsim) 
  curve(probs_from_pars(x,  0, intercept, slope_value, slope_familiarity, slope_valxfam)
        , from = -1, to = 1
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )
points(jitter(chose_flowerA, factor = 0.2) ~ PropValDiff_FlowerA
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[2], 0.5)
       , cex = 2
)
if(showBayes) curve(probs_from_pars(x, 0, precis(posterior_bees)[1,1]
                                    , precis(posterior_bees)[2,1]
                                    , precis(posterior_bees)[3,1]
                                    , precis(posterior_bees)[4,1])
                    , from = -1, to = 1
                    , add = TRUE
                    , lwd = 4
                    , col = "grey34"
                    , lty = 2
)
curve(probs_from_pars(x, 0, interc, par1, 0, 0)
      , from = -1, to = 1
      , add = TRUE
      , col = colorsfam[2]
      , lwd = 5)
ose <- round(exp(par1),1) # 'odds slope', how much the odds change with a change in x
int <- round(probs_from_pars(0, 0, interc, par1, 0, 0), 2) # probability at x=0
ose_sd <- round(exp(par1+summary(chooseAmod2)$coefficients[2,2]) - ose, 1)
int_sd <- round(probs_from_pars(0, 0, interc+summary(chooseAmod2)$coefficients[1,2], par1, 0, 0) - int, 2)
pvalue <- round(summary(chooseAmod2)$coefficients[2,4], 3)
overall <- round(mean(d_graph$chose_flowerA), 2)
samples <- length(d_graph$chose_flowerA)
text(x = -1, y = 1.13, labels = paste("N = ", samples, sep = ""), col = colorsfam[2], adj = 0)
text(x = -1, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue, sep = ""), cex = 1, col = colorsfam[2], adj = 0)
text(x = -1, y = 1.07, labels = paste("'A' pref (glm): ", int, " +/- ", int_sd, sep = ""), cex = 1, col = colorsfam[2], adj = 0)

### Panel 3: HIGH FAMILIARITY - model and graph ----------------------
d_graph <- subset(dat, dat$RelativeFamiliarity_FlowerA > 0)
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , yaxt = 'n'
     , xlim = c(-1, 1)
)
axis(1, at = c(-1, -0.5, 0, 0.5, 1)) # Define where x-axis tick marks are
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseAmod3 <- glm(chose_flowerA ~ PropValDiff_FlowerA, family = binomial, data = d_graph)
interc <- chooseAmod3$coefficients[[1]]
par1 <- chooseAmod3$coefficients[[2]]
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseAmod3))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 1, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
        , from = -1, to = 1
        , add = TRUE
        , col = alpha(colorsfam[3], transparency_glm_uncertainty)
        , lwd = 3
  )
  if(showBayes) curve(probs_from_pars(x, 1, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
                      , from = -1, to = 1
                      , add = TRUE
                      , lwd = 3
                      , col = alpha("grey34", transparency_Bay_uncertainty)
                      , lty = 2
  )
}
if(showsim) 
  curve(probs_from_pars(x,  1, intercept, slope_value, slope_familiarity, slope_valxfam)
        , from = -1, to = 1
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )
points(jitter(chose_flowerA, factor = 0.2) ~ PropValDiff_FlowerA
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[3], 0.5)
       , cex = 2
)
if(showBayes) curve(probs_from_pars(x, 1, precis(posterior_bees)[1,1]
                                    , precis(posterior_bees)[2,1]
                                    , precis(posterior_bees)[3,1]
                                    , precis(posterior_bees)[4,1])
                    , from = -1, to = 1
                    , add = TRUE
                    , lwd = 4
                    , col = "grey34"
                    , lty = 2
)
curve(probs_from_pars(x, 1, interc, par1, 0, 0)
      , from = -1, to = 1
      , add = TRUE
      , col = colorsfam[3]
      , lwd = 5)
ose <- round(exp(par1),1) # 'odds slope', how much the odds change with a change in x
int <- round(probs_from_pars(0, 1, interc, par1, 0, 0), 2) # probability at x=0
ose_sd <- round(exp(par1+summary(chooseAmod2)$coefficients[2,2]) - ose, 1)
int_sd <- round(probs_from_pars(0, 1, interc+summary(chooseAmod2)$coefficients[1,2], par1, 0, 0) - int, 2)
pvalue <- round(summary(chooseAmod2)$coefficients[2,4], 3)
overall <- round(mean(d_graph$chose_flowerA), 2)
samples <- length(d_graph$chose_flowerA)
text(x = -1, y = 1.13, labels = paste("N = ", samples, sep = ""), col = colorsfam[3], adj = 0)
text(x = -1, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue, sep = ""), cex = 1, col = colorsfam[3], adj = 0)
text(x = -1, y = 1.07, labels = paste("'A' pref (glm): ", int, " +/- ", int_sd, sep = ""), cex = 1, col = colorsfam[3], adj = 0)
### Joint x axis label for all panels: ---------------
mtext("Concentration Difference", 1, 2, outer = TRUE)
### end figure -------------------------

## Results Fig II. value + familiarity shown separately ---------------------
## Illustrate effects of familiarity and value 
## (values on the other factor pooled)
# x - Prop reward diff or Prop familiarity diff
# y - choose flower A
par(mfrow=c(1,2))
par(oma = c(4,4,0,0), mar = c(1,1,1,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
# Each panel doing its own modeling separately

### Panel 1: Familiarity effect ----------------
d_graph <- dat
# Plot frame
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(-1, 1)
)
axis(1, at = c(-1, -0.5, 0, 0.5, 1)) # Define where x-axis tick marks are
mtext("Chose 'Flower type A'", 2, 3)
mtext("Relative Familiarity", 1, 3)
# Two gridlines to show random choice and equal reward
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseAmod_fam <- glm(chose_flowerA ~ RelativeFamiliarity_FlowerA, family = binomial, data = d_graph)
interc <- chooseAmod_fam$coefficients[[1]]
par1 <- chooseAmod_fam$coefficients[[2]]
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseAmod_fam))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
        , from = -1, to = 1
        , add = TRUE
        , col = alpha(colorsfam[1], transparency_glm_uncertainty)
        , lwd = 3
  )
  # The model formula for the Bayesian fit is given above in the 'Model assumptions'
  # section. It is:
  #   logit(prob) <- a + (b+rnd*hor)*val + (c+dir*hor)*fam + d*val*fam + exp * ord
  # So the code below is giving us the fits for horizon = 0 and order = 0. 
  if(showBayes) curve(probs_from_pars(0, x, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
                      , from = -2, to = 2
                      , add = TRUE
                      , lwd = 3
                      , col = alpha("grey34", transparency_Bay_uncertainty)
                      , lty = 2
  )
}

# 'True' relationship if using simdata; note that this is just for familiarity
# difference -0.5 (whereas the simdata are uniformly distributed from -1 to 0).
if(showsim) 
  curve(probs_from_pars(0,  x, intercept, slope_value, slope_familiarity, slope_valxfam)
        , from = -1, to = 1
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )

# Original data points with slight jitter
points(jitter(chose_flowerA, factor = 0.2) ~ RelativeFamiliarity_FlowerA
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[1], 0.5)
       , cex = 2
)

# Plotting Bayesian fit line
if(showBayes) curve(probs_from_pars(0, x, precis(posterior_bees)[1,1]
                                    , precis(posterior_bees)[2,1]
                                    , precis(posterior_bees)[3,1]
                                    , precis(posterior_bees)[4,1])
                    , from = -2, to = 2
                    , add = TRUE
                    , lwd = 4
                    , col = "grey34"
                    , lty = 2
)

# Plotting the estimated fit from the glm:
curve(probs_from_pars(x, 0, interc, par1, 0, 0)
      , from = -2, to = 2
      , add = TRUE
      , col = colorsfam[1]
      , lwd = 5)


### Panel 2: Value effect --------------------
# We follow all the same steps as for Panel 1 (hence no annotations)
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(-1, 1)
     , yaxt = 'n'
)
mtext("Relative Value", 1, 3)
axis(1, at = c(-1, -0.5, 0, 0.5, 1)) # Define where x-axis tick marks are
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseAmod_val <- glm(chose_flowerA ~ PropValDiff_FlowerA, family = binomial, data = d_graph)
interc <- chooseAmod_val$coefficients[[1]]
par1 <- chooseAmod_val$coefficients[[2]]
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseAmod_val))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
        , from = -1, to = 1
        , add = TRUE
        , col = alpha(colorsfam[1], transparency_glm_uncertainty)
        , lwd = 3
  )
  if(showBayes) curve(probs_from_pars(x, 0, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
                      , from = -1, to = 1
                      , add = TRUE
                      , lwd = 3
                      , col = alpha("grey34", transparency_Bay_uncertainty)
                      , lty = 2
  )
}
if(showsim) 
  curve(probs_from_pars(x,  0, intercept, slope_value, slope_familiarity, slope_valxfam)
        , from = -1, to = 1
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )
points(jitter(chose_flowerA, factor = 0.2) ~ PropValDiff_FlowerA
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[1], 0.5)
       , cex = 2
)
if(showBayes) curve(probs_from_pars(x, 0, precis(posterior_bees)[1,1]
                                    , precis(posterior_bees)[2,1]
                                    , precis(posterior_bees)[3,1]
                                    , precis(posterior_bees)[4,1])
                    , from = -1, to = 1
                    , add = TRUE
                    , lwd = 4
                    , col = "grey34"
                    , lty = 2
)
curve(probs_from_pars(x, 0, interc, par1, 0, 0)
      , from = -1, to = 1
      , add = TRUE
      , col = colorsfam[1]
      , lwd = 5)



## Results Fig III. horizon x order Bayes with flowerA as response ---------------------
# x value of informative
# y chose informative
# panels order 1 order 2
# horizon 2 vs 16 as different colors
# Since this figure shows the choices of the informative flower,
# what it looks like is independent of what is designated 'flower A'.
## Bayesian analysis -----------------------
# The Bayesian model above really includes both random and directed exploration. But
# to directly compare the parameters between that and this directed exploration test,
# we need to convert the parameters such that we treat 'choosing the low familiarity option' 
# as the response variable. 
# We can simply make a posterior predictive distribution of essentially simulated 
# points according to the posteriors for the parameters. 
if(showBayes) {
set_of_valuediffs <- c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)
valuediffs <- sample(set_of_valuediffs, n_ppp_per_set, replace = TRUE)
famdiffs <- sample(c(-1, 1), n_ppp_per_set, replace = TRUE)
horizons <- sample(c(0, 1), n_uncertainty, replace = TRUE)
orders <- sample(c(0, 1), n_uncertainty, replace = TRUE)
PPPoints <- data.frame(NULL)
# logit(prob) <- a + (b+rnd*hor)*val + (c+dir*hor)*fam + d*val*fam + exp * ord
for(i in 1:n_uncertainty) {
  choices <- sim_choice(probs_from_pars(valuediffs,  famdiffs, intercepts_post[i] + slope_ord[i]*orders[i], slopes_c[i]+rnd_expl[i]*horizons[i], slopes_f[i]+dir_expl[i]*horizons[i], slopes_cf[i]))
  set <- data.frame(choice = choices, PropValDiff_FlowerA = valuediffs, RelativeFamiliarity_FlowerA = famdiffs, Horizon = horizons[i], Order = orders[i], Set = i)
  PPPoints <- rbind(PPPoints, set)
}
flip_these <- subset(PPPoints, PPPoints$RelativeFamiliarity_FlowerA > 0)
flip_these$choice <- ifelse(flip_these$choice == 1, 0, 1)
## !! This code above needs changing if choices are A and B instead of 0 and 1
flip_these$RelativeFamiliarity_FlowerA <- -1*flip_these$RelativeFamiliarity_FlowerA
flip_these$PropValDiff_FlowerA <- -1*flip_these$PropValDiff_FlowerA
PPPoints <- rbind(subset(PPPoints, PPPoints$RelativeFamiliarity_FlowerA < 0), flip_these)
# Now, for all these points, the "right" choice is the low-familiarity
# option.
# Each 'set' here has the same parameter values drawn together from the posterior, 
# but different specific value differences between options. Familiarity difference
# is always -1.
}
par(mfrow=c(1,2))
par(oma = c(4,4,0,0), mar = c(1,1,1,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
# Each panel doing its own modeling separately

### Panel 1: order 1 ----------------
d_graph <- subset(dat, dat$Order == 1)
d_graph1 <- subset(d_graph, d_graph$Horizon == 2)
d_graph2 <- subset(d_graph, d_graph$Horizon == 16)
PPPoints_panel1_H1 <- subset(PPPoints, PPPoints$Order == 0 & PPPoints$Horizon == 0)
PPPoints_panel1_H2 <- subset(PPPoints, PPPoints$Order == 0 & PPPoints$Horizon == 1)
# Plot frame
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(-1, 1)
)
axis(1, at = c(-1, -0.5, 0, 0.5, 1)) # Define where x-axis tick marks are
mtext("Chose Low Familiarity Option", 2, 3)
# Two gridlines to show random choice and equal reward
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
Expl_modA_2 <- glm(choseInformative ~ PropValDiff_Fam, family = binomial, data = d_graph1)
Expl_modA_16 <- glm(choseInformative ~ PropValDiff_Fam, family = binomial, data = d_graph2)
interc_2 <- Expl_modA_2$coefficients[[1]]
par1_2 <- Expl_modA_2$coefficients[[2]]
fit_covar_matrix_2 <- mvrnorm(n_uncertainty, mu=c(interc_2, par1_2), Sigma=vcov(Expl_modA_2))
interc_16 <- Expl_modA_16$coefficients[[1]]
par1_16 <- Expl_modA_16$coefficients[[2]]
fit_covar_matrix_16 <- mvrnorm(n_uncertainty, mu=c(interc_16, par1_16), Sigma=vcov(Expl_modA_16))
PPD_prob <- array(NaN, c(length(set_of_valuediffs), n_uncertainty, 2))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 0, fit_covar_matrix_2[i,1], fit_covar_matrix_2[i,2], 0, 0)
        , from = -1, to = 1
        , add = TRUE
        , col = alpha(colorshor[1], transparency_glm_uncertainty)
        , lwd = 3
  )
  curve(probs_from_pars(x, 0, fit_covar_matrix_16[i,1], fit_covar_matrix_16[i,2], 0, 0)
        , from = -1, to = 1
        , add = TRUE
        , col = alpha(colorshor[2], transparency_glm_uncertainty)
        , lwd = 3
  )
  # Plotting Bayesian PPD
  if(showBayes) {
    # We want to plot a probability, not an actual choice point. 
    # First we subset the posterior predicted points to a single set of parameters
    # and a horizon x order combination, then we calculate how many of them were
    # choice '1' and thus the probability of choosing A (or, here, the familiar flower).
    # Then we plot a line connecting these probabilities across the value differences.
    # Since these are lines between effectively simulated points (albeit with model-
    # fit parameters), they aren't smooth functions. 
    for(j in 1:length(set_of_valuediffs)) {
      PPPs <- subset(PPPoints_panel1_H1, PPPoints_panel1_H1$Set==i & PPPoints_panel1_H1$PropValDiff_FlowerA==set_of_valuediffs[j])
      PPD_prob[j, i, 1] <- length(subset(PPPs$choice, PPPs$choice==1))/length(PPPs$choice)
      PPPs <- subset(PPPoints_panel1_H2, PPPoints_panel1_H2$Set==i & PPPoints_panel1_H2$PropValDiff_FlowerA==set_of_valuediffs[j])
      PPD_prob[j, i, 2] <- length(subset(PPPs$choice, PPPs$choice==1))/length(PPPs$choice)
    }
    if(length(PPD_prob[,i,1]) == length(set_of_valuediffs))
    lines(PPD_prob[,i, 1] ~ set_of_valuediffs
          , col = alpha("grey34", transparency_Bay_uncertainty)
          , lwd = 3
          , lty = 2
    )
    if(length(PPD_prob[,i,2]) == length(set_of_valuediffs))
    lines(PPD_prob[,i, 2] ~ set_of_valuediffs
          , col = alpha("grey34", transparency_Bay_uncertainty)
          , lwd = 3
          , lty = 3
    )
  }
}

# 'True' relationship if using simdata for horizon = 2
if(showsim) 
  curve(probs_from_pars(x, 0, intercept, slope_value, 0, slope_valxfam)
        , from = -1, to = 1
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )

# Original data points with slight jitter
points(jitter(choseInformative, factor = 0.2) ~ PropValDiff_FlowerA
       , data = d_graph1
       , pch = 19
       , col = alpha(colorshor[1], 0.3)
       , cex = 2
)
points(jitter(choseInformative, factor = 0.2) ~ PropValDiff_FlowerA
       , data = d_graph2
       , pch = 19
       , col = alpha(colorshor[2], 0.3)
       , cex = 2
)

# Plotting average of the Bayesian posterior predicted fits
lines(rowMeans(PPD_prob[,, 1], na.rm = TRUE) ~ set_of_valuediffs
      , col = "grey34"
      , lwd = 3
      , lty = 2
)
lines(rowMeans(PPD_prob[,, 2], na.rm = TRUE) ~ set_of_valuediffs
      , col = "grey34"
      , lwd = 3
      , lty = 3
)

# Plotting the estimated fit from the glm:
curve(probs_from_pars(x, 0, interc_2, par1_2, 0, 0)
      , from = -1, to = 1
      , add = TRUE
      , col = colorshor[1]
      , lwd = 5)
curve(probs_from_pars(x, 0, interc_16, par1_16, 0, 0)
      , from = -1, to = 1
      , add = TRUE
      , col = colorshor[2]
      , lwd = 5)

### Panel 2: order 2 ----------------
d_graph <- subset(dat, dat$Order == 2)
d_graph1 <- subset(d_graph, d_graph$Horizon == 2)
d_graph2 <- subset(d_graph, d_graph$Horizon == 16)
PPPoints_panel2_H1 <- subset(PPPoints, PPPoints$Order == 1 & PPPoints$Horizon == 0)
PPPoints_panel2_H2 <- subset(PPPoints, PPPoints$Order == 1 & PPPoints$Horizon == 1)
# Plot frame
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , yaxt = 'n'
     , xlim = c(-1, 1)
)
axis(1, at = c(-1, -0.5, 0, 0.5, 1)) # Define where x-axis tick marks are
# Two gridlines to show random choice and equal reward
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
Expl_modA_2 <- glm(choseInformative ~ PropValDiff_Fam, family = binomial, data = d_graph1)
Expl_modA_16 <- glm(choseInformative ~ PropValDiff_Fam, family = binomial, data = d_graph2)
interc_2 <- Expl_modA_2$coefficients[[1]]
par1_2 <- Expl_modA_2$coefficients[[2]]
fit_covar_matrix_2 <- mvrnorm(n_uncertainty, mu=c(interc_2, par1_2), Sigma=vcov(Expl_modA_2))
interc_16 <- Expl_modA_16$coefficients[[1]]
par1_16 <- Expl_modA_16$coefficients[[2]]
fit_covar_matrix_16 <- mvrnorm(n_uncertainty, mu=c(interc_16, par1_16), Sigma=vcov(Expl_modA_16))
PPD_prob <- array(NaN, c(length(set_of_valuediffs), n_uncertainty, 2))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 0, fit_covar_matrix_2[i,1], fit_covar_matrix_2[i,2], 0, 0)
        , from = -1, to = 1
        , add = TRUE
        , col = alpha(colorshor[1], transparency_glm_uncertainty)
        , lwd = 3
  )
  curve(probs_from_pars(x, 0, fit_covar_matrix_16[i,1], fit_covar_matrix_16[i,2], 0, 0)
        , from = -1, to = 1
        , add = TRUE
        , col = alpha(colorshor[2], transparency_glm_uncertainty)
        , lwd = 3
  )
  # Plotting Bayesian PPD
  if(showBayes) {
    for(j in 1:length(set_of_valuediffs)) {
      PPPs <- subset(PPPoints_panel2_H1, PPPoints_panel2_H1$Set==i & PPPoints_panel2_H1$PropValDiff_FlowerA==set_of_valuediffs[j])
      PPD_prob[j, i, 1] <- length(subset(PPPs$choice, PPPs$choice==1))/length(PPPs$choice)
      PPPs <- subset(PPPoints_panel2_H2, PPPoints_panel2_H2$Set==i & PPPoints_panel2_H2$PropValDiff_FlowerA==set_of_valuediffs[j])
      PPD_prob[j, i, 2] <- length(subset(PPPs$choice, PPPs$choice==1))/length(PPPs$choice)
    }
    if(length(PPD_prob[,i,1]) == length(set_of_valuediffs))
    lines(PPD_prob[,i, 1] ~ set_of_valuediffs
          , col = alpha("grey34", transparency_Bay_uncertainty)
          , lwd = 3
          , lty = 2
    )
    if(length(PPD_prob[,i,2]) == length(set_of_valuediffs))
    lines(PPD_prob[,i, 2] ~ set_of_valuediffs
          , col = alpha("grey34", transparency_Bay_uncertainty)
          , lwd = 3
          , lty = 3
    )
  }
}

# 'True' relationship if using simdata for horizon = 2
if(showsim) 
  curve(probs_from_pars(x, 0, intercept, slope_value, 0, slope_valxfam)
        , from = -1, to = 1
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )

# Original data points with slight jitter
points(jitter(choseInformative, factor = 0.2) ~ PropValDiff_FlowerA
       , data = d_graph1
       , pch = 19
       , col = alpha(colorshor[1], 0.3)
       , cex = 2
)
points(jitter(choseInformative, factor = 0.2) ~ PropValDiff_FlowerA
       , data = d_graph2
       , pch = 19
       , col = alpha(colorshor[2], 0.3)
       , cex = 2
)

# Plotting average of the Bayesian posterior predicted fits
lines(rowMeans(PPD_prob[,, 1], na.rm = TRUE) ~ set_of_valuediffs
      , col = "grey34"
      , lwd = 3
      , lty = 2
)
lines(rowMeans(PPD_prob[,, 2], na.rm = TRUE) ~ set_of_valuediffs
      , col = "grey34"
      , lwd = 3
      , lty = 3
)

# Plotting the estimated fit from the glm:
curve(probs_from_pars(x, 0, interc_2, par1_2, 0, 0)
      , from = -1, to = 1
      , add = TRUE
      , col = colorshor[1]
      , lwd = 5)
curve(probs_from_pars(x, 0, interc_16, par1_16, 0, 0)
      , from = -1, to = 1
      , add = TRUE
      , col = colorshor[2]
      , lwd = 5)
### Joint x axis label for all panels: ---------------
mtext("Concentration Difference", 1, 2, outer = TRUE)

### end Figs --------------------------


## GLM Summary outputs in Console if desired ------------------
summary(chooseA)
summary(rndExplA)
summary(dirExpl_HI_modA)
