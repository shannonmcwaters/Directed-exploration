# Shannon McWaters, Jack-Morgan Mizell, Ren Calabro, Kiara Alexis Casas, Robert C Wilson, Anna Dornhaus
# (c) 2020-2026
# Directed Exploration in Bumble bees
# Data analysis & graphs - part 2: ARENA

## ARENA Exp
# Data problems:
# - FlowerColor has only 630 entries, choices had 662
# - Yellow19 and Green83: 
# number of choices is 759; rewards is 707, chosen color 690, informCh 707

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
# Yellow19 and Green83 have a mismatch between recorded choices and rewards
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
Horizon <- as.factor(Horizon)
Phase <- ifelse(Horizon==6, "Training", "Test")
Horizon[Horizon=="6"] <- NA
Horizon <- droplevels(Horizon)

#Combine to form new dataset
ArenaDataLong <- data.frame(Bee,Horizon,Order,Session,Condition,Choices,Reward,InformativeColor,InformChoices)

# Mark choice number per session, i.e. within the same arena visit
ArenaDataLong <- ArenaDataLong %>%
  arrange(Bee, Order, Session) %>%
  group_by(Bee, Order, Session) %>%
  mutate(ChoiceNumber = row_number()) %>%
  ungroup()
# Mark choice number per horizon, i.e. training + test arena trips
# (all trips with same color pair)
ArenaDataLong <- ArenaDataLong %>%
  arrange(Bee, Order, Session, ChoiceNumber) %>%  # Ensure data is ordered correctly
  group_by(Bee, Order) %>%
  mutate(HorizonChoiceNum = row_number()) %>%       # Generate a continuous choice number within each Horizon
  ungroup()

# Add new column randomizing the assignment of Flower A and B
# Step 1: Create a random flip assignment per bee
#set.seed(123)  # Remove this if you want different randomization each time
bee_map <- ArenaDataLong %>%
  distinct(Bee, Order) %>% # This gets all unique color pairs (which are repeated across
  # horizon 6 (training phase) and whatever the Test horizon is)
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

# Check for repeat visits and clean up reward values
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
# This is the actual column that should be used to evaluate rewards
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
    TypeChoice = ifelse(CountedVisit, FlowerType, NA),
    chose_flowerA = ifelse(!is.na(TypeChoice), ifelse(TypeChoice == "A", 1, 0), NA),
    choseUnfamiliar = ifelse(!is.na(TypeChoice), ifelse(InformChoices == 1, 1, 0), NA),
    ColorChosen = ifelse(InformChoices==1, InformativeColor, 
                        case_when(
                          InformativeColor=="B" ~ "DG",
                          InformativeColor=="DG" ~ "B",
                          InformativeColor=="LG" ~ "O",
                          InformativeColor=="O" ~ "LG"
                          )),
    FamiliarityA = ifelse(InformChoices==1, 
                          ifelse(FlowerType=="A", 0, 1),
                          ifelse(FlowerType=="A", 1, 0)),
    RewAvg_chosen = ifelse(FlowerType=="A", RewAvg_A,
                           ifelse(FlowerType=="B", RewAvg_B,
                                  NA)),
    RewAvg_unfamiliar = ifelse(InformChoices==1, 
                               ifelse(FlowerType=="A", RewAvg_A, ifelse(FlowerType=="B", RewAvg_B, NA)),
                               ifelse(FlowerType=="A", RewAvg_B, ifelse(FlowerType=="B", RewAvg_A, NA))),
    RewAvgDiff_unfamiliar = ifelse(RewAvg_unfamiliar==RewAvg_A, RewAvg_A-RewAvg_B,
                                   ifelse(RewAvg_unfamiliar==RewAvg_B, RewAvg_B-RewAvg_A, NA))
  ) %>%
  ungroup()

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
model_comp <- function(posterior, model_fit, par_labels, no_pars, skipB = NA, skipGLM = NA) {
  pars_Bayes <- precis(posterior)[,1]
  pars_GLM <- coef(model_fit)
  se_Bayes <- precis(posterior)[,2]
  se_GLM <- summary(model_fit)$coefficients[, 2]
  if(!is.na(skipB)) {
    pars_Bayes <- insert(pars_Bayes, ats = skipB, values = NA)
    se_Bayes <- insert(se_Bayes, ats = skipB, values = NA)
  }
  or_Bayes <- exp(pars_Bayes)
  or_GLM <- exp(pars_GLM)
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
dat <- dat[!is.na(dat$RewAvg_A),]
# Bayesian analysis with quap ----------------------
## First, define priors -------------------
# Priors involve the probability distributions of all the parameters, so typically
# means and standard deviations.
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
  logit(prob) <- pval*val + pfam*fam + pcol * col + pvxf*val*fam, 
  # And the following describes the priors for the parameters
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
        pval = slopeval_prior_mean
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
    , data = list(CL = dat$chose_flowerA, val = dat$RewAvg_A, fam = dat$FamiliarityA, col = dat$ColorPref)
    , start = start(dat)
  )
}

# I. GLM & Bayes  choose right ------------------------
chooseA <- glm(chose_flowerA ~ RewAvg_A * FamiliarityA + ColorPref, family = binomial, data = dat)

# Bayesian model for choosing right flower
posterior_bees <- BayesChooseA(dat)
# Done!

## Table I. GLM  -------------------------
tab_model(chooseA
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward (concentration flower 'A')"
                            , "Familiarity A"
                            , "Color Bias"
                            , "Interaction Rew x Fam"
          )
          , dv.labels = "Effect on probability of choosing flower type 'A'"
)
#######################################################
## Effect sizes figure - Fig S2 to go with Fig. 3 -------------------------
lab <- c("Interaction Val x Fam", "Color", "Familiarity", "Value", "Intercept")
model_comp(posterior_bees, chooseA, lab, 5, skipB = 1)


#######################################################
# MODELING EFFECT OF HORIZON TWO WAYS: on 'A' choices and on choosing 'unfamiliar' -----------------
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
  logit(prob) <- pcol*col + pval*val + pvxf*val*fam + rnd*hor*val + dir*hor*fam, 
  # And the following describes the priors for the parameters
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
        pval = slopeval_prior_mean
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
    , data = list(CL = dat$chose_flowerA, val = dat$RewAvg_A, fam = dat$FamiliarityA, col = dat$ColorPref, hor = dat$Horizon_fac)
    , start = start(dat)
  )
}

## Bayesian fitting -----------------------
posterior_ExplA <- BayesExploreR(dat)
# GLM choice right flower' with exploration ------------------------------------
expl_A <- glm(chose_flowerA ~ ColorPref + RewAvg_A + RewAvg_A:FamiliarityA + RewAvg_A:Horizon_fac + FamiliarityA:Horizon_fac, family = binomial, data = dat)
## Table II. Random exploration GLM ---------------------
tab_model(expl_A
          , show.re.var = TRUE
          , pred.labels = c("Preference for 'flower A' (Intercept)"
                            , "Color Bias"
                            , "Reward (Avg. A)"
                            , "Learning (interaction Rew x Fam)"
                            , "Random exploration (interaction Rew x Horizon)"
                            , "Directed exploration (interaction Familiarity x Horizon)"
          )
          , dv.labels = "Effect on prob. of choosing flower type 'A'"
)
## Effect sizes figure Fig S4 -------------------------
lab <- c("Directed exploration", "Random exploration", "Learning", "Reward", "Color Bias", "Right preference")
model_comp(posterior_ExplA, expl_A, lab, 6, skipB = 1)
########################################################
# III. Modeling choice of unfamiliar option directly ------------
## GLM for directed exploration -----------------------
expl_UF_arena <- glm(choseUnfamiliar ~ RewAvgDiff_unfamiliar * Horizon_fac, family = binomial, data = dat)
## Table III. Directed exploration GLM --------------
tab_model(expl_UF_arena
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward (avg. for unfamiliar)"
                            , "Horizon"
                            , "Interaction Rew x Horizon"
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
    , data = list(CL = dat$choseUnfamiliar, val = dat$RewAvgDiff_unfamiliar, hor = dat$Horizon_fac)
    , start = start(dat)
  )
}
### Running the Bayesian analysis ----------------------
posterior_ExplUF_arena <- BayesExploreUnf_arena(dat)
## Effect sizes figure choosing unfamiliar flower -- Fig S4 to go with Fig. 4 -------------------------
lab <- c("Interaction Val x Hor", "Horizon", "Value Diff\n(unfamiliar - familiar)", "Intercept")
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
H1_Expl_mod <- glm(choseUnfamiliar ~ RewAvgDiff_unfamiliar, family = binomial, data = d_graph)
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
points(jitter(choseUnfamiliar, factor = 0.2) ~ jitter(RewAvgDiff_unfamiliar, factor = 1)
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
H6_Expl_mod <- glm(choseUnfamiliar ~ RewAvgDiff_unfamiliar, family = binomial, data = d_graph)
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
points(jitter(choseUnfamiliar, factor = 0.2) ~ jitter(RewAvgDiff_unfamiliar, factor = 1)
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
mtext("Concentration Difference (unfamiliar - familiar)", 1, 2, outer = TRUE)
if(saveFigs) {dev.off()}
### end fig --------------------------------------
