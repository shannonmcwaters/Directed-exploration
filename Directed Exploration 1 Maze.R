# Shannon McWaters, Jack-Morgan Mizell, Ren Calabro, Kiara Alexis Casas, Robert C Wilson, Anna Dornhaus
# (c) 2020-2026
# Directed Exploration in Bumble bees
# Data analysis & graphs - part 1: MAZE

# Packages used ---------------------
library(tidyverse) # Data handling/converting
library(lme4) # Linear models
library(sjPlot) # For nice table output
library(viridis)
library(MASS) # For simulating from covariance matrix
library(rethinking)

# Importing data for experiment 1 directly from github -----------------------
MazeData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/refs/heads/main/Maze%20Data%20Raw")
# Colors & options -----------------------------
colorsfam <- viridis(3, begin = 0.2, end = 0.8)
colorshor <- inferno(2, begin = 0.2, end = 0.8)
colorsparameters <- inferno(4, alpha = 0.8, begin = 0.2, end = 0.8)
transparency_glm_uncertainty <- 0.005 #0.1
transparency_Bay_uncertainty <- 0.01 #0.15
colorassumption <- "darkred"
colorprior <- "slateblue"
# This is how many lines we plot when illustrating uncertainty around fits:
n_uncertainty <- 1000
N_sim <- 1000
showsim <- TRUE
showBayes <- TRUE

# Data formatting ---------------------------------
MazeData$Horizon <- as.factor(MazeData$Horizon)
# Adding a column 'HighInfoChoice' indicating which choice (flower type) the bees
# had more information about. In the 'EQ' treatment, bees have the same amount of
# information about both types. 
# 'HighInfo' generally refers to 'highly informative', i.e. little information at present,
# in other words low familiarity with the flower type
MazeData <- MazeData %>%
  mutate(HighInfoChoice = case_when(
    Info_Treatment == "HH" & Choice1HV == "0" ~ 0,
    Info_Treatment == "HH" & Choice1HV == "1" ~ 1,
    Info_Treatment == "HL" & Choice1HV == "0" ~ 1,
    Info_Treatment == "HL" & Choice1HV == "1" ~ 0,
    Info_Treatment == "EQ" & Choice1HV == "0" ~ NA,
    Info_Treatment == "EQ" & Choice1HV == "1" ~ NA
  ))

# When bees have more information about one flower type, we define the concentration
# difference as (High Information - Low Information) 
MazeData <- MazeData %>%
  mutate(HighInfoConcentrationDiff = ifelse(Info_Treatment == "HL", -ConcentrationDifference, ifelse(Info_Treatment == "EG", NA, ConcentrationDifference)))

# We also calculate the concentration difference relative to the flower on the right,
# as well as the information they have about the flower on the right
MazeData <- MazeData %>%
  mutate(RightConcentrationDiff = ifelse(Choice1HV == "1" & Side == "Right" | Choice1HV == "0" & Side == "Left", ConcentrationDifference, -ConcentrationDifference)) %>%
  mutate(chose_R = ifelse(Side == "Right", "1", "0")) %>%
  mutate(RightInfo = ifelse(is.na(HighInfoChoice), "EQ", ifelse(HighInfoChoice == "1"&Side == "Right" | HighInfoChoice == "0"& Side =="Left","HighInfo","LowInfo")))
MazeData <- MazeData %>%
  mutate(
    famdiffs = case_when(
      RightInfo == "EQ" ~ 0,
      RightInfo == "HighInfo" ~ -1,
      RightInfo == "LowInfo" ~ 1,
      TRUE ~ NA_real_  # for safety in case of unexpected values
    )
  )

# Recoding 'High Informativeness' as 'Low Familiarity' and vice versa
MazeData <- MazeData %>%
  mutate(chose_R_numeric = as.numeric(chose_R)) %>%
  mutate(RightFamiliarity = factor(RightInfo,
                                   levels = c("HighInfo", "EQ", "LowInfo"),
                                   labels = c("Low Familiarity", "Equal", "High Familiarity"))
  )

#######################################################
# MODELING PROBABILITY OF CHOOSING FLOWER ON RIGHT AS REWARD x FAMILIARITY ------
# Data simulation for workflow check and power analysis -----------------------

# So a key first step is to define a generative model that we think underlies the process.
# This model should have parameters (which we will estimate later using the data)
# that reflect the question(s) we want to answer.

# First, we define two helper functions:
# Let's say that bees choose a probability for selecting the left or right flower
# based on their estimated concentration difference as well as the difference in familiarity.
probs_from_pars <- function(valuediffs_right, famdiffs_right, intercept, slope_value, slope_familiarity, slope_valxfam) {
  samplesize <- length(valuediffs_right)
  linear_predictor <- intercept + slope_value * valuediffs_right + slope_familiarity * famdiffs_right + slope_valxfam * valuediffs_right * famdiffs_right
  probability_right <- 1/(1+exp(-linear_predictor))
  return(probability_right)
}
# This function just picks actual choices from probabilities
sim_choice <- function(probabilities) {
  samplesize <- length(probabilities)
  choice_roll <- runif(samplesize, 0, 1)
  choices <- ifelse(choice_roll<probabilities, "right", "left")
  return(choices)
}

slope_value <- 0.5
slope_familiarity <- 0
slope_valxfam <- 0.8
intercept <- 0

valuediffs <- runif(N_sim, min = -2, max = 2)
famdiffs <- sample(c(-1, 0, 1), N_sim, replace = TRUE)
bees <- sample(c("onebee", "twobee"), N_sim, replace = TRUE)

choices_simdata <- sim_choice(probs_from_pars(valuediffs,  famdiffs, intercept, slope_value, slope_familiarity, slope_valxfam))
simdata <- data.frame(choice = choices_simdata, RightConcentrationDiff = valuediffs, famdiffs, Bee = bees)
simdata$chose_R_numeric <- as.numeric(ifelse(simdata$choice == "right", "1", "0"))
simdata$RightFamiliarity <- factor(
  ifelse(simdata$famdiffs == 0, "Equal"
         , ifelse(simdata$famdiffs > 0, "High Familiarity"
                  , "Low Familiarity")
  ),
  levels = c("Low Familiarity", "Equal", "High Familiarity")
)

ifelse(showsim
       , dat <- simdata
       , dat <- MazeData
)
# Bayesian analysis with quap ----------------------

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
  logit(prob) <- a + b*val + c*fam + d*val*fam, 
  # And the following describes the priors for the parameters
  a ~ dnorm(intercept_prior_mean, intercept_prior_SD), 
  b ~ dnorm(slopeval_prior_mean, slopeval_prior_sd), 
  c ~ dnorm(slopefam_prior_mean, slopefam_prior_sd),
  d ~ dnorm(slopevf_prior_mean, slopevf_prior_sd)
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
    )
  )
}
# CL stands for choice list, val is the value or reward difference between the options,
# fam is the familiarity difference between the options. 
BeeChoiceModel <- function(dat) {
  quap(
    list_of_assumptions
    , data = list(CL = dat$chose_R_numeric, val = dat$RightConcentrationDiff, fam = dat$famdiffs)
    , start = start(dat)
  )
}

# I. GLM & Bayes full ------------------------
chooseRmod_full <- glm(chose_R_numeric ~ RightConcentrationDiff * famdiffs, family = binomial, data = dat)
# Bayesian model full
posterior_bees <- BeeChoiceModel(dat)
# Done! The parameter estimates here are for the entire model/dataset, so 
# not directly comparable to the ones from the separate glms for each
# familiarity level. We'll do an overall glm now. 

# Extracting values from Bayesian model for graphing
samples_of_post <- extract.samples(posterior_bees, n=n_uncertainty)
intercepts_post <- samples_of_post$a
slopes_c <- samples_of_post$b
slopes_f <- samples_of_post$c
slopes_cf <- samples_of_post$d

## Table I. Full GLM  -------------------------
tab_model(chooseRmod_full
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward Difference (concentration right - left)"
                            , "Familiarity Difference (right - left)"
                            , "Interaction Rew Diff x Fam Diff"
          )
          , dv.labels = "Effect on probability of choosing right flower"
)

## Results Fig 1 - detailed version in base R with Bayesian and and GLM results -----------
# 3-panel Base R Plot to illustrate binomial fit and uncertainty
# Layout
par(mfrow=c(1,3))
par(oma = c(4,4,0,0), mar = c(1,1,1,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
# Each panel doing its own modeling separately
### Panel 1: LOW FAMILIARITY - model and graph ----------------
d_graph <- subset(dat, dat$RightFamiliarity == "Low Familiarity")
# Plot frame
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(-2, 2)
)
axis(1, at = c(-1.75, -0.75, 0.75, 1.75)) # Define where x-axis tick marks are
# Y axis label for all panels
mtext("Chose Right", 2, 3)
# Two gridlines to show random choice and equal reward
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
# The binomial glm estimates a parameter that is the multiplied with x to give the odds, 
# odds: the probability / (1-probability)
# Sine we are using only data with a single familiarity value, we do not model
# any effect of familiarity here. 
chooseRmod1 <- glm(chose_R_numeric ~ RightConcentrationDiff, family = binomial, data = d_graph)
# Link=logit means the coefficients and factors in the model are fitted to the log odds, e.g.:
# intercept + estimated_par1 * x1 + estimated_par2 * x2 = log(prob/(1-prob))
# (although here there is only one parameter, par1).
# Extracting the parameter estimates:
interc <- chooseRmod1$coefficients[[1]]
par1 <- chooseRmod1$coefficients[[2]]
# What is our certainty around this?
# Here in the glm version, inferential certainty is given by the standard error of
# the parameter estimates. We can generate a bunch of other values for slope and 
# intercept to illustrate the 'error' around our estimated function.
# We could extract slope and intercept separately, but they could depend on each other,
# that is they may not vary independently. We can extract the covariance from 
# the model with vcov(chooseRmod) and generate covarying distributions of values
# with mvrnorm().
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseRmod1))
for(i in 1:n_uncertainty) {
  # 'n_uncertainty' (100 or so) lines normally distributed to illustrate the range of
  # plausible fits from GLM. The GLM fit lines are colored, slightly transparent lines. 
  curve(probs_from_pars(x, -1, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
        , from = -2, to = 2
        , add = TRUE
        , col = alpha(colorsfam[1], transparency_glm_uncertainty)
        , lwd = 3
  )
  # 'n_uncertainty' (100 or so) lines from the posterior distribution from the 
  # Bayesian model fit. The information from these is slightly different than from the 
  # GLM but the gist is the same: illustrating a distribution of possible 'correct'
  # fits, with a tapering (decreasing) likelihood the further away from the mean
  # or overall fit line they are. 
  # The Bayesian fit lines are grey stripes.
  if(showBayes) curve(probs_from_pars(x, -1, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
        , from = -2, to = 2
        , add = TRUE
        , lwd = 3
        , col = alpha("grey34", transparency_Bay_uncertainty)
        , lty = 2
  )
}
# 'True' relationship if using simdata
if(showsim) 
  curve(probs_from_pars(x,  -1, intercept, slope_value, slope_familiarity, slope_valxfam)
        , from = -2, to = 2
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )
# Original data points with slight jitter
points(jitter(chose_R_numeric, factor = 0.2) ~ jitter(RightConcentrationDiff, factor = 1)
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
# (We're doing this last instead of earlier so it comes out on top for better
# visibility.)
curve(probs_from_pars(x, -1, interc, par1, 0, 0)
      , from = -2, to = 2
      , add = TRUE
      , col = colorsfam[1]
      , lwd = 5)
# Text labels for panel
# Converting parameters for labels:
ose <- round(exp(par1),1) # 'odds slope', how much the odds change with a change in x
int <- round(probs_from_pars(0, -1, interc, par1, 0, 0), 2) # probability at x=0
ose_sd <- round(exp(par1+summary(chooseRmod1)$coefficients[2,2]) - ose, 1)
int_sd <- round(probs_from_pars(0, -1, interc+summary(chooseRmod1)$coefficients[1,2], par1, 0, 0) - int, 2)
pvalue <- round(summary(chooseRmod1)$coefficients[2,4], 3)
# Just for better understanding and data checking, we'll also label with the
# sample size for the entire panel and the overall proportion of right choices.
overall <- round(mean(d_graph$chose_R_numeric), 2)
# Now label with overall and per-level sample sizes, and add model result. 
samples <- length(d_graph$chose_R_numeric)
text(x = -2, y = 1.13, labels = paste("N = ", samples, sep = ""), col = colorsfam[1], adj = 0)
text(x = -2, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue, sep = ""), cex = 1, col = colorsfam[1], adj = 0)
text(x = -2, y = 1.07, labels = paste("Right pref (glm): ", int, " +/- ", int_sd, sep = ""), cex = 1, col = colorsfam[1], adj = 0)
samples2 <- table(d_graph$RightConcentrationDiff)
text(x = -1.75, y =-0.1, labels = paste("n=", samples2[1]), col = colorsfam[1])
text(x = -0.75, y =-0.1, labels = paste("n=", samples2[2]), col = colorsfam[1])
text(x = 0.75, y =-0.1, labels = paste("n=", samples2[3]), col = colorsfam[1])
text(x = 1.75, y =-0.1, labels = paste("n=", samples2[4]), col = colorsfam[1])

### Panel 2: EQUAL FAMILIARITY - model and graph --------------------
# We follow all the same steps as for Panel 1 (hence no annotations)
d_graph <- subset(dat, dat$RightFamiliarity == "Equal")
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(-2, 2)
     , yaxt = 'n'
)
axis(1, at = c(-1.75, -0.75, 0.75, 1.75)) # Define where x-axis tick marks are
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseRmod2 <- glm(chose_R_numeric ~ RightConcentrationDiff, family = binomial, data = d_graph)
interc <- chooseRmod2$coefficients[[1]]
par1 <- chooseRmod2$coefficients[[2]]
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseRmod2))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
        , from = -2, to = 2
        , add = TRUE
        , col = alpha(colorsfam[2], transparency_glm_uncertainty)
        , lwd = 3
  )
  if(showBayes) curve(probs_from_pars(x, 0, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
        , from = -2, to = 2
        , add = TRUE
        , lwd = 3
        , col = alpha("grey34", transparency_Bay_uncertainty)
        , lty = 2
  )
}
if(showsim) 
  curve(probs_from_pars(x,  0, intercept, slope_value, slope_familiarity, slope_valxfam)
        , from = -2, to = 2
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )
points(jitter(chose_R_numeric, factor = 0.2) ~ jitter(RightConcentrationDiff, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[2], 0.5)
       , cex = 2
)
if(showBayes) curve(probs_from_pars(x, 0, precis(posterior_bees)[1,1]
                      , precis(posterior_bees)[2,1]
                      , precis(posterior_bees)[3,1]
                      , precis(posterior_bees)[4,1])
      , from = -2, to = 2
      , add = TRUE
      , lwd = 4
      , col = "grey34"
      , lty = 2
)
curve(probs_from_pars(x, 0, interc, par1, 0, 0)
      , from = -2, to = 2
      , add = TRUE
      , col = colorsfam[2]
      , lwd = 5)
ose <- round(exp(par1),1) # 'odds slope', how much the odds change with a change in x
int <- round(probs_from_pars(0, 0, interc, par1, 0, 0), 2) # probability at x=0
ose_sd <- round(exp(par1+summary(chooseRmod2)$coefficients[2,2]) - ose, 1)
int_sd <- round(probs_from_pars(0, 0, interc+summary(chooseRmod2)$coefficients[1,2], par1, 0, 0) - int, 2)
pvalue <- round(summary(chooseRmod2)$coefficients[2,4], 3)
overall <- round(mean(d_graph$chose_R_numeric), 2)
if(showsim) text(x = -1, y = 0.8, labels = paste("Prop. ", overall), col = colorsfam[2])
# Now label with overall and per-level sample sizes, and add model result. 
samples <- length(d_graph$chose_R_numeric)
text(x = -2, y = 1.13, labels = paste("N = ", samples, sep = ""), col = colorsfam[2], adj = 0)
text(x = -2, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue, sep = ""), cex = 1, col = colorsfam[2], adj = 0)
text(x = -2, y = 1.07, labels = paste("Right pref (glm): ", int, " +/- ", int_sd, sep = ""), cex = 1, col = colorsfam[2], adj = 0)
samples2 <- table(d_graph$RightConcentrationDiff)
text(x = -1.75, y =-0.1, labels = paste("n=", samples2[1]), col = colorsfam[2])
text(x = -0.75, y =-0.1, labels = paste("n=", samples2[2]), col = colorsfam[2])
text(x = 0.75, y =-0.1, labels = paste("n=", samples2[3]), col = colorsfam[2])
text(x = 1.75, y =-0.1, labels = paste("n=", samples2[4]), col = colorsfam[2])


### Panel 3: HIGH FAMILIARITY - model and graph ----------------------
d_graph <- subset(dat, dat$RightFamiliarity == "High Familiarity")
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , yaxt = 'n'
     , xlim = c(-2, 2)
)
axis(1, at = c(-1.75, -0.75, 0.75, 1.75)) # Define where x-axis tick marks are
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseRmod3 <- glm(chose_R_numeric ~ RightConcentrationDiff, family = binomial, data = d_graph)
interc <- chooseRmod3$coefficients[[1]]
par1 <- chooseRmod3$coefficients[[2]]
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseRmod2))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 1, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
        , from = -2, to = 2
        , add = TRUE
        , col = alpha(colorsfam[3], transparency_glm_uncertainty)
        , lwd = 3
  )
  if(showBayes) curve(probs_from_pars(x, 1, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
        , from = -2, to = 2
        , add = TRUE
        , lwd = 3
        , col = alpha("grey34", transparency_Bay_uncertainty)
        , lty = 2
  )
}
if(showsim) 
  curve(probs_from_pars(x,  1, intercept, slope_value, slope_familiarity, slope_valxfam)
        , from = -2, to = 2
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )
points(jitter(chose_R_numeric, factor = 0.2) ~ jitter(RightConcentrationDiff, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[3], 0.5)
       , cex = 2
)
if(showBayes) curve(probs_from_pars(x, 1, precis(posterior_bees)[1,1]
                      , precis(posterior_bees)[2,1]
                      , precis(posterior_bees)[3,1]
                      , precis(posterior_bees)[4,1])
      , from = -2, to = 2
      , add = TRUE
      , lwd = 4
      , col = "grey34"
      , lty = 2
)
curve(probs_from_pars(x, 1, interc, par1, 0, 0)
      , from = -2, to = 2
      , add = TRUE
      , col = colorsfam[3]
      , lwd = 5)
ose <- round(exp(par1),1) # 'odds slope', how much the odds change with a change in x
int <- round(probs_from_pars(0, 1, interc, par1, 0, 0), 2) # probability at x=0
ose_sd <- round(exp(par1+summary(chooseRmod2)$coefficients[2,2]) - ose, 1)
int_sd <- round(probs_from_pars(0, 1, interc+summary(chooseRmod2)$coefficients[1,2], par1, 0, 0) - int, 2)
pvalue <- round(summary(chooseRmod2)$coefficients[2,4], 3)
overall <- round(mean(d_graph$chose_R_numeric), 2)
if(showsim) text(x = -1, y = 0.8, labels = paste("Prop. ", overall), col = colorsfam[2])
# Now label with overall and per-level sample sizes, and add model result. 
samples <- length(d_graph$chose_R_numeric)
text(x = -2, y = 1.13, labels = paste("N = ", samples, sep = ""), col = colorsfam[3], adj = 0)
text(x = -2, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue, sep = ""), cex = 1, col = colorsfam[3], adj = 0)
text(x = -2, y = 1.07, labels = paste("Right pref (glm): ", int, " +/- ", int_sd, sep = ""), cex = 1, col = colorsfam[3], adj = 0)
samples2 <- table(d_graph$RightConcentrationDiff)
text(x = -1.75, y =-0.1, labels = paste("n=", samples2[1]), col = colorsfam[3])
text(x = -0.75, y =-0.1, labels = paste("n=", samples2[2]), col = colorsfam[3])
text(x = 0.75, y =-0.1, labels = paste("n=", samples2[3]), col = colorsfam[3])
text(x = 1.75, y =-0.1, labels = paste("n=", samples2[4]), col = colorsfam[3])

### Joint x axis label for all panels: ---------------
mtext("Concentration Difference", 1, 2, outer = TRUE)
### end figure -------------------------


#######################################################
# II. Degree of exploration -------
# Data simulation for workflow check and power analysis -----------------------
# We use the parameters above, intercept, slope_value, slope_familiarity, and slope_valxfam
# as 'default', i.e. with horizon = 1. 
# If there is random exploration , then bees should be more likely to make random decisions
# with longer horizons. We model this as an interaction between reward and horizon,
# since reward will matter less if there is more random exploration in longer horizons.
rnd_expl_simpar <- 0.4
# Directed exploration on the other hand implies deliberate, i.e. adaptive, 
# information seeking, in other words bees should particularly choose less familiar
# flowers in longer horizons. 
dir_expl_simpar <- 0.2
# Now we generate data both for horizon = 1 (essentially as above) and for horizon 
# = 6 (with the new interactions). We code horizon = 6 as '1' in the model, thus
# the exploration parameters simply get added to the parameters multiplied with 
# the factor - reward or familiarity. 

# Horizon 1
valuediffs <- runif(N_sim, min = -2, max = 2)
famdiffs <- sample(c(-1, 0, 1), N_sim, replace = TRUE)
bees <- sample(c("onebee", "twobee"), N_sim, replace = TRUE)
choices_simdata <- sim_choice(probs_from_pars(valuediffs,  famdiffs, intercept, slope_value, slope_familiarity, slope_valxfam))
simdata_h1 <- data.frame(choice = choices_simdata, RightConcentrationDiff = valuediffs, famdiffs, Bee = bees, Horizon = 1)

# Horizon 6
valuediffs <- runif(N_sim, min = -2, max = 2)
famdiffs <- sample(c(-1, 0, 1), N_sim, replace = TRUE)
bees <- sample(c("onebee", "twobee"), N_sim, replace = TRUE)
choices_simdata <- sim_choice(probs_from_pars(valuediffs,  famdiffs, intercept, slope_value+rnd_expl_simpar, slope_familiarity+dir_expl_simpar, slope_valxfam))
simdata_h6 <- data.frame(choice = choices_simdata, RightConcentrationDiff = valuediffs, famdiffs, Bee = bees, Horizon = 6)

# Now joining the two datasets together
simdata <- rbind(simdata_h1, simdata_h6)
# Some data formatting to match empirical data
simdata$Horizon <- as.factor(simdata$Horizon)
simdata$chose_R_numeric <- as.numeric(ifelse(simdata$choice == "right", "1", "0"))
simdata$RightFamiliarity <- factor(
  ifelse(simdata$famdiffs == 0, "Equal"
         , ifelse(simdata$famdiffs > 0, "High Familiarity"
                  , "Low Familiarity")
  ),
  levels = c("Low Familiarity", "Equal", "High Familiarity")
)

# 'HighInfo' in the original data is the 'highly informative', thus
# low familiarity choice. 
simdata <- simdata %>%
  mutate(HighInfoChoice = case_when(
    chose_R_numeric == "0" & RightFamiliarity == "High Familiarity" ~ 1,
    chose_R_numeric == "1" & RightFamiliarity == "High Familiarity" ~ 0,
    chose_R_numeric == "0" & RightFamiliarity == "Low Familiarity" ~ 0,
    chose_R_numeric == "1" & RightFamiliarity == "Low Familiarity" ~ 1,
    chose_R_numeric == "0" & RightFamiliarity == "Equal" ~ NA,
    chose_R_numeric == "1" & RightFamiliarity == "Equal" ~ NA
  ))

# When bees have different amounts of information about one flower type, we 
# define the concentration difference as (High Info - Low Info(rmativeness)) 
simdata <- simdata %>%
  mutate(HighInfoConcentrationDiff = 
           ifelse(RightFamiliarity == "High Familiarity", -RightConcentrationDiff
                  , ifelse(RightFamiliarity == "Equal", NA
                           , RightConcentrationDiff)))

# Bayesian analysis with quap ----------------------
## Priors -------------------
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
  logit(prob) <- a + (b+rnd*hor)*val + (c+dir*hor)*fam + d*val*fam, 
  # And the following describes the priors for the parameters
  a ~ dnorm(intercept_prior_mean, intercept_prior_SD), 
  b ~ dnorm(slopeval_prior_mean, slopeval_prior_sd), 
  c ~ dnorm(slopefam_prior_mean, slopefam_prior_sd),
  d ~ dnorm(slopevf_prior_mean, slopevf_prior_sd),
  rnd ~ dnorm(rnd_explor_prior_mean, rnd_explor_prior_sd),
  dir ~ dnorm(dir_explor_prior_mean, dir_explor_prior_sd)
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
    )
  )
}
# CL stands for choice list, val is the value or reward difference between the options,
# fam is the familiarity difference between the options. 
ExplorModel <- function(dat) {
  quap(
    list_of_assumptions
    , data = list(CL = dat$chose_R_numeric, val = dat$RightConcentrationDiff, fam = dat$famdiffs, hor = as.numeric(dat$Horizon))
    , start = start(dat)
  )
}

# Choose data to use ---------------------------------
ifelse(showsim
       , dat <- simdata
       , dat <- MazeData
)

## GLM ------------------------------------
rndExploration_mod <- glm(chose_R_numeric ~ RightConcentrationDiff * Horizon, family = binomial, data = dat)
## Bayesian analysis -----------------------
posterior_bees <- ExplorModel(dat)
# Extracting values from Bayesian model for graphing
samples_of_post <- extract.samples(posterior_bees, n=n_uncertainty)
intercepts_post <- samples_of_post$a
slopes_c <- samples_of_post$b
slopes_f <- samples_of_post$c
slopes_cf <- samples_of_post$d
## Table II. Random exploration GLM ---------------------
tab_model(rndExploration_mod
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward Difference (right - left)"
                            , "Horizon"
                            , "Interaction Rew Diff x Horizon"
          )
          , dv.labels = "Effect on probability of choosing right flower"
)
#######################################################
# III. Directed exploration -------
## GLM
dirExpl_HI_mod <- glm(HighInfoChoice ~ HighInfoConcentrationDiff * Horizon, family = binomial, data = dat)
## Table III. Directed exploration GLM --------------
tab_model(dirExpl_HI_mod
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward Difference (unfamiliar - familiar)"
                            , "Horizon"
                            , "Interaction Rew Diff x Horizon"
          )
          , dv.labels = "Effect on probability of choosing less familiar flower"
)

## Results Fig 2 - detailed version in base R with Bayesian and and GLM results -----------
# 2-panel Base R Plot to illustrate binomial fit and uncertainty
# Layout
par(mfrow=c(1,2))
par(oma = c(4,4,0,0), mar = c(1,1,1,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
n_uncertainty <- 250
# Each panel doing its own modeling separately
### Horizon 1 - model and graph ----------------
d_graph <- subset(dat, dat$Horizon == 1)
# Plot frame
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(-2, 2)
)
axis(1, at = c(-1.75, -0.75, 0.75, 1.75)) # Define where x-axis tick marks are
# Bottom axis label for all panels
mtext("Chose Low Familiarity Option", 2, 3)
# Gridlines
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
# GLM - model and extracting estimates
H1_Expl_mod <- glm(HighInfoChoice ~ HighInfoConcentrationDiff, family = binomial, data = dat)
interc <- H1_Expl_mod$coefficients[[1]]
par1 <- H1_Expl_mod$coefficients[[2]]
ose <- round(exp(par1),1)
ose_sd <- round(exp(par1+summary(H1_Expl_mod)$coefficients[2,2]) - ose, 1)
pvalue <- round(summary(H1_Expl_mod)$coefficients[2,4], 3)
int <- round(probs_from_pars(0, 0, interc, par1, 0, 0), 2)
int_sd <- round(probs_from_pars(0, 0, interc+summary(H1_Expl_mod)$coefficients[1,2], par1, 0, 0) - int, 2)
# Extracting uncertainty
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(H1_Expl_mod))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0), from = -2, to = 2
        , add = TRUE
        , col = alpha(colorshor[1], transparency_glm_uncertainty)
        , lwd = 3
  )
  curve(probs_from_pars(x, -1, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
        , from = -2, to = 2
        , add = TRUE
        , lwd = 3
        , col = alpha("grey34", transparency_Bay_uncertainty)
        , lty = 2
  )
}
# Original data points with slight jitter
points(jitter(HighInfoChoice, factor = 0.2) ~ jitter(HighInfoConcentrationDiff, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorshor[1], 0.5)
       , cex = 1.5
)
# Plotting Bayesian fit line
curve(probs_from_pars(x, -1, precis(posterior_bees)[1,1]
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
# (We're doing this last instead of earlier so it comes out on top for better
# visibility.)
curve(probs_from_pars(x, 0, interc, par1, 0, 0), from = -2, to = 2
      , add = TRUE
      , col = colorshor[1]
      , lwd = 5)
# Text labels for panel
overall <- round(mean(d_graph$HighInfoChoice), 2)
samples <- length(d_graph$HighInfoChoice)
text(x = -2, y = 1.13, labels = paste("N = ", samples), col = colorshor[1], cex = 0.7, adj = 0)
text(x = -2, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue), cex = 0.7, col = colorshor[1], adj = 0)
text(x = -2, y = 1.07, labels = paste("Right pref (glm): ", int, " +/- ", int_sd), cex = 0.7, col = colorshor[1], adj = 0)
samples2 <- table(d_graph$HighInfoConcentrationDiff)
text(x = -1.75, y =-0.1, labels = paste("n=", samples2[1]), col = colorshor[1])
text(x = -0.75, y =-0.1, labels = paste("n=", samples2[2]), col = colorshor[1])
text(x = 0.75, y =-0.1, labels = paste("n=", samples2[3]), col = colorshor[1])
text(x = 1.75, y =-0.1, labels = paste("n=", samples2[4]), col = colorshor[1])

### Horizon 6 - model and graph ----------------------
d_graph <- subset(dat, dat$Horizon == 6)
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , yaxt = 'n'
     , xaxt = 'n'
     , xlim = c(-2, 2)
)
axis(1, at = c(-1.75, -0.75, 0.75, 1.75)) # Define where x-axis tick marks are
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
# GLM - model and extracting estimates
H6_Expl_mod <- glm(HighInfoChoice ~ HighInfoConcentrationDiff, family = binomial, data = d_graph)
interc <- H6_Expl_mod$coefficients[[1]]
par1 <- H6_Expl_mod$coefficients[[2]]
ose <- round(exp(par1),1)
ose_sd <- round(exp(par1+summary(H6_Expl_mod)$coefficients[2,2]) - ose, 1)
pvalue <- round(summary(H6_Expl_mod)$coefficients[2,4], 3)
int <- round(probs_from_pars(0, 0, interc, par1, 0, 0), 2)
int_sd <- round(probs_from_pars(0, 0, interc+summary(H6_Expl_mod)$coefficients[1,2], par1, 0, 0) - int, 2)
# Bayesian model and extracting estimates
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(H6_Expl_mod))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0), from = -2, to = 2
        , add = TRUE
        , col = alpha(colorshor[2], transparency_glm_uncertainty)
        , lwd = 3
  )
}
# Original data points with slight jitter
points(jitter(HighInfoChoice, factor = 0.2) ~ jitter(HighInfoConcentrationDiff, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorshor[2], 0.5)
       , cex = 1.5
)
curve(probs_from_pars(x, 0, interc, par1, 0, 0), from = -2, to = 2
      , add = TRUE
      , col = colorshor[2]
      , lwd = 5)
overall <- round(mean(d_graph$HighInfoChoice), 2)
samples <- length(d_graph$HighInfoChoice)
text(x = -2, y = 1.13, labels = paste("N = ", samples), col = colorshor[2], cex = 0.7, adj = 0)
text(x = -2, y = 1.1, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue), cex = 0.7, col = colorshor[2], adj = 0)
text(x = -2, y = 1.07, labels = paste("Right pref (glm): ", int, " +/- ", int_sd), cex = 0.7, col = colorshor[2], adj = 0)
samples2 <- table(d_graph$HighInfoConcentrationDiff)
text(x = -1.75, y =-0.1, labels = paste("n=", samples2[1]), col = colorshor[2])
text(x = -0.75, y =-0.1, labels = paste("n=", samples2[2]), col = colorshor[2])
text(x = 0.75, y =-0.1, labels = paste("n=", samples2[3]), col = colorshor[2])
text(x = 1.75, y =-0.1, labels = paste("n=", samples2[4]), col = colorshor[2])

mtext("Concentration Difference", 1, 2, outer = TRUE)

### end fig --------------------------------------
#######################################################

