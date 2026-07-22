# Shannon McWaters, Jack-Morgan Mizell, Ren Calabro, Kiara Alexis Casas, Robert C Wilson, Anna Dornhaus
# (c) 2020-2026
# Directed Exploration in Bumble bees
# Data analysis & graphs - part 1: MAZE
# SET THESE OPTIONS BEFORE RUNNING SCRIPT -----------------
showsim <- FALSE
showBayes <- TRUE
showGLM <- FALSE
showDetail <- FALSE
saveFigs <- FALSE
figs_path <- paste(getwd(), "/Figures", sep = "")

# Packages used ---------------------
library(tidyverse) # Data handling/converting
library(lme4) # Linear models
library(sjPlot) # For nice table output
library(viridis) # Color scale
library(MASS) # For simulating from covariance matrix
library(rethinking) # For Bayesian analysis
library(R.utils) # To insert a value in a vector 

# Importing data for experiment 1 directly from github -----------------------
MazeData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/refs/heads/main/Maze%20Data%20Raw")
# Colors -----------------------------
colorsfam <- viridis(3, begin = 0.2, end = 0.8)
colorshor <- inferno(2, begin = 0.2, end = 0.8)
colorsparameters <- inferno(4, alpha = 0.8, begin = 0.2, end = 0.8)
transparency_glm_uncertainty <- 0.15 #0.01 #0.1
transparency_Bay_uncertainty <- 0.05 #0.018 #0.15
colorassumption <- "darkred"
colorprior <- "slateblue"
# This is how many lines we plot when illustrating uncertainty around fits:
n_uncertainty <- 300 
n_ppp_per_set <- 200 
N_sim <- 1000
# Data formatting ---------------------------------
MazeData$Horizon <- as.factor(MazeData$Horizon)
MazeData$Bee_tag <- MazeData$Bee
MazeData$BeeID <- paste(MazeData$Bee, MazeData$RA)
# Adding a column 'choseUnfamiliar' indicating which choice (flower type) the bees
# had more information about. In the 'EQ' treatment, bees have the same amount of
# information about both types. 
# 'HighInfo' generally refers to 'highly informative', i.e. little information at present,
# in other words low familiarity with the flower type
MazeData <- MazeData %>%
  mutate(choseUnfamiliar = case_when(
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
  mutate(Unfamiliar_ConcDiff = ifelse(Info_Treatment == "EQ", NA, ifelse(Info_Treatment == "HL", -ConcentrationDifference, ConcentrationDifference)))

# We also calculate the concentration difference relative to the flower on the right,
# as well as the information they have about the flower on the right
MazeData <- MazeData %>%
  mutate(Right_ConcDiff = ifelse(Choice1HV == "1" & Side == "Right" | Choice1HV == "0" & Side == "Left", ConcentrationDifference, -ConcentrationDifference)) %>%
  mutate(chose_R = ifelse(Side == "Right", "1", "0")) %>%
  mutate(RightInfo = ifelse(is.na(choseUnfamiliar), "EQ", ifelse(choseUnfamiliar == "1"&Side == "Right" | choseUnfamiliar == "0"& Side =="Left","HighInfo","LowInfo"))) %>%
  mutate(RightValue = case_when(
    Right_ConcDiff == -1.75 ~ 0.25,
    Right_ConcDiff == -0.75 ~ 0.75,
    Right_ConcDiff ==  0.75 ~ 1.5,
    Right_ConcDiff ==  1.75 ~ 2.0
  ))
MazeData <- MazeData %>%
  mutate(
    Right_FamDiff = case_when(
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
MazeData$ColorChosen[MazeData$ColorChosen=="LimeGreen"] <- "Light Green"
MazeData$ColorChosen[MazeData$ColorChosen=="Lime Green"] <- "Light Green"
MazeData$ColorChosen <- as.factor(MazeData$ColorChosen)
MazeData$ColorChosen <- factor(MazeData$ColorChosen, levels = c("Red", "Light Blue", "Green", "Orange", "Dark Blue", "White", "Light Green", "Black"))
MazeData$ColorNotChosen <- ifelse(as.numeric(MazeData$ColorChosen) %in% c(1,3,5,7), as.numeric(MazeData$ColorChosen)+1, as.numeric(MazeData$ColorChosen)-1)
MazeData$ColorNotChosen <- as.factor(MazeData$ColorNotChosen)
levels(MazeData$ColorNotChosen) <- c("Red", "Light Blue", "Green", "Orange", "Dark Blue", "White", "Light Green", "Black")
colpref <- table(MazeData$ColorChosen) / (table(MazeData$ColorChosen) + table(MazeData$ColorNotChosen))
MazeData$ColorPref <- as.vector(colpref[MazeData$ColorChosen])
MazeData$ColorPref[is.na(MazeData$ColorPref)] <- 0.5
#######################################################
## Figure showing color preferences ----------------------------
if(saveFigs) {
  setwd(figs_path)
  pdf(file = "FigS1 Maze color prefs.pdf", width = 10, height = 6)
}
par(mfrow=c(1,1), mar=c(4,4,0,0), oma=c(0,0,0,0))
colorlist_colors <- c("red3", "deepskyblue", "green4", "orange", "blue3","white","green1", "black")
spacing <- c(0.1, 0, 0.1, 0, 0.1, 0, 0.1, 0)
Nice_plot <- barplot(colpref
        , ylim = c(0,1)
        , ylab = "Proportion of choices out of available trials"
        , xlab = "Color"
        , col = colorlist_colors
        , space = spacing
        , cex.names = 0.8
        )
textpos <- cumsum(spacing)
text((1:8)+textpos-0.5, 0.1, paste("n=",table(MazeData$ColorChosen), sep=""), col = c("black","black","black","black","black","black","black","white"))
if(saveFigs) dev.off()
#######################################################
# MODELING PROBABILITY OF CHOOSING FLOWER ON RIGHT AS REWARD x FAMILIARITY ------
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

# Data simulation for workflow check and power analysis -----------------------

# So a key first step is to define a generative model that we think underlies the process.
# This model should have parameters (which we will estimate later using the data)
# that reflect the question(s) we want to answer.


# Parameter values chosen for simulation
slope_value <- 0.5
slope_familiarity <- 0
slope_valxfam <- 0.8
slope_color <- 0
intercept <- 0

# There is no attempt here to simulate an actual experiment; we merely
# simulate a dataset that varies these parameters uniformly and independently, 
# and calculate the resulting 'choice' from the modeled function. 
valuediffs <- runif(N_sim, min = -2, max = 2)
famdiffs <- sample(c(-1, 0, 1), N_sim, replace = TRUE)
colorprefs <- runif(N_sim, min = 0.3, max = 0.7)
bees <- sample(c("onebee", "twobee"), N_sim, replace = TRUE)

choices_simdata <- sim_choiceR(probs_from_pars(valuediffs,  famdiffs, colorprefs, intercept, slope_value, slope_familiarity, slope_valxfam, slope_color))
simdata <- data.frame(choice = choices_simdata, Right_ConcDiff = valuediffs, famdiffs, ColorPref = colorprefs, Bee = bees)
simdata$chose_R_numeric <- as.numeric(ifelse(simdata$choice == "right", "1", "0"))
simdata$RightFamiliarity <- factor(
  ifelse(simdata$famdiffs == 0, "Equal"
         , ifelse(simdata$famdiffs > 0, "High Familiarity"
                  , "Low Familiarity")
  ),
  levels = c("Low Familiarity", "Equal", "High Familiarity")
)

## Choose data to use ----------------------------
if(showsim) {
  dat <- simdata
} else {
  dat <- MazeData
}
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

## BAYESIAN MODEL FORMUA and assumption list ---------------
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
BayesChooseR <- function(dat) {
  quap(
    list_of_assumptions
    , data = list(CL = dat$chose_R_numeric, val = dat$RightValue, fam = dat$Right_FamDiff, col = dat$ColorPref)
    , start = start(dat)
  )
}

# I. GLM & Bayes  choose right ------------------------
chooseR <- glm(chose_R_numeric ~ RightValue * Right_FamDiff + ColorPref, family = binomial, data = dat)

# Bayesian model for choosing right flower
posterior_bees <- BayesChooseR(dat)
# Done!

## Table I. GLM  -------------------------
tab_model(chooseR
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward (concentration right)"
                            , "Familiarity (right - left)"
                            , "Color Bias"
                            , "Interaction Reward x Familiarity"
          )
          , dv.labels = "Effect on probability of choosing right flower"
)
#######################################################
## Effect sizes figure -------------------------
if(saveFigs) {
  setwd(figs_path)
  pdf(file = "FigS3 Maze effect sizes val x fam.pdf", width = 10, height = 6)
}
lab <- c("Interaction Val x Fam", "Color", "Familiarity", "Value", "Intercept")
model_comp(posterior_bees, chooseR, lab, 5)
if(saveFigs) dev.off()
#######################################################
## Results Fig: chose R ~ reward by fam -----------
# Extracting values from Bayesian model for graphing
samples_of_post <- extract.samples(posterior_bees, n=n_uncertainty)
intercepts_post <- samples_of_post$intpt
slopes_c <- samples_of_post$pval
slopes_f <- samples_of_post$pfam
slopes_cf <- samples_of_post$pvxf
slopes_col <- samples_of_post$pcol

# 3-panel Base R Plot to illustrate binomial fit and uncertainty
if(saveFigs) {
  setwd(figs_path)
  pdf(file = "Fig2 Maze chose right.pdf", width = 10, height = 6)
}
# Layout
par(mfrow=c(1,3))
par(oma = c(4,4,0,0), mar = c(1,1,3,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
# Each panel doing its own modeling separately
### Panel 1: LOW FAMILIARITY - model and graph ----------------
d_graph <- subset(dat, dat$RightFamiliarity == "Low Familiarity")
# Plot frame
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(0, 2.25)
)
axis(1, at = c(0.25, 0.75, 1.5, 2)) # Define where x-axis tick marks are
# Y axis label for all panels
mtext("Chose Right", 2, 3)
# The binomial glm estimates a parameter that is the multiplied with x to give the odds, 
# odds: the probability / (1-probability)
# Sine we are using only data with a single familiarity value, we do not model
# any effect of familiarity here. 
chooseRmod1 <- glm(chose_R_numeric ~ RightValue, family = binomial, data = d_graph)
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
  # 'n_uncertainty' (100 or so) lines from the posterior distribution from the 
  # Bayesian model fit. The information from these is slightly different than from the 
  # GLM but the gist is the same: illustrating a distribution of possible 'correct'
  # fits, with a tapering (decreasing) likelihood the further away from the mean
  # or overall fit line they are. 
  # The Bayesian fit lines are grey stripes.
  if(showBayes) curve(probs_from_pars(x, -1, 0, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i], slopes_col[i])
        , from = -2, to = 2.5
        , add = TRUE
        , lwd = 3
        , col = alpha(colorsfam[1], transparency_Bay_uncertainty)
        , lty = 1
  )
  # 'n_uncertainty' (100 or so) lines normally distributed to illustrate the range of
  # plausible fits from GLM. The GLM fit lines are colored, slightly transparent lines. 
  if(showGLM) curve(probs_from_pars(x, -1, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
                    , from = -2, to = 2.5
                    , add = TRUE
                    , col = alpha("grey34", transparency_glm_uncertainty)
                    , lwd = 3
                    , lty = 2
  )
}
# 'True' relationship if using simdata
if(showsim) 
  curve(probs_from_pars(x,  -1, 0, intercept, slope_value, slope_familiarity, slope_valxfam, slope_color)
        , from = -2, to = 2.5
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )
# Original data points with slight jitter
points(jitter(chose_R_numeric, factor = 0.2) ~ jitter(RightValue, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[1], 0.5)
       , cex = 2
)
# Plotting the estimated fit from the glm:
# (We're doing this last instead of earlier so it comes out on top for better
# visibility.)
if(showGLM) curve(probs_from_pars(x, -1, 0, interc, par1, 0, 0)
      , from = -2, to = 2.5
      , add = TRUE
      , col = "grey34" 
      , lwd = 5
      , lty = 2)
# Plotting Bayesian fit line
if(showBayes) curve(probs_from_pars(x, -1, 0
                                    , precis(posterior_bees)[1,1]
                                    , precis(posterior_bees)[2,1]
                                    , precis(posterior_bees)[3,1]
                                    , precis(posterior_bees)[5,1]
                                    , precis(posterior_bees)[4,1])
                    , from = -2, to = 2.5
                    , add = TRUE
                    , lwd = 4
                    , col = colorsfam[1]
                    , lty = 1
)
# Gridline to show random choice
abline(h = 0.5, col = "grey", lty = 2, lwd = 2)
# Text labels for panel
# Converting parameters for labels:
ose <- round(exp(par1),1) # 'odds slope', how much the odds change with a change in x
int <- round(probs_from_pars(0, -1, 0, interc, par1, 0, 0, 0), 2) # probability at x=0
ose_sd <- round(exp(par1+summary(chooseRmod1)$coefficients[2,2]) - ose, 1)
int_sd <- round(probs_from_pars(0, -1, 0, interc+summary(chooseRmod1)$coefficients[1,2], par1, 0, 0, 0) - int, 2)
pvalue <- round(summary(chooseRmod1)$coefficients[2,4], 3)
# Just for better understanding and data checking, we'll also label with the
# sample size for the entire panel and the overall proportion of right choices.
overall <- round(mean(d_graph$chose_R_numeric), 2)
# Now label with overall and per-level sample sizes, and add model result. 
samples <- length(d_graph$chose_R_numeric)
samples2 <- table(d_graph$RightValue)
if(showDetail) {
  text(x = -2, y = 1.13, labels = paste("N = ", samples, sep = ""), col = colorsfam[1], adj = 0)
  text(x = -2, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue, sep = ""), cex = 1, col = colorsfam[1], adj = 0)
  text(x = -2, y = 1.07, labels = paste("Right pref (glm): ", int, " +/- ", int_sd, sep = ""), cex = 1, col = colorsfam[1], adj = 0)
  if(showsim) text(x = -1, y = 0.8, labels = paste("Prop. ", overall), col = colorsfam[1])
}
text(x = 0.25, y =-0.1, labels = paste("n=", samples2[1]), col = colorsfam[1])
text(x = 0.75, y =-0.1, labels = paste("n=", samples2[2]), col = colorsfam[1])
text(x = 1.5, y =-0.1, labels = paste("n=", samples2[3]), col = colorsfam[1])
text(x = 2, y =-0.1, labels = paste("n=", samples2[4]), col = colorsfam[1])
mtext("1 visit right, 3 left", 3, 1)

### Panel 2: EQUAL FAMILIARITY - model and graph --------------------
# We follow all the same steps as for Panel 1 (hence no annotations)
d_graph <- subset(dat, dat$RightFamiliarity == "Equal")
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , yaxt = 'n'
     , xlim = c(0, 2.25)
)
axis(1, at = c(0.25, 0.75, 1.5, 2)) # Define where x-axis tick marks are
chooseRmod2 <- glm(chose_R_numeric ~ RightValue, family = binomial, data = d_graph)
interc <- chooseRmod2$coefficients[[1]]
par1 <- chooseRmod2$coefficients[[2]]
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseRmod2))
for(i in 1:n_uncertainty) {
  if(showBayes) curve(probs_from_pars(x, 0, 0, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i], slopes_col[i])
                      , from = -2, to = 2.5
                      , add = TRUE
                      , lwd = 3
                      , col = alpha(colorsfam[2], transparency_Bay_uncertainty)
                      , lty = 1
  )
  if(showGLM) curve(probs_from_pars(x, 0, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0, 0)
        , from = -2, to = 2.5
        , add = TRUE
        , col = alpha("grey34", transparency_glm_uncertainty)
        , lwd = 3
        , lty = 2
  )
}
if(showsim) 
  curve(probs_from_pars(x,  0, 0, intercept, slope_value, slope_familiarity, slope_valxfam)
        , from = -2, to = 2.5
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )
points(jitter(chose_R_numeric, factor = 0.2) ~ jitter(RightValue, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[2], 0.5)
       , cex = 2
)
if(showGLM) curve(probs_from_pars(x, 0, 0, interc, par1, 0, 0, 0)
      , from = -2, to = 2.5
      , add = TRUE
      , col = "grey34"
      , lwd = 5
      , lty = 2)
if(showBayes) curve(probs_from_pars(x, 0, 0
                                    , precis(posterior_bees)[1,1]
                                    , precis(posterior_bees)[2,1]
                                    , precis(posterior_bees)[3,1]
                                    , precis(posterior_bees)[5,1]
                                    , precis(posterior_bees)[4,1])
                    , from = -2, to = 2.5
                    , add = TRUE
                    , lwd = 4
                    , col = colorsfam[2]
                    , lty = 1
)
# Gridline to show random choice
abline(h = 0.5, col = "grey", lty = 2, lwd = 2)
# Text labels
ose <- round(exp(par1),1) # 'odds slope', how much the odds change with a change in x
int <- round(probs_from_pars(0, 0, 0, interc, par1, 0, 0, 0), 2) # probability at x=0
ose_sd <- round(exp(par1+summary(chooseRmod2)$coefficients[2,2]) - ose, 1)
int_sd <- round(probs_from_pars(0, 0, 0, interc+summary(chooseRmod2)$coefficients[1,2], par1, 0, 0, 0) - int, 2)
pvalue <- round(summary(chooseRmod2)$coefficients[2,4], 3)
overall <- round(mean(d_graph$chose_R_numeric), 2)
# Now label with overall and per-level sample sizes, and add model result. 
samples <- length(d_graph$chose_R_numeric)
samples2 <- table(d_graph$RightValue)
if(showDetail) {
  text(x = -2, y = 1.13, labels = paste("N = ", samples, sep = ""), col = colorsfam[2], adj = 0)
  text(x = -2, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue, sep = ""), cex = 1, col = colorsfam[2], adj = 0)
  text(x = -2, y = 1.07, labels = paste("Right pref (glm): ", int, " +/- ", int_sd, sep = ""), cex = 1, col = colorsfam[2], adj = 0)
  if(showsim) text(x = -1, y = 0.8, labels = paste("Prop. ", overall), col = colorsfam[2])
}
text(x = 0.25, y =-0.1, labels = paste("n=", samples2[1]), col = colorsfam[2])
text(x = 0.75, y =-0.1, labels = paste("n=", samples2[2]), col = colorsfam[2])
text(x = 1.5, y =-0.1, labels = paste("n=", samples2[3]), col = colorsfam[2])
text(x = 2, y =-0.1, labels = paste("n=", samples2[4]), col = colorsfam[2])
mtext("2 visits right, 2 left", 3, 1)


### Panel 3: HIGH FAMILIARITY - model and graph ----------------------
d_graph <- subset(dat, dat$RightFamiliarity == "High Familiarity")
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , yaxt = 'n'
     , xlim = c(0, 2.25)
)
axis(1, at = c(0.25, 0.75, 1.5, 2)) # Define where x-axis tick marks are
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseRmod3 <- glm(chose_R_numeric ~ RightValue, family = binomial, data = d_graph)
interc <- chooseRmod3$coefficients[[1]]
par1 <- chooseRmod3$coefficients[[2]]
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseRmod2))
for(i in 1:n_uncertainty) {
  if(showBayes) curve(probs_from_pars(x, 1, 0, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i], slopes_col[i])
        , from = -2, to = 2.5
        , add = TRUE
        , lwd = 3
        , col = alpha(colorsfam[3], transparency_Bay_uncertainty)
        , lty = 1
  )
  if(showGLM) curve(probs_from_pars(x, 1, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0, 0)
                    , from = -2, to = 2.5
                    , add = TRUE
                    , col = alpha("grey34", transparency_glm_uncertainty)
                    , lwd = 3
                    , lty = 2
  )
}
if(showsim) 
  curve(probs_from_pars(x,  1, 0, intercept, slope_value, slope_familiarity, slope_valxfam, slope_color)
        , from = -2, to = 2.5
        , add = TRUE
        , col = colorassumption
        , lwd = 2
        , lty = 3
  )
points(jitter(chose_R_numeric, factor = 0.2) ~ jitter(RightValue, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorsfam[3], 0.5)
       , cex = 2
)
if(showGLM) curve(probs_from_pars(x, 1, 0, interc, par1, 0, 0, 0)
      , from = -2, to = 2.5
      , add = TRUE
      , col = "grey34"
      , lwd = 5
      , lty = 2)
if(showBayes) curve(probs_from_pars(x, 1, 0
                                    , precis(posterior_bees)[1,1]
                                    , precis(posterior_bees)[2,1]
                                    , precis(posterior_bees)[3,1]
                                    , precis(posterior_bees)[5,1]
                                    , precis(posterior_bees)[4,1])
                    , from = -2, to = 2.5
                    , add = TRUE
                    , lwd = 4
                    , col = colorsfam[3]
                    , lty = 1
)
# Gridline to show random choice
abline(h = 0.5, col = "grey", lty = 2, lwd = 2)
# Text labels
ose <- round(exp(par1),1) # 'odds slope', how much the odds change with a change in x
int <- round(probs_from_pars(0, 1, 0, interc, par1, 0, 0), 2) # probability at x=0
ose_sd <- round(exp(par1+summary(chooseRmod2)$coefficients[2,2]) - ose, 1)
int_sd <- round(probs_from_pars(0, 1, 0, interc+summary(chooseRmod2)$coefficients[1,2], par1, 0, 0) - int, 2)
pvalue <- round(summary(chooseRmod2)$coefficients[2,4], 3)
overall <- round(mean(d_graph$chose_R_numeric), 2)
# Now label with overall and per-level sample sizes, and add model result. 
samples <- length(d_graph$chose_R_numeric)
samples2 <- table(d_graph$RightValue)
if(showDetail) {
  text(x = -2, y = 1.13, labels = paste("N = ", samples, sep = ""), col = colorsfam[3], adj = 0)
  text(x = -2, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue, sep = ""), cex = 1, col = colorsfam[3], adj = 0)
  text(x = -2, y = 1.07, labels = paste("Right pref (glm): ", int, " +/- ", int_sd, sep = ""), cex = 1, col = colorsfam[3], adj = 0)
  if(showsim) text(x = -1, y = 0.8, labels = paste("Prop. ", overall), col = colorsfam[3])
}
text(x = 0.25, y =-0.1, labels = paste("n=", samples2[1]), col = colorsfam[3])
text(x = 0.75, y =-0.1, labels = paste("n=", samples2[2]), col = colorsfam[3])
text(x = 1.5, y =-0.1, labels = paste("n=", samples2[3]), col = colorsfam[3])
text(x = 2, y =-0.1, labels = paste("n=", samples2[4]), col = colorsfam[3])
mtext("3 visit right, 1 left", 3, 1)

### Joint x axis label for all panels: ---------------
mtext("Sugar Concentration Right Option", 1, 2, outer = TRUE)
if(saveFigs) {
  dev.off()
}
### end figure -------------------------

#######################################################
# MODELING EFFECT OF HORIZON TWO WAYS: on 'right' choices and on choosing 'unfamiliar' -----------------
#######################################################
# II. Modeling choice of flower on right with random and directed exploration ------------
# Data simulation for workflow check and power analysis -----------------------
# We use the parameters above, intercept, slope_value, slope_familiarity, and slope_valxfam
# as 'default', i.e. with horizon = 1. 
# If there is random exploration, then bees should be more likely to make random decisions
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
choices_simdata <- sim_choiceR(probs_from_pars(valuediffs,  famdiffs, colorprefs, intercept, slope_value, slope_familiarity, slope_valxfam))
simdata_h1 <- data.frame(choice = choices_simdata, Right_ConcDiff = valuediffs, famdiffs, ColorPref = colorprefs, Bee = bees, Horizon = 1)

# Horizon 6
choices_simdata <- sim_choiceR(probs_from_pars(valuediffs,  famdiffs, colorprefs, intercept, slope_value+rnd_expl_simpar, slope_familiarity+dir_expl_simpar, slope_valxfam))
simdata_h6 <- data.frame(choice = choices_simdata, Right_ConcDiff = valuediffs, famdiffs, ColorPref = colorprefs, Bee = bees, Horizon = 6)

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
  mutate(choseUnfamiliar = case_when(
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
  mutate(Unfamiliar_ConcDiff = 
           ifelse(RightFamiliarity == "High Familiarity", -Right_ConcDiff
                  , ifelse(RightFamiliarity == "Equal", NA
                           , Right_ConcDiff)))

# Choose data to use ---------------------------------
ifelse(showsim
       , dat <- simdata
       , dat <- MazeData
)
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
  #Right_ConcDiff:Horizon + Right_FamDiff:Horizon
  logit(prob) <- intpt + pcol*col + pval*val + pfam * fam + pvxf*val*fam + rnd*hor*val + dir*hor*fam, 
  # And the following describes the priors for the parameters
  intpt ~ dnorm(intercept_prior_mean, intercept_prior_sd),
  pval ~ dnorm(slopeval_prior_mean, slopeval_prior_sd), 
  pfam ~ dnorm(slopefam_prior_mean, slopefam_prior_sd),
  pcol ~ dnorm(slopecol_prior_mean, slopecol_prior_sd),
  pvxf ~ dnorm(slopevf_prior_mean, slopevf_prior_sd),
  rnd ~ dnorm(rnd_explor_prior_mean, rnd_explor_prior_sd),
  dir ~ dnorm(dir_explor_prior_mean, dir_explor_prior_sd)
)
# We may or may not need 'start' to help the model converge on a solution. 
# This is not a prior and should not affect the actual resulting estimates.
# However, note that the order in which parameters are mentioned here determines
# the order in the output.
start <- function(dat) {
  return(
    list(
      intpt = intercept_prior_mean
      , pcol = slopecol_prior_mean
      , pval = slopeval_prior_mean
      , pfam = slopefam_prior_mean
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
    , data = list(CL = dat$chose_R_numeric, val = dat$RightValue, fam = dat$Right_FamDiff, col = dat$ColorPref, hor = (as.numeric(dat$Horizon)-1))
    , start = start(dat)
  )
}


## Bayesian fitting -----------------------
posterior_ExplR <- BayesExploreR(dat)
# GLM choice right flower' with exploration ------------------------------------
expl_R <- glm(chose_R_numeric ~ ColorPref + RightValue*Right_FamDiff + RightValue:Horizon + Right_FamDiff:Horizon, family = binomial, data = dat)
## Table II. Random exploration GLM ---------------------
tab_model(expl_R
          , show.re.var = TRUE
          , pred.labels = c("Right preference (Intercept)"
                            , "Color Bias"
                            , "Reward (concentration right)"
                            , "Familiarity (right vs left)"
                            , "Learning (interaction Rew x Fam)"
                            , "Random exploration (interaction Rew x Horizon)"
                            , "Directed exploration (interaction Fam x Horizon)"
          )
          , dv.labels = "Effect on probability of choosing right flower"
)
#######################################################
## Effect sizes figure -------------------------
if(saveFigs) {
  setwd(figs_path)
  pdf(file = "Fig3 Maze choose R all effect sizes.pdf", width = 10, height = 6)
}
lab <- c("Directed exploration", "Random exploration", "Learning", "Familiarity", "Reward", "Color Bias", "Right preference")
model_comp(posterior_ExplR, expl_R, lab, 7)
if(saveFigs) dev.off()
#######################################################
# III. Modeling choice of unfamiliar option directly ------------
## GLM for directed exploration -----------------------
expl_UF <- glm(choseUnfamiliar ~ Unfamiliar_ConcDiff * Horizon, family = binomial, data = dat)
## Table III. Directed exploration GLM --------------
tab_model(expl_UF
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
  logit(prob) <- intpt + pval*val + phor*hor + direx*val*hor, 
  # And the following describes the priors for the parameters
  intpt ~ dnorm(intercept_prior_mean, intercept_prior_sd), 
  pval ~ dnorm(slopeval_prior_mean, slopeval_prior_sd), 
  phor ~ dnorm(slopehor_prior_mean, slopehor_prior_sd),
  direx ~ dnorm(slopevh_prior_mean, slopevh_prior_sd)
)
start <- function(dat) {
  return(
    list(
      intpt = intercept_prior_mean
      , pval = slopeval_prior_mean
      , phor = slopehor_prior_mean
      , direx = slopevh_prior_mean
    )
  )
}

### Bayesian estimation function -----------------------

# CL stands for choice list, val is the value or reward difference between the options,
# fam is the familiarity difference between the options. 
BayesExploreUnf <- function(dat) {
  quap(
    list_of_assumptions
    , data = list(CL = dat$choseUnfamiliar, val = dat$Unfamiliar_ConcDiff, hor = (as.numeric(dat$Horizon)-1))
    , start = start(dat)
  )
}
### Running the Bayesian analysis ----------------------
posterior_ExplUF <- BayesExploreUnf(subset(dat, !is.na(dat$choseUnfamiliar)))
#######################################################
## Effect sizes figure choosing unfamiliar flower -------------------------
if(saveFigs) {
  setwd(figs_path)
  pdf(file = "FigS5 Maze effect size chose unfamiliar.pdf", width = 10, height = 6)
}
lab <- c("Interaction Val x Hor", "Horizon", "Value", "Intercept")
model_comp(posterior_ExplUF, expl_UF, lab, 4)
if(saveFigs) dev.off()
#######################################################
## Results Fig: choice of unfamiliar flower w GLM & Bayes II -----------
# 2-panel Base R Plot to illustrate binomial fit and uncertainty
if(saveFigs) {
  setwd(figs_path)
  pdf(file = "Fig5a Maze chose unfamiliar.pdf", width = 10, height = 6)
}
# Layout
par(mfrow=c(1,2))
par(oma = c(4,4,1,0), mar = c(1,1,2,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
# Each panel doing its own modeling separately for glm
# Extracting values from Bayesian model for graphing
samples_of_post <- extract.samples(posterior_ExplUF, n=n_uncertainty)
intercepts_post <- samples_of_post$intpt
slopes_val <- samples_of_post$pval
slopes_hor <- samples_of_post$phor
slopes_valhor <- samples_of_post$direx

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
mtext("Horizon 1", 3, 1)
# Y axis label for all panels
mtext("Chose Low Familiarity Option", 2, 3)
# GLM - model and extracting estimates
H1_Expl_mod <- glm(choseUnfamiliar ~ Unfamiliar_ConcDiff, family = binomial, data = d_graph)
interc <- H1_Expl_mod$coefficients[[1]]
par1 <- H1_Expl_mod$coefficients[[2]]
ose <- round(exp(par1),1)
ose_sd <- round(exp(par1+summary(H1_Expl_mod)$coefficients[2,2]) - ose, 1)
pvalue <- round(summary(H1_Expl_mod)$coefficients[2,4], 3)
int <- round(probs_from_pars(0, 0, 0, interc, par1, 0, 0), 2)
int_sd <- round(probs_from_pars(0, 0, 0, interc+summary(H1_Expl_mod)$coefficients[1,2], par1, 0, 0) - int, 2)
# Extracting uncertainty
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(H1_Expl_mod))
for(i in 1:n_uncertainty) {
  if(showGLM) curve(probs_from_pars(x, 0, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0), from = -2, to = 2.5
        , add = TRUE
        , col = alpha("grey34", transparency_glm_uncertainty)
        , lwd = 3
        , lty = 2
  )
  if(showBayes) curve(probs_from_pars(x, 0, 0, intercepts_post[i], slopes_val[i], slopes_hor[i], slopes_valhor[i])
                      , from = -2, to = 2.5
                      , add = TRUE
                      , lwd = 3
                      , col = alpha(colorshor[1], transparency_Bay_uncertainty)
                      , lty = 1
  )
}
# Original data points with slight jitter
points(jitter(choseUnfamiliar, factor = 0.2) ~ jitter(Unfamiliar_ConcDiff, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorshor[1], 0.5)
       , cex = 1.5
)
# Plotting the estimated fit from the glm:
# (We're doing this last instead of earlier so it comes out on top for better
# visibility.)
if(showGLM) curve(probs_from_pars(x, 0, 0, interc, par1, 0, 0), from = -2, to = 2.5
      , add = TRUE
      , col = "grey34"
      , lwd = 5
      , lty = 2)
# Plotting Bayesian fit line
if(showBayes) curve(probs_from_pars(x, 0, 0, precis(posterior_ExplUF)[1,1]
                                    , precis(posterior_ExplUF)[2,1]
                                    , precis(posterior_ExplUF)[3,1]
                                    , precis(posterior_ExplUF)[4,1])
                    , from = -2, to = 2.5
                    , add = TRUE
                    , lwd = 4
                    , col = colorshor[1]
                    , lty = 1
)
# Gridlines
abline(h = 0.5, col = "grey", lty = 2, lwd = 2)
abline(v = 0, col = "grey", lty = 2, lwd = 2)
# Text labels for panel
overall <- round(mean(d_graph$choseUnfamiliar), 2)
samples <- length(d_graph$choseUnfamiliar)
if(showDetail) {
  text(x = -2, y = 1.13, labels = paste("N = ", samples), col = colorshor[1], cex = 0.7, adj = 0)
  text(x = -2, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue), cex = 0.7, col = colorshor[1], adj = 0)
  text(x = -2, y = 1.07, labels = paste("Right pref (glm): ", int, " +/- ", int_sd), cex = 0.7, col = colorshor[1], adj = 0)
}
samples2 <- table(d_graph$Unfamiliar_ConcDiff)
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
mtext("Maze experiment", 3, 0, outer = TRUE)
mtext("Horizon 6", 3, 1)
# GLM - model and extracting estimates
H6_Expl_mod <- glm(choseUnfamiliar ~ Unfamiliar_ConcDiff, family = binomial, data = d_graph)
interc <- H6_Expl_mod$coefficients[[1]]
par1 <- H6_Expl_mod$coefficients[[2]]
ose <- round(exp(par1),1)
ose_sd <- round(exp(par1+summary(H6_Expl_mod)$coefficients[2,2]) - ose, 1)
pvalue <- round(summary(H6_Expl_mod)$coefficients[2,4], 3)
int <- round(probs_from_pars(0, 0, 0, interc, par1, 0, 0), 2)
int_sd <- round(probs_from_pars(0, 0, 0, interc+summary(H6_Expl_mod)$coefficients[1,2], par1, 0, 0) - int, 2)
# Bayesian model and extracting estimates
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(H6_Expl_mod))
for(i in 1:n_uncertainty) {
  if(showGLM) curve(probs_from_pars(x, -1, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0), from = -2, to = 2.5
        , add = TRUE
        , col = alpha("grey34", transparency_glm_uncertainty)
        , lwd = 3
        , lty = 2
  )
  if(showBayes) curve(probs_from_pars(x, 1, 0, intercepts_post[i], slopes_val[i], slopes_hor[i], slopes_valhor[i])
                      , from = -2, to = 2.5
                      , add = TRUE
                      , lwd = 3
                      , col = alpha(colorshor[2], transparency_Bay_uncertainty)
                      , lty = 1
  )
}
# Original data points with slight jitter
points(jitter(choseUnfamiliar, factor = 0.2) ~ jitter(Unfamiliar_ConcDiff, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorshor[2], 0.5)
       , cex = 1.5
)
if(showGLM) curve(probs_from_pars(x, 0, 0, interc, par1, 0, 0), from = -2, to = 2.5
      , add = TRUE
      , col = "grey34"
      , lwd = 5
      , lty = 2)
# Plotting Bayesian fit line
if(showBayes) curve(probs_from_pars(x, 1, 0, precis(posterior_ExplUF)[1,1]
                                    , precis(posterior_ExplUF)[2,1]
                                    , precis(posterior_ExplUF)[3,1]
                                    , precis(posterior_ExplUF)[4,1])
                    , from = -2, to = 2.5
                    , add = TRUE
                    , lwd = 4
                    , col = colorshor[2]
                    , lty = 1
)
# Gridlines
abline(h = 0.5, col = "grey", lty = 2, lwd = 2)
abline(v = 0, col = "grey", lty = 2, lwd = 2)
overall <- round(mean(d_graph$choseUnfamiliar), 2)
samples <- length(d_graph$choseUnfamiliar)
if(showDetail) {
  text(x = -2, y = 1.13, labels = paste("N = ", samples), col = colorshor[2], cex = 0.7, adj = 0)
  text(x = -2, y = 1.1, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue), cex = 0.7, col = colorshor[2], adj = 0)
  text(x = -2, y = 1.07, labels = paste("Right pref (glm): ", int, " +/- ", int_sd), cex = 0.7, col = colorshor[2], adj = 0)
}
samples2 <- table(d_graph$Unfamiliar_ConcDiff)
text(x = -1.75, y =-0.1, labels = paste("n=", samples2[1]), col = colorshor[2])
text(x = -0.75, y =-0.1, labels = paste("n=", samples2[2]), col = colorshor[2])
text(x = 0.75, y =-0.1, labels = paste("n=", samples2[3]), col = colorshor[2])
text(x = 1.75, y =-0.1, labels = paste("n=", samples2[4]), col = colorshor[2])
### Axis label -------------
mtext("Concentration Difference (unfamiliar - familiar)", 1, 2, outer = TRUE)
if(saveFigs) {dev.off()}
### end fig --------------------------------------

