# Shannon McWaters, Jack-Morgan Mizell, Ren Calabro, Kiara Alexis Casas, Robert C Wilson, Anna Dornhaus
# (c) 2020-2026
# Directed Exploration in Bumble bees
# Data analysis & graphs 

# Packages used ---------------------
library(tidyverse) # Data handling/converting
library(lme4) # Linear models
library(ggeffects)
library(patchwork) # For combining graphs
library(sjPlot) # For nice table output
library(viridis)
library(MASS) # For simulating from covariance matrix
library(rethinking)
library(pwrss)

# Decide whether to work with simulated or real data ------------------
# What do we want:
showsim <- TRUE

# Importing data for experiment 1 directly from github -----------------------
MazeData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/refs/heads/main/Maze%20Data%20Raw")

# Colors -----------------------------
colorsfam <- viridis(3, begin = 0.2, end = 0.8)
colorshor <- inferno(2, begin = 0.2, end = 0.8)

colorsbees <- inferno(length(unique(MazeData$Bee)), begin = 0.2, alpha = 0.5)
colorsparameters <- inferno(4, alpha = 0.8, begin = 0.2, end = 0.8)
colorassumption <- "darkred"
colorprior <- "slateblue"
# This is how many lines we plot when illustrating uncertainty around fits:
n_uncertainty <- 500

# Data formatting ---------------------------------

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

# MODELING PROBABILITY OF CHOOSING FLOWER ON RIGHT AS REWARD x FAMILIARITY ------
# Data simulation for workflow check and power analysis -----------------------

# So a key first step is to define a generative model that we think underlies the process.
# This model should have parameters (which we will estimate later using the data)
# that reflect the question(s) we want to answer.

# Let's say that bees choose a probability for selecting the left or right flower
# based on their estimated concentration difference as well as the difference in familiarity.
probs_from_pars <- function(valuediffs_right, famdiffs_right, intercept, slope_value, slope_familiarity, slope_valxfam) {
  samplesize <- length(valuediffs_right)
  linear_predictor <- intercept + slope_value * valuediffs_right + slope_familiarity * famdiffs_right + slope_valxfam * valuediffs_right * famdiffs_right
  probability_right <- 1/(1+exp(-linear_predictor))
  return(probability_right)
}

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

N_sim <- 1000
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
# end sim -------------------

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

### Check these priors by plotting some lines from them. ---------------
# To do that we have to pick actual values from the probability distributions that
# are the priors. 
n_plot <- 100
intercepts <- rnorm(n_plot, intercept_prior_mean, intercept_prior_SD)
slopevals <- rnorm(n_plot, slopeval_prior_mean, slopeval_prior_sd)
slopefams <- rnorm(n_plot, slopefam_prior_mean, slopefam_prior_sd)
slopevfs <- rnorm(n_plot, slopevf_prior_mean, slopevf_prior_sd)
# Each line representing a sample from the prior distribution has 4 values
# picked from these distributions, so n_plot needs to be decently large to even 
# approach filling the possible parameter space.

# We'll repeat the plot from before but just with the model-fit line and the priors:
#### Rest of the plot code part 1 ----------------
par(mfrow=c(1,3))
par(oma = c(4,4,0,0), mar = c(1,1,1,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
d_graph <- subset(dat, dat$RightFamiliarity == "Low Familiarity")
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4)
     , xlim = c(-2, 2)
     , xlab = "Concentration Difference"
)
mtext("Chose Right", 2, 3)
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseRmod <- glm(chose_R_numeric ~ RightConcentrationDiff, family = binomial, data = d_graph)
interc <- chooseRmod$coefficients[[1]]
par1 <- chooseRmod$coefficients[[2]]
#### Prior lines part 1 --------------------------
for(i in 1:n_plot) {
  curve(probs_from_pars(x, -1, intercepts[i], slopevals[i], slopefams[i], slopevfs[i]), from = -2, to = 2
        , add = TRUE
        , col = colorprior
        , lwd = 1
        , lty = 3
  )
}
#### Rest of plot part 2 ------------------
curve(probs_from_pars(x, 0, interc, par1, 0, 0)
      , from = -2, to = 2
      , add = TRUE
      , col = colorsfam[1]
      , lwd = 2)
d_graph <- subset(dat, dat$RightFamiliarity == "Equal")
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4)
     , yaxt = 'n'
     , xlim = c(-2, 2)
     , xlab = ""
)
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseRmod <- glm(chose_R_numeric ~ RightConcentrationDiff, family = binomial, data = d_graph)
interc <- chooseRmod$coefficients[[1]]
par1 <- chooseRmod$coefficients[[2]]
#### Prior lines part 2 --------------------------
for(i in 1:n_plot) {
  curve(probs_from_pars(x, 0, intercepts[i], slopevals[i], slopefams[i], slopevfs[i]), from = -2, to = 2
        , add = TRUE
        , col = colorprior
        , lwd = 1
        , lty = 3
  )
}
#### Rest of plot part 3 ------------------
curve(probs_from_pars(x, 0, interc, par1, 0, 0)
      , from = -2, to = 2
      , add = TRUE
      , col = colorsfam[2]
      , lwd = 3)
d_graph <- subset(dat, dat$RightFamiliarity == "High Familiarity")
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4)
     , yaxt = 'n'
     , xlim = c(-2, 2)
     , xlab = ""
)
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
chooseRmod <- glm(chose_R_numeric ~ RightConcentrationDiff, family = binomial, data = d_graph)
interc <- chooseRmod$coefficients[[1]]
par1 <- chooseRmod$coefficients[[2]]
#### Prior lines part 3 ----------------------
for(i in 1:n_plot) {
  curve(probs_from_pars(x, 1, intercepts[i], slopevals[i], slopefams[i], slopevfs[i]), from = -2, to = 2
        , add = TRUE
        , col = colorprior
        , lwd = 1
        , lty = 3
  )
}
#### Rest of plot part 4 ------------------------
curve(probs_from_pars(x, 0, interc, par1, 0, 0)
      , from = -2, to = 2
      , add = TRUE
      , col = colorsfam[2]
      , lwd = 3)
mtext("Concentration Difference", 1, 2, outer = TRUE)
### end plotting priors ----------------------
### Excellent! If our prior is symmetric around 0 for the 
### familiarity effect, the outer two panels should look pretty similar. 
### The middle panel only shows variation in the first slope and the intercept, 
### instead of from 4 parameters, and thus the lines should be tighter.

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

## Now run it ---------------------------
# Bayesian model full
posterior_bees <- BeeChoiceModel(dat)
# Done! The parameter estimates here are for the entire model/dataset, so 
# not directly comparable to the ones from the separate glms for each
# familiarity level. We'll do an overall glm now. 

# Results from both models, and table ------------
# GLM full
chooseRmod_full <- glm(chose_R_numeric ~ RightConcentrationDiff * famdiffs, family = binomial, data = dat)
# Extracting some values for text summary below. 
glm_interc_full <- round(chooseRmod_full$coefficients[[1]], 2)
glm_par1_full <- round(chooseRmod_full$coefficients[[2]], 2)
glm_par2_full <- round(chooseRmod_full$coefficients[[3]], 2)
glm_par1x2_full <- round(chooseRmod_full$coefficients[[4]], 2)
pvalue_c_full <- round(summary(chooseRmod_full)$coefficients[2,4], 3)
pvalue_f_full <- round(summary(chooseRmod_full)$coefficients[3,4], 3)
pvalue_cf_full <- round(summary(chooseRmod_full)$coefficients[4,4], 3)
# Extrcting values from Bayesian model for graphing
bay_interc_full <- round(precis(posterior_bees)[1,1], 2)
bay_par1_full <- round(precis(posterior_bees)[2,1], 2)
bay_par2_full <- round(precis(posterior_bees)[3,1], 2)
bay_par3_full <- round(precis(posterior_bees)[4,1], 2)
samples_of_post <- extract.samples(posterior_bees, n=n_uncertainty)
intercepts_post <- samples_of_post$a
slopes_c <- samples_of_post$b
slopes_f <- samples_of_post$c
slopes_cf <- samples_of_post$d

## Output table GLM for paper -------------------------
tab_model(chooseRmod_full
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward Difference (concentration right - left)"
                            , "Familiarity Difference (right - left)"
                            , "Interaction Rew Diff x Fam Diff"
          )
          , dv.labels = "Effect on probability of choosing right flower"
)
## Text outputs --------------------------
# Bayesian model full output
precis(posterior_bees)
# GLM full output
summary(chooseRmod_full)
# Text summary
paste("Overall N =", length(dat$chose_R_numeric)
      , ", GLM fit: logit=", glm_interc_full, "+conc diff*", glm_par1_full
      , "+fam diff*", glm_par2_full, "+conc diff*fam diff*", glm_par1x2_full
      , ", implying an odds slope (increase in odds of choosing right per unit of increase in concentration difference) of "
      , round(exp(glm_par1_full),1), " (p=", pvalue_c_full, "), and "
      , round(exp(glm_par2_full),1), " with each step of increased familiarity (p=", pvalue_f_full, ")."
      , sep="")


#######################################################
# Illustrating results ------------------------
## Results Fig 1 - cleaner version in ggplot ----------------
# ggplot in this case does its own modeling.
ggplot(dat, aes(x = RightConcentrationDiff, y = chose_R_numeric)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4, size = 2.5) +  # increased size from 1 to 2.5
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_wrap(~ RightFamiliarity) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.4) +
  labs(
    x = "Relative Value of Right Flower",
    y = "Probability of Going Right"
  ) +
  theme_minimal(base_size = 14)
## end clean version fig ----------------

## Results Fig 1 - detailed version in base R with Bayesian and and GLM results -----------
# 3-panel Base R Plot to illustrate binomial fit and uncertainty
# Layout
par(mfrow=c(1,3))
par(oma = c(4,4,0,0), mar = c(1,1,1,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
# Each panel doing its own modeling separately
## Panel 1: LOW FAMILIARITY - model and graph ----------------
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
  curve(probs_from_pars(x, -1, fit_covar_matrix[i,1], fit_covar_matrix[i,2], 0, 0)
        , from = -2, to = 2
        , add = TRUE
        , col = alpha(colorsfam[1], 0.1)
        , lwd = 3
  )
  curve(probs_from_pars(x, -1, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
        , from = -2, to = 2
        , add = TRUE
        , lwd = 3
        , col = alpha("grey34", 0.15)
        , lty = 2
  )
}
## 'True' relationship if using simdata
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
# Compare to par values we put into the simulation, converted for familiarity = -1:
os_input <- round(exp(slope_value - slope_valxfam),1) # 'odds slope'
int_input <- round(probs_from_pars(0, -1, intercept, slope_value, slope_familiarity, slope_valxfam), 2)
if(showsim) text(x = 1, y = 1.1, labels = paste("input: ", os_input), cex = 1, col = colorassumption)
if(showsim) text(x = 1, y = 1.07, labels = paste("input: ", int_input), cex = 1, col = colorassumption)
# Just for better understanding and data checking, we'll also label with the
# sample size for the entire panel and the overall proportion of right choices.
overall <- round(mean(d_graph$chose_R_numeric), 2)
if(showsim) text(x = -1, y = 0.8, labels = paste("Prop. ", overall), col = colorsfam[1])
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




## Panel 2: EQUAL FAMILIARITY - model and graph --------------------
# We follow all the same steps as for Panel 1 (hence no annotations)
d_graph <- subset(dat, dat$RightFamiliarity == "Equal")
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
     , xlim = c(-2, 2)
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
        , col = alpha(colorsfam[2], 0.1)
        , lwd = 3
  )
  curve(probs_from_pars(x, 0, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
        , from = -2, to = 2
        , add = TRUE
        , lwd = 3
        , col = alpha("grey34", 0.15)
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
curve(probs_from_pars(x, 0, precis(posterior_bees)[1,1]
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
# Compare to par values we put into the simulation, converted for familiarity = -1:
os_input <- round(exp(slope_value - slope_valxfam),1) # 'odds slope'
int_input <- round(probs_from_pars(0, 0, intercept, slope_value, slope_familiarity, slope_valxfam), 2)
if(showsim) text(x = 1, y = 1.1, labels = paste("input: ", os_input), cex = 1, col = colorassumption)
if(showsim) text(x = 1, y = 1.07, labels = paste("input: ", int_input), cex = 1, col = colorassumption)
# Just for better understanding and data checking, we'll also label with the
# sample size for the entire panel and the overall proportion of right choices.
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


## Panel 3: HIGH FAMILIARITY - model and graph ----------------------
d_graph <- subset(dat, dat$RightFamiliarity == "High Familiarity")
plot(NULL
     , ylab = ""
     , ylim = c(-0.1, 1.1)
     , yaxp = c(0, 1, 4) # Define where y-axis tick marks are
     , xaxt = 'n'
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
        , col = alpha(colorsfam[3], 0.1)
        , lwd = 3
  )
  curve(probs_from_pars(x, 1, intercepts_post[i], slopes_c[i], slopes_f[i], slopes_cf[i])
        , from = -2, to = 2
        , add = TRUE
        , lwd = 3
        , col = alpha("grey34", 0.15)
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
curve(probs_from_pars(x, 1, precis(posterior_bees)[1,1]
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
# Compare to par values we put into the simulation, converted for familiarity = -1:
os_input <- round(exp(slope_value - slope_valxfam),1) # 'odds slope'
int_input <- round(probs_from_pars(0, 1, intercept, slope_value, slope_familiarity, slope_valxfam), 2)
if(showsim) text(x = 1, y = 1.1, labels = paste("input: ", os_input), cex = 1, col = colorassumption)
if(showsim) text(x = 1, y = 1.07, labels = paste("input: ", int_input), cex = 1, col = colorassumption)
# Just for better understanding and data checking, we'll also label with the
# sample size for the entire panel and the overall proportion of right choices.
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

# Joint x axis label for all panels: ---------------
mtext("Concentration Difference", 1, 2, outer = TRUE)
### end figure -------------------------

#######################################################
# 'Power analysis' with simulation ------------------------------------
# Simulates several datasets with different sample sizes, repeat the same analysis,
# save results in a table. 
# Define sample sizes to use
N_levels <- c(20, 50, 70, 80, 95, 100, 150, 200, 500, 1000, 5000)
N_real <- nrow(MazeData)
Ns_real <- c()
Ns_sim <- c()
for(i in 1:length(N_levels)) {
  Ns_sim <- c(Ns_sim, rep(N_levels[i], 100))
  Ns_real <- c(Ns_real, rep(N_levels[i], 100))
}
Ns_real <- Ns_real[Ns_real <= N_real]
ifelse(showsim
       , Ns <- Ns_sim
       , {
         Ns <- Ns_real
         N_levels <- N_levels[N_levels <= N_real]
       }
)
# Output parameter table prep
parnames <- c("intercept", "slope_value", "slope_familiarity", "slope_valxfam")
inputpars <- c(0, slope_value, slope_familiarity, slope_valxfam)
outcomes <- data.frame(parnames, inputpars)
colnames(outcomes) <- c("Parameter", "Input value")
outcomes2 <- data.frame(parnames, inputpars)
colnames(outcomes2) <- c("Parameter", "Input value")
# Power outcome prep
pvals <- c()
pvals1_2 <- c()
pvals2_2 <- c()
pvals3_2 <- c()

# Repeat for all sample sizes we want
for(i in 1:length(Ns)) {
  
  # Make simdata just like above
  if(showsim) {
    valuediffs <- runif(Ns[i], min = -2, max = 2)
    famdiffs <- c(0, sample(c(-1, 0, 1), (Ns[i]-1), replace = TRUE))
    choices_dat <- sim_choice(probs_from_pars(valuediffs,  famdiffs, intercept, slope_value, slope_familiarity, slope_valxfam))
    choices_numeric <- as.numeric(ifelse(choices_dat == "right", "1", "0"))
  }
  # If not using simdata, generate permutations of real data (bootstrapping)
  if(!showsim) {
    colorsparameters <- alpha(colorsparameters, 0.5)
    row_subset <- sample(seq(1, length(dat$chose_R_numeric), by = 1), Ns[i], replace = FALSE)
    dat_subset <- dat %>% slice(row_subset)
    choices_numeric <- dat_subset$chose_R_numeric
    valuediffs <- dat_subset$RightConcentrationDiff
    famdiffs <- dat_subset$famdiffs
  }
  # Assemble data table for the current sample size
  dataset <- data.frame(chose_R_numeric = choices_numeric, RightConcentrationDiff = valuediffs, famdiffs)
  dataset$RightFamiliarity <- factor(
    ifelse(dataset$famdiffs == 0, "Equal"
           , ifelse(dataset$famdiffs > 0, "High Familiarity"
                    , "Low Familiarity")
    ),
    levels = c("Low Familiarity", "Equal", "High Familiarity")
  )
  
  #### Here are the actual models: one with and one without interaction -----------
  chooseRmodpwr <- glm(chose_R_numeric ~ RightConcentrationDiff, family = binomial
                    , data = dataset) #subset(dataset, dataset$RightFamiliarity == "Equal"))
  interc <- round(chooseRmodpwr$coefficients[[1]], 2)
  par1 <- round(chooseRmodpwr$coefficients[[2]], 2)
  pval <- summary(chooseRmodpwr)$coefficients[2,4]
  
  chooseRmodpwr2 <- glm(chose_R_numeric ~ RightConcentrationDiff * famdiffs, family = binomial
                     , data = dataset)
  interc_2 <- round(chooseRmodpwr2$coefficients[[1]], 2)
  par1_2 <- round(chooseRmodpwr2$coefficients[[2]], 2)
  par2_2 <- round(chooseRmodpwr2$coefficients[[3]], 2)
  par3_2 <- round(chooseRmodpwr2$coefficients[[4]], 2)
  pval1_2 <- summary(chooseRmodpwr2)$coefficients[2,4]
  pval2_2 <- summary(chooseRmodpwr2)$coefficients[3,4]
  pval3_2 <- summary(chooseRmodpwr2)$coefficients[4,4]
  # Save output in table
  newoutcome <- data.frame(c(interc, par1, NA, NA))
  colnames(newoutcome) <- paste("N=", Ns[i], sep="")
  outcomes <- cbind(outcomes, newoutcome)
  pvals <- c(pvals, pval)
  newoutcome <- data.frame(c(interc_2, par1_2, par2_2, par3_2))
  colnames(newoutcome) <- paste("N=", Ns[i], sep="")
  outcomes2 <- cbind(outcomes2, newoutcome)
  pvals1_2 <- c(pvals1_2, pval1_2)
  pvals2_2 <- c(pvals2_2, pval2_2)
  pvals3_2 <- c(pvals3_2, pval3_2)
}
# Transpose table for easier graphing
outcomes_f_graph <- as.data.frame(t(outcomes[,-1]))
colnames(outcomes_f_graph) <- outcomes[,1]
outcomes2_f_graph <- as.data.frame(t(outcomes2[,-1]))
colnames(outcomes2_f_graph) <- outcomes2[,1]
# Summarize p values by sample size and calculate power
# First for simple model with just one factor
sign_list <- factor(ifelse(pvals<0.05, "sig", "n.s.")
                    , levels = c("n.s.", "sig"))
# Then for multifactorial glm
sign_list1_2 <- factor(ifelse(pvals1_2<0.05, "sig", "n.s.")
                       , levels = c("n.s.", "sig"))
sign_list2_2 <- factor(ifelse(pvals2_2<0.05, "sig", "n.s.")
                       , levels = c("n.s.", "sig"))
sign_list3_2 <- factor(ifelse(pvals3_2<0.05, "sig", "n.s.")
                       , levels = c("n.s.", "sig"))
# All together in data frame
sign_df <- data.frame(Ns
                      , pvals, sign_list
                      , pvals1_2, sign_list1_2
                      , pvals2_2, sign_list2_2
                      , pvals3_2, sign_list3_2
                      )
# Count significant for one-factor model
sign_table <- table(sign_df$Ns, sign_df$sign_list)
powers <- data.frame(N_levels, sign_table[,2]/100, ((sign_table[,2]/100)*3)-3)
colnames(powers) <- c("Sample size", "Power", "Power blown up")
# Count significant for multifactorial model
sign_table1 <- table(sign_df$Ns, sign_df$sign_list1_2)
sign_table2 <- table(sign_df$Ns, sign_df$sign_list2_2)
sign_table3 <- table(sign_df$Ns, sign_df$sign_list3_2)
powers2 <- data.frame(N_levels
                      , sign_table1[,2]/100
                      , ((sign_table1[,2]/100)*3)-3
                      , sign_table2[,2]/100
                      , ((sign_table2[,2]/100)*3)-3
                      , sign_table3[,2]/100
                      , ((sign_table3[,2]/100)*3)-3
)
colnames(powers2) <- c("Sample_size"
                       , "Power_Par1", "Power_Par1_stretched"
                       , "Power_Par2", "Power_Par2_stretched"
                       , "Power_Interaction", "Power_Interaction_stretched"
                       )

### Graph ----------------
par(mfrow=c(1,1))
par(oma = c(0,0,0,0), mar = c(4,4,1,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
ymax <- 3
plot(intercept ~ jitter(Ns, factor = 1)
     , data = outcomes_f_graph[-1,]
     , pch = 1
     , log = "x"
     , cex = 2
     , ylim = c(-ymax, ymax)
     , xlab = "Sample size"
     , ylab = "Fitted parameter value"
     , col = colorsparameters[1]
)
points(slope_value ~ jitter(Ns, factor = 1)
       , data = outcomes_f_graph[-1,]
       , pch = 1
       , col = colorsparameters[2]
       , cex = 2
)
points(intercept ~ jitter(Ns, factor = 1)
       , data = outcomes2_f_graph[-1,]
       , pch = 19
       , col = alpha(colorsparameters[1], 0.1)
)
points(slope_value ~ jitter(Ns, factor = 1)
       , data = outcomes2_f_graph[-1,]
       , pch = 19
       , col = alpha(colorsparameters[2], 0.1)
)
points(slope_familiarity ~ jitter(Ns, factor = 1)
       , data = outcomes2_f_graph[-1,]
       , pch = 19
       , col = alpha(colorsparameters[3], 0.1)
)
points(slope_valxfam ~ jitter(Ns, factor = 1)
       , data = outcomes2_f_graph[-1,]
       , pch = 19
       , col = alpha(colorsparameters[4], 0.1)
)
lines(`Power blown up` ~ `Sample size`
                  , data = powers
                  , lty = 1
                  , lwd = 3
                  , col = "grey34"
                  )
abline(h=(0.8*3)-3, lty = 1, col = "black", lwd = 1)
lines(Power_Par1_stretched ~ Sample_size
      , data = powers2
      , lty = 2
      , lwd = 3
      , col = "grey34"
)
lines(Power_Par2_stretched ~ Sample_size
      , data = powers2
      , lty = 3
      , lwd = 3
      , col = "grey50"
)
lines(Power_Interaction_stretched ~ Sample_size
      , data = powers2
      , lty = 4
      , lwd = 3
      , col = "grey80"
)
abline(h=0, lty = 2, col = "grey")
abline(h=median(outcomes2_f_graph$intercept[-1]), lty = 1, col = colorsparameters[1])
abline(h=median(outcomes2_f_graph$slope_value[-1]), lty = 1, col = colorsparameters[2])
abline(h=median(outcomes2_f_graph$slope_familiarity[-1]), lty = 1, col = colorsparameters[3])
abline(h=median(outcomes2_f_graph$slope_valxfam[-1]), lty = 1, col = colorsparameters[4])
if(showsim) abline(h=0, lwd = 2, lty = 2, col = colorsparameters[1])
if(showsim) abline(h=slope_value, lwd = 2, lty = 2, col = colorsparameters[2])
if(showsim) abline(h=slope_familiarity, lwd = 2, lty = 2, col = colorsparameters[3])
if(showsim) abline(h=slope_valxfam, lwd = 2, lty = 2, col = colorsparameters[4])
legend("topright"
       , title = "Parameters"
       , c("Intercept", "Value", "Familiarity", "Val x Fam")
       , col = colorsparameters
       , pch = 19
)
legend(35, 3.29
       , title = "  Power"
       , title.adj = 0
       , c(
         paste("One Factor Model, max=", max(powers$Power))
         , paste("Value, max=", max(powers2$Power_Par1))
         , paste("Familiarity, max=", max(powers2$Power_Par2))
         , paste("Val x Fam, max=", max(powers2$Power_Interaction))
         )
       , col = c("grey34", "grey34", "grey50", "grey80")
       , lty = c(1,2,3,4)
       , lwd = 3
)
## This seems to show several things. 
## First, the fit for the two parameters does not differ between models, at least
## for high sample sizes. This is as it should be, as the 'left out factors' are
## just two in many unmodeled ones that are operating at any time. 
## Second, the grey lines indicating power show that power of course really
## depends on effect size; if you use the simulated values, you can show this by 
## trying different assumed effect sizes. 
## IMPORTANT: the calculated 'power' is only that if there are in fact effects of
## both parameters and the interaction. Also note the power lines are plotted so 
## that the y-axis value of 0 is 100% significant, and -3 is 0% for visibility. 
## end resampling graph -------------------------------
# Simplified analytical power analysis options: --------------
power.exact.oneprop(prob = 0.6, # probability of success under alternative
                    null.prob = 0.5, # probability of success under null
                    power = 0.80,
                    alpha = 0.05,
                    alternative = "two.sided")
## !!
# Or:
power.z.logistic(beta1 = glm_par2_full,
                 base.prob = 0.5,
                 r.squared.predictor = 0.20,
                 power = 0.80,
                 alpha = 0.05,
                 alternative = "two.sided",
                 distribution = "normal")
# end power --------------------------

#######################################################
# Degree of exploration: choice of unfamiliar flower, not right flower -------
#HighInfoChoice is reponse variable
#HighInfoConcentrationDiff is reward
#Horizon is really factor of interest

## Generative model ----------------------------------
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
N_sim <- 5000
valuediffs <- runif(N_sim, min = -2, max = 2)
famdiffs <- sample(c(-1, 0, 1), N_sim, replace = TRUE)
bees <- sample(c("onebee", "twobee"), N_sim, replace = TRUE)
choices_simdata <- sim_choice(probs_from_pars(valuediffs,  famdiffs, intercept, slope_value, slope_familiarity, slope_valxfam))
simdata_h1 <- data.frame(choice = choices_simdata, RightConcentrationDiff = valuediffs, famdiffs, Bee = bees, Horizon = 0)

# Horizon 6
valuediffs <- runif(N_sim, min = -2, max = 2)
famdiffs <- sample(c(-1, 0, 1), N_sim, replace = TRUE)
bees <- sample(c("onebee", "twobee"), N_sim, replace = TRUE)
choices_simdata <- sim_choice(probs_from_pars(valuediffs,  famdiffs, intercept, slope_value+rnd_expl_simpar, slope_familiarity+dir_expl_simpar, slope_valxfam))
simdata_h6 <- data.frame(choice = choices_simdata, RightConcentrationDiff = valuediffs, famdiffs, Bee = bees, Horizon = 1)

# Now joining the two datasets together
simdata <- rbind(simdata_h1, simdata_h6)
# Some data formatting to match empirical data
simdata$chose_R_numeric <- as.numeric(ifelse(simdata$choice == "right", "1", "0"))
simdata$RightFamiliarity <- factor(
  ifelse(simdata$famdiffs == 0, "Equal"
         , ifelse(simdata$famdiffs > 0, "High Familiarity"
                  , "Low Familiarity")
  ),
  levels = c("Low Familiarity", "Equal", "High Familiarity")
)

## Model and stat results table ---------------------------------
ifelse(showsim
       , dat <- simdata
       , dat <- MazeData
)

rndExploration_mod <- glm(chose_R_numeric ~ RightConcentrationDiff * Horizon, family = binomial, data = dat)
summary(rndExploration_mod)
tab_model(rndExploration_mod
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward Difference (right - left)"
                            , "Horizon"
                            , "Interaction Rew Diff x Horizon"
          )
          , dv.labels = "Effect on probability of choosing right flower"
)
dirExploration_mod <- glm(chose_R_numeric ~ famdiffs * Horizon, family = binomial, data = dat)
summary(dirExploration_mod)
tab_model(dirExploration_mod
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Familiarity Difference (right - left)"
                            , "Horizon"
                            , "Interaction Fam Diff x Horizon"
          )
          , dv.labels = "Effect on probability of choosing right flower"
)




dirExploration_mod <- glm(HighInfoChoice ~ HighInfoConcentrationDiff * Horizon, family = binomial, data = dat)
summary(dirExploration_mod)
tab_model(dirExploration_mod
          , show.re.var = TRUE
          , pred.labels = c("Intercept"
                            , "Reward Difference (unfamiliar - familiar)"
                            , "Horizon"
                            , "Interaction Rew Diff x Horizon"
          )
          , dv.labels = "Effect on probability of choosing less familiar flower"
)

## Analogous simple ggplot to above -------------------
ggplot(dat, aes(x = HighInfoConcentrationDiff, y = HighInfoChoice)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4, size = 2.5) +  # increased size from 1 to 2.5
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_wrap(~ Horizon) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.4) +
  labs(
    x = "Relative Value of Right Flower",
    y = "Probability of Choosing Unfamiliar Flower"
  ) +
  theme_minimal(base_size = 14)
## end ggplot version --------------------------


## Results Fig 2 - detailed version in base R with Bayesian and and GLM results -----------
par(mfrow=c(1,2))
par(oma = c(4,4,0,0), mar = c(1,1,1,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
n_uncertainty <- 250

## Horizon 1 - model and graph ----------------
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
mtext("Chose Right", 2, 3)
# Gridlines
abline(h = 0.5, col = "grey", lty = 2, lwd = 1)
abline(v = 0, col = "grey", lty = 2, lwd = 1)
# GLM - model and extracting estimates
rndExpl_mod <- glm(chose_R_numeric ~ RightConcentrationDiff, family = binomial, data = dat)
interc <- rndExpl_mod$coefficients[[1]]
par1 <- rndExpl_mod$coefficients[[2]]
ose <- round(exp(par1),1)
ose_sd <- round(exp(par1+summary(rndExpl_mod)$coefficients[2,2]) - ose, 1)
pvalue <- round(summary(rndExpl_mod)$coefficients[2,4], 3)
int <- round(probs_from_pars(0, 0, interc, par1, 0, 0), 2)
int_sd <- round(probs_from_pars(0, interc+summary(rndExpl_mod)$coefficients[1,2], par1, 0, 0) - int, 2)
# Extracting uncertainty
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(rndExpl_mod))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, fit_covar_matrix[i,1], fit_covar_matrix[i,2]), from = -2, to = 2
        , add = TRUE
        , col = alpha(colorshor[1], 0.05)
        , lwd = 3
  )
}
# Original data points with slight jitter
points(jitter(chose_R_numeric, factor = 0.2) ~ jitter(RightConcentrationDiff, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorshor[1], 0.5)
       , cex = 1.5
)
# Plotting fit line
curve(probs_from_pars(x, interc, par1), from = -2, to = 2
      , add = TRUE
      , col = colorshor[1]
      , lwd = 5)
# Text labels for panel
overall <- round(mean(d_graph$chose_R_numeric), 2)
samples <- length(d_graph$chose_R_numeric)
text(x = -2, y = 1.13, labels = paste("N = ", samples), col = colorshor[1], cex = 0.7, adj = 0)
text(x = -2, y = 1.10, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue), cex = 0.7, col = colorshor[1], adj = 0)
text(x = -2, y = 1.07, labels = paste("Right pref (glm): ", int, " +/- ", int_sd), cex = 0.7, col = colorshor[1], adj = 0)
samples2 <- table(d_graph$RightConcentrationDiff)
text(x = -1.75, y =-0.1, labels = paste("n=", samples2[1]), col = colorshor[1])
text(x = -0.75, y =-0.1, labels = paste("n=", samples2[2]), col = colorshor[1])
text(x = 0.75, y =-0.1, labels = paste("n=", samples2[3]), col = colorshor[1])
text(x = 1.75, y =-0.1, labels = paste("n=", samples2[4]), col = colorshor[1])

## Horizon 6 - model and graph ----------------------
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
chooseRmod <- glm(chose_R_numeric ~ RightConcentrationDiff, family = binomial, data = d_graph)
interc <- chooseRmod$coefficients[[1]]
par1 <- chooseRmod$coefficients[[2]]
ose <- round(exp(par1),1)
ose_sd <- round(exp(par1+summary(chooseRmod)$coefficients[2,2]) - ose, 1)
pvalue <- round(summary(chooseRmod)$coefficients[2,4], 3)
int <- round(probs_from_pars(0, interc, par1), 2)
int_sd <- round(probs_from_pars(0, interc+summary(chooseRmod)$coefficients[1,2], par1) - int, 2)
# Bayesian model and extracting estimates
fit_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseRmod))
for(i in 1:n_uncertainty) {
  curve(probs_from_pars(x, 0, fit_covar_matrix[i,1], fit_covar_matrix[i,2]), from = -2, to = 2
        , add = TRUE
        , col = alpha(colorshor[2], 0.1)
        , lwd = 3
  )
}
# Original data points with slight jitter
points(jitter(chose_R_numeric, factor = 0.2) ~ jitter(RightConcentrationDiff, factor = 1)
       , data = d_graph
       , pch = 19
       , col = alpha(colorshor[2], 0.5)
       , cex = 1.5
)
curve(probs_from_pars(x, ?, interc, par1), from = -2, to = 2
      , add = TRUE
      , col = colorshor[2]
      , lwd = 5)
overall <- round(mean(d_graph$chose_R_numeric), 2)
samples <- length(d_graph$chose_R_numeric)
text(x = -2, y = 1.13, labels = paste("N = ", samples), col = colorshor[2], cex = 0.7, adj = 0)
text(x = -2, y = 1.1, labels = paste("Odds slope (glm): ", ose, " +/- ", ose_sd, ", p=", pvalue), cex = 0.7, col = colorshor[2], adj = 0)
text(x = -2, y = 1.07, labels = paste("Right pref (glm): ", int, " +/- ", int_sd), cex = 0.7, col = colorshor[2], adj = 0)
samples2 <- table(d_graph$RightConcentrationDiff)
text(x = -1.75, y =-0.1, labels = paste("n=", samples2[1]), col = colorshor[2])
text(x = -0.75, y =-0.1, labels = paste("n=", samples2[2]), col = colorshor[2])
text(x = 0.75, y =-0.1, labels = paste("n=", samples2[3]), col = colorshor[2])
text(x = 1.75, y =-0.1, labels = paste("n=", samples2[4]), col = colorshor[2])

mtext("Concentration Difference", 1, 2, outer = TRUE)

### end fig --------------------------------------




### Other graphs --------------------------------

# Experimenting with distinguishing by bee and/or adding bee averages
# x-axis values can only be -1.75, -0.75, + 0.75, or +1.75
plot(jitter(chose_R_numeric, factor = 0.2) ~ jitter(RightConcentrationDiff, factor = 0.2)
     , data = dat
     , col = colorsbees[as.factor(MazeData$Bee)]
     , pch = 19
     )

# Reverse plot
ggplot(dat, aes(x = as.numeric(RightFamiliarity), y = chose_R_numeric)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4, size = 2.5) +  # increased size from 1 to 2.5
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  facet_wrap(~ RightConcentrationDiff) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.4) +
  labs(
    x = "Relative Familiarity of Right Flower",
    y = "Probability of Going Right"
  ) +
  theme_minimal(base_size = 14)

