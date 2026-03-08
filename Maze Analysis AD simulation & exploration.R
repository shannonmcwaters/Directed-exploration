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

# What do we want:
showsim <- FALSE # TRUE

# Importing data for experiment 1 directly from github -----------------------
MazeData = read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/refs/heads/main/Maze%20Data%20Raw")

# Colors - TBD, experimenting right now
colorsbees <- viridis(length(unique(MazeData$Bee)), begin = 0.2, alpha = 0.5)
colorsfam <- c("olivedrab2", "olivedrab3", "olivedrab4")
colorassumption <- "darkred"
colorsparameters <- magma(4, alpha = 0.8, begin = 0.2, end = 0.8)

# Data formatting ---------------------------------

# Adding a column 'HighInfoChoice' indicating which choice (flower type) the bees
# had more information about. In the 'EQ' treatment, bees have the same amount of
# information about both types. 
# 'HighInfo' generally refers to 'highly informative', i.e. little information at present,
# in other words low familiarity with the flower type
MazeData <- MazeData %>%
  mutate(HighInfoChoice = case_when(
    Info_Treatment == "HH" & Choice1HV == "0" ~ "0",
    Info_Treatment == "HH" & Choice1HV == "1" ~ "1",
    Info_Treatment == "HL" & Choice1HV == "0" ~ "1",
    Info_Treatment == "HL" & Choice1HV == "1" ~ "0",
    Info_Treatment == "EQ" & Choice1HV == "0" ~ "EQ",
    Info_Treatment == "EQ" & Choice1HV == "1" ~ "EQ"
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
  mutate(RightInfo = ifelse(HighInfoChoice == "EQ", "EQ", ifelse(HighInfoChoice == "1"&Side == "Right" | HighInfoChoice == "0"& Side =="Left","HighInfo","LowInfo")))
MazeData <- MazeData %>%
  mutate(
    Familiarity = case_when(
      RightInfo == "EQ" ~ 0,
      RightInfo == "HighInfo" ~ -0.5,
      RightInfo == "LowInfo" ~ 0.5,
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

## Attempt at data simulation for workflow check and power analysis

# So a key first step is to define a generative model that we think underlies the process.
# This model should have parameters (which we will estimate later using the data)
# that reflect the question(s) we want to answer.

# Let's say that bees choose a probability for selecting the left or right flower
# based on their estimated concentration difference as well as the difference in familiarity.
sim_probs <- function(valuediffs_right, famdiffs_right, slope_value, slope_familiarity, slope_valxfam) {
  samplesize <- length(valuediffs_right)
  linear_predictor <- slope_value * valuediffs_right + slope_familiarity * famdiffs_right + slope_valxfam * valuediffs_right * famdiffs_right
  probability_right <- 1/(1+exp(-linear_predictor))
  return(probability_right)
}

sim_choice <- function(probabilities) {
  samplesize <- length(probabilities)
  choice_roll <- runif(samplesize, 0, 1)
  choices <- ifelse(choice_roll<probabilities, "right", "left")
  return(choices)
}

slope_value <- 0.7
slope_familiarity <- -0.5
slope_valxfam <- 2

N_sim <- 500
valuediffs <- runif(N_sim, min = -2, max = 2)
famdiffs <- sample(c(-1, 0, 1), N_sim, replace = TRUE)
bees <- sample(c("onebee", "twobee"), N_sim, replace = TRUE)
  
choices_simdata <- sim_choice(sim_probs(valuediffs,  famdiffs, slope_value, slope_familiarity, slope_valxfam))
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



# Base R Plot to illustrate binomial fit and uncertainty
par(mfrow=c(1,3))
par(oma = c(4,4,0,0), mar = c(1,1,1,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right

## LOW FAMILIARITY - model and graph
d_graph <- subset(dat, dat$RightFamiliarity == "Low Familiarity")
# Plot frame
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

# The binomial glm estimates a parameter that is the multiplied with x to give the odds, 
# odds: the probability / (1-probability)
chooseRmod <- glm(chose_R_numeric ~ RightConcentrationDiff, family = binomial, data = d_graph)
# Link=logit means the coefficients and factors in the model are fitted to the log odds:
# intercept + estimated_par1 * x1 + estimated_par2 * x2 = log(prob/(1-prob))
interc <- chooseRmod$coefficients[[1]]
par1 <- chooseRmod$coefficients[[2]]
# From this we know that 
# exp(any one coefficient) gives change in odds ratio for one unit change in x.
# So, the actual probability = 
# exp(model terms)/(1+exp(model terms))
prob_fun <- function(x, interc, par1) {
  exp(interc + par1 * x)/(1+exp(interc + par1 * x))
}
ose <- round(exp(par1),1)
int <- round(prob_fun(0, interc, par1), 2)
text(x = -1, y = 1.1, labels = paste("Odds slope est.: ", ose), cex = 1, col = colorsfam[1])
text(x = -1, y = 1.07, labels = paste("Intercept est.: ", int), cex = 1, col = colorsfam[1])
os_input <- round(exp(slope_value - slope_valxfam),1)
int_input <- round(prob_fun(0, -slope_familiarity, slope_value - slope_valxfam), 2)
if(showsim) text(x = 1, y = 1.1, labels = paste("input: ", os_input), cex = 1, col = colorassumption)
if(showsim) text(x = 1, y = 1.07, labels = paste("input: ", int_input), cex = 1, col = colorassumption)
# We can plot this function with the fitted parameters from the model:
curve(prob_fun(x, interc, par1), from = -2, to = 2
      , add = TRUE
      , col = colorsfam[1]
      , lwd = 2)
# What is our certainty around this?
# Here in the glm version, inferential certainty is given by the standard error of
# the parameter estimates. 
# That is, we can generate a bunch of other values for slope and intercept.
# First for slope:
n_uncertainty <- 500
sim_w_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseRmod))
for(i in 1:n_uncertainty) {
  curve(prob_fun(x, sim_w_covar_matrix[i,1], sim_w_covar_matrix[i,2]), from = -2, to = 2
      , add = TRUE
      , col = alpha("grey", 0.2)
      , lwd = 1
      )
}
## 'True' relationship if using simdata
if(showsim) 
  curve(sim_probs(x,  -1, slope_value, slope_familiarity, slope_valxfam), from = -2, to = 2
      , add = TRUE
      , col = colorassumption
      , lwd = 2
      , lty = 3
)
points(jitter(chose_R_numeric, 0.2) ~ RightConcentrationDiff
     , data = d_graph
     , pch = 19
     , col = alpha(colorsfam[1], 0.5)
)
overall <- round(mean(d_graph$chose_R_numeric), 2)
samples <- length(d_graph$chose_R_numeric)
text(x = -1, y = 0.8, labels = paste("Prop. ", overall), col = colorsfam[1])
text(x = -1, y = 0.77, labels = paste("N= ", samples), col = colorsfam[1])
curve(prob_fun(x, interc, par1), from = -2, to = 2
      , add = TRUE
      , col = colorsfam[1]
      , lwd = 3)

## EQUAL FAMILIARITY - model and graph
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
ose <- round(exp(par1),1)
int <- round(prob_fun(0, interc, par1), 2)
text(x = -1, y = 1.1, labels = paste("Odds slope est.: ", ose), cex = 1, col = colorsfam[2])
text(x = -1, y = 1.07, labels = paste("Intercept est.: ", int), cex = 1, col = colorsfam[2])
os_input <- round(exp(slope_value),1)
int_input <- round(prob_fun(0, 0, slope_value), 2)
if(showsim) text(x = 1, y = 1.1, labels = paste("input: ", os_input), cex = 1, col = colorassumption)
if(showsim) text(x = 1, y = 1.07, labels = paste("input: ", int_input), cex = 1, col = colorassumption)
sim_w_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseRmod))
for(i in 1:n_uncertainty) {
  curve(prob_fun(x, sim_w_covar_matrix[i,1], sim_w_covar_matrix[i,2]), from = -2, to = 2
        , add = TRUE
        , col = alpha("grey", 0.2)
        , lwd = 1
  )
}
if(showsim) curve(sim_probs(x,  0, slope_value, slope_familiarity, slope_valxfam), from = -2, to = 2
      , add = TRUE
      , col = colorassumption 
      , lwd = 2
      , lty = 3
)
points(jitter(chose_R_numeric, 0.2) ~ RightConcentrationDiff
       , data = dat
       , pch = 19
       , col = alpha(colorsfam[2], 0.5)
)
overall <- round(mean(d_graph$chose_R_numeric), 2)
samples <- length(d_graph$chose_R_numeric)
text(x = -1, y = 0.8, labels = paste("Prop. ", overall), col = colorsfam[2])
text(x = -1, y = 0.77, labels = paste("N= ", samples), col = colorsfam[2])
curve(prob_fun(x, interc, par1), from = -2, to = 2
      , add = TRUE
      , col = colorsfam[2]
      , lwd = 3)

## HIGH FAMILIARITY - model and graph
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
ose <- round(exp(par1),1)
int <- round(prob_fun(0, interc, par1), 2)
text(x = -1, y = 1.1, labels = paste("Odds slope est.: ", ose), cex = 1, col = colorsfam[3])
text(x = -1, y = 1.07, labels = paste("Intercept est.: ", int), cex = 1, col = colorsfam[3])
os_input <- round(exp(slope_value + slope_valxfam),1)
int_input <- round(prob_fun(0, slope_familiarity, slope_value + slope_valxfam), 2)
if(showsim) text(x = 1, y = 1.1, labels = paste("input: ", os_input), cex = 1, col = colorassumption)
if(showsim) text(x = 1, y = 1.07, labels = paste("input: ", int_input), cex = 1, col = colorassumption)
sim_w_covar_matrix <- mvrnorm(n_uncertainty, mu=c(interc, par1), Sigma=vcov(chooseRmod))
for(i in 1:n_uncertainty) {
  curve(prob_fun(x, sim_w_covar_matrix[i,1], sim_w_covar_matrix[i,2]), from = -2, to = 2
        , add = TRUE
        , col = alpha("grey", 0.2)
        , lwd = 1
  )
}
if(showsim) curve(sim_probs(x,  1, slope_value, slope_familiarity, slope_valxfam), from = -2, to = 2
      , add = TRUE
      , col = colorassumption 
      , lwd = 2
      , lty = 3
)
points(jitter(chose_R_numeric, 0.2) ~ RightConcentrationDiff
       , data = dat
       , pch = 19
       , col = alpha(colorsfam[3], 0.5)
)
overall <- round(mean(d_graph$chose_R_numeric), 2)
samples <- length(d_graph$chose_R_numeric)
text(x = -1, y = 0.8, labels = paste("Prop. ", overall), col = colorsfam[3])
text(x = -1, y = 0.77, labels = paste("N= ", samples), col = colorsfam[3])
curve(prob_fun(x, interc, par1), from = -2, to = 2
      , add = TRUE
      , col = colorsfam[3]
      , lwd = 3)
mtext("Concentration Difference", 1, 2, outer = TRUE)






#### Power analysis

# Simulate several datasets with different sample sizes, repeat the same analysis,
# save results in a table. 
Ns_sim <- c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 97, 100, 110, 120, 130, 140, 150, 200, 500, 5000)
N_real <- nrow(MazeData)
Ns_real <- sample(seq(20, N_real), 1000, replace = TRUE)
ifelse(showsim
       , Ns <- Ns_sim
       , Ns <- Ns_real
)
# Output table prep
parnames <- c("intercept", "slope_value", "slope_familiarity", "slope_valxfam")
inputpars <- c(0, slope_value, slope_familiarity, slope_valxfam)
outcomes <- data.frame(parnames, inputpars)
colnames(outcomes) <- c("Parameter", "Input value")
outcomes2 <- data.frame(parnames, inputpars)
colnames(outcomes2) <- c("Parameter", "Input value")
# Repeat for all sample sizes we want
for(i in 1:length(Ns)) {
  # Make simdata just like above
  
  if(showsim) {
    valuediffs <- runif(Ns[i], min = -2, max = 2)
    famdiffs <- c(0, sample(c(-1, 0, 1), (Ns[i]-1), replace = TRUE))
    choices_dat <- sim_choice(sim_probs(valuediffs,  famdiffs, slope_value, slope_familiarity, slope_valxfam))
    choices_numeric <- as.numeric(ifelse(choices_dat == "right", "1", "0"))
  }
  if(!showsim) {
    colorsparameters <- alpha(colorsparameters, 0.5)
    row_subset <- sample(seq(1, length(dat$chose_R_numeric), by = 1), Ns[i], replace = FALSE)
    dat_subset <- dat %>% slice(row_subset)
    choices_numeric <- dat_subset$chose_R_numeric
    valuediffs <- dat_subset$RightConcentrationDiff
    famdiffs <- dat_subset$Familiarity * 2
  }
  dataset <- data.frame(chose_R_numeric = choices_numeric, RightConcentrationDiff = valuediffs, famdiffs)
  dataset$RightFamiliarity <- factor(
      ifelse(dataset$famdiffs == 0, "Equal"
             , ifelse(dataset$famdiffs > 0, "High Familiarity"
                      , "Low Familiarity")
      ),
      levels = c("Low Familiarity", "Equal", "High Familiarity")
    )
  
  # Models: one with and one without interaction
  chooseRmod <- glm(chose_R_numeric ~ RightConcentrationDiff, family = binomial
                    , data = subset(dataset, dataset$RightFamiliarity == "Equal"))
  interc <- round(chooseRmod$coefficients[[1]], 2)
  par1 <- round(chooseRmod$coefficients[[2]], 2)

  chooseRmod2 <- glm(chose_R_numeric ~ RightConcentrationDiff * famdiffs, family = binomial
                     , data = dataset)
  interc_2 <- round(chooseRmod2$coefficients[[1]], 2)
  par1_2 <- round(chooseRmod2$coefficients[[2]], 2)
  par2_2 <- round(chooseRmod2$coefficients[[3]], 2)
  par3_2 <- round(chooseRmod2$coefficients[[4]], 2)
  # Save output in table
  newoutcome <- data.frame(c(interc, par1, NA, NA))
  colnames(newoutcome) <- paste("N=", Ns[i], sep="")
  outcomes <- cbind(outcomes, newoutcome)
  newoutcome <- data.frame(c(interc_2, par1_2, par2_2, par3_2))
  colnames(newoutcome) <- paste("N=", Ns[i], sep="")
  outcomes2 <- cbind(outcomes2, newoutcome)
}
# Transpose table for easier graphing
outcomes_f_graph <- as.data.frame(t(outcomes[,-1]))
colnames(outcomes_f_graph) <- outcomes[,1]
outcomes2_f_graph <- as.data.frame(t(outcomes2[,-1]))
colnames(outcomes2_f_graph) <- outcomes2[,1]
# Graph
par(mfrow=c(1,1))
par(oma = c(0,0,0,0), mar = c(4,4,1,1), mgp=c(3, 1, 0), las=0) # bottom, left, top, right
ymax <- round(quantile(abs(c(outcomes_f_graph[,1]
                             , outcomes_f_graph[,2]
                             , outcomes2_f_graph[,1]
                             , outcomes2_f_graph[,2]
                             , outcomes2_f_graph[,3]
                             , outcomes2_f_graph[,4]
                             ))
                       , 0.8, na.rm = TRUE))
plot(intercept ~ Ns
     , data = outcomes_f_graph[-1,]
     , pch = 1
     , log = "x"
     , cex = 2
     , ylim = c(-ymax, ymax)
     , xlab = "Sample size"
     , ylab = "Fitted parameter value"
     , col = colorsparameters[1]
     )
points(slope_value ~ Ns
     , data = outcomes_f_graph[-1,]
     , pch = 1
     , col = colorsparameters[2]
     , cex = 2
)
points(intercept ~ Ns
     , data = outcomes2_f_graph[-1,]
     , pch = 19
     , col = colorsparameters[1]
)
points(slope_value ~ Ns
       , data = outcomes2_f_graph[-1,]
       , pch = 19
       , col = colorsparameters[2]
)
points(slope_familiarity ~ Ns
       , data = outcomes2_f_graph[-1,]
       , pch = 19
       , col = colorsparameters[3]
)
points(slope_valxfam ~ Ns
       , data = outcomes2_f_graph[-1,]
       , pch = 19
       , col = colorsparameters[4]
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
legend("bottomright"
       , title = "Parameters"
       , c("Intercept", "Value", "Familiarity", "Val x Fam")
       , col = colorsparameters
       , pch = 19
       )
## This is obviously not a formal power analysis. But semi-anecdotally, it seems
## to show two things. 
## First, the fit for the two parameters in the first model isn't really any better
## from leaving out the other two (although the p-value may well be different).
## And second, our sample size around 100 is not producing a great fit particularly for 
## deciding whether the fourth parameter is in fact zero, but actually the fit is
## not bad for the other parameters. 
## Lesson from bootstrapping with real data: perhaps not much, except that the 
## value obviously approaches the estimate with the entire dataset. 

# NEXT
#### Bayesian analysis with quap
#### Formal power analysis? With biologically significant values for parameters?

# Real fig:
# - very slight x jitter?
# - thicker fit lines and first color darker
# - N for each concentration difference?
# what is p-value for each panel separately?
# what is p-value for fam difference?

# 2nd analysis with choice for unfamiliar flower regardless of side


#### What conclusion regarding dependence on concentration difference, familiarity difference, and 
#### interaction?

# plot concentration vs familiiarity



LRmod <- glm(chose_R_numeric ~ RightConcentrationDiff + RightFamiliarity, family = binomial(link = "logit"), data = dat)
prob_parameters <- (LRmod$coefficients)/(1+LRmod$coefficients)


## FIGURES - OLD
# Plot A: Logistic regression curves by familiarity
# p1 <- 
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


LRmod_int <- glmer(chose_R_numeric ~ RightConcentrationDiff * Horizon + RightInfo + (1|Bee), family = binomial, data = dat)
summary(LRmod_int)
LRmod <- glmer(chose_R_numeric~ RightInfo + RightConcentrationDiff * as.factor(Horizon)+ (1|Bee), family = binomial, data = dat)
summary(LRmod)

#glm for choosing high info in unequal info treatments
uneq_mod = glmer(as.numeric(HighInfoChoice) ~ HighInfoConcentrationDiff + as.factor(Horizon) + (1|Bee), family = binomial, data = dat)
summary(uneq_mod)
















# Experimenting with distinguishing by bee and/or adding bee averages
# x-axis values can only be -1.75, -0.75, + 0.75, or +1.75
plot(jitter(chose_R_numeric, factor = 0.2) ~ jitter(RightConcentrationDiff, factor = 0.2)
     , data = dat
     , col = colorsbees[as.factor(MazeData$Bee)]
     , pch = 19)

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




# Summarize per-bee average choice
bee_summary <- dat %>%
  group_by(Bee) %>%
  summarise(
    mean_chose_R = mean(chose_R_numeric, na.rm = TRUE)
    #, add an equal vs unequal column?
  )




# Plot B: Boxplot of mean right choices per bee
p2 <- ggplot(bee_summary, aes(x = Familiarity, y = mean_chose_R)) +
  geom_boxplot(fill = "#a6bddb", alpha = 0.8, width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  labs(
    x = "Familiarity of Right Flower",
    y = "Proportion of Choices to the Right"
  ) +
  theme_minimal(base_size = 14)

# Combine plots side by side
p1 
/ p2










LRdata_clean <- LRdata %>%
  filter(!is.na(RightConcentrationDiff), !is.na(chose_R_numeric)) %>%
  mutate(
    Horizon = as.factor(Horizon),
    Info_Treatment = factor(Info_Treatment, levels = c("HH", "HL", "EQ"))
  )
# Plot
#raw data
ggplot(LRdata_clean, aes(x = RightConcentrationDiff, y = chose_R_numeric)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.4, size = 1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 0.4) +
  facet_wrap(~ RightInfo) +
  labs(
    x = "Relative Value of Right Flower",
    y = "Probability of Choosing Right Flower"
  ) +
  theme_minimal(base_size = 14)
#model based predictions
mod <- glm(chose_R_numeric ~ RightConcentrationDiff + Horizon + RightInfo, 
           data = LRdata_clean, family = binomial)

preds <- ggpredict(mod, terms = c("RightConcentrationDiff", "Horizon", "RightInfo"))

ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  facet_wrap(~ facet) +
  labs(x = "Relative Value of Right Flower",
       y = "Probability of Choosing Right Flower",
       color = "Horizon", fill = "Horizon") +
  theme_minimal(base_size = 14)


#Chosing high info vs Horizon
preds_inf <- ggpredict(uneq_mod, terms = c("NewConcentrationDiff", "Horizon"))
ggplot(preds_inf, aes(x = -x, y = 1 - predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = 1 - conf.high, ymax = 1 - conf.low, fill = group), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Relative Value of the Familiar Flower",
    y = "Probability of Choosing the Familiar Flower",
    color = "Horizon",
    fill = "Horizon"
  ) +
  theme_minimal(base_size = 14)

#Boxplot with horizon and conc diff
unequal$HighInfoChoice <- as.factor(unequal$HighInfoChoice)
unequal$Horizon <- as.factor(unequal$Horizon)
ggplot(unequal, aes(x = NewConcentrationDiff, y = HighInfoChoice, fill = Horizon)) +
  geom_boxplot() +
  labs(x = "Concentration Difference", y = "Chose Informative", fill = "Horizon") +
  theme_minimal()


# Use tab_model to produce a well-formatted HTML/Word/LaTeX table
tab_model(LRmod,
          show.re.var = TRUE,
          pred.labels = c("Intercept",
                          "Low Familiarity (Right flower)",
                          "High Familiarity (Right flower)",
                          "Relative Reward of Right Flower",
                          "Long Horizon",
                          "Reward × Horizon Interaction"),
          dv.labels = "Maze Task – Choosing Right Flower")
tab_model(LRmod_int,
          show.re.var = TRUE,
          pred.labels = c("Intercept",
                          "Low Familiarity (Right flower)",
                          "High Familiarity (Right flower)",
                          "Relative Reward of Right Flower",
                          "Long Horizon",
                          "Reward × Horizon Interaction"),
          dv.labels = "Maze Task – Choosing Right Flower")


tab_model(
  uneq_mod,  
  show.re.var = TRUE,
  pred.labels = c(
    "Intercept",
    "Relative Reward of Informative Flower",
    "Long Horizon"
  ),
  dv.labels = "Maze Task – Choice of Informative Flower (Unequal Info)"
)


