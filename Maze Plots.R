sample_sizes <- LRdata %>%
  group_by(RightInfo) %>%
  summarise(n = n())

ggplot(LRdata, aes(x = RightInfo, y = mean_chose_R)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7, width = 0.5, outlier.shape = NA) +
  geom_text(data = sample_sizes, aes(x = RightInfo, y = 1.02, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 4) +
  labs(
    x = "Information Value of Right Flower over Left",
    y = "Proportion of Choices to Right Flower over Left"
  ) +
  ylim(0, 1.05) +  # Adjusted to make space for n labels
  theme_minimal(base_size = 14)

library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

# Force RightInfo to the correct order: LowInfo -> EQ -> HighInfo
LRdata <- LRdata %>%
  mutate(RightInfo = factor(RightInfo, levels = c("LowInfo", "EQ", "HighInfo")))

# Create new data for prediction
new_data <- data.frame(
  RightConcentrationDiff = mean(LRdata$RightConcentrationDiff, na.rm = TRUE),  # hold concentration constant
  RightInfo = factor(c("LowInfo", "EQ", "HighInfo"), levels = c("LowInfo", "EQ", "HighInfo"))
)

library(dplyr)
library(ggplot2)

# Force RightInfo to the correct order: LowInfo -> EQ -> HighInfo
LRdata <- LRdata %>%
  mutate(RightInfo = factor(RightInfo, levels = c("LowInfo", "EQ", "HighInfo")))

# Create new data for prediction
new_data <- data.frame(
  RightConcentrationDiff = mean(LRdata$RightConcentrationDiff, na.rm = TRUE),  # hold concentration constant
  RightInfo = factor(c("LowInfo", "EQ", "HighInfo"), levels = c("LowInfo", "EQ", "HighInfo"))
)

# Predict from the model
predictions <- predict(LRmod, newdata = new_data, se.fit = TRUE)

# Combine predictions
new_data$predicted_mean <- predictions$fit
new_data$se <- predictions$se.fit

# Plot
individual_data <- LRdata %>%
  group_by(Bee, RightInfo) %>%
  summarize(ProportionRight = mean(chose_R_numeric, na.rm = TRUE)) %>%
  ungroup()
ggplot(individual_data, aes(x = RightInfo, y = ProportionRight)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  labs(
    x = "Informativeness of Right Compared to Left",
    y = "Proportion of Choices to Right over Left"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")  # Remove legend if not needed

