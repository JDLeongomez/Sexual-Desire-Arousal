library(ordinal)
library(dplyr)
library(ggplot2)

library(ordinal)
library(dplyr)
library(ggplot2)

# Define a sequence of Solitary.sexual.desire values (100 evenly spaced points)
solitary_seq <- seq(0, 31, length.out = 32)

# Expand grid of all combinations of predictors and ordinal outcome levels
new_data <- expand.grid(
  Solitary.sexual.desire = solitary_seq,
  Gender = levels(dat_m2$Gender),
  Stimuli.sex = levels(dat_m2$Stimuli.sex),
  Subjective.sexual.arousal.factor = levels(dat_m2$Subjective.sexual.arousal.factor)  # Ensure y is treated as ordinal
)

# Predict probabilities from the clmm model
pred_probs <- predict(m2a_clmm, newdata = new_data, type = "prob")

# Convert predicted probabilities into a tidy format
pred_df <- new_data %>%
  mutate(prob = as.vector(pred_probs)) %>%
  filter(prob > 0)  # Remove zero-probability cases for cleaner plots

# Convert predicted probabilities into a tidy format, grouped by Gender
pred_df <- new_data %>%
  mutate(prob = as.vector(pred_probs)) %>%
  group_by(Gender, Solitary.sexual.desire, Stimuli.sex) %>% 
  mutate(prob = prob / sum(prob)) %>%  # Normalize within Gender groups
  ungroup() %>%
  filter(prob > 0)  # Remove zero-probability cases for cleaner plots

head(pred_df)  # Quick check of structure


probs_m2a_clmm <- ggplot(pred_df, 
                         aes(x = Solitary.sexual.desire, 
                             y = as.numeric(Subjective.sexual.arousal.factor), 
                             fill = prob)) +
  geom_tile(color = "grey") +  
  facet_wrap(Gender ~ Stimuli.sex) +  
  scale_fill_gradientn(
    colors = c("#FFFFFF", "blue", "purple", "red"),  
    values = scales::rescale(c(0, 0.3, 0.4, 1)),  
    name = "Probability (%)",  # Update legend name
    labels = label_percent(accuracy = 1)  # Converts legend values to percentages
  ) +
  scale_y_continuous(breaks = 1:7) +  
  labs(
    x = "Solitary Sexual Desire",
    y = "Level of Subjective Sexual Arousal",
    title = "Probabilities"
  ) +
  theme_minimal() +
  guides(alpha = "none")

ggarrange(probs_m2a_clmm,
          p_m2a_clmm)




ggplot(pred_df, 
       aes(x = Solitary.sexual.desire, 
           y = as.numeric(Subjective.sexual.arousal.factor), 
           alpha = prob)) +
  geom_tile(fill = "red") +  
  facet_wrap(Gender ~ Stimuli.sex) +  
  #scale_fill_gradientn(
  #  colors = c("#FFFFFF", "blue", "purple", "red"),  
  #  values = scales::rescale(c(0, 0.3, 0.4, 1)),  
  #  name = "Probability (%)",  # Update legend name
  #  labels = label_percent(accuracy = 1)  # Converts legend values to percentages
  #) +
  scale_y_continuous(breaks = 1:7) +  
  labs(
    x = "Solitary Sexual Desire",
    y = "Level of Subjective Sexual Arousal",
    title = "Probabilities"
  ) +
  theme_minimal() +
  scale_alpha_manual()
