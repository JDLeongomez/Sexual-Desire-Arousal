# H1

## Data
dat_m1 <- dat |>
  # Group the data by participant and then select one (the top) observation for each one
  group_by(Participant) |>
  slice_head() |>
  ungroup()

## H1a 
m1a <- lm(`Solitary sexual desire` ~ Gender * Relationship,
            data = dat_m1)
Anova(m1a, type = 3)
check_model(m1a)
check_distribution(m1a)

ggplot(dat_m1, aes(x = Gender, y = `Solitary sexual desire`, color = Gender)) +
  geom_violin() +
  geom_jitter(alpha = 0.3, width = 0.1) +
  facet_wrap(~Relationship) +
  scale_color_manual(values = color.Gender) +
  scale_fill_manual(values = color.Gender) +
  stat_summary(fun.data = "mean_cl_boot",
               color = "black", size = 0.2) +
  guides(color = "none") +
  theme_tq()

emmeans(m1a, pairwise ~ Gender | Relationship)

## H1b 
m1b <- lm(`Dyadic sexual desire (Attractive person)` ~ Gender * Relationship,
          data = dat_m1)
Anova(m1b, type = 3)
check_model(m1b)
check_distribution(m1b)

ggplot(dat_m1, aes(x = Gender, y = `Dyadic sexual desire (Attractive person)`, color = Gender)) +
  geom_violin() +
  geom_jitter(alpha = 0.3, width = 0.1) +
  facet_wrap(~Relationship) +
  scale_color_manual(values = color.Gender) +
  scale_fill_manual(values = color.Gender) +
  stat_summary(fun.data = "mean_cl_boot",
               color = "black", size = 0.2) +
  guides(color = "none") +
  theme_tq()

emmeans(m1b, pairwise ~ Gender | Relationship)

## H1c 
m1c <- lm(`Dyadic sexual desire (Partner)` ~ Gender * Relationship,
          data = dat_m1)
Anova(m1c, type = 3)
check_model(m1c)
check_distribution(m1c)

ggplot(dat_m1, aes(x = Gender, y = `Dyadic sexual desire (Partner)`, color = Gender)) +
  geom_violin() +
  geom_jitter(alpha = 0.3, width = 0.1) +
  facet_wrap(~Relationship) +
  scale_color_manual(values = color.Gender) +
  scale_fill_manual(values = color.Gender) +
  stat_summary(fun.data = "mean_cl_boot",
               color = "black", size = 0.2) +
  guides(color = "none") +
  theme_tq()

emmeans(m1c, pairwise ~ Gender | Relationship)

# H2

## Modelo para filtrar solo eroticos
m2 <- lmer(`Subjective sexual arousal` ~ 
             Gender * `Stimuli sex` * `Stimuli content` +
             (1 | `Stimuli code`) +
             (1 | Participant),
           data = dat,
           control = lmerControl(optimizer = "bobyqa"))
anova(m2)

emmip(m2, `Stimuli sex` ~ `Stimuli content` | Gender, cov.reduce = range, CIs = TRUE) +
  theme_tq()
library(ggeffects)
predict_response(m2, terms = c("Stimuli sex", "Stimuli content", "Gender")) |> 
  plot() +
  theme_tq()

emmeans(m2, pairwise ~ `Stimuli content` | `Stimuli sex` + Gender)

## Data
dat_m2 <- dat |>
  filter(`Stimuli content` == "Erotic")

## H2a 
m2a <- lmer(`Subjective sexual arousal` ~ 
              `Solitary sexual desire` * Gender * `Stimuli sex` +
              (1 | `Stimuli code`) +
              (1 | Participant),
            data = dat_m2,
            control = lmerControl(optimizer = "bobyqa"))
anova(m2a)

interact_plot(m2a, pred = `Solitary sexual desire`, modx = `Stimuli sex`, mod2 = Gender,
              interval = TRUE) +
  theme_tq()
sim_slopes(m2a, pred = `Solitary sexual desire`, modx = `Stimuli sex`, mod2 = Gender)

## H2b 
m2b <- lmer(`Subjective sexual arousal` ~ 
              `Dyadic sexual desire (Attractive person)` * Gender * `Stimuli sex` +
              (1 | `Stimuli code`) +
              (1 | Participant),
            data = dat_m2,
            control = lmerControl(optimizer = "bobyqa"))
anova(m2b)

interact_plot(m2b, pred = `Dyadic sexual desire (Attractive person)`, modx = `Stimuli sex`, mod2 = Gender,
              interval = TRUE) +
  theme_tq()
sim_slopes(m2b, pred = `Dyadic sexual desire (Attractive person)`, modx = `Stimuli sex`, mod2 = Gender)

## H2c 
m2c <- lmer(`Subjective sexual arousal` ~ 
              `Dyadic sexual desire (Partner)` * Gender * `Stimuli sex` +
              (1 | `Stimuli code`) +
              (1 | Participant),
            data = dat_m2,
            control = lmerControl(optimizer = "bobyqa"))
anova(m2c)

interact_plot(m2c, pred = `Dyadic sexual desire (Partner)`, modx = `Stimuli sex`, mod2 = Gender,
              interval = TRUE) +
  theme_tq()
sim_slopes(m2c, pred = `Dyadic sexual desire (Partner)`, modx = `Stimuli sex`, mod2 = Gender)

# H3

## Data
dat_m3 <- dat |>
  filter(`Stimuli content` == "Erotic" &
           `Stimuli sex` == `Preferred sex`)

## H3a 
m3a <- lmer(`Subjective sexual arousal` ~ 
              `Solitary sexual desire` * Gender * Relationship +
              (1 | `Stimuli code`) +
              (1 | Participant),
            data = dat_m3,
            control = lmerControl(optimizer = "bobyqa"))
anova(m3a)

interact_plot(m3a, pred = `Solitary sexual desire`, modx = Gender, mod2 = Relationship,
              interval = TRUE) +
  theme_tq()

sim_slopes(m3a, pred = `Solitary sexual desire`, modx = Gender, mod2 = Relationship)

## H3b 
m3b <- lmer(`Subjective sexual arousal` ~ 
              `Dyadic sexual desire (Attractive person)` * Gender * Relationship +
              (1 | `Stimuli code`) +
              (1 | Participant),
            data = dat_m3,
            control = lmerControl(optimizer = "bobyqa"))
anova(m3b)

interact_plot(m3b, pred = `Dyadic sexual desire (Attractive person)`, modx = Gender, mod2 = Relationship,
              interval = TRUE) +
  theme_tq()

sim_slopes(m3b, pred = `Dyadic sexual desire (Attractive person)`, modx = Gender, mod2 = Relationship)

## H3c 
m3c <- lmer(`Subjective sexual arousal` ~ 
              `Dyadic sexual desire (Partner)` * Gender * Relationship +
              (1 | `Stimuli code`) +
              (1 | Participant),
            data = dat_m3,
            control = lmerControl(optimizer = "bobyqa"))
anova(m3c)

interact_plot(m3c, pred = `Dyadic sexual desire (Partner)`, modx = Gender, mod2 = Relationship,
              interval = TRUE) +
  theme_tq()

sim_slopes(m3c, pred = `Dyadic sexual desire (Partner)`, modx = Gender, mod2 = Relationship)
