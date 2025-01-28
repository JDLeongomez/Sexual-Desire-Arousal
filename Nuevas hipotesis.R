# H1

library(bestNormalize)

## Data
dat_m1 <- dat |>
  # Group the data by participant and then select one (the top) observation for each one
  group_by(Participant) |>
  slice_head() |>
  ungroup() |> 
  mutate("Solitary sexual desire (proportion)" = `Solitary sexual desire`/31,
         "Dyadic sexual desire: Attractive person (proportion)" = `Dyadic sexual desire (Attractive person)`/32,
         "Dyadic sexual desire: Partner (proportion)" = `Dyadic sexual desire (Partner)`/38)

trs_SSD <- orderNorm(dat_m1$`Solitary sexual desire (proportion)`)
trs_DSDat <- orderNorm(dat_m1$`Dyadic sexual desire: Attractive person (proportion)`)
trs_DSDpt <- orderNorm(dat_m1$`Dyadic sexual desire: Partner (proportion)`)

dat_m1 <- dat_m1 |> 
  mutate("Solitary sexual desire (trans)" = predict(trs_SSD),
         "Dyadic sexual desire: Attractive person (trans)" = predict(trs_DSDat),
         "Dyadic sexual desire: Partner (trans)" = predict(trs_DSDpt))

## H1a 
m1a <- lm(`Solitary sexual desire (proportion)` ~ Gender * Relationship,
            data = dat_m1)
Anova(m1a, type = 3)
check_model(m1a)
check_distribution(m1a)

ggplot(dat_m1, aes(x = Gender, y = `Solitary sexual desire (proportion)`, color = Gender)) +
  geom_violin(trim = FALSE) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  facet_wrap(~Relationship) +
  scale_color_manual(values = color.Gender) +
  scale_fill_manual(values = color.Gender) +
  stat_summary(fun.data = "mean_cl_boot",
               color = "black", size = 0.2) +
  guides(color = "none") +
  theme_tq()

emmeans(m1a, pairwise ~ Gender | Relationship)


m1a2 <- lm(`Solitary sexual desire (trans)` ~ Gender * Relationship,
           data = dat_m1)
Anova(m1a2, type = 3)
check_model(m1a2)
check_distribution(m1a2)

ggplot(dat_m1, aes(x = Gender, y = `Solitary sexual desire (trans)`, color = Gender)) +
  geom_violin(trim = FALSE) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  facet_wrap(~Relationship) +
  scale_color_manual(values = color.Gender) +
  scale_fill_manual(values = color.Gender) +
  stat_summary(fun.data = "mean_cl_boot",
               color = "black", size = 0.2) +
  guides(color = "none") +
  theme_tq()

emmeans(m1a2, pairwise ~ Gender | Relationship)

## H1b 
m1b <- lm(`Dyadic sexual desire: Attractive person (proportion)` ~ Gender * Relationship,
          data = dat_m1)
Anova(m1b, type = 3)
check_model(m1b)
check_distribution(m1b)

ggplot(dat_m1, aes(x = Gender, y = `Dyadic sexual desire: Attractive person (proportion)`, color = Gender)) +
  geom_violin(trim = FALSE) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  facet_wrap(~Relationship) +
  scale_color_manual(values = color.Gender) +
  scale_fill_manual(values = color.Gender) +
  stat_summary(fun.data = "mean_cl_boot",
               color = "black", size = 0.2) +
  guides(color = "none") +
  theme_tq()

emmeans(m1b, pairwise ~ Gender | Relationship)


m1b2 <- lm(`Dyadic sexual desire: Attractive person (trans)` ~ Gender * Relationship,
           data = dat_m1)
Anova(m1b2, type = 3)
check_model(m1b2)
check_distribution(m1b2)

ggplot(dat_m1, aes(x = Gender, y = `Dyadic sexual desire: Attractive person (trans)`, color = Gender)) +
  geom_violin(trim = FALSE) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  facet_wrap(~Relationship) +
  scale_color_manual(values = color.Gender) +
  scale_fill_manual(values = color.Gender) +
  stat_summary(fun.data = "mean_cl_boot",
               color = "black", size = 0.2) +
  guides(color = "none") +
  theme_tq()

emmeans(m1b2, pairwise ~ Gender | Relationship)

## H1c 
m1c <- lm(`Dyadic sexual desire: Partner (proportion)` ~ Gender * Relationship,
          data = dat_m1)
Anova(m1c, type = 3)
check_model(m1c)
check_distribution(m1c)

ggplot(dat_m1, aes(x = Gender, y = `Dyadic sexual desire: Partner (proportion)`, color = Gender)) +
  geom_violin(trim = FALSE) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  facet_wrap(~Relationship) +
  scale_color_manual(values = color.Gender) +
  scale_fill_manual(values = color.Gender) +
  stat_summary(fun.data = "mean_cl_boot",
               color = "black", size = 0.2) +
  guides(color = "none") +
  theme_tq()

emmeans(m1c, pairwise ~ Gender | Relationship)


m1c2 <- lm(`Dyadic sexual desire: Partner (trans)` ~ Gender * Relationship,
           data = dat_m1)
Anova(m1c2, type = 3)
check_model(m1c2)
check_distribution(m1c2)

ggplot(dat_m1, aes(x = Gender, y = `Solitary sexual desire (trans)`, color = Gender)) +
  geom_violin(trim = FALSE) +
  geom_jitter(alpha = 0.3, width = 0.1) +
  facet_wrap(~Relationship) +
  scale_color_manual(values = color.Gender) +
  scale_fill_manual(values = color.Gender) +
  stat_summary(fun.data = "mean_cl_boot",
               color = "black", size = 0.2) +
  guides(color = "none") +
  theme_tq()

emmeans(m1c2, pairwise ~ Gender | Relationship)

# H2

## Modelo para filtrar solo eroticos
m2 <- lmer(`Subjective sexual arousal` ~ 
             Gender * `Stimuli sex` * `Stimuli content` +
             (1 | `Stimuli code`) +
             (1 | Participant),
           data = dat,
           control = lmerControl(optimizer = "bobyqa"))
anova(m2)

check_model(m2)

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
check_model(m2a)

interact_plot(m2a, pred = `Solitary sexual desire`, modx = `Stimuli sex`, mod2 = Gender,
              interval = TRUE) +
  theme_tq()
sim_slopes(m2a, pred = `Solitary sexual desire`, modx = `Stimuli sex`, mod2 = Gender)


library(ordinal)

dat_m2a <- dat_m2 |> 
  rename(Subjective.sexual.arousal = `Subjective sexual arousal`,
         Solitary.sexual.desire = `Solitary sexual desire`,
         Stimuli.sex = `Stimuli sex`,
         Stimuli.code = `Stimuli code`)

m2a2 <- clmm(factor(Subjective.sexual.arousal) ~ 
               Solitary.sexual.desire * Gender * Stimuli.sex +
              (1 | Stimuli.code) +
              (1 | Participant),
            data = dat_m2a,
            link = "logit",
            control = list(method = "nlminb"))

options(contrasts = c("contr.sum","contr.poly"))
summary(m2a2)

emmip(m2a2, Stimuli.sex ~ Solitary.sexual.desire | Gender,
      cov.reduce = range, 
      CIs = TRUE,
      type = "response",
      at = list(Solitary.sexual.desire = seq(from = 0, to = 31, by = 0.5)))

emtrends(m2a2, ~ Solitary.sexual.desire | Gender + Stimuli.sex,
         var = "Solitary.sexual.desire")

# Como no podemos ver significancia de las pendientes, quiero ver si es posible calcular la predicción del modelo,
# y hacer correlaciones de Spearman para cada combinacion de genero y sexo del estímulo. 
# Para esto, sin em,bargo, hay que transformar la predicción en la escala original a partir de los "tresholds"
bla <- m2a2$model
bla <- bla |> 
  mutate(dependiente = predict(m2a2))


trs_SSA <- orderNorm(dat_m2a$Subjective.sexual.arousal)
dat_m2.trans <- dat_m2a |>
  mutate(Subjective.sexual.arousal.TRANS = predict(trs_SSA))

m2a3 <- lmer(Subjective.sexual.arousal.TRANS ~ 
              Solitary.sexual.desire * Gender * Stimuli.sex +
              (1 | Stimuli.code) +
              (1 | Participant),
            data = dat_m2.trans,
            control = lmerControl(optimizer = "bobyqa"))
anova(m2a3)
check_model(m2a3)

interact_plot(m2a3, pred = Solitary.sexual.desire, modx = Stimuli.sex, mod2 = Gender,
              interval = TRUE) +
  theme_tq()
sim_slopes(m2a3, pred = Solitary.sexual.desire, modx = Stimuli.sex, mod2 = Gender)


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
