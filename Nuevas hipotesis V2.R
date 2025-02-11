# H1----

library(bestNormalize)
library(effectsize)

## Data----
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
  mutate("Solitary sexual desire (normalized)" = predict(trs_SSD),
         "Dyadic sexual desire: Attractive person (normalized)" = predict(trs_DSDat),
         "Dyadic sexual desire: Partner (normalized)" = predict(trs_DSDpt))

## H1a ----
m1a <- lm(`Solitary sexual desire (proportion)` ~ Gender * Relationship,
            data = dat_m1)
Anova(m1a, type = 3)
#check_model(m1a)
#check_distribution(m1a)

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

emmeans(m1a, pairwise ~ Gender)
emmeans(m1a, pairwise ~ Relationship)
emmeans(m1a, pairwise ~ Gender | Relationship)
effectsize::eta_squared(m1a)

### V2----
m1a2 <- lm(`Solitary sexual desire (normalized)` ~ Gender * Relationship,
           data = dat_m1)
Anova(m1a2, type = 3)
#check_model(m1a2)
#check_distribution(m1a2)

ggplot(dat_m1, aes(x = Gender, y = `Solitary sexual desire (normalized)`, color = Gender)) +
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

## H1b ----
m1b <- lm(`Dyadic sexual desire: Attractive person (proportion)` ~ Gender * Relationship,
          data = dat_m1)
Anova(m1b, type = 3)
#check_model(m1b)
#check_distribution(m1b)

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

### V2----
m1b2 <- lm(`Dyadic sexual desire: Attractive person (normalized)` ~ Gender * Relationship,
           data = dat_m1)
Anova(m1b2, type = 3)
#check_model(m1b2)
#check_distribution(m1b2)

ggplot(dat_m1, aes(x = Gender, y = `Dyadic sexual desire: Attractive person (normalized)`, color = Gender)) +
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

## H1c ----
m1c <- lm(`Dyadic sexual desire: Partner (proportion)` ~ Gender * Relationship,
          data = dat_m1)
Anova(m1c, type = 3)
#check_model(m1c)
#check_distribution(m1c)

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

### V2----
m1c2 <- lm(`Dyadic sexual desire: Partner (normalized)` ~ Gender * Relationship,
           data = dat_m1)
Anova(m1c2, type = 3)
#check_model(m1c2)
#check_distribution(m1c2)

ggplot(dat_m1, aes(x = Gender, y = `Dyadic sexual desire: Partner (normalized)`, color = Gender)) +
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

# H2----

## Modelo para filtrar solo eroticos----
m2 <- lmer(`Subjective sexual arousal` ~ 
             Gender * `Stimuli sex` * `Stimuli content` +
             (1 | `Stimuli code`) +
             (1 + `Stimuli content` | Participant),
           data = dat,
           control = lmerControl(optimizer = "bobyqa"))
anova(m2)

#check_model(m2)

emmip(m2, `Stimuli sex` ~ `Stimuli content` | Gender, cov.reduce = range, CIs = TRUE) +
  theme_tq()
library(ggeffects)
predict_response(m2, terms = c("Stimuli sex", "Stimuli content", "Gender")) |> 
  plot() +
  theme_tq()

emmeans(m2, pairwise ~ `Stimuli content` | `Stimuli sex` + Gender)

## Data----
dat_m2 <- dat |>
  filter(`Stimuli content` == "Erotic")

## H2a 

### V1 lmer ----
m2a1 <- lmer(`Subjective sexual arousal` ~ 
              `Solitary sexual desire` * Gender * `Stimuli sex` +
              (1 | `Stimuli code`) +
              (1 + `Solitary sexual desire` | Participant),
            data = dat_m2,
            control = lmerControl(optimizer = "bobyqa"))
anova(m2a1)
#check_model(m2a1)
#check_distribution(m2a1)

predict_response(m2a1, terms = c("Solitary sexual desire", "Stimuli sex", "Gender")) |> 
  plot() +
  theme_tq()

sim_slopes(m2a1, pred = `Solitary sexual desire`, modx = `Stimuli sex`, mod2 = Gender,
           confint = TRUE)



### V2 clmm----
library(ordinal)

dat_m2a <- dat_m2 |> 
  rename(Subjective.sexual.arousal = `Subjective sexual arousal`,
         Solitary.sexual.desire = `Solitary sexual desire`,
         Stimuli.sex = `Stimuli sex`,
         Stimuli.code = `Stimuli code`) |> 
  mutate(Subjective.sexual.arousal.fact = as.factor(Subjective.sexual.arousal))

m2a2 <- clmm(Subjective.sexual.arousal.fact ~ 
               Solitary.sexual.desire * Gender * Stimuli.sex +
              (1 | Stimuli.code) +
              (1 + Stimuli.sex | Participant),
            data = dat_m2a,
            link = "probit",
            control = list(method = "nlminb"))

options(contrasts = c("contr.sum","contr.poly"))
summary(m2a2)

emmip(m2a2, Stimuli.sex ~ Solitary.sexual.desire,
      cov.reduce = range, 
      CIs = TRUE,
      type = "response",
      at = list(Solitary.sexual.desire = seq(from = 0, to = 31, by = 0.5)))

predict_response(m2a2, terms = c("Solitary.sexual.desire", "Stimuli.sex", "Gender")) |> 
  as_tibble() |> 
  ggplot(aes(y = predicted, x = x, color = response.level)) +
  geom_smooth() +
  facet_wrap(~group)

#check_model(m2a2M)


emtrends(m2a2, ~ Solitary.sexual.desire | Gender + Stimuli.sex,
         var = "Solitary.sexual.desire")


### V3 poisson ----

m2a3 <- glmer(Subjective.sexual.arousal ~ 
                Solitary.sexual.desire * Stimuli.sex * Gender +
                (1 | Stimuli.code) +
                (1 + Stimuli.sex | Participant),
              data = dat_m2a,
              family = poisson)

anova(m2a3)
#check_model(m2a3)
#check_distribution(m2a3)

predict_response(m2a3, terms = c("Solitary.sexual.desire", "Stimuli.sex", "Gender")) |> 
  plot() +
  theme_tq()

#sim_slopes(m2a3, pred = Solitary.sexual.desire, modx = Stimuli.sex, mod2 = Gender)

## H2b ----
### V1 lmer ----
m2b1 <- lmer(`Subjective sexual arousal` ~ 
               `Dyadic sexual desire (Attractive person)` * Gender * `Stimuli sex` +
               (1 | `Stimuli code`) +
               (1 + `Stimuli sex` | Participant),
             data = dat_m2,
             control = lmerControl(optimizer = "bobyqa"))
anova(m2b1)
#check_model(m2b1)
#check_distribution(m2b1)

predict_response(m2b1, terms = c("Dyadic sexual desire (Attractive person)", "Stimuli sex", "Gender")) |> 
  plot() +
  theme_tq()

sim_slopes(m2b1, pred = `Dyadic sexual desire (Attractive person)`, modx = `Stimuli sex`, mod2 = Gender,
           confint = TRUE)


### V2 clmm----
library(ordinal)

m2b2 <- clmm(Subjective.sexual.arousal.fact ~ 
               `Dyadic sexual desire (Attractive person)` * Gender * Stimuli.sex +
               (1 | Stimuli.code) +
               (1 + Stimuli.sex | Participant),
             data = dat_m2a,
             link = "probit",
             control = list(method = "nlminb"))

options(contrasts = c("contr.sum","contr.poly"))
summary(m2b2)

emmip(m2b2, Stimuli.sex ~ `Dyadic sexual desire (Attractive person)` | Gender,
      cov.reduce = range, 
      CIs = TRUE,
      type = "response",
      at = list(`Dyadic sexual desire (Attractive person)` = seq(from = 0, to = 31, by = 0.5)))

predict_response(m2b2, terms = c("Dyadic sexual desire (Attractive person)", "Stimuli.sex", "Gender")) |> 
  as_tibble() |> 
  ggplot(aes(y = response.level, x = x, color = group)) +
  geom_point(aes(alpha = predicted)) +
  facet_wrap(~facet)

emtrends(m2b2, ~ `Dyadic sexual desire (Attractive person)` | Gender + Stimuli.sex,
         var = "Dyadic sexual desire (Attractive person)")

### V3 poisson ----

m2b3 <- glmer(Subjective.sexual.arousal ~ 
                `Dyadic sexual desire (Attractive person)` * Stimuli.sex * Gender +
                (1 | Stimuli.code) +
                (1 + Stimuli.sex | Participant),
              data = dat_m2a,
              family = poisson)

anova(m2b3)
#check_model(m2b3)
#check_distribution(m2b3)

predict_response(m2b3, terms = c("Dyadic sexual desire (Attractive person)", "Stimuli.sex", "Gender")) |> 
  plot() +
  theme_tq()

#sim_slopes(m2b3, pred = `Dyadic sexual desire (Attractive person)`, modx = Stimuli.sex, mod2 = Gender)



## H2c ----
m2c <- lmer(`Subjective sexual arousal` ~ 
              `Dyadic sexual desire (Partner)` * Gender * `Stimuli sex` +
              (1 | `Stimuli code`) +
              (1 | Participant),
            data = dat_m2,
            control = lmerControl(optimizer = "bobyqa"))


### V1 lmer ----
m2c1 <- lmer(`Subjective sexual arousal` ~ 
               `Dyadic sexual desire (Partner)` * Gender * `Stimuli sex` +
               (1 | `Stimuli code`) +
               (1 + `Stimuli sex` | Participant),
             data = dat_m2,
             control = lmerControl(optimizer = "bobyqa"))
anova(m2c1)
#check_model(m2c1)
#check_distribution(m2c1)

predict_response(m2c1, terms = c("Dyadic sexual desire (Partner)", "Stimuli sex", "Gender")) |> 
  plot() +
  theme_tq()

sim_slopes(m2c1, pred = `Dyadic sexual desire (Partner)`, modx = `Stimuli sex`, mod2 = Gender,
           confint = TRUE)


### V2 clmm----

m2c2 <- clmm(Subjective.sexual.arousal.fact ~ 
               `Dyadic sexual desire (Partner)` * Gender * Stimuli.sex +
               (1 | Stimuli.code) +
               (1 + Stimuli.sex | Participant),
             data = dat_m2a,
             link = "probit",
             control = list(method = "nlminb"))

options(contrasts = c("contr.sum","contr.poly"))
summary(m2c2)

emmip(m2c2, Stimuli.sex ~ `Dyadic sexual desire (Partner)` | Gender,
      cov.reduce = range, 
      CIs = TRUE,
      type = "response",
      at = list(`Dyadic sexual desire (Partner)` = seq(from = 0, to = 31, by = 0.5)))

predict_response(m2c2, terms = c("Dyadic sexual desire (Partner)", "Stimuli.sex", "Gender")) |> 
  as_tibble() |> 
  ggplot(aes(y = response.level, x = x, color = group)) +
  geom_point(aes(alpha = predicted)) +
  facet_wrap(~facet)

emtrends(m2c2, ~ `Dyadic sexual desire (Partner)` | Gender + Stimuli.sex,
         var = "Dyadic sexual desire (Partner)")

### V3 poisson ----

m2c3 <- glmer(Subjective.sexual.arousal ~ 
                `Dyadic sexual desire (Partner)` * Stimuli.sex * Gender +
                (1 | Stimuli.code) +
                (1 + Stimuli.sex | Participant),
              data = dat_m2a,
              family = poisson)

anova(m2c3)
#check_model(m2c3)
#check_distribution(m2c3)

predict_response(m2c3, terms = c("Dyadic sexual desire (Partner)", "Stimuli.sex", "Gender")) |> 
  plot() +
  theme_tq()

#sim_slopes(m2c3, pred = `Dyadic sexual desire (Partner)`, modx = Stimuli.sex, mod2 = Gender)



# H3----

## Data----
dat_m3 <- dat |>
  filter(`Stimuli content` == "Erotic" &
           `Stimuli sex` == `Preferred sex`)

## H3a ----

### V1 lmer ----
m3a1 <- lmer(`Subjective sexual arousal` ~ 
               `Solitary sexual desire` * Gender * Relationship +
               (1 | `Stimuli code`) +
               (1 | Participant), #quitar pendientes aleatorias para evitar ajuste singular
             data = dat_m3,
             control = lmerControl(optimizer = "bobyqa"))
anova(m3a1)
#check_model(m3a1)
#check_distribution(m3a1)

predict_response(m3a1, terms = c("Solitary sexual desire", "Relationship", "Gender")) |> 
  plot() +
  theme_tq()

sim_slopes(m3a1, pred = `Solitary sexual desire`, modx = `Relationship`, mod2 = Gender,
           confint = TRUE)


### V2 clmm----
library(ordinal)

dat_m3a <- dat_m3 |> 
  rename(Subjective.sexual.arousal = `Subjective sexual arousal`,
         Solitary.sexual.desire = `Solitary sexual desire`,
         Stimuli.code = `Stimuli code`) |> 
  mutate(Subjective.sexual.arousal.fact = as.factor(Subjective.sexual.arousal))

m3a2 <- clmm(Subjective.sexual.arousal.fact ~ 
               `Solitary.sexual.desire` * Gender * Relationship +
               (1 | Stimuli.code) +
               (1 | Participant),
             data = dat_m3a,
             link = "probit",
             control = list(method = "nlminb"))

options(contrasts = c("contr.sum","contr.poly"))
summary(m3a2)

emmip(m3a2, Relationship ~ Solitary.sexual.desire | Gender,
      cov.reduce = range, 
      CIs = TRUE,
      type = "response",
      at = list(Solitary.sexual.desire = seq(from = 0, to = 31, by = 0.5))) 

predict_response(m3a2, terms = c("Solitary.sexual.desire", "Relationship", "Gender")) |> 
  as_tibble() |> 
  ggplot(aes(y = predicted, x = x, color = response.level)) +
  geom_smooth() +
  facet_wrap(~group)

#check_model(m2a2M)


emtrends(m3a2, ~ Solitary.sexual.desire | Gender + Relationship,
         var = "Solitary.sexual.desire")


### V3 poisson ----

m3a3 <- glmer(Subjective.sexual.arousal ~ 
                Solitary.sexual.desire * Relationship * Gender +
                (1 | Stimuli.code) +
                (1 | Participant),
              data = dat_m3a,
              family = poisson,
              control = glmerControl(optimizer = "bobyqa"))

anova(m3a3)
#check_model(m3a3)
#check_distribution(m3a3)

predict_response(m3a3, terms = c("Solitary.sexual.desire", "Relationship", "Gender")) |> 
  plot() +
  theme_tq()

#sim_slopes(m3a3, pred = Solitary.sexual.desire, modx = Relationship, mod2 = Gender)


## H3b ----

### V1 lmer ----
m3b1 <- lmer(`Subjective sexual arousal` ~ 
               `Dyadic sexual desire (Attractive person)` * Gender * Relationship +
               (1 | `Stimuli code`) +
               (1 | Participant), #quitar pendientes aleatorias para evitar ajuste singular
             data = dat_m3,
             control = lmerControl(optimizer = "bobyqa"))
anova(m3b1)
#check_model(m3b1)
#check_distribution(m3b1)

predict_response(m3b1, terms = c("Dyadic sexual desire (Attractive person)", "Relationship", "Gender")) |> 
  plot() +
  theme_tq()

sim_slopes(m3b1, pred = `Dyadic sexual desire (Attractive person)`, modx = `Relationship`, mod2 = Gender,
           confint = TRUE)


### V2 clmm----

m3b2 <- clmm(Subjective.sexual.arousal.fact ~ 
               `Dyadic sexual desire (Attractive person)` * Gender * Relationship +
               (1 | Stimuli.code) +
               (1 | Participant),
             data = dat_m3a,
             link = "probit",
             control = list(method = "nlminb"))

options(contrasts = c("contr.sum","contr.poly"))
summary(m3b2)

emmip(m3b2, Relationship ~ `Dyadic sexual desire (Attractive person)` | Gender,
      cov.reduce = range, 
      CIs = TRUE,
      type = "response",
      at = list(`Dyadic sexual desire (Attractive person)` = seq(from = 0, to = 31, by = 0.5))) 

predict_response(m3b2, terms = c("Dyadic sexual desire (Attractive person)", "Relationship", "Gender")) |> 
  as_tibble() |> 
  ggplot(aes(y = predicted, x = x, color = response.level)) +
  geom_smooth() +
  facet_wrap(~group)

#check_model(m2a2M)


emtrends(m3a2, ~ Solitary.sexual.desire | Gender + Relationship,
         var = "Solitary.sexual.desire")


### V3 poisson ----

m3b3 <- glmer(Subjective.sexual.arousal ~ 
                `Dyadic sexual desire (Attractive person)` * Relationship * Gender +
                (1 | Stimuli.code) +
                (1 | Participant),
              data = dat_m3a,
              family = poisson,
              control = glmerControl(optimizer = "bobyqa"))

anova(m3b3)
#check_model(m3b3)
#check_distribution(m3b3)

predict_response(m3b3, terms = c("Dyadic sexual desire (Attractive person)", "Relationship", "Gender")) |> 
  plot() +
  theme_tq()

#sim_slopes(m3b3, pred = `Dyadic sexual desire (Attractive person)`, modx = Relationship, mod2 = Gender)




## H3c ----




### V1 lmer ----
m3c1 <- lmer(`Subjective sexual arousal` ~ 
               `Dyadic sexual desire (Partner)` * Gender * Relationship +
               (1 | `Stimuli code`) +
               (1 | Participant), #quitar pendientes aleatorias para evitar ajuste singular
             data = dat_m3,
             control = lmerControl(optimizer = "bobyqa"))
anova(m3c1)
#check_model(m3b1)
#check_distribution(m3b1)

predict_response(m3c1, terms = c("Dyadic sexual desire (Partner)", "Relationship", "Gender")) |> 
  plot() +
  theme_tq()

sim_slopes(m3c1, pred = `Dyadic sexual desire (Partner)`, modx = `Relationship`, mod2 = Gender,
           confint = TRUE)


### V2 clmm----

m3c2 <- clmm(Subjective.sexual.arousal.fact ~ 
               `Dyadic sexual desire (Partner)` * Gender * Relationship +
               (1 | Stimuli.code) +
               (1 | Participant),
             data = dat_m3a,
             link = "probit",
             control = list(method = "nlminb"))

options(contrasts = c("contr.sum","contr.poly"))
summary(m3c2)

emmip(m3c2, Relationship ~ `Dyadic sexual desire (Partner)` | Gender,
      cov.reduce = range, 
      CIs = TRUE,
      type = "response",
      at = list(`Dyadic sexual desire (Partner)` = seq(from = 0, to = 31, by = 0.5))) 

predict_response(m3c2, terms = c("Dyadic sexual desire (Partner)", "Relationship", "Gender")) |> 
  as_tibble() |> 
  ggplot(aes(y = predicted, x = x, color = response.level)) +
  geom_smooth() +
  facet_wrap(~group)

#check_model(m2a2M)


emtrends(m3c2, ~ `Dyadic sexual desire (Partner)` | Gender + Relationship,
         var = "Dyadic sexual desire (Partner)")


### V3 poisson ----

m3c3 <- glmer(Subjective.sexual.arousal ~ 
                `Dyadic sexual desire (Partner)` * Relationship * Gender +
                (1 | Stimuli.code) +
                (1 | Participant),
              data = dat_m3a,
              family = poisson,
              control = glmerControl(optimizer = "bobyqa"))

anova(m3c3)
options(contrasts = c("contr.sum","contr.poly"))
summary(m3c3)
#check_model(m3c3)
#check_distribution(m3c3)

predict_response(m3c3, terms = c("Dyadic sexual desire (Partner)", "Relationship", "Gender")) |> 
  plot() +
  theme_tq()

#sim_slopes(m3c3, pred = `Dyadic sexual desire (Partner)`, modx = Relationship, mod2 = Gender)

