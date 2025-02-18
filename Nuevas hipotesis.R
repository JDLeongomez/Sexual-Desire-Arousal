# H1----

library(bestNormalize)

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
  mutate("Solitary sexual desire (trans)" = predict(trs_SSD),
         "Dyadic sexual desire: Attractive person (trans)" = predict(trs_DSDat),
         "Dyadic sexual desire: Partner (trans)" = predict(trs_DSDpt))

## H1a ----
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

### V2----
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

## H1b ----
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

### V2----
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

## H1c ----
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

### V2----
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

# H2----

## Modelo para filtrar solo eroticos----
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

## Data----
dat_m2 <- dat |>
  filter(`Stimuli content` == "Erotic")

## H2a lmer ----
m2a <- lmer(`Subjective sexual arousal` ~ 
              `Solitary sexual desire` * Gender * `Stimuli sex` +
              (1 | `Stimuli code`) +
              (1 + `Solitary sexual desire` | Participant),
            data = dat_m2,
            control = lmerControl(optimizer = "bobyqa"))
anova(m2a)
check_model(m2a)

bbmle::AICtab(m2a, m2a2, m2a3, m2a4, m2a5,
              base = TRUE, weights = TRUE)

predict_response(m2a, terms = c("Solitary sexual desire", "Stimuli sex", "Gender")) |> 
  plot() +
  theme_tq()

interact_plot(m2a, pred = `Solitary sexual desire`, modx = `Stimuli sex`, mod2 = Gender,
              interval = TRUE) +
  theme_tq()
sim_slopes(m2a, pred = `Solitary sexual desire`, modx = `Stimuli sex`, mod2 = Gender)


m2aM <- lmer(`Subjective sexual arousal` ~ 
              `Solitary sexual desire` * `Stimuli sex` +
              (1 | `Stimuli code`) +
              (1 + `Stimuli sex` | Participant),
            data = dat_m2 |> 
              filter(Gender == "Men"),
            control = lmerControl(optimizer = "bobyqa"))
anova(m2aM)
check_model(m2aM)
check_distribution(m2aM)

predict_response(m2aM, terms = c("Solitary sexual desire", "Stimuli sex")) |> 
  plot() +
  theme_tq()

interact_plot(m2aM, pred = `Solitary sexual desire`, modx = `Stimuli sex`,
              interval = TRUE) +
  theme_tq()
sim_slopes(m2aM, pred = `Solitary sexual desire`, modx = `Stimuli sex`)


m2aW <- lmer(`Subjective sexual arousal` ~ 
               `Solitary sexual desire` * `Stimuli sex` +
               (1 | `Stimuli code`) +
               (1 + `Stimuli sex` | Participant),
             data = dat_m2 |> 
               filter(Gender == "Women"),
             control = lmerControl(optimizer = "bobyqa"))
anova(m2aW)
check_model(m2aW)
check_distribution(m2aW)

predict_response(m2aW, terms = c("Solitary sexual desire", "Stimuli sex")) |> 
  plot() +
  theme_tq()

interact_plot(m2aW, pred = `Solitary sexual desire`, modx = `Stimuli sex`,
              interval = TRUE) +
  theme_tq()
sim_slopes(m2aW, pred = `Solitary sexual desire`, modx = `Stimuli sex`)

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

m2a2M <- clmm(Subjective.sexual.arousal.fact ~ 
               Solitary.sexual.desire * Stimuli.sex+
               (1 | Stimuli.code) +
               (1 + Stimuli.sex | Participant),
             data = dat_m2a |> 
               filter(Gender == "Men"),
             link = "probit",
             control = list(method = "nlminb"))

options(contrasts = c("contr.sum","contr.poly"))
summary(m2a2M)

emmip(m2a2M, Stimuli.sex ~ Solitary.sexual.desire,
      cov.reduce = range, 
      CIs = TRUE,
      type = "response",
      at = list(Solitary.sexual.desire = seq(from = 0, to = 31, by = 0.5)))

predict_response(m2a2M, terms = c("Solitary.sexual.desire", "Stimuli.sex")) |> 
  as_tibble() |> 
  ggplot(aes(y = predicted, x = x, color = response.level)) +
  geom_smooth() +
  facet_wrap(~group)

check_model(m2a2M)


emtrends(m2a2, ~ Solitary.sexual.desire | Gender + Stimuli.sex,
         var = "Solitary.sexual.desire")

# Como no podemos ver significancia de las pendientes, quiero ver si es posible calcular la predicción del modelo,
# y hacer correlaciones de Spearman para cada combinacion de genero y sexo del estímulo. 
# Para esto, sin em,bargo, hay que transformar la predicción en la escala original a partir de los "tresholds"

bla <- m2a2$model
bla <- bla |> 
  mutate(dependiente = predict(m2a2))

### V3 transformado----

trs_SSA <- orderNorm(dat_m2a$Subjective.sexual.arousal)
dat_m2.trans <- dat_m2a |>
  mutate(Subjective.sexual.arousal.TRANS = predict(trs_SSA))

m2a3 <- lmer(Subjective.sexual.arousal.TRANS ~ 
              Solitary.sexual.desire * Gender * Stimuli.sex +
              (1 | Stimuli.code) +
              (1 + Stimuli.sex | Participant),
            data = dat_m2.trans,
            control = lmerControl(optimizer = "bobyqa"))
anova(m2a3)
check_model(m2a3)
check_distribution(m2a3)

predict_response(m2a3, terms = c("Solitary.sexual.desire", "Stimuli.sex", "Gender")) |> 
  plot() +
  theme_tq()

sim_slopes(m2a3, pred = Solitary.sexual.desire, modx = Stimuli.sex, mod2 = Gender)

### V4 poisson ----

m2a4 <- glmer(Subjective.sexual.arousal ~ 
                Solitary.sexual.desire * Stimuli.sex * Gender +
                (1 | Stimuli.code) +
                (1 + Stimuli.sex | Participant),
              data = dat_m2a,
              family = poisson)

anova(m2a4)
check_model(m2a4)
check_distribution(m2a4)

interact_plot(m2a4, pred = Solitary.sexual.desire, modx = Stimuli.sex, mod2 = Gender,
              interval = TRUE) +
  theme_tq()+

sim_slopes(m2a4, pred = Solitary.sexual.desire, modx = Stimuli.sex, mod2 = Gender)

predict_response(m2a4, terms = c("Solitary.sexual.desire", "Stimuli.sex", "Gender")) |> 
  plot() +
  theme_tq()


m2a4W <- glmer(Subjective.sexual.arousal ~ 
                 Solitary.sexual.desire * Stimuli.sex +
                 (1 | Stimuli.code) +
                 (1 + Stimuli.sex | Participant),
               data = dat_m2a |> 
                 filter(Gender == "Women"),
               family = poisson)

anova(m2a4W)
check_model(m2a4W)
check_distribution(m2a4W)

interact_plot(m2a4W, pred = Solitary.sexual.desire, modx = Stimuli.sex,
              interval = TRUE) +
  theme_tq()
#sim_slopes(m2a4W, pred = Solitary.sexual.desire, modx = Stimuli.sex)

predict_response(m2a4W, terms = c("Solitary.sexual.desire", "Stimuli.sex")) |> 
  plot() +
  theme_tq()



### V5 negativo binomial----

m2a5 <- glmer.nb(Subjective.sexual.arousal ~ 
                   Solitary.sexual.desire * Stimuli.sex * Gender +
                   (1 | Stimuli.code) +
                   (1 + Stimuli.sex | Participant),
                 data = dat_m2a)

anova(m2a5)
check_model(m2a5)
check_distribution(m2a5)

#interact_plot(m2a5, pred = Solitary.sexual.desire, modx = Stimuli.sex, mod2 = Gender,
#              interval = TRUE) +
#  theme_tq()
  
sim_slopes(m2a5, pred = Solitary.sexual.desire, modx = Stimuli.sex, mod2 = Gender)

predict_response(m2a5, terms = c("Solitary.sexual.desire", "Stimuli.sex", "Gender")) |> 
  plot() +
  theme_tq()


## H2b ----
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




m2bM <- lmer(`Subjective sexual arousal` ~ 
               `Dyadic sexual desire (Attractive person)` * `Stimuli sex` +
               (1 | `Stimuli code`) +
               (1 + `Stimuli sex` | Participant),
             data = dat_m2 |> 
               filter(Gender == "Men"),
             control = lmerControl(optimizer = "bobyqa"))
anova(m2bM)
check_model(m2bM)
check_distribution(m2bM)

predict_response(m2bM, terms = c("Dyadic sexual desire (Attractive person)", "Stimuli sex")) |> 
  plot() +
  theme_tq()

interact_plot(m2bM, pred = `Dyadic sexual desire (Attractive person)`, modx = `Stimuli sex`,
              interval = TRUE) +
  theme_tq()
sim_slopes(m2bM, pred = `Dyadic sexual desire (Attractive person)`, modx = `Stimuli sex`)


m2bW <- lmer(`Subjective sexual arousal` ~ 
               `Dyadic sexual desire (Attractive person)` * `Stimuli sex` +
               (1 | `Stimuli code`) +
               (1 + `Stimuli sex` | Participant),
             data = dat_m2 |> 
               filter(Gender == "Women"),
             control = lmerControl(optimizer = "bobyqa"))
anova(m2bW)
check_model(m2bW)
check_distribution(m2bW)

predict_response(m2bW, terms = c("Dyadic sexual desire (Attractive person)", "Stimuli sex")) |> 
  plot() +
  theme_tq()

interact_plot(m2bW, pred = `Dyadic sexual desire (Attractive person)`, modx = `Stimuli sex`,
              interval = TRUE) +
  theme_tq()
sim_slopes(m2bW, pred = `Dyadic sexual desire (Attractive person)`, modx = `Stimuli sex`)



## H2c ----
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

# H3----

## Data----
dat_m3 <- dat |>
  filter(`Stimuli content` == "Erotic" &
           `Stimuli sex` == `Preferred sex`)

## H3a ----
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

## H3b ----
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

## H3c ----
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
