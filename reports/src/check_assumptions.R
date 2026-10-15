source(here::here("setup/setup.R"))

# load data
load(here("data/clean-data/escdata.RData"))

dataass <- mice::complete(impdata, 3)
dataass <- mice::complete(impdata, 6)

# check assumptions for cox models ----------------------------------------

mod <- coxph(formula(paste0(
  "Surv(time_death_fu, death_fu == 'Yes') ~ Bili_mgdL_cat  + ", paste(modvars, collapse = " + ")
)), data = dataass)

# prop hazard assumption
testpat <- cox.zph(mod)
print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

x11()
plot(testpat[1], resid = T, col = "red")
plot(testpat[10], resid = T, col = "red")

ggcoxdiagnostics(mod,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)

mod <- coxph(formula(paste0(
  "Surv(time_death_fu, death_fu == 'Yes') ~ ns(Bili_mgdL, df = 4)"
)), data = escdata)

# prop hazard assumption
testpat <- cox.zph(mod)

ggcoxdiagnostics(mod,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)

# For log reg models

mod <- glm(formula(paste0("death_inhosp ~ Bili_mgdL_cat  + ", paste(modvars, collapse = " + "))),
  data = dataass, family = binomial(link = "logit"),
)

# vif
print(car::vif(mod))

# outliers
cooks <- cooks.distance(mod)
plot(cooks)
abline(h = 4 / nrow(dataass), lty = 2, col = "red") # add cutoff line
