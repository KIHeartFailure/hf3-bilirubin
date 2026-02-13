## check no cores
cores_2_use <- parallel::detectCores() - 1
if (cores_2_use >= 20) {
  cores_2_use <- 20
} else {
  stop("Need >= 20 cores for this computation")
}

escdataimp <- escdata %>%
  select(!!!syms(impvars), death_baseline_C, time_death_fup, death_fup, cvdhfh, Bili_mgdL_cat)

noimpvars <- names(escdataimp)[!names(escdataimp) %in% impvars]

ini <- mice(escdataimp, maxit = 0, print = F)

pred <- ini$pred
pred[, noimpvars] <- 0
pred[noimpvars, ] <- 0 # redundant

meth <- ini$method
meth[noimpvars] <- ""

impdata <- futuremice(escdataimp, m = 20, parallelseed = 7833, n.core = cores_2_use, method = meth, predictorMatrix = pred)

# Check if all variables have been fully imputed --------------------------

datacheck <- mice::complete(impdata, 1)
# datacheck %>% count(death_fup)

for (i in seq_along(impvars)) {
  if (any(is.na(datacheck[, impvars[i]]))) stop("Missing for imp vars")
}
for (i in seq_along(impvars)) {
  if (any(is.na(datacheck[, impvars[i]]))) print(paste0("Missing for ", impvars[i]))
}

# categorize age

# keep org imp
imp.org <- impdata

# Convert to Long
long <- mice::complete(impdata, action = "long", include = TRUE)

# Create change vars -----------------------------------------------------------

long <- long %>%
  mutate(
    DMAGE_cat = factor(case_when(
      is.na(DMAGE) ~ NA_real_,
      DMAGE <= 70 ~ 0,
      DMAGE > 70 ~ 1
    ), levels = 0:1, labels = c("<=70", ">70"))
  )
# Convert back to Mids
imput.short <- as.mids(long)
impdata <- imput.short

# Check if all variables have been fully imputed --------------------------

datacheck <- mice::complete(impdata, 1)

datacheck %>% count(DMAGE_cat)
