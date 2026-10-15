# Inclusion/exclusion criteria cont. --------------------------------------------------

escdata_misbil <- escdata %>%
  mutate(misbil = factor(if_else(!is.na(OPTBIL), 0, 1), levels = 0:1, labels = c("Included", "Excluded due to missing bilirubin")))

escdata <- escdata %>%
  filter(!is.na(OPTBIL))
flow <- flow %>%
  add_row(
    Criteria = "Exclude bilirubin missing",
    N = nrow(escdata)
  )

flow <- flow %>%
  add_row(
    Criteria = "  Patients with follow-up data (used for long-term outcomes analysis)",
    N = nrow(escdata %>% filter(!is.na(time_death_fu)))
  )
