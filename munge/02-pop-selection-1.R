# Inclusion/exclusion criteria --------------------------------------------------------

flow <- tibble(Criteria = "N patients in ESC HF III", N = nrow(escdata))
escdata <- escdata %>%
  filter(!is.na(DMVTYPE))
flow <- flow %>%
  add_row(
    Criteria = "Exclude AHF/CHF missing",
    N = nrow(escdata)
  )

escdata <- escdata %>%
  filter(DMVTYPE == "AHF")
flow <- flow %>%
  add_row(
    Criteria = "Exclude CHF",
    N = nrow(escdata)
  )

escdata <- escdata %>%
  mutate(ef = coalesce(OPEF, ITEF, DMEFLP)) %>%
  filter(!is.na(ef))
flow <- flow %>%
  add_row(
    Criteria = "Exclude EF missing",
    N = nrow(escdata)
  )
