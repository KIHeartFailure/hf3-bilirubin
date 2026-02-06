# ESC ---------------------------------------------------------------------

pathesc <- "P:/k2_stat_heartfailure/Projects/HF3/HFIII_FUP/"

escdata <- read_sas(
  paste0(pathesc, "/hfiii_fup.sas7bdat"),
  paste0(pathesc, "/formats.sas7bcat")
)


# Fix labels ect. ---------------------------------------------------------

escdata <- escdata %>%
  mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), ~ droplevels(., exclude = "Unknown"))) %>%
  mutate(across(where(is.factor), ~ fct_na_level_to_value(.)))
# mutate(across(where(is.factor), factor)) %>% ## Something odd with num_opRyth (two diff kinds of NA). This fixes it
# mutate(num_dmHepa = recode(num_dmHepa, "A" = "Yes", .default = levels(num_dmHepa))) # uses format for other hep variable. this fixes it


# Get labels for tables ---------------------------------------------------

esclab <- sjlabelled::get_label(escdata)
esclab <- tibble(variable = names(escdata), label = esclab) %>%
  mutate(label = if_else(label == "", NA_character_, label))

esclab <- left_join(tibble(variable = names(escdata)), esclab, by = "variable")
