escdata <- escdata %>%
  mutate(
    ef_cat = factor(case_when(
      EF <= 40 ~ 1,
      EF < 50 ~ 2,
      EF >= 50 ~ 3,
    ), levels = 1:3, labels = c("AHF-ref", "AHF-mref", "AHF-pef")),
    DMAGE_cat = factor(case_when(
      is.na(DMAGE) ~ NA_real_,
      DMAGE < 70 ~ 0,
      DMAGE >= 70 ~ 1
    ), levels = 0:1, labels = c("<70", ">=70")),
    BASEDT = dmy(BASEDT),
    Bili_mgdL_cat = ynfac(if_else(Bili_mgdL > 1, 1, 0)),
    hfdur = factor(case_when(
      is.na(DMHFHIST) ~ NA_character_,
      DMHFHIST == "No (now de Novo)" ~ "No",
      TRUE ~ DMHFDUR
    ), levels = c("No", levels(escdata$DMHFDUR))),
    prevhfhosp = ynfac(case_when(
      is.na(DMHFHIST) ~ NA_real_,
      DMHFHIST == "Yes, without previous hospitalization" ~ 1,
      TRUE ~ 0
    )),
    OPBP1_cat = factor(case_when(
      is.na(OPBP1) ~ NA_real_,
      OPBP1 < 90 ~ 1,
      OPBP1 < 100 ~ 2,
      OPBP1 < 110 ~ 3,
      OPBP1 < 130 ~ 4,
      OPBP1 < 160 ~ 5,
      OPBP1 >= 160 ~ 6
    ), levels = 1:6, labels = c("<90", "90-99", "100-109", "110-129", "130-159", ">=160")),
    Nt_pgmL_cat = factor(case_when(
      is.na(Nt_pgmL) ~ NA_real_,
      Nt_pgmL < 1000 ~ 1,
      Nt_pgmL < 3000 ~ 2,
      Nt_pgmL < 10000 ~ 3,
      Nt_pgmL >= 10000 ~ 4
    ), levels = 1:4, labels = c("<1000", "1000-2999", "3000-9999", ">=10000")),
    logNt_pgmL = log(Nt_pgmL),
    OPABD_congest = ynfac(case_when(
      is.na(OPABD) ~ NA_real_,
      OPABD == "Pulmonary congestion or oedema" ~ 1,
      TRUE ~ 0
    )),
    OPABD_pe = ynfac(case_when(
      is.na(OPABD) ~ NA_real_,
      OPABD == "Pleural effusion" ~ 1,
      TRUE ~ 0
    )),
    OPMITREG_cat = factor(case_when(
      is.na(OPMITREG) ~ NA_real_,
      OPMITREG == "No/mild" ~ 0,
      TRUE ~ 1
    ), levels = 0:1, labels = c("No/mild", "Moderate/severe")),
    OPTRICUS_cat = factor(case_when(
      is.na(OPTRICUS) ~ NA_real_,
      OPTRICUS == "No/mild" ~ 0,
      TRUE ~ 1
    ), levels = 0:1, labels = c("No/mild", "Moderate/severe")),
    rasiarni = ynfac(case_when(
      is.na(MDACEH_C) | is.na(MDATH_C) | is.na(MDARNIH_C) ~ NA_real_,
      MDACEH_C == "Yes" | MDATH_C == "Yes" | MDARNIH_C == "Yes" ~ 1,
      TRUE ~ 1
    )),
    vaso_dobuta_dopa_milri_enox_lev = ynfac(case_when(
      ITVASDOB == "Yes" |
        ITVASDOP == "Yes" |
        ITVASMIL == "Yes" |
        ITVASENO == "Yes" |
        ITVASLEV == "Yes" ~ 1,
      TRUE ~ 0
    )),
    Vaso_norepi_epi_vasopress = ynfac(case_when(
      ITVASNOR == "Yes" |
        ITVASEPI == "Yes" |
        ITVASVAS == "Yes" ~ 1,
      TRUE ~ 0
    )),
    death = case_when(
      death_baseline_C == 0 ~ "Alive",
      TRUE ~ as.character(DCDTHCS)
    ),
    death = str_remove_all(death, " cause"),
    death = factor(death, levels = c("Alive", str_remove_all(levels(escdata %>% pull(DCDTHCS)), " cause"))),
    cvd_fu = case_when(
      is.na(F1VITAL) ~ NA_real_,
      F1VITAL == "Alive" | F1DTHCSP == "Other cardiovascular cause" ~ 0,
      TRUE ~ 1
    ),
    cvdhfh = case_when(
      is.na(F1HOSP) | is.na(cvd_fu) ~ NA_real_,
      F1HOSP > 0 | cvd_fu == 1 ~ 1,
      TRUE ~ 0
    )
  )

escdata <- escdata %>%
  mutate(across(where(is_character), factor))
