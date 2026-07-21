escdata <- escdata %>%
  mutate(
    ef_cat = factor(case_when(
      ef <= 40 ~ 1,
      ef < 50 ~ 2,
      ef >= 50 ~ 3,
    ), levels = 1:3, labels = c("AHF-ref", "AHF-mref", "AHF-pef")),
    DMAGE_cat = factor(case_when(
      is.na(DMAGE) ~ NA_real_,
      DMAGE <= 70 ~ 0,
      DMAGE > 70 ~ 1
    ), levels = 0:1, labels = c("<=70", ">70")),
    BASEDT = dmy(BASEDT),
    Bili_mgdL_cat = factor(if_else(Bili_mgdL > 1, 1, 0), levels = 0:1, labels = c("Bilirubin <=1.0 mg/dL", "Bilirubin >1.0 mg/dL")),
    OPARYT_cat = ynfac(case_when(
      is.na(OPARYT) ~ NA_real_,
      OPARYT %in% c("Atrial fibrillation", "Atrial flutter") ~ 1,
      TRUE ~ 0
    )),
    hfdur = factor(case_when(
      is.na(DMHFHIST) ~ NA_character_,
      DMHFHIST == "No (now de Novo)" ~ "No",
      TRUE ~ DMHFDUR
    ), levels = c("No", levels(escdata$DMHFDUR))),
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
    # ITLAVOLI = ITLAVOL / BSA,
    # OPLAVOLI = coalesce(OPLAVOLI, ITLAVOLI),
    # OPINTST = coalesce(OPINTST, ITINTST),
    # OPPOSTWT = coalesce(OPPOSTWT, ITPOSTWT),
    # ITLVMI = ITLVM / BSA,
    # OPLVMI = coalesce(OPLVMI, ITLVMI),
    # OPMITREG = coalesce(OPMITREG, ITMITREG),
    # OPTRICUS = coalesce(OPTRICUS, ITTRICUS),
    # OPRVF = coalesce(OPRVF, ITRVF),
    # OPTAPSE = coalesce(OPTAPSE, ITTAPSE),
    # OPIVDIM = coalesce(OPIVDIM, ITIVDIM),
    # OPINFVAR = coalesce(OPINFVAR, ITINFVAR),
    # OPPAP = coalesce(OPPAP, ITPAP),
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
      TRUE ~ 0
    )),
    vaso_dobuta_dopa_milri_enox_lev = ynfac(case_when(
      ITVASDOB == "Yes" |
        ITVASDOP == "Yes" |
        ITVASMIL == "Yes" |
        ITVASENO == "Yes" |
        ITVASLEV == "Yes" ~ 1,
      TRUE ~ 0
    )),
    vaso_norepi_epi_vasopress = ynfac(case_when(
      ITVASNOR == "Yes" |
        ITVASEPI == "Yes" |
        ITVASVAS == "Yes" |
        ITVASOTH == "Yes" ~ 1,
      TRUE ~ 0
    )),
    OPCPRO = str_to_title(OPCPRO), 
    death_baseline_C_fac = ynfac(death_baseline_C),
    death = factor(case_when(
      is.na(death_baseline_C) ~ NA_real_,
      death_baseline_C == 0 ~ 0,
      DCDTHCS == "Heart Failure" ~ 1,
      DCDTHCS %in% c("Myocardial Infarction", "Peripheral embolism", "Procedure related", "Other cardiovascular cause", "Sudden death") |
        is.na(DCDTHCS) ~ 2,
      DCDTHCS == "Non-cardiovascular cause" ~ 3
    ), levels = 0:3, labels = c("Alive", "Heart Failure", "Other CV", "Non-CV")),
    # fix that deaths after contact date
    enddtm = pmin(F1DEATHDTD1, F1CONTDTD1, na.rm = T),
    time_death_fup = as.numeric(enddtm - BASEDT),
    death_fup = case_when(
      F1DEATHDTD1 > F1CONTDTD1 ~ 0,
      TRUE ~ death_fup
    ),
    cvd_fu = case_when(
      is.na(death_fup) ~ NA_real_,
      death_fup == 0 | F1DTHCSP %in% c("Non-cardiovascular cause", "Systemic haemorrhage") ~ 0,
      TRUE ~ 1
    ),
    cvdhfh = case_when(
      is.na(F1HOSP) | is.na(cvd_fu) ~ NA_real_,
      TRUE ~ F1HOSP + cvd_fu
    )
  )

# nt/bnp tertiles
nt <- escdata %>%
  reframe(ntsum = list(enframe(quantile(Nt_pgmL,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  ))), .by = ef_cat) %>%
  unnest(cols = c(ntsum)) %>%
  pivot_wider(names_from = name, values_from = value)

bnp <- escdata %>%
  reframe(bnpsum = list(enframe(quantile(BNP_pgmL,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  ))), .by = ef_cat) %>%
  unnest(cols = c(bnpsum)) %>%
  pivot_wider(names_from = name, values_from = value)

escdata <- left_join(
  escdata,
  nt,
  by = "ef_cat"
)
escdata <- left_join(
  escdata,
  bnp,
  by = "ef_cat"
) %>%
  mutate(
    tmpNT = case_when(
      Nt_pgmL < `33%.x` ~ 1,
      Nt_pgmL < `66%.x` ~ 2,
      Nt_pgmL >= `66%.x` ~ 3
    ),
    tmpBNP = case_when(
      BNP_pgmL < `33%.y` ~ 1,
      BNP_pgmL < `66%.y` ~ 2,
      BNP_pgmL >= `66%.y` ~ 3
    ),
    BNP_NT_cat = factor(
      coalesce(tmpNT, tmpBNP),
      levels = 1:3,
      labels = c("1st tertile within EF", "2nd tertile within EF", "3rd tertile within EF")
    )
  ) %>%
  select(-`33%.x`, -`66%.x`, -`33%.y`, -`66%.y`)

tapse <- escdata %>%
  summarise(med = quantile(OPTAPSE, probs = c(0.5), na.rm = TRUE))

escdata <- escdata %>%
  mutate(
    OPTAPSE_cat = factor(
      case_when(
        OPTAPSE <= tapse$med ~ 1,
        OPTAPSE > tapse$med ~ 2
      ),
      levels = 1:2,
      labels = c("<=median", ">median")
    )
  )

escdata <- escdata %>%
  mutate(across(where(is_character), factor)) %>%
  mutate(across(where(is.factor), droplevels))

for (i in seq_along(tabvars)) {
  if (is.factor(escdata %>% pull(!!sym(tabvars[i]))) & nlevels(escdata %>% pull(!!sym(tabvars[i]))) <= 1) stop("Only 1 level for tab vars")
}

for (i in seq_along(tabvars)) {
  if (is.factor(escdata %>% pull(!!sym(tabvars[i]))) & nlevels(escdata %>% pull(!!sym(tabvars[i]))) <= 1) print(paste0("Only 1 level for ", tabvars[i]))
}
