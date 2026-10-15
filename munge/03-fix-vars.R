escdata <- escdata %>%
  mutate(
    OPNYHA_cat = factor(case_when(
      OPNYHA %in% c("I", "II") ~ 1,
      OPNYHA %in% c("III", "IV") ~ 2
    ), levels = 1:2, labels = c("I-II", "III-IV")),
    ef_cat = factor(case_when(
      ef <= 40 ~ 1,
      ef < 50 ~ 2,
      ef >= 50 ~ 3,
    ), levels = 1:3, labels = c("<=40", "41-49", ">=50")),
    DMAGE_cat = factor(case_when(
      is.na(DMAGE) ~ NA_real_,
      DMAGE <= 70 ~ 0,
      DMAGE > 70 ~ 1
    ), levels = 0:1, labels = c("<=70", ">70")),
    Bili_mgdL_cat = factor(if_else(Bili_mgdL > 1.2, 1, 0), levels = 0:1, labels = c("Bilirubin <=1.2 mg/dL", "Bilirubin >1.2 mg/dL")),
    OPARYT_cat = ynfac(case_when(
      is.na(OPARYT) ~ NA_real_,
      OPARYT %in% c("Atrial fibrillation", "Atrial flutter") ~ 1,
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
    OPNT_cat = factor(case_when(
      is.na(OPNT) ~ NA_real_,
      OPNT < 1000 ~ 1,
      OPNT < 3000 ~ 2,
      OPNT < 10000 ~ 3,
      OPNT >= 10000 ~ 4
    ), levels = 1:4, labels = c("<1000", "1000-2999", "3000-9999", ">=10000")),
    logOPNT = log(OPNT),
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
    all_vaso_na = if_all(all_of(c(
      "ITVASNO",
      "ITVASDOB",
      "ITVASDOP",
      "ITVASMIL",
      "ITVASENO",
      "ITVASLEV",
      "ITVASNOR",
      "ITVASEPI",
      "ITVASVAS",
      "ITVASOTH"
    )), is.na),
    vaso_any = ynfac(case_when(
      all_vaso_na ~ NA_real_,
      ITVASNO == "Yes" ~ 0,
      TRUE ~ 1
    )),
    vaso_dobuta_dopa_milri_enox_lev = ynfac(case_when(
      all_vaso_na ~ NA_real_,
      ITVASDOB == "Yes" |
        ITVASDOP == "Yes" |
        ITVASMIL == "Yes" |
        ITVASENO == "Yes" |
        ITVASLEV == "Yes" ~ 1,
      TRUE ~ 0
    )),
    vaso_norepi_epi_vasopress = ynfac(case_when(
      all_vaso_na ~ NA_real_,
      ITVASNOR == "Yes" |
        ITVASEPI == "Yes" |
        ITVASVAS == "Yes" |
        ITVASOTH == "Yes" ~ 1,
      TRUE ~ 0
    )),
    OPCPRO = str_to_title(OPCPRO),
    OPCPRO_cat = factor(case_when(
      is.na(OPCPRO) ~ NA_real_,
      OPCPRO %in% c("Warm/Wet", "Cold/Wet") ~ 1,
      TRUE ~ 0
    ), levels = c(1, 0), labels = c("Wet", "Dry")),
    death = factor(case_when(
      is.na(death_inhosp) ~ NA_real_,
      death_inhosp == "No" ~ 0,
      DCDTHCS == "Heart Failure" ~ 1,
      DCDTHCS %in% c("Myocardial Infarction", "Peripheral embolism", "Procedure related", "Other cardiovascular cause", "Sudden death") |
        is.na(DCDTHCS) ~ 2,
      DCDTHCS == "Non-cardiovascular cause" ~ 3
    ), levels = 0:3, labels = c("Alive", "Heart Failure", "Other CV", "Non-CV"))
  )

# nt/bnp tertiles
nt <- escdata %>%
  filter(!is.na(OPTBIL)) %>%
  reframe(ntsum = list(enframe(quantile(OPNT,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  ))), .by = ef_cat) %>%
  unnest(cols = c(ntsum)) %>%
  pivot_wider(names_from = name, values_from = value)

bnp <- escdata %>%
  filter(!is.na(OPTBIL)) %>%
  reframe(bnpsum = list(enframe(quantile(OPBNP,
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
      OPNT < `33%.x` ~ 1,
      OPNT < `66%.x` ~ 2,
      OPNT >= `66%.x` ~ 3
    ),
    tmpBNP = case_when(
      OPBNP < `33%.y` ~ 1,
      OPBNP < `66%.y` ~ 2,
      OPBNP >= `66%.y` ~ 3
    ),
    BNP_NT_cat = factor(
      coalesce(tmpNT, tmpBNP),
      levels = 1:3,
      labels = c("1st tertile within EF", "2nd tertile within EF", "3rd tertile within EF")
    )
  ) %>%
  select(-`33%.x`, -`66%.x`, -`33%.y`, -`66%.y`, -tmpNT, -tmpBNP)

tapse <- escdata %>%
  filter(!is.na(OPTBIL)) %>%
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
