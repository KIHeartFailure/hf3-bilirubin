# Variables for baseline tables -----------------------------------------------

tabvars <- c(
  "Bili_mgdL",
  "OPALB",
  "DMHDYS",
  "ef_cat",
  "OPNYHA",
  "OPNYHA_cat",
  "DMAGE",
  "DMSEX",
  "BMI",
  "DMHFHIST_DUR",
  "DMHFHOSP",
  "DMPUA_C",
  "DMAFIB",
  "DMDIAB_C",
  "DMAHYP",
  "IDCRT",
  "IDICD",
  "DMCOPD",
  "DMDIAL",
  "DMCCANC",
  "DMCOGD",
  "DMSAPN",
  "OPBP1",
  "OPBP1_cat",
  "OPBP2",
  "OPHR",
  "OPCPRO",
  "OPCPRO_cat",
  "OPPULRA",
  "OPPULOE",
  "OPDYSP",
  "OPORTHO",
  "OPJVP",
  "OPHB",
  "OPEGFR",
  "OPPOT",
  "OPSOD",
  "OPALB",
  "OPBNP",
  "OPNT",
  "OPNT_cat",
  "BNP_NT_cat",
  "OPTROPI",
  "OPTROPT",
  "OPCRP",
  "OPARYT_cat",
  "OPABD_congest",
  "OPABD_pe",
  "ef",
  "OPLAVOLI",
  "OPINTST",
  "OPPOSTWT",
  "OPLVMI",
  "OPMITREG_cat",
  "OPTRICUS_cat",
  "OPRVF",
  "OPTAPSE",
  "OPIVDIM",
  "OPINFVAR",
  "OPPAP",
  "MDRASIARNIH",
  "MDARNIH_C",
  "MDBBH_C",
  "MDALH_C",
  "MDSGLT2H",
  "MDGLP1H",
  "MDDIURH_C"
)

modvars <- c(
  "DMAGE",
  "DMSEX",
  "ef_cat",
  "OPBP1",
  "OPHB",
  "OPEGFR",
  "BNP_NT_cat",
  "OPNYHA_cat",
  "OPCPRO_cat"
)
contvars <- c(
  "DMAGE",
  "OPBP1",
  "OPHB",
  "OPEGFR"
)

modvars[modvars %in% contvars] <- paste0("ns(", modvars[modvars %in% contvars], ", df = 4)")

impvars <- c(
  "OPNYHA_cat",
  "OPCPRO_cat",
  "DMAGE",
  "DMSEX",
  "BMI",
  "DMHFHIST_DUR",
  "DMHFHOSP",
  "DMPUA_C",
  "DMDIAB_C",
  "IDICD",
  "DMCOPD",
  "DMCCANC",
  "DMCOGD",
  "DMSAPN",
  "ef_cat",
  "OPBP1",
  "OPHR",
  "OPHB",
  "OPEGFR",
  "BNP_NT_cat",
  "OPARYT_cat",
  "MDRASIARNIH",
  "MDBBH_C",
  "MDALH_C",
  "MDSGLT2H",
  "MDDIURH_C"
)


metaesc <- bind_rows(
  metaesc,
  tibble(
    variable = c(
      "ef",
      "BNP_NT",
      "OPABD_congest",
      "OPABD_pe",
      "vaso_any",
      "OPCPRO_OPBP1"
    ),
    label = c(
      "LVEF",
      "NT-proBNP/BNP",
      "Chest X-ray: Pulmonary congestion or oedema",
      "Chest X-ray: Pleural effusion",
      "Vasoactive support",
      "Clinical Profile/Systolic Blood pressure (mmHg)"
    ),
    unit = c("%", "pg/mL", rep(NA, 4))
  )
)
