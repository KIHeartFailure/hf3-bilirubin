# Project specific packages, functions and settings -----------------------

source(here::here("setup/setup.R"))

# Load data ---------------------------------------------------------------

load("P:/k2_stat_heartfailure/Projects/20261005_HF3/data/clean-data/v001/escdata001.RData")

# Munge data --------------------------------------------------------------

source(here("munge/01-vars.R"))
source(here("munge/02-pop-selection-1.R"))
source(here("munge/03-fix-vars.R"))
source(here("munge/04-pop-selection-2.R"))
source(here("munge/05-imputation.R"))

# Cache/save data ---------------------------------------------------------

save(
  file = here("data/clean-data/escdata.RData"),
  list = c(
    "escdata",
    "flow",
    "tabvars",
    "modvars",
    "impvars",
    "metaesc",
    "impdata",
    "escdata_misbil"
  )
)

# create workbook to write tables to Excel
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, sheet = "Information")
openxlsx::writeData(wb, sheet = "Information", x = "Tables in xlsx format for tables in Statistical report: Comprehensive assessment of bilirubin in acute HFrEF, HFmrEF and HFpEF: associations with clinical characteristics, in-hospital course and outcomes, and 1-year cause-specific outcomes", rowNames = FALSE, keepNA = FALSE)
openxlsx::saveWorkbook(wb,
  file = here::here("output/tabs/tables.xlsx"),
  overwrite = TRUE
)

# create powerpoint to write figs to PowerPoint
figs <- officer::read_pptx()
print(figs, target = here::here("output/figs/figs.pptx"))
