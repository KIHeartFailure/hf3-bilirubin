# Project specific packages, functions and settings -----------------------

source(here::here("setup/setup.R"))

# Munge data --------------------------------------------------------------

source(here("munge/01-load-data.R"))
source(here("munge/02-vars.R"))
source(here("munge/03-pop-selection.R"))
source(here("munge/04-fix-vars.R"))

# Cache/save data ---------------------------------------------------------

metaesc <- read.xlsx("./data/meta-data/metavars.xlsx")

save(
  file = here("data/clean-data/escdata.RData"),
  list = c(
    "escdata",
    "flow",
    "tabvars",
    "modvars",
    "esclab",
    "metaesc"
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
