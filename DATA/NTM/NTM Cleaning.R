library(dplyr)
library(readr)
library(readxl)
library(tidyr)

# ── WORKING DIRECTORY ─────────────────────────────────────────────────────────
# Set your working directory to the project root (NTM_Estimation/) before running.
# In RStudio: Session > Set Working Directory > To Project Directory
# ─────────────────────────────────────────────────────────────────────────────

# Load data
NTM_data <- read.csv("DATA/NTM/interventions (Download from Global Trade Alert).csv", stringsAsFactors = FALSE)
HS6_data <- read_excel("DATA/MAcMap-HS6/HS6 TO GTAP UPDATED.xlsx")

# Select columns
NTM_data <- NTM_data %>%
  select(`GTA.Evaluation`, `Implementing.Jurisdictions`, `Affected.Products`, `Affected.Jurisdictions`, `Mast.Chapter`)

# Expand multi-entry rows
NTM_data_split <- NTM_data %>%
  separate_rows(Affected.Products, sep = ",\\s*")

NTM_final <- NTM_data_split %>%
  separate_rows(Affected.Jurisdictions, sep = ",\\s*") %>%
  separate_rows(Implementing.Jurisdictions, sep = ",\\s*") %>%
  filter(GTA.Evaluation != "Green")

# Standardise types
NTM_final$Affected.Products <- as.character(NTM_final$Affected.Products)
HS6_data$`HS6 code` <- as.character(HS6_data$`HS6 code`)

# Pad HS6 codes
NTM_final$Affected.Products <- sprintf("%06d", as.numeric(NTM_final$Affected.Products))
HS6_data$`HS6 code` <- sprintf("%06d", as.numeric(HS6_data$`HS6 code`))

# Map to GTAP sectors
NTM_final$Affected.Products <- HS6_data$`GTAP sector`[
  match(NTM_final$Affected.Products, HS6_data$`HS6 code`)
]

# Load country codes
cepii <- read_excel("DATA/Country Code.xls")

# Map affected jurisdictions
NTM_final <- NTM_final %>%
  left_join(cepii %>% select(Definition, Code_Value = `Code Value`),
            by = c("Affected.Jurisdictions" = "Definition")) %>%
  mutate(Affected.Jurisdictions = ifelse(!is.na(Code_Value), Code_Value, Affected.Jurisdictions)) %>%
  select(-Code_Value)

# Map implementing jurisdictions
NTM_final <- NTM_final %>%
  left_join(cepii %>% select(Definition, Code_Value = `Code Value`),
            by = c("Implementing.Jurisdictions" = "Definition")) %>%
  mutate(Implementing.Jurisdictions = ifelse(!is.na(Code_Value), Code_Value, Implementing.Jurisdictions)) %>%
  select(-Code_Value)

# Deduplicate
NTM_final <- NTM_final %>% distinct()

# Filter measures
NTM_final <- NTM_final %>%
  filter(Mast.Chapter != "P: Export-related measures (incl. subsidies)")
NTM_final <- NTM_final %>%
  filter(Mast.Chapter != "Tariff measures")

# Write output
write.csv(NTM_final, "DATA/NTM/NTM_CLEANED.csv", row.names = FALSE)

# Inspect
head(NTM_final)
