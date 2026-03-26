library(dplyr)
library(readr)
library(readxl)
library(tidyr)

# ── WORKING DIRECTORY ─────────────────────────────────────────────────────────
# Set your working directory to the project root (NTM_Estimation/) before running.
# In RStudio: Session > Set Working Directory > To Project Directory
# ─────────────────────────────────────────────────────────────────────────────

# Read the CSV file and the HS6 Excel file
NTM_data <- read.csv("DATA/NTM/interventions (Download from Global Trade Alert).csv", stringsAsFactors = FALSE)
HS6_data <- read_excel("DATA/MAcMap-HS6/HS6 TO GTAP UPDATED.xlsx")

# Keep only the desired columns from NTM_data
NTM_data <- NTM_data %>%
  select(`GTA.Evaluation`, `Implementing.Jurisdictions`, `Affected.Products`, `Affected.Jurisdictions`, `Mast.Chapter`)

# Split rows if there are multiple entries in Affected.Products, Affected.Jurisdictions, and Implementing.Jurisdictions
NTM_data_split <- NTM_data %>%
  separate_rows(Affected.Products, sep = ",\\s*")

NTM_final <- NTM_data_split %>%
  separate_rows(Affected.Jurisdictions, sep = ",\\s*") %>%
  separate_rows(Implementing.Jurisdictions, sep = ",\\s*") %>%
  filter(GTA.Evaluation != "Green")

# Ensure Affected.Products and HS6 code are character
NTM_final$Affected.Products <- as.character(NTM_final$Affected.Products)
HS6_data$`HS6 code` <- as.character(HS6_data$`HS6 code`)

# Convert to 6-digit strings with leading zeros where necessary
NTM_final$Affected.Products <- sprintf("%06d", as.numeric(NTM_final$Affected.Products))
HS6_data$`HS6 code` <- sprintf("%06d", as.numeric(HS6_data$`HS6 code`))

# Map Affected.Products to the corresponding GTAP sector from HS6_data
NTM_final$Affected.Products <- HS6_data$`GTAP sector`[
  match(NTM_final$Affected.Products, HS6_data$`HS6 code`)
]

# Read the CEPII Country Code Excel file
# Place your CEPII country code file at: DATA/Country Code.xls
cepii <- read_excel("DATA/Country Code.xls")

# Join NTM_final with the CEPII data for Affected.Jurisdictions
NTM_final <- NTM_final %>%
  left_join(cepii %>% select(Definition, Code_Value = `Code Value`),
            by = c("Affected.Jurisdictions" = "Definition")) %>%
  mutate(Affected.Jurisdictions = ifelse(!is.na(Code_Value), Code_Value, Affected.Jurisdictions)) %>%
  select(-Code_Value)

# Join NTM_final with the CEPII data for Implementing.Jurisdictions
NTM_final <- NTM_final %>%
  left_join(cepii %>% select(Definition, Code_Value = `Code Value`),
            by = c("Implementing.Jurisdictions" = "Definition")) %>%
  mutate(Implementing.Jurisdictions = ifelse(!is.na(Code_Value), Code_Value, Implementing.Jurisdictions)) %>%
  select(-Code_Value)

# Remove duplicate rows based on all columns
NTM_final <- NTM_final %>% distinct()

# Remove rows with Mast.Chapter entry of "P: Export-related measures (incl. subsidies)"
NTM_final <- NTM_final %>%
  filter(Mast.Chapter != "P: Export-related measures (incl. subsidies)")
NTM_final <- NTM_final %>%
  filter(Mast.Chapter != "Tariff measures")

# Write the cleaned data to CSV
write.csv(NTM_final, "DATA/NTM/NTM_CLEANED.csv", row.names = FALSE)

# Optionally, view the updated data frame
head(NTM_final)
