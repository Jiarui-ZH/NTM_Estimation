# Libraries
library(dplyr)
library(stringr)
library(readxl)

# ── WORKING DIRECTORY ─────────────────────────────────────────────────────────
# Set your working directory to the project root (NTM_Estimation/) before running.
# In RStudio: Session > Set Working Directory > To Project Directory
# ─────────────────────────────────────────────────────────────────────────────

# Load data
Baci_original <- read.csv(
  "DATA/BACI/BACI_HS12_V202501/BACI_HS12_Y2019_V202501.csv",
  stringsAsFactors = FALSE
)
HS6_to_GTAP  <- read_excel("DATA/MAcMap-HS6/HS6 TO GTAP UPDATED.xlsx")
country_code <- read.csv("DATA/BACI/BACI_HS12_V202501/country_codes_V202501.csv")

# Clean BACI
Baci_clean <- Baci_original %>%
  select(-t, -v) %>%
  rename(
    importer  = i,
    exporter  = j,
    product   = k,
    trade_qty = q
  )

# Prep HS6 mapping
HS6_to_GTAP <- HS6_to_GTAP %>%
  mutate(
    HS6_code    = str_pad(as.character(`HS6 code`), width = 6, side = "left", pad = "0"),
    GTAP_sector = toupper(`GTAP sector`)
  ) %>%
  select(HS6_code, GTAP_sector)

# Map to GTAP sectors
Baci_mapped <- Baci_clean %>%
  mutate(
    product = str_pad(as.character(product), width = 6, side = "left", pad = "0")
  ) %>%
  left_join(
    HS6_to_GTAP,
    by = c("product" = "HS6_code")
  ) %>%
  mutate(
    product = GTAP_sector
  ) %>%
  select(-GTAP_sector)

# Map country codes to ISO3
lookup_codes <- country_code %>%
  select(country_code, country_iso3)

Baci_mapped <- Baci_mapped %>%
  mutate(
    importer = lookup_codes$country_iso3[match(importer, lookup_codes$country_code)],
    exporter = lookup_codes$country_iso3[match(exporter, lookup_codes$country_code)]
  )

# Inspect
head(Baci_mapped)


# Inspect
head(Baci_mapped)

# Write output
write.csv(
  Baci_mapped,
  file      = "DATA/BACI/BACI_CLEANED.csv",
  row.names = FALSE
)
