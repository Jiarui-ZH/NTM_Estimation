library(readxl)
library(dplyr)
library(readr)

# ── WORKING DIRECTORY ─────────────────────────────────────────────────────────
# Set your working directory to the project root (NTM_Estimation/) before running.
# In RStudio: Session > Set Working Directory > To Project Directory
# ─────────────────────────────────────────────────────────────────────────────

# Read the CSV file for MAcMap_data
MAcMap_data <- read.csv("DATA/MAcMap-HS6/Tariffs_2001_2019 (Download from CEPII)/Tariffs_2001_2019/mmhs6_2019.csv",
                        stringsAsFactors = FALSE)

# Read the Excel file for HS6_data
HS6_data <- read_excel("DATA/MAcMap-HS6/HS6 TO GTAP UPDATED.xlsx")

# 1. Convert the columns to character
MAcMap_data$hs6_2007 <- as.character(MAcMap_data$hs6_2007)
HS6_data$`HS6 code`  <- as.character(HS6_data$`HS6 code`)

# 2. Convert to 6-digit strings with leading zeros where necessary
MAcMap_data$hs6_2007 <- sprintf("%06d", as.numeric(MAcMap_data$hs6_2007))
HS6_data$`HS6 code`  <- sprintf("%06d", as.numeric(HS6_data$`HS6 code`))

# 3. Match HS6 codes to GTAP sectors
MAcMap_data$hs6_2007 <- HS6_data$`GTAP sector`[
  match(MAcMap_data$hs6_2007, HS6_data$`HS6 code`)
]

# 4. Group by specified columns and compute simple average of 'adv'
averaged_data <- MAcMap_data %>%
  group_by(importer, exporter, hs6_2007, year) %>%
  summarise(adv = mean(adv, na.rm = TRUE)) %>%
  ungroup()

head(averaged_data)

# 5. Write final output
output_file <- "DATA/MAcMap-HS6/mmhs6_2019_CLEANED_SimpleAverage.csv"
write_csv(averaged_data, output_file)
cat("Output file written to:", output_file, "\n")