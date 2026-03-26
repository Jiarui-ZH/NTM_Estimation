library(dplyr)
library(readxl)
library(tidyr)

# ── WORKING DIRECTORY ─────────────────────────────────────────────────────────
# Set your working directory to the project root (NTM_Estimation/) before running.
# In RStudio: Session > Set Working Directory > To Project Directory
# ─────────────────────────────────────────────────────────────────────────────

# --- 1. Load & prep your base tables ---

tariff_data <- read.csv(
  "DATA/MAcMap-HS6/mmhs6_2019_CLEANED_SimpleAverage.csv",
  stringsAsFactors = FALSE
) %>%
  rename(
    hs6 = hs6_2007,   # rename here
    adv = adv
  ) %>%
  select(importer, exporter, hs6, adv) %>%
  distinct()

gravity_vars <- read.csv(
  "DATA/gravity_variables (needs to be updated for year of interest).csv",
  stringsAsFactors = FALSE
) %>%
  select(importer, exporter, dist) %>%
  distinct()

# Place your NTM summary file at: DATA/NTM/NTM_summary.csv
ntm_summary <- read.csv(
  "DATA/NTM/NTM_summary.csv",
  stringsAsFactors = FALSE
) %>%
  rename(hs6 = hs6_2007) %>%   # rename here
  select(importer, exporter, hs6, count) %>%
  group_by(importer, exporter, hs6) %>%
  summarise(NTM = sum(count), .groups="drop")

# Place your three closest countries Excel file at: DATA/Three_Clostest_Countries/three_closest_countries.xlsx
closest_xlsx <- "DATA/Three_Clostest_Countries/three_closest_countries.xlsx"
closest_csv  <- sub("\\.xlsx$", ".csv", closest_xlsx)
if (!file.exists(closest_csv) && file.exists(closest_xlsx)) {
  read_excel(closest_xlsx) %>%
    write.csv(closest_csv, row.names=FALSE)
}
closest <- read.csv(closest_csv, stringsAsFactors=FALSE) %>%
  select(importer, closest_country_1, closest_country_2, closest_country_3) %>%
  distinct()

# --- 2. Build the main “base” frame (with placeholder columns) ---

base <- tariff_data %>%
  left_join(gravity_vars, by = c("importer","exporter")) %>%
  left_join(ntm_summary,  by = c("importer","exporter","hs6")) %>%
  left_join(closest,      by = "importer") %>%
  mutate(NTM = coalesce(NTM, 0L))

# --- 3. Unpivot the 3 neighbors into one long table ---

neighbors_long <- base %>%
  select(importer, exporter, hs6,
         closest_country_1, closest_country_2, closest_country_3) %>%
  pivot_longer(
    cols = starts_with("closest_country_"),
    names_to = "rank", 
    values_to = "nbr"
  ) %>%
  filter(!is.na(nbr))

# --- 4. Join-on adv and NTM for each neighbor link --- 

nbr_with_vals <- neighbors_long %>%
  # bring neighbor's tariff adv
  left_join(
    tariff_data,
    by = c("nbr"      = "importer",
           "exporter" = "exporter",
           "hs6"      = "hs6")
  ) %>%
  rename(adv_nbr = adv) %>%
  # bring neighbor's NTM
  left_join(
    ntm_summary,
    by = c("nbr"      = "importer",
           "exporter" = "exporter",
           "hs6"      = "hs6")
  ) %>%
  rename(ntm_nbr = NTM)

# --- 5. Compute the averages by importer–exporter–hs6 ---

avg_by_pair <- nbr_with_vals %>%
  group_by(importer, exporter, hs6) %>%
  summarise(
    average_tariff = mean(adv_nbr, na.rm = TRUE),
    average_NTM    = mean(ntm_nbr, na.rm = TRUE),
    .groups = "drop"
  )

# --- 6. Bring the averages back into your base frame ---

final <- base %>%
  left_join(avg_by_pair,
            by = c("importer","exporter","hs6")) %>%
  mutate(
    average_tariff = coalesce(average_tariff, 0),
    average_NTM    = coalesce(average_NTM,    0)
  )

# --- 7. Write out the final CSV ---

out_path <- "DATA/Three_Clostest_Countries/Three_closest_countries (for stage 1 reg).csv"
write.csv(final, out_path, row.names = FALSE)

cat("✅ Wrote fast, vectorized final dataset (with column 'hs6') to", out_path, "\n")
