# 0. Libraries
libs <- c("readxl", "dplyr", "fixest", "openxlsx", "stringr", "combinat", "boot", "writexl")
sapply(libs, require, character.only = TRUE)
library(dplyr, warn.conflicts = FALSE)

options(boot.nsim = 50)

# ── WORKING DIRECTORY ─────────────────────────────────────────────────────────
# Set your working directory to the project root (NTM_Estimation/) before running.
# In RStudio: Session > Set Working Directory > To Project Directory
# ─────────────────────────────────────────────────────────────────────────────

# 1. Input files
test_data   <- read.csv("DATA/gravity_variables (needs to be updated for year of interest).csv")
baci_data   <- read.csv("DATA/BACI/BACI_CLEANED.csv")
tariff_data <- read.csv("DATA/MAcMap-HS6/mmhs6_2019_CLEANED_SimpleAverage.csv")
NTM_data    <- read.csv("DATA/NTM/NTM_CLEANED.csv")
landlock    <- readxl::read_excel("DATA/landlock.xlsx")
border_data <- read.csv("DATA/common border cleaned.csv")
neigh       <- read.csv("DATA/Three_Clostest_Countries/Three_closest_countries (for stage 1 reg).csv")
secotr      <- readxl::read_excel("Sectors and Regions for NTM.xlsx", sheet = "Sector")
Region      <- readxl::read_excel("Sectors and Regions for NTM.xlsx", sheet = "Region")

# 2. Tidy BACI & tariffs
baci_data <- baci_data %>%
  rename(hs6 = product) %>%
  mutate(year = 2019, hs6 = toupper(hs6)) %>%
  group_by(importer, exporter, hs6, year) %>%
  summarise(trade_qty = sum(trade_qty), .groups = "drop")

tariff_data <- tariff_data %>%
  rename(hs6 = hs6_2007) %>%
  mutate(year = 2019, hs6 = toupper(hs6))

# 3. Find common pairs
pairs_both <- baci_data %>%
  dplyr::select(importer, exporter) %>%
  distinct() %>%
  inner_join(
    tariff_data %>% dplyr::select(importer, exporter) %>% distinct(),
    by = c("importer", "exporter")
  )

# 4. Merge BACI + tariffs
trade_qty_tariff <- baci_data %>%
  semi_join(pairs_both, by = c("importer", "exporter")) %>%
  left_join(
    tariff_data %>% semi_join(pairs_both, by = c("importer", "exporter")),
    by = c("importer", "exporter", "hs6", "year")
  )

# 5. Build final data from gravity
final_data <- test_data %>%
  semi_join(pairs_both, by = c("importer", "exporter")) %>%
  left_join(trade_qty_tariff, by = c("year", "importer", "exporter"))

# 6. Count NTMs
ntm_count <- NTM_data %>%
  group_by(Implementing.Jurisdictions, Affected.Jurisdictions, Affected.Products) %>%
  summarise(NTM = n(), .groups = "drop")

final_data <- final_data %>%
  left_join(ntm_count,
            by = c("importer" = "Implementing.Jurisdictions",
                   "exporter" = "Affected.Jurisdictions",
                   "hs6"      = "Affected.Products")) %>%
  mutate(NTM = coalesce(NTM, 0L))

# 7. Add landlock
final_data <- final_data %>%
  left_join(landlock %>% rename(importer_landlock = Landlock, importer = Country),
            by = "importer") %>%
  left_join(landlock %>% rename(exporter_landlock = Landlock, exporter = Country),
            by = "exporter")

# 8. Add common border
final_data <- final_data %>%
  {
    bd <- bind_rows(
      border_data,
      border_data %>% rename(Country1 = Country2, Country2 = Country1)
    ) %>% distinct() %>% mutate(common_border = 1L)
    left_join(., bd, by = c("exporter" = "Country1", "importer" = "Country2"))
  } %>%
  mutate(common_border = coalesce(common_border, 0L))

# 9. Prepare for regression
reg_data <- final_data %>%
  dplyr::select(-year) %>%
  filter(!is.na(trade_qty), trade_qty > 0) %>%
  mutate(across(c(importer, exporter, hs6), as.factor)) %>%
  filter(complete.cases(.))

# 10. Trade shares & log transforms
imp_shares <- reg_data %>%
  group_by(hs6, importer) %>%
  summarise(val_imp = sum(trade_qty), .groups = "drop") %>%
  group_by(hs6) %>%
  mutate(share_imp = val_imp / sum(val_imp)) %>%
  ungroup()

exp_shares <- reg_data %>%
  group_by(hs6, exporter) %>%
  summarise(val_exp = sum(trade_qty), .groups = "drop") %>%
  group_by(hs6) %>%
  mutate(share_exp = val_exp / sum(val_exp)) %>%
  ungroup()

reg_data2 <- reg_data %>%
  left_join(dplyr::select(imp_shares, hs6, importer, share_imp),
            by = c("hs6", "importer")) %>%
  left_join(dplyr::select(exp_shares, hs6, exporter, share_exp),
            by = c("hs6", "exporter")) %>%
  mutate(
    share_imp = coalesce(share_imp, 0),
    share_exp = coalesce(share_exp, 0),
    log_adv   = log1p(adv),
    log_dist  = log(dist),
    log_gdpi  = log(gdp_importer),
    log_gdpe  = log(gdp_exporter)
  )

# 11. Merge neighbour instruments
reg_data2 <- reg_data2 %>%
  left_join(
    dplyr::select(neigh, importer, exporter, hs6, average_NTM, average_tariff),
    by = c("importer", "exporter", "hs6")
  )

# 11b. Log-transform NTM
reg_data2 <- reg_data2 %>% mutate(NTM_log = log1p(NTM))

# 12. First stages
first_tariff <- lm(
  log_adv ~ log1p(average_tariff) + log_dist + log_gdpi + log_gdpe,
  data = reg_data2
)
first_ntm_cnt <- lm(
  NTM ~ average_NTM + log_dist + log_gdpi + log_gdpe,
  data = reg_data2
)
reg_data2$fitted_NTM <- predict(first_ntm_cnt, newdata = reg_data2)

first_tariff_imp <- lm(
  I(share_imp * log_adv) ~ I(share_imp * log1p(average_tariff)) + log_dist + log_gdpi + log_gdpe,
  data = reg_data2
)
first_tariff_exp <- lm(
  I(share_exp * log_adv) ~ I(share_exp * log1p(average_tariff)) + log_dist + log_gdpi + log_gdpe,
  data = reg_data2
)
first_ntm_imp <- lm(
  I(share_imp * NTM) ~ I(share_imp * average_NTM) + log_dist + log_gdpi + log_gdpe,
  data = reg_data2
)
reg_data2$share_imp_NTM_hat <- predict(first_ntm_imp, newdata = reg_data2)
first_ntm_exp <- lm(
  I(share_exp * NTM) ~ I(share_exp * average_NTM) + log_dist + log_gdpi + log_gdpe,
  data = reg_data2
)
reg_data2$share_exp_NTM_hat <- predict(first_ntm_exp, newdata = reg_data2)
first_ntm_log <- lm(
  NTM_log ~ average_NTM + log_dist + log_gdpi + log_gdpe,
  data = reg_data2
)
reg_data2$fitted_NTM_log <- predict(first_ntm_log, newdata = reg_data2)

# 13. Fitted values
reg_data2 <- reg_data2 %>%
  mutate(
    log_adv_hat     = predict(first_tariff,     newdata = reg_data2),
    share_imp_t_hat = predict(first_tariff_imp, newdata = reg_data2),
    share_exp_t_hat = predict(first_tariff_exp, newdata = reg_data2)
  )

# 14. Second stage
poisson_formula <- as.formula(
  "trade_qty ~
     common_border + importer_landlock + exporter_landlock +
     log_dist + log_gdpi + log_gdpe +
     log_adv_hat + share_imp_t_hat + share_exp_t_hat +
     fitted_NTM_log + share_imp_NTM_hat + share_exp_NTM_hat"
)

fm_data_log <- reg_data2 %>%
  filter(complete.cases(
    trade_qty, common_border, importer_landlock, exporter_landlock,
    log_dist, log_gdpi, log_gdpe, share_imp, share_exp,
    log_adv, average_tariff, NTM, NTM_log, average_NTM,
    hs6
  )) %>%
  mutate(hs6 = droplevels(factor(hs6)))

stopifnot(is.factor(fm_data_log$hs6))
stopifnot(!any(is.na(fm_data_log$hs6)))
stopifnot(all(is.finite(fm_data_log$log_dist)))

second_stage_log <- fixest::feglm(
  poisson_formula,
  data   = fm_data_log,
  family = poisson("log")
)

# 15. Bootstrap
boot_fun_full <- function(data, indices){
  d <- data[indices, , drop = FALSE]

  keep_1st <- complete.cases(d[, c("log_adv", "average_tariff",
                                   "NTM", "NTM_log", "average_NTM",
                                   "share_imp", "share_exp",
                                   "log_dist", "log_gdpi", "log_gdpe")])
  d <- d[keep_1st, , drop = FALSE]
  if (nrow(d) < 100L) return(rep(NA_real_, 6L))

  safe_lm <- function(f, data) try(stats::lm(f, data = data), silent = TRUE)

  ft   <- safe_lm(log_adv ~ log1p(average_tariff) + log_dist + log_gdpi + log_gdpe, d)
  fti  <- safe_lm(I(share_imp * log_adv) ~ I(share_imp * log1p(average_tariff)) + log_dist + log_gdpi + log_gdpe, d)
  fte  <- safe_lm(I(share_exp * log_adv) ~ I(share_exp * log1p(average_tariff)) + log_dist + log_gdpi + log_gdpe, d)
  fni  <- safe_lm(I(share_imp * NTM) ~ I(share_imp * average_NTM) + log_dist + log_gdpi + log_gdpe, d)
  fne  <- safe_lm(I(share_exp * NTM) ~ I(share_exp * average_NTM) + log_dist + log_gdpi + log_gdpe, d)
  fnl  <- safe_lm(NTM_log ~ average_NTM + log_dist + log_gdpi + log_gdpe, d)

  if (any(vapply(list(ft, fti, fte, fni, fne, fnl), inherits, TRUE, "try-error")))
    return(rep(NA_real_, 6L))

  d$log_adv_hat       <- as.numeric(predict(ft,  newdata = d))
  d$share_imp_t_hat   <- as.numeric(predict(fti, newdata = d))
  d$share_exp_t_hat   <- as.numeric(predict(fte, newdata = d))
  d$share_imp_NTM_hat <- as.numeric(predict(fni, newdata = d))
  d$share_exp_NTM_hat <- as.numeric(predict(fne, newdata = d))
  d$fitted_NTM_log    <- as.numeric(predict(fnl, newdata = d))

  keep_2nd <- complete.cases(d[, c("trade_qty",
                                   "common_border", "importer_landlock", "exporter_landlock",
                                   "log_dist", "log_gdpi", "log_gdpe",
                                   "log_adv_hat", "share_imp_t_hat", "share_exp_t_hat",
                                   "fitted_NTM_log", "share_imp_NTM_hat", "share_exp_NTM_hat")])
  d2 <- d[keep_2nd, , drop = FALSE]
  if (nrow(d2) < 100L) return(rep(NA_real_, 6L))

  m <- try(fixest::feglm(poisson_formula, data = d2, family = poisson("log")), silent = TRUE)
  if (inherits(m, "try-error")) return(rep(NA_real_, 6L))

  cf <- coef(m)
  pick <- function(co, nm) if (nm %in% names(co)) unname(co[[nm]]) else NA_real_

  c(
    bt0 = pick(cf, "log_adv_hat"),
    bt1 = pick(cf, "share_imp_t_hat"),
    bt2 = pick(cf, "share_exp_t_hat"),
    bn0 = pick(cf, "fitted_NTM_log"),
    bn1 = pick(cf, "share_imp_NTM_hat"),
    bn2 = pick(cf, "share_exp_NTM_hat")
  )
}

set.seed(42)
R_BOOT <- 2000
b <- boot::boot(
  data      = fm_data_log,
  statistic = boot_fun_full,
  R         = R_BOOT,
  strata    = fm_data_log$hs6,
  sim       = "balanced"
)

se_coefs <- apply(b$t, 2, sd, na.rm = TRUE)
print(se_coefs)

# 16. Extract coefficients
cf_full <- coef(second_stage_log)
bt0 <- cf_full["log_adv_hat"];     bt1 <- cf_full["share_imp_t_hat"];   bt2 <- cf_full["share_exp_t_hat"]
bn0 <- cf_full["fitted_NTM_log"];  bn1 <- cf_full["share_imp_NTM_hat"]; bn2 <- cf_full["share_exp_NTM_hat"]

beta_table <- reg_data2 %>%
  distinct(importer, exporter, hs6, trade_qty, share_imp, share_exp) %>%
  mutate(
    beta_tariff_ij = bt0 + bt1 * share_imp + bt2 * share_exp,
    beta_NTM_ij    = bn0 + bn1 * share_imp + bn2 * share_exp
  ) %>%
  dplyr::select(importer, exporter, hs6, trade_qty, beta_tariff_ij, beta_NTM_ij)

cat("Bootstrap SEs (bt0, bt1, bt2, bn0, bn1, bn2):\n", round(se_coefs, 6), "\n")

# 17. AVE computation

# Compute AVE
ave_data <- beta_table %>%
  mutate(
    AVE = suppressWarnings(as.numeric(-(beta_NTM_ij / beta_tariff_ij))),
    trade_qty = suppressWarnings(as.numeric(trade_qty))
  )

# Trim outliers
pct <- quantile(ave_data$AVE, probs = c(0.01, 0.99), na.rm = TRUE)
ave_data_trimmed <- ave_data %>% filter(AVE > pct[1], AVE < pct[2])

cat("After trim: min/max/mean AVE = ",
    min(ave_data_trimmed$AVE, na.rm = TRUE), "/",
    max(ave_data_trimmed$AVE, na.rm = TRUE), "/",
    mean(ave_data_trimmed$AVE, na.rm = TRUE), "\n")

# 18. Sector remap
sec_map <- secotr %>%
  rename(code_old = `Code.old`, code_new = `Code.New`) %>%
  mutate(
    code_old = stringr::str_trim(code_old),
    code_new = stringr::str_trim(code_new)
  ) %>%
  dplyr::select(code_old, code_new) %>%
  distinct()

ave_mapped <- ave_data_trimmed %>%
  mutate(hs6 = stringr::str_trim(hs6)) %>%
  left_join(sec_map, by = c("hs6" = "code_old")) %>%
  mutate(hs6 = if_else(!is.na(code_new), code_new, hs6)) %>%
  dplyr::select(-code_new)

# 19. Trade-weighted AVE
result <- ave_mapped %>%
  dplyr::select(-any_of(c("beta_tariff_ij", "beta_NTM_ij"))) %>%
  group_by(importer, exporter, hs6) %>%
  summarise(
    n             = n(),
    min_AVE       = suppressWarnings(min(AVE, na.rm = TRUE)),
    max_AVE       = suppressWarnings(max(AVE, na.rm = TRUE)),
    w_sum         = sum(pmax(trade_qty, 0), na.rm = TRUE),
    AVE           = ifelse(
      w_sum > 0,
      sum(AVE * pmax(trade_qty, 0), na.rm = TRUE) / w_sum,
      NA_real_
    ),
    trade_qty_sum = sum(trade_qty, na.rm = TRUE),
    .groups = "drop"
  )

# Sanity check
anomalies <- result %>%
  filter(!is.na(AVE),
         !is.infinite(min_AVE), !is.infinite(max_AVE),
         (AVE < min_AVE - 1e-9) | (AVE > max_AVE + 1e-9))
if (nrow(anomalies) > 0) {
  cat("WARNING:", nrow(anomalies), "groups with AVE outside input min–max.\n")
  print(utils::head(anomalies, 20))
} else {
  cat("OK: All aggregated AVEs are within each group's min–max.\n")
}

result_final <- result %>%
  transmute(importer, exporter, hs6,
            trade_qty = trade_qty_sum,
            AVE)

# 20. Region remap
region_map <- Region %>%
  rename(code_old = `Code.Old`, code_new = `Code.New`) %>%
  mutate(
    code_old = stringr::str_trim(code_old),
    code_new = stringr::str_trim(code_new)
  ) %>%
  dplyr::select(code_old, code_new) %>%
  distinct()

region_dups <- region_map %>% dplyr::count(code_old) %>% filter(n > 1)
if (nrow(region_dups) > 0) {
  cat("NOTE: Region has", nrow(region_dups), "codes with multiple mappings. Using first match on join.\n")
}

result_final_mapped <- result_final %>%
  mutate(importer = stringr::str_trim(importer),
         exporter = stringr::str_trim(exporter)) %>%
  left_join(region_map, by = c("importer" = "code_old")) %>%
  mutate(importer = stringr::str_trim(if_else(!is.na(code_new), code_new, importer))) %>%
  dplyr::select(-code_new) %>%
  left_join(region_map, by = c("exporter" = "code_old")) %>%
  mutate(exporter = stringr::str_trim(if_else(!is.na(code_new), code_new, exporter))) %>%
  dplyr::select(-code_new)

# 21. Collapse duplicates
result_final_collapsed <- result_final_mapped %>%
  group_by(importer, exporter, hs6) %>%
  summarise(
    trade_qty_total = sum(pmax(.data$trade_qty, 0), na.rm = TRUE),
    AVE = {
      w <- pmax(.data$trade_qty, 0)
      if (sum(w, na.rm = TRUE) > 0) {
        sum(.data$AVE * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  ) %>%
  rename(trade_qty = trade_qty_total)

# Sanity checks
cat("Rows before remap/collapse:", nrow(result_final), "\n")
cat("Rows after remap/collapse :", nrow(result_final_collapsed), "\n")
cat("Exporter levels (clean):\n")
print(sort(unique(result_final_collapsed$exporter)))
cat("\nSummary of AVE after collapse:\n")
print(summary(result_final_collapsed$AVE))

extreme <- result_final_collapsed %>%
  filter(AVE > max(pct) + 5 | AVE < min(pct) - 5 | is.infinite(AVE))
cat("Extreme rows after collapse:", nrow(extreme), "\n")
if (nrow(extreme) > 0) print(utils::head(extreme, 10))

# 22. Write outputs
out_dir <- "Output/"
dir.create(out_dir, showWarnings = FALSE)

writexl::write_xlsx(
  result_final_collapsed %>% dplyr::filter(importer != exporter),
  paste0(out_dir, "AVE_codeNew_collapsed.xlsx")
)

cat("DONE — AVE_codeNew_collapsed.xlsx written to", out_dir, "\n")
cat("Bootstrap SEs (bt0, bt1, bt2, bn0, bn1, bn2):\n", round(se_coefs, 6), "\n")
