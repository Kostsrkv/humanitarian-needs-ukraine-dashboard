# Clean HNRP 2026 data

library(tidyverse)
library(readr)
library(janitor)

processed_dir <- "data/processed"

overall_sp_uoa <- read_csv(file.path(processed_dir, "overall_sp_uoa_imported.csv"))
idps <- read_csv(file.path(processed_dir, "idps_imported.csv"))
wand <- read_csv(file.path(processed_dir, "wand_imported.csv"))

# Keep only real oblast-level records.
# This removes totals, empty rows, and aggregate GCA/OT rows.
clean_uoa_rows <- function(df) {
  df %>%
    filter(
      !is.na(adm1_en),
      !is.na(adm1_pcode),
      str_detect(adm1_pcode, "^UA\\d{2}$")
    ) %>%
    mutate(
      adm1_en = str_squish(adm1_en),
      adm1_pcode = str_squish(adm1_pcode)
    ) %>%
    mutate(
      across(matches("_joint$"), readr::parse_number)
    )
}

overall_clean <- clean_uoa_rows(overall_sp_uoa)
idps_clean <- clean_uoa_rows(idps)
wand_clean <- clean_uoa_rows(wand)

# Aggregate UoA rows to oblast level.
# Counts are summed; severity is kept as the maximum observed score.
overall_oblast <- overall_clean %>%
  group_by(adm1_en, adm1_pcode) %>%
  summarise(
    population_total = sum(overall_all_total_joint, na.rm = TRUE),
    people_affected = sum(overall_aff_total_joint, na.rm = TRUE),
    people_in_need = sum(overall_pin_total_joint, na.rm = TRUE),
    planned_reach = sum(overall_target_total_joint, na.rm = TRUE),
    severity_max = max(overall_sev_total_joint, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    response_coverage = if_else(
      people_in_need > 0,
      planned_reach / people_in_need,
      NA_real_
    )
  )

idps_oblast <- idps_clean %>%
  group_by(adm1_en, adm1_pcode) %>%
  summarise(
    idp_total = sum(idp_all_total_joint, na.rm = TRUE),
    idp_in_need = sum(idp_pin_total_joint, na.rm = TRUE),
    idp_planned_reach = sum(idp_target_total_joint, na.rm = TRUE),
    idp_severity_max = max(idp_sev_total_joint, na.rm = TRUE),
    .groups = "drop"
  )

wand_oblast <- wand_clean %>%
  group_by(adm1_en, adm1_pcode) %>%
  summarise(
    non_displaced_total = sum(ndp_all_total_joint, na.rm = TRUE),
    non_displaced_in_need = sum(ndp_pin_total_joint, na.rm = TRUE),
    non_displaced_planned_reach = sum(ndp_target_total_joint, na.rm = TRUE),
    non_displaced_severity_max = max(ndp_sev_total_joint, na.rm = TRUE),
    .groups = "drop"
  )

humanitarian_needs_clean <- overall_oblast %>%
  left_join(idps_oblast, by = c("adm1_en", "adm1_pcode")) %>%
  left_join(wand_oblast, by = c("adm1_en", "adm1_pcode")) %>%
  arrange(desc(people_in_need))

glimpse(humanitarian_needs_clean)

write_csv(
  overall_oblast,
  file.path(processed_dir, "overall_oblast_clean.csv")
)

write_csv(
  idps_oblast,
  file.path(processed_dir, "idps_oblast_clean.csv")
)

write_csv(
  wand_oblast,
  file.path(processed_dir, "wand_oblast_clean.csv")
)

write_csv(
  humanitarian_needs_clean,
  file.path(processed_dir, "humanitarian_needs_clean.csv")
)
