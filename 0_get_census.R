library(tidyverse)
library(tidycensus)
library(tidylog)

# ACS 5-year estimates, population and employment status by metropolitan area
acs_metro <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = c("B23025_001", "B23025_002", "B23025_003"),
  year = 2022,
  survey = "acs5", 
  geometry = TRUE
)

saveRDS(acs_metro, "data_raw/acs_metro.rds")


# get pums for phd for all states

phd_pums <- get_pums(
  variables = c("PUMA", "FOD1P", "FOD2P"),
  state = "all",
  year = 2022,
  survey = "acs1",
  recode = TRUE
)

saveRDS(phd_pums, "data_raw/phd_pums.rds")

# devtools::install_github("BrookingsInstitution/metro-data-warehouse")
library(metro.data)

# cs phd
phd_pums <- readRDS("~/Documents/GitHub/brookings_metro/metro-ai-readiness/data_raw/phd_pums.rds")

phd_cbsa <- phd_pums |> 
  filter(FOD1P %in% c("2100", "2101", "2102", "2105") | FOD2P %in% c("2100", "2101", "2102", "2105")) |> 
  count(ST, PUMA, wt = PWGTP) |> 
  left_join(puma2county, by = c("PUMA" = 'puma_code', 'ST' = 'st_code')) |> 
  left_join(metro.data::county_cbsa_st_18, by = "stco_code" ) |> 
  filter(cbsa_type != 'micro') |>
  group_by(cbsa_code, cbsa_name, cbsa_emp, cbsa_type, cbsa_pop) |>
  summarise(n = sum(n * afact1, na.rm = TRUE)) |> 
  mutate(pct_cs_phd = n / cbsa_pop)
