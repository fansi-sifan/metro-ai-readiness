library(googledrive)
library(googlesheets4)
library(tidygeocoder)
library(tidyverse)
library(tidycensus)
library(tidylog)

# SETUP ========================================================================
drive_auth(email = "sifan1121@gmail.com")
gs4_auth(token = drive_token())
url <- "https://docs.google.com/spreadsheets/d/19NWvZ3kkDv2KYPvd9QdC6wH0vMQlXeD6NPty1OrjpUM/edit?gid=0#gid=0"

# LOAD DATA ====================================================================

acs_metro <- readRDS("data_raw/acs_metro.rds")
phd_pums <- readRDS("~/Documents/GitHub/brookings_metro/metro-ai-readiness/data_raw/phd_pums.rds")
puma2county_raw <- read_csv("data_raw/puma2county.csv")

puma2county <- puma2county_raw %>%
  # remove the long-form variable names in the second row
  slice(-1) %>%
  # because of this second header, numerics were coerced to character
  # change them back
  mutate_at(.vars = vars(contains("pop"),
                         contains("afact"),
                         contains("AFACT")),
            .funs = as.numeric) %>%
  # select needed columns and rename
  select(stco_code = county,
         st_code = state,
         st_ab = stab,
         puma_code = puma22,
         puma_name = PUMA22name,
         stco_name = CountyName,
         pl_pop20 = pop20,
         afact1 = afact)

# CLEAN DATA ===================================================================
# cs phd -------

phd_cbsa <- phd_pums |> 
  filter(FOD1P %in% c("2100", "2101", "2102", "2105") | FOD2P %in% c("2100", "2101", "2102", "2105")) |> 
  count(ST, PUMA, wt = PWGTP) |> 
  left_join(puma2county, by = c("PUMA" = 'puma_code', 'ST' = 'st_code')) |> 
  left_join(metro.data::county_cbsa_st_18, by = "stco_code" ) |> 
  filter(cbsa_type != 'micro') |>
  group_by(cbsa_code) |>
  summarise(phd_count = sum(n * afact1, na.rm = TRUE)) 

# cs ba------
cs_ba <- read_sheet(url, range = "CS degree", skip = 3)
colnames(cs_ba) <- c("cbsa_code", "cbsa_name", "sci_eng_ba", "cs_math_ba")

cs_ba_cbsa <- cs_ba |> 
  select(-cbsa_name) |> 
  mutate(cbsa_code = str_sub(cbsa_code, -5,-1)) |>
  left_join(metro.data::cbsa_18, by = "cbsa_code") 

# ai jobs ------
jobs <- read_sheet(url, range = "AI job postings and profiles")
# jobs <- read_csv('data_raw/Job_Postings_Map_20_Skills_and_Qualifications_in_United_States_6415.csv')
colnames(jobs) <- c("cbsa_code", "cbsa_name", "ai_job_postings_22", "ai_job_postings_23", "ai_job_profiles")

jobs_cbsa <- jobs |> 
  mutate(cbsa_code = as.character(cbsa_code)) |> 
  select(-cbsa_name)

# ai patent ------
patent <- read_csv("data_raw/data_ai_UScounty_2periods.csv")

patent_cbsa <- patent |> 
  left_join(metro.data::county_cbsa_st_18, by = c("county_fips" = "stco_code")) |>
  filter(cbsa_type != 'micro') |>
  group_by(cbsa_code) |>
  summarise(ai_patent = sum(n_ai_uscounty_2001_2020)) 

# ai contracts ----
# https://files.usaspending.gov/generated_downloads/PrimeAwardSummariesAndSubawards_2024-07-29_H04M56S34994439.zip
# contracts <- read_sheet(url, range = "AI contracts", skip = 1)
# contracts |> 
#   left_join(metro.data::cbsa_18, by = c("MSA" = "cbsa_name"))

contracts <- read_csv("data_raw/PrimeAwardSummariesAndSubawards_2024-07-29_H04M35S52324709/Contracts_PrimeAwardSummaries_2024-07-29_H04M35S53_1.csv")
contracts_cbsa <- contracts |> 
  select(stco_code = prime_award_summary_place_of_performance_county_fips_code, 
         award_type, 
         year = award_base_action_date_fiscal_year, 
         total_obligated_amount) |>
  filter(year >= 2020) |> 
  left_join(metro.data::county_cbsa_st_18, by = c("stco_code" = "stco_code")) |>
  group_by(cbsa_code) |>
  filter(cbsa_type != 'micro') |>
  summarise(ai_contract_value = sum(total_obligated_amount),
            ai_contract_count = n()) 
# ai publications ----
publications <- read_sheet(url, "AI_publications")
ai_pub <- publications |> 
  geocode_combine(
    queries = list(
      list(method = 'osm', address = 'Institute'),
      list(method = "arcgis", address = "Institute")
    )
  )
  
sf_ai_pub <- st_as_sf(ai_pub  %>% filter(!is.na(lat)), 
                    coords = c("long", "lat"), crs = st_crs(metro_sf))

inset_ai_pub <- st_intersection(metro_sf, sf_ai_pub)

ai_pub_cbsa <- ai_pub |> 
  left_join(inset_ai_pub |> distinct(Institute, GEOID), by = "Institute") |> 
  rename(cbsa_code = GEOID) |> 
  group_by(cbsa_code) |> 
  summarise(ai_publications = sum(`Adjusted Publications`))

# ai data readiness ----
ai_data <- read_sheet(url, "AI data readiness", skip = 1)
ai_data_cbsa <- ai_data |> 
  select(cbsa_name = metro,
         index_ai_data_readiness = Mean)

ai_cloud <- read_sheet(url, "ai_cloud_adoption", skip = 1)
ai_cloud_cbsa <- ai_cloud |> 
  select(cbsa_name = metro,
         index_ai_cloud_readiness = Mean)

ai_tech <- read_sheet(url, "ai_tech_adoption", skip = 1)
ai_tech_cbsa <- ai_tech |> 
  select(cbsa_name = metro,
         index_ai_tech_readiness = Mean)

# ai companies ----
company <- read_sheet(url, "AI companies", skip = 1)
company_cbsa <- company |> 
  mutate(cbsa_code = as.character(`MSA CODE`)) 

# gpu hours ----
# gpu <- read_csv("data_raw/GPU_Hours__Total__by_PI_Institution_2021-07-12_to_2024-07-12_timeseries.csv", skip = 8)
gpu <- read_sheet(url, "GPU_hours", skip = 9)
uni_gpu <- gpu |> 
  pivot_longer(cols = -Month, names_to = "month", values_to = "hours") |> 
  rename(institution = Month) |> 
  mutate(year = str_sub(month, 1, 4)) |>
  filter(year != "2024") |> 
  # group_by(institution, year) |>
  # summarise(total_hours = sum(hours)) |>
  # pivot_wider(names_from = year, values_from = total_hours)
  group_by(institution) |>
  summarise(total_hours_21_23 = sum(hours)) 

# hpc usage ----
hpc <- read_sheet(url, "HPC_usage", skip = 7)
uni <- hpc |> 
  full_join(uni_gpu, by = c("PI Institution" = "institution"))

# use tidygeocoder to geocode PI institution
uni_coded <- uni |> 
  distinct(`PI Institution`) |> 
  geocode_combine(
    queries = list(
      list(method = 'osm', address = 'PI Institution'),
      list(method = "arcgis", address = "PI Institution")
    )
  )


# SPATIAL JOINS -----
library(sf)

# find insets=============
metro_sf <- acs_metro |> 
  distinct(GEOID, geometry) |>
  st_as_sf(crs = "ESRI:102009") 

df_sf <- st_as_sf(uni_coded %>% filter(!is.na(lat)), 
                  coords = c("long", "lat"), crs = st_crs(metro_sf))

inset <- st_intersection(metro_sf, df_sf)

uni_cbsa <- uni |> 
  left_join(uni_coded, by = 'PI Institution') |> 
  left_join(inset |> distinct(GEOID, PI.Institution), by = c("PI Institution" = "PI.Institution")) %>%
  rename(cbsa_code = GEOID) |> 
  group_by(cbsa_code) |> 
  summarise(hpc_credits = sum(`ACCESS Credit Equivalents Charged: Total (SU)`, na.rm = T),
            gpu_hours = sum(total_hours_21_23, na.rm = T))


# MERGE ------
ai_results <- cs_ba_cbsa |> 
  filter(cbsa_size %in% c('large metros', "very large metros", "midsized metros")) |> 
  left_join(phd_cbsa, by = "cbsa_code") |> 
  left_join(jobs_cbsa, by = "cbsa_code") |> 
  left_join(patent_cbsa, by = "cbsa_code") |>
  left_join(contracts_cbsa, by = "cbsa_code") |>
  left_join(ai_data_cbsa, by = "cbsa_name") |>
  left_join(ai_tech_cbsa, by = "cbsa_name") |>
  left_join(ai_cloud_cbsa, by = 'cbsa_name') |> 
  left_join(ai_pub_cbsa, by ='cbsa_code') |>
  left_join(company_cbsa, by = "cbsa_code") |>
  left_join(uni_cbsa, by = "cbsa_code") |>
  mutate(index_cs_phd = phd_count / cbsa_emp)|>
  mutate(index_cs_eng_sci_ba = cs_math_ba / cbsa_emp) |> 
  mutate(index_ai_job_postings = (ai_job_postings_22 + ai_job_postings_23) / cbsa_emp,
         index_ai_job_profiles = ai_job_profiles / cbsa_emp) |> 
  mutate(index_ai_patent = ai_patent / cbsa_emp * 1000) |>
  mutate(index_ai_contracts = ai_contract_value / cbsa_emp) |>
  mutate(index_ai_publications = ai_publications / cbsa_emp * 1000) |>
  mutate(index_ai_companies = `Number of companies providing AI solutions` / cbsa_emp * 1000) |>
  mutate(index_gpu_hours = gpu_hours / cbsa_emp,
         index_hpc_usage = hpc_credits / cbsa_emp) 

ai_results |> 
  filter(cbsa_type != "micro") |> 
  select(starts_with('cbsa'), starts_with('index'), everything()) |>
  write_sheet(url, "ai_results")

