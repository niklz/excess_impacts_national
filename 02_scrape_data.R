source("00_libraries.R")
source("01_utils.R")


# NUMBER DTAS and ED attends

year_range <- 2020:2026

# NOTE: this changes each year and I'm not 100% on the logic of generating the
# right URL

url_stem <- glue::glue("https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-20{(year_range[-length(year_range)] - 2000)}-{(year_range[-1] - 2000)}/")


urls_csv <- map(url_stem, \(url) str_subset(html_attr(html_nodes(read_html(url), "a"), "href"), "csv")) %>%
  reduce(c)

ae_data <- map(urls_csv, \(x, y){
  read_csv(x) %>%
    mutate(url = x,
           Period = first(Period) # Remove "TOTAL" flag from period which is duplicated in org
    )
}) %>%
  bind_rows()


ae_data$`Org Code`[1] %>%
  ods_lookup()


urls_xls <- map(url_stem, \(url) str_subset(html_attr(html_nodes(read_html(url), "a"), "href"), "xls$")) %>%
  reduce(c) %>%
  str_subset(pattern = regex("supplementary|quarter", ignore_case = TRUE), negate = TRUE) %>%
  str_subset(pattern = regex("provider", ignore_case = TRUE))


ae_data_sum <- ae_data %>%
  mutate(tot_ae_adm = `Emergency admissions via A&E - Type 1` +	`Emergency admissions via A&E - Type 2` +	`Emergency admissions via A&E - Other A&E department`) %>%
  select(
    period = Period, 
    parent_org = `Parent Org`,
    org = `Org name`,
    tot_ae_adm,
    dta_4_12 = `Patients who have waited 4-12 hs from DTA to admission`,
    dta_gt12 = `Patients who have waited 12+ hrs from DTA to admission`
  ) %>%
  mutate(
    period = str_to_lower(period),
    across(matches("org", ignore.case = TRUE), str_to_upper),
    period = my(str_remove_all(period, regex("MSitAE-", ignore_case = TRUE))) 
  ) %>%
  arrange(period) 



# # AE attends
# read_xls_safe <- purrr::safely(readxl::read_xls)

# ae_attends_sum <- bind_rows(
#   map_dfr(urls_xls[1:24], ~{
#     tmp <- tempfile(fileext = ".xls")
#     on.exit(unlink(tmp))
#     download.file(.x,
#                   destfile = tmp,
#                   quiet = TRUE,
#                   mode = "wb")
    
#     dat <- read_xls_safe(path.expand(tmp), col_names = FALSE)
#     period <- dat$result %>% filter(...1 == "Period:") %>% pull(...2)
#     ae_tot <- dat$result %>% filter(...1 == "-", ...2 == "-", ...3 == "England") %>% pull(...4) 
#     tibble(period = my(period), ae_tot = ae_tot)}),
  
#   map_dfr(urls_xls[25:length(urls_xls)], ~{
#     tmp <- tempfile(fileext = ".xls")
#     on.exit(unlink(tmp))
#     download.file(.x,
#                   destfile = tmp,
#                   quiet = TRUE,
#                   mode = "wb")
    
#     dat <- read_xls_safe(path.expand(tmp), col_names = FALSE)
#     period <- dat$result %>% filter(...1 == "Period:") %>% pull(...2)
#     ae_tot <- dat$result %>% filter(...1 == "-", ...2 == "-") %>% pull(...3) 
#     tibble(period = my(period), ae_tot = ae_tot)})
# ) %>%
#   arrange(period) 


# # NUMBER AMBULANCE RESPONSE AND HANDOVER

# readxl::read_excel(
#   col_names = FALSE,
#   path = "data/AmbSYS-Time-Series-to-20250930.xlsx",
#   sheet = "Response times",
#   skip = 15,
#   n_max = 98) %>%
#   select(period = ...1, c1_n = ...5, c1t_n = ...11, c2_n = ...17, c3_n = ...23, c4_n = ...29) %>% 
#   # fix trailing NAs
#   mutate(period = case_when(is.na(period) ~ max(period, na.rm = TRUE) + months(cumsum(is.na(period))), .default = period)) %>% 
#   saveRDS(file = "data/aqi_resp_dat.RDS")


# readxl::read_excel(
#   col_names = FALSE,
#   path = "data/AmbSYS-Time-Series-to-20250930.xlsx",
#   sheet = "Handovers",
#   skip = 89) %>%
#   select(period = ...1, handover_n = ...5)  %>% 
#   mutate(period = case_when(is.na(period) ~ max(period, na.rm = TRUE) + months(cumsum(is.na(period))), .default = period)) %>% 
#   saveRDS(file = "data/aqi_handover_dat.RDS")

