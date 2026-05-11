source("00_libraries.R")
source("01_utils.R")


ae_data_sum <- local({
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

 ae_data %>%
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
})



