source("00_libraries.R")
source("01_utils.R")


ae_data_sum <- local({
year_range <- 2020:2026

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
  
 icb_lkup <- tibble(org_code = ae_data$`Org Code` %>% unique()) %>%
  mutate(icb_code = map_chr(org_code, \(x) ods_info(x) %>% ods_icb_extract() %>% coalesce("")),
        icb_name = map_chr(icb_code, \(x) ods_info(x) %>% pluck("Organisation") %>% pluck("Name") %>% coalesce("")))
  
  ae_data %>%
  janitor::clean_names() %>%
  pivot_longer(
    cols = c(emergency_admissions_via_a_e_type_1, 
             emergency_admissions_via_a_e_type_2, 
             emergency_admissions_via_a_e_other_a_e_department),
    names_to = "ae_type",
    values_to = "admissions"
  ) %>%
  mutate(ae_type = case_when(
    str_detect(ae_type, "type_1") ~ "Type 1 (Major)",
    str_detect(ae_type, "type_2") ~ "Type 2 (Specialist)",
    TRUE ~ "Type 3 (Minor/Other)"
  ))  %>%
  select(
    period = period,
    parent_org = parent_org,
    org = org_name,
    org_code = org_code,
    ae_type,
    tot_ae_adm = admissions,
    dta_4_12 = patients_who_have_waited_4_12_hs_from_dta_to_admission,
    dta_gt12 = patients_who_have_waited_12_hrs_from_dta_to_admission
  ) %>%
  mutate(
    period = str_to_lower(period),
    across(matches("org", ignore.case = TRUE), str_to_upper),
    period = my(str_remove_all(period, regex("MSitAE-", ignore_case = TRUE))),
    # Since DTA waits usually only occur in Type 1, we ensure they aren't double-counted
    # or misaligned if you join by ae_type later.
    tot_ae_adm = pmax(tot_ae_adm, dta_4_12 + dta_gt12)
  ) %>%
  left_join(icb_lkup, by = "org_code")
  
})




 


