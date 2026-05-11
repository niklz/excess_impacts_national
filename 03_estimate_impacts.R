require(dplyr)
require(purrr)
require(here)
require(glue)
require(stringr)
require(lubridate)
require(stringr)
require(lubridate)
require(tidyr)
require(readr)
require(ggplot2)
library(RODBC)

start <- ymd("2024-12-01")
end <- ymd("2025-11-30")



# Proportion of DTA < 4 hours that are medical patients
con_string <- "driver={SQL Server};\n  server=xsw-000-sp09;\n  trusted_connection=true"
con<-odbcDriverConnect(con_string)

ecds_dat <-glue::glue("select 
                        NHS_Number,
                        Arrival_Date,
                        Arrival_Time,
                        Decision_To_Admit_Date,
                        Decision_To_Admit_Time,
                        Departure_Date,
                        Departure_Time,
                        Conclusion_Date,
                        Conclusion_Time,
                        Site_Name,
                        Treatment_Function_Code
      FROM Analyst_SQL_Area.dbo.tbl_BNSSG_ECDS
      where Site_Name in ('BRISTOL ROYAL INFIRMARY', 'SOUTHMEAD HOSPITAL', 'WESTON GENERAL HOSPITAL')
      and Arrival_Date between '{start}' and '{end}'
      --and destination_desc = 'Discharge to ward'
      and coalesce(destination_desc, 'Missing') not in ('Emergency department discharge to emergency department short stay ward',
      'Emergency department discharge to same day emergency care service',
      'Emergency department discharge to ambulatory emergency care service',
      'Emergency department discharge to neonatal intensive care unit',
      'Emergency department discharge to special care baby unit',
      'Admission to the mortuary',
      'Discharge to home',
      'Discharge to hospital at home service',
      'Discharge to nursing home',
      'Discharge to residential home',
      'Discharge to police custody',
      'Patient discharge, to legal custody',
      'Urgent admission to hospice'
      )
      ") %>%
  sqlQuery(con, query = .)


ecds_dat %>%
  mutate(tfc = ifelse(Treatment_Function_Code %in% c(300, 326, 430), "medical", "not medical")) %>% 
  filter(!is.na(Decision_To_Admit_Date)) %>%
  mutate(dta_dt=paste0(Decision_To_Admit_Date," ",substr(Decision_To_Admit_Time,1,8))) %>%
  mutate(dta_dt=as.POSIXct(ifelse(grepl("NA",dta_dt),NA,dta_dt),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(departure_dt=as.POSIXct(paste0(Departure_Date," ",substr(Departure_Time,1,8)),format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(dta_time = interval(dta_dt, departure_dt)/dminutes(1)) %>%
  mutate(dta_gt4 = dta_time > 4*60) %>% 
  summarise(prop_medical = mean(tfc == "medical"), .by = c(dta_gt4)) %>% 
  arrange(dta_gt4) %>%
  filter(dta_gt4) %>%
  pull(prop_medical) # 75%

prop_medical <- 0.75
  

ae_data_sum <- readRDS("data/ae_data_sum.RDS")

# mort model
mort_model <- readRDS("data/mort_effects_output_multilvl.RDS")
readm_model <- readRDS("data/readm_effects_output_multilvl.RDS")
los_model <- readRDS("data/los_outputs_reformulated_multilvl.RDS")

mod_df <- los_model$mod_df

los_eff <- los_model$mod_full %>%
  update(. - los + I(24*los) ~ . -dta_time + I(dta_time>4)) %>%
  broom.mixed::tidy(conf.int = TRUE) %>%
  filter(str_detect(term, "dta_time")) %>%
  select(estimate, conf.low, conf.high, p.value)

# Assumptions to tackle
# 1. Our figure is for medical patients, can we adjust with our 2/3 proportion measured in bristol?

# hours_per_period = ((dyears(1)/dhours(1))/12) 

# ae_data_sum %>%
#   mutate(dta_gt4 = dta_4_12 + dta_gt12,
#   excess_mort = dta_gt4 %/% mort_model$nnh) %>%
#   summarise(excess_mort = sum(excess_mort), .by = period) %>%
#   ggplot(aes(x = period, y = excess_mort)) +
#   geom_path() 

# ae_data_sum %>%
#   mutate(dta_gt4 = dta_4_12 + dta_gt12,
#   excess_bed = (dta_gt4 * los_eff$estimate[1])) %>%
#   summarise(excess_bed = sum(excess_bed), .by = period) %>%
#   mutate(excess_bed = excess_bed/hours_per_period) %>%
#   ggplot(aes(x = period, y = excess_bed)) +
#   geom_path() 

dta_gt4_tot <- ae_data_sum %>%
  filter(between(period, start, end)) %>%
  filter(org == "TOTAL") %>%
  mutate(dta_gt4 = dta_4_12 + dta_gt12) %>% # Note that the CSV has 4-12 and 12+ but these are summed in the excel doc to make 4+ and 12+ (here we are reading the CSV)
  summarise(dta_gt4 = sum(dta_gt4)) %>%
  pull(dta_gt4)

# excess
hours_per_period = ((dyears(1)/dhours(1))) # how many hours in a year, this denominator will adjust our total hours into persitiant utilisation

# excess mort/readm is just the total number of DTA > 4 hours modulo the NNH 
# excess LOS is the number of DTA > 4 hours * the coef will computed above, we then divide through by hours in a year to produce excess beds utilised
excess_mort <- dta_gt4_tot %/% mort_model$nnh
excess_readm <- dta_gt4_tot %/% readm_model$nnh
excess_beds <- (dta_gt4_tot * los_eff$estimate[1])/hours_per_period


tibble(
  adm_gt4.eng_full = c(dta_gt4_tot),
  adm_gt4.eng_medical = adm_gt4.eng_full*prop_medical,
  adm_gt4.UK_medical = adm_gt4.eng_full*1.18
) %>%
mutate(across(matches("adm_gt4"),
 list(
  excess_mort = \(x) x %/% mort_model$nnh,
  excess_readm = \(x) x %/% readm_model$nnh,
  excess_beds = \(x) (x * los_eff$estimate[1])/hours_per_period
 ),
  .names = "{.fn}_{.col}")) %>% 
  pivot_longer(cols = everything(), names_sep = "\\.", names_to = c("metric", "grp")) %>%
  pivot_wider(names_from = grp) %>%
  mutate(metric = recode(metric,
     "adm_gt4" = "Total admissions >4 hours",
     "excess_mort_adm_gt4" = "Excess deaths",
     "excess_readm_adm_gt4" = "Excess readmissions",
     "excess_beds_adm_gt4" = "Excess bed days",
    )) %>% show_in_excel()





# UPSTREAM
bnssg_excess <- readRDS("data/excess_impacts_out_10-24_10-25.RDS")

aqi_resp <- readRDS("data/aqi_resp_dat.RDS")
aqi_handover <- readRDS("data/aqi_handover_dat.RDS")
ae_attends <- readRDS("data/ae_attends_sum.RDS")

resp_tot_c1 <- aqi_resp %>%
  filter(between(period, start, end)) %>%
  pull(c1_n) %>%
  as.numeric() %>%
  sum()

resp_tot_c1 <- aqi_resp %>%
  filter(between(period, start, end)) %>%
  pull(c1_n) %>%
  as.numeric() %>%
  sum()

resp_tot_c2 <- aqi_resp %>%
  filter(between(period, start, end)) %>%
  pull(c2_n) %>%
  as.numeric() %>%
  sum()

resp_tot_c3 <- aqi_resp %>%
  filter(between(period, start, end)) %>%
  pull(c3_n) %>%
  as.numeric() %>%
  sum()

handover_tot <- aqi_handover %>%
  filter(between(period, start, end)) %>%
  pull(handover_n) %>%
  as.numeric() %>%
  sum()

ae_attends_tot <- ae_attends %>%
  filter(between(period, start, end)) %>%
  pull(ae_tot) %>%
  as.numeric() %>%
  {
  sum(.)*(12/length(.)) # fix missing months
  }

# BNSSG pop/National pop factor
pop_uplift <- 69.23E6 / 1.1E6

excess_c1_hours <-bnssg_excess  %>%
  filter(impact == "om1a_full_raw") %>%
  pull(value) %>%
  `/`(60) %>%
  `*`(pop_uplift)

excess_c2_hours <- bnssg_excess %>%
  filter(impact == "om1b_full_raw") %>%
  pull(value) %>%
  `/`(60) %>%
  `*`(pop_uplift)

excess_c3_hours <- bnssg_excess %>%
  filter(impact == "om1c_full_raw") %>%
  pull(value) %>%
  `/`(60) %>%
  `*`(pop_uplift)

c1_excess_per <- excess_c1_hours/resp_tot_c1
c2_excess_per <- excess_c2_hours/resp_tot_c2
c3_excess_per <- excess_c3_hours/resp_tot_c3


excess_handover_hours <- bnssg_excess %>%
  filter(impact == "om2_full_raw", grp == "System total") %>%
  pull(value) %>%
  `/`(60) %>%
  `*`(pop_uplift)

handover_excess_per <- excess_handover_hours/handover_tot

ed_time_excess <- bnssg_excess %>%
  filter(impact == "om2_full_raw", grp == "System total") %>%
    pull(value) %>%
      `/`(60) %>%
        `*`(pop_uplift)
      
ed_time_excess_per <- ed_time_excess/ae_attends_tot

tibble(
  grp = c(rep("direct", 3), rep("indirect", 5)), 
  impact = c("mort", "readm", "beds", "resp c1 (hours)", "resp c2 (hours)", "resp c3 (hours)", "handover (hours)", "ed_los (hours)"),
  value = c(excess_mort, excess_readm, excess_beds, excess_c1_hours, excess_c2_hours, excess_c3_hours, excess_handover_hours, ed_time_excess),
  value_per = c(excess_mort, excess_readm, excess_beds, c1_excess_per, c2_excess_per, c3_excess_per, handover_excess_per, ed_time_excess_per)
) %>% show_in_excel()
