source("02_scrape_data.R")

ae_impacts <- local({
  # effect sizes (from 2026 paper)
  mort_nnh <- 68.63375

  # Note these LOS effects are for ALL DTA > 4, which is why the effect is larger than in the paper (which repressents the associated increase per 4 hour)
  los_eff <- data.frame(
    estimate = c(27.2195616793324),
    conf.low = c(22.554111464927),
    conf.high = c(31.8850118937377),
    p.value = c(3.04514339395093e-30)
  )

  hours_per_period <- ae_data_sum %>%
    distinct(period) %>%
    mutate(
      hours_per_period = lubridate::interval(
        period,
        lubridate::rollforward(period)
      ) / dhours(1)
    )

  ae_data_sum %>%
    mutate(dta_gt4 = dta_4_12 + dta_gt12) %>% # Note that the CSV has 4-12 and 12+ but these are summed in the excel doc to make 4+ and 12+ (here we are reading the CSV)
    left_join(hours_per_period) %>%
    mutate(
      excess_mort = dta_gt4 / mort_nnh,
      excess_beds = (dta_gt4 * los_eff$estimate[1]) / hours_per_period
    ) %>%
      mutate(across(c(excess_mort, excess_beds), \(x) x/tot_ae_adm, .names = "{.col}_per_adm"))
})
