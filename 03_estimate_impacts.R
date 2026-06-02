source("02_scrape_data.R")

ae_impacts <- local({

  # We only compute impacts for type 1 AE deps
  ae_impacts <- ae_data_sum %>%
    filter(ae_type == "Type 1 (Major)")

  # effect sizes (from 2026 paper)
  mort_nnh <- 68.63375

  # Note these LOS effects are for ALL DTA > 4, which is why the effect is larger than in the paper (which repressents the associated increase per 4 hour)
  los_eff <- data.frame(
    estimate = c(27.2195616793324),
    conf.low = c(22.554111464927),
    conf.high = c(31.8850118937377),
    p.value = c(3.04514339395093e-30)
  )

  hours_per_period <- ae_impacts %>%
    distinct(period) %>%
    mutate(
      hours_per_period = lubridate::interval(
        period,
        lubridate::rollforward(period)
      ) / dhours(1)
    )
  

  ae_impacts <- ae_impacts %>%
    mutate(dta_gt4 = dta_4_12 + dta_gt12) %>% # Note that the CSV has 4-12 and 12+ but these are summed in the excel doc to make 4+ and 12+ (here we are reading the CSV)
    left_join(hours_per_period) %>%
    mutate(
      excess_mort = dta_gt4 / mort_nnh,
      excess_beds = (dta_gt4 * los_eff$estimate[1]) / hours_per_period
    ) %>%
      mutate(across(c(excess_mort, excess_beds), \(x) x/tot_ae_adm, .names = "{.col}_per_adm"))

  
  ae_trends <- ae_impacts %>%
    mutate(yearmon = yearmonth(period)) %>%
    as_tsibble(index = yearmon, key = c(org)) %>%
    group_by_key() %>%
    fill_gaps() %>%
    tidyr::fill(
      c(tot_ae_adm, dta_gt4, excess_mort, excess_beds),
      .direction = "down"
    ) %>%
    add_count(org) %>%
    filter(n > 24) %>%
    select(-n) 
  
  
waiter_history <- ae_trends %>%
  as_tibble() %>%
  group_by(org) %>%
  arrange(yearmon) %>%
  summarise(
    last_3_months_waiters = list(tail(dta_gt4, 3))
  )

  
  ae_trends <- ae_trends %>%
    model(
      stl_decomposition = STL(
        excess_mort ~ trend() + season(window = "periodic")
      )
    ) %>%
    components() %>% # Drop time-series restrictions for standard aggregation
    as_tibble() %>%
    group_by(org) %>%
    arrange(yearmon) %>%
    summarise(
      latest_raw_value = last(excess_mort),
      latest_pure_trend = last(trend),
      trend_mom_delta = last(trend) - nth(trend, -2),
      last_3_months_trend = list(tail(trend, 3)),
      trend_velocity_pct = map_dbl(last_3_months_trend, function(x) {
        slope <- lm(x ~ seq_along(x))$coefficients[2]
        mean_val <- mean(x, na.rm = TRUE)
if (is.na(mean_val) || abs(mean_val) < 1e-5) {
    return(0)
  }
        return((slope / mean_val) * 100) # Percentage growth velocity
      })
    ) %>%
    # mutate(
    #   # Find the 1st and 99th percentile thresholds across all organizations
    #   p01 = quantile(trend_velocity_pct, 0.01, na.rm = TRUE),
    #   p99 = quantile(trend_velocity_pct, 0.99, na.rm = TRUE),

    #   # Clamp the velocity values into those boundaries
    #   trend_velocity_pct = pmax(p01, pmin(p99, trend_velocity_pct))
    # ) %>%
    left_join(waiter_history, by = "org") %>%
    mutate(
      is_low_volume = map_lgl(last_3_months_waiters, ~ mean(.x, na.rm = TRUE) < 50),
      status_arrow = case_when(
        is.na(trend_velocity_pct) ~ "⬦ Insufficient Data", # less than 24 records
        is_low_volume         ~ "⬦ Low Baseline",
        trend_velocity_pct > 1 ~ "▲ Growth",
        trend_velocity_pct < -1 ~ "▼ Decline",
        TRUE ~ "■ Stable"
      )
    ) %>%
    ungroup() %>%
    select(org, trend_velocity_pct, status_arrow)

   
  left_join(ae_impacts, ae_trends, by = join_by(org == org))
  
})

write_csv(ae_impacts, "data/ae_impacts.csv")
