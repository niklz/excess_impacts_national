source("02_scrape_data.R")

ae_impacts <- local({
  # We only compute impacts for type 1 AE deps
  ae_impacts_clean <- ae_data_sum %>%
    filter(ae_type == "Type 1 (Major)")

  # effect sizes (from 2026 paper)
  mort_nnh <- 68.63375

  los_eff <- data.frame(
    estimate = c(27.2195616793324),
    conf.low = c(22.554111464927),
    conf.high = c(31.8850118937377),
    p.value = c(3.04514339395093e-30)
  )

  hours_per_period <- ae_impacts_clean %>%
    distinct(period) %>%
    mutate(
      hours_per_period = lubridate::interval(
        period,
        lubridate::rollforward(period)
      ) /
        dhours(1)
    )

  ae_impacts_clean <- ae_impacts_clean %>%
    mutate(dta_gt4 = dta_4_12 + dta_gt12) %>%
    left_join(hours_per_period, by = "period") %>%
    mutate(
      excess_mort = dta_gt4 / mort_nnh,
      excess_beds = (dta_gt4 * los_eff$estimate[1]) / hours_per_period
    ) %>%
    mutate(across(
      c(excess_mort, excess_beds),
      \(x) x / tot_ae_adm,
      .names = "{.col}_per_adm"
    ))

  # Base time-series tracking structure
  ae_ts_data <- ae_impacts_clean %>%
    mutate(yearmon = yearmonth(period)) %>%
    as_tsibble(index = yearmon, key = c(org)) %>%
    group_by_key() %>%
    fill_gaps() %>%
    tidyr::fill(
      c(tot_ae_adm, dta_gt4, excess_mort, excess_beds),
      .direction = "down"
    ) %>%
    add_count(org)

  # Extract waiter history for all organizations before filtering history size
  waiter_history <- ae_ts_data %>%
    as_tibble() %>%
    group_by(org) %>%
    arrange(yearmon) %>%
    summarise(
      last_3_months_waiters = list(tail(dta_gt4, 3))
    )

  # Run modeling ONLY on organizations with sufficient history
  ae_trends_calculated <- ae_ts_data %>%
    filter(n > 24) %>%
    select(-n) %>%
    model(
      stl_decomposition = STL(
        excess_mort ~ trend() + season(window = "periodic")
      )
    ) %>%
    components() %>%
    as_tibble() %>%
    group_by(org) %>%
    arrange(yearmon) %>%
    summarise(
      latest_raw_value = last(excess_mort),
      latest_pure_trend = last(trend),
      trend_mom_delta = last(trend) - nth(trend, -2),
      last_3_months_trend = list(tail(trend, 3)),
      trend_velocity_pct = map_dbl(last_3_months_trend, function(x) {
        if (any(is.na(x)) || all(x == 0)) {
          return(0)
        }

        slope <- lm(x ~ seq_along(x))$coefficients[2]
        mean_val <- mean(x, na.rm = TRUE)

        if (is.na(slope) || is.na(mean_val) || abs(mean_val) < 1e-5) {
          return(0)
        }

        # --- THE FIX: VOLATILITY GATEKEEPER ---
        # If the baseline trend is tiny, avoid percentage division explosions.
        # Instead, map a small absolute change to a sensible, capped percentage scale.
        if (mean_val < 5) {
          # e.g., an absolute slope of -0.2 becomes -2% instead of -2000%
          return(slope * 10)
        }

        # Standard percentage calculation for robust baselines
        return((slope / mean_val) * 100)
      })
    )

  # Combine calculations with waiter profiles and calculate final metrics
  ae_trends_final <- waiter_history %>%
    left_join(ae_trends_calculated, by = "org") %>%
    mutate(
      is_low_volume = map_lgl(
        last_3_months_waiters,
        ~ mean(.x, na.rm = TRUE) < 50
      ),

      status_arrow = case_when(
        # If an organization was dropped due to short history, trend_velocity_pct is NA
        is.na(trend_velocity_pct) ~ "⬦ Insufficient Data",
        is_low_volume ~ "⬦ Low Baseline",
        trend_velocity_pct > 1 ~ "▲ Growth",
        trend_velocity_pct < -1 ~ "▼ Decline",
        TRUE ~ "■ Stable"
      )
    ) %>%
    select(org, trend_velocity_pct, status_arrow)

  # Map back to the full structural history cleanly
  left_join(ae_impacts_clean, ae_trends_final, by = "org")
})

write_csv(ae_impacts, "data/ae_impacts.csv")