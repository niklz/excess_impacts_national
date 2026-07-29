source("02_scrape_data.R")

ae_impacts <- local({
  # --- PARAMETERS ---
  trend_window_months <- 12        # Observation window for trend calculation
  status_cutoff <- 2.5             # Minimum percentage change required (e.g., 2.5%)
  volatility_threshold <- 0.25      # Max allowed IQR-to-Median ratio (50% dispersion)
  # ------------------
  
  # We only compute impacts for type 1 AE deps, however delays are reported for
  # types 1, 2 & 3, so we must deduct any admissions from types 2 & 3 from the
  # total delayed
  
  ae_impacts_clean <- ae_data_sum %>%
    # compute total delayed > 4 hours
    mutate(dta_gt4 = dta_4_12 + dta_gt12) %>%
    # subtract any admissions from types 2 & 3
    mutate(ae_adm_typ23 = sum(tot_ae_adm[ae_type %in% c("Type 2 (Specialist)", "Type 3 (Minor/Other)")], na.rm = TRUE), .by = c(org, period)) %>%
    mutate(dta_gt4 = pmax(0, dta_gt4 - ae_adm_typ23), .by = c(org, period)) %>%
    filter(ae_type == "Type 1 (Major)") %>%
    select(-ae_adm_typ23)
  
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
  
  # Extract waiter history dynamically based on the parameter
  waiter_history <- ae_ts_data %>%
    as_tibble() %>%
    group_by(org) %>%
    arrange(yearmon) %>%
    summarise(
      recent_waiters = list(tail(dta_gt4, trend_window_months))
    )
  
  # Run modeling on organizations with sufficient history
  ae_trends_calculated <- ae_ts_data %>%
    filter(n > 48) %>%
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
      
      # 1. EXTRACT DYNAMIC WINDOW
      recent_trend = list(tail(trend, trend_window_months)),
      
      # 2. HISTORICAL CONTEXT (Data prior to the recent window)
      # historical_baseline = mean(head(trend, -trend_window_months), na.rm = TRUE),
      historical_baseline = quantile(head(trend, -trend_window_months), 0.75, na.rm = TRUE),
      # 3. RECENT REALITY (Where the metric actually sits right now)
      recent_reality = mean(tail(trend, 3), na.rm = TRUE),
      
      # 4. VOLATILITY METRICS (IQR and Median of the recent window)
      recent_iqr = IQR(tail(trend, trend_window_months), na.rm = TRUE),
      recent_median = median(tail(trend, trend_window_months), na.rm = TRUE),
      
      # 5. STATISTICAL CONSERVATISM: Mann-Kendall & Sen's Slope
      trend_metrics = map(recent_trend, function(x) {
        if (any(is.na(x)) || length(x) < trend_window_months || var(x, na.rm = TRUE) == 0) {
          return(list(p_val = 1, tau = 0, pct_change = 0))
        }
        
        # Run Mann-Kendall Test
        mk_result <- Kendall::MannKendall(x)
        p_val <- as.numeric(mk_result$sl[1])
        tau   <- as.numeric(mk_result$tau[1])
        
        # Calculate robust magnitude via Sen's Slope
        sen_fit <- trend::sens.slope(ts(x))
        sen_slope <- as.numeric(sen_fit$estimates)
        mean_val <- mean(x, na.rm = TRUE)
        
        # Calculate percentage change
        pct_change <- if (abs(mean_val) > 1e-5) (sen_slope / mean_val) * 100 else 0
        
        # Volatility gatekeeper for small baselines
        if (mean_val < 5 && abs(mean_val) > 1e-5) {
          pct_change <- sen_slope * 10 
        }
        
        list(p_val = p_val, tau = tau, pct_change = pct_change)
      }),
      
      # Unpack metrics from the list
      mk_p_val = map_dbl(trend_metrics, ~ .x$p_val),
      mk_tau   = map_dbl(trend_metrics, ~ .x$tau),
      trend_velocity_pct = map_dbl(trend_metrics, ~ .x$pct_change)
    )
  
  # Combine calculations and calculate final metrics
  ae_trends_final <- waiter_history %>%
    left_join(ae_trends_calculated, by = "org") %>%
    mutate(
      # Gatekeeper 1: Low Volume Check
      is_low_volume = map_lgl(
        recent_waiters,
        ~ mean(.x, na.rm = TRUE) < 50
      ),
      
      # Gatekeeper 2: High Volatility Check (Coefficient of Dispersion > threshold)
      is_highly_volatile = if_else(
        abs(recent_median) > 1e-5, 
        (recent_iqr / abs(recent_median)) > volatility_threshold, 
        FALSE
      ),
      
      # Gatekeeper 3: Historical Relevance Check
      exceeds_history = recent_reality > (historical_baseline * (1 + (status_cutoff / 100))),
      below_history   = recent_reality < (historical_baseline * (1 - (status_cutoff / 100))),
      
      # Final Status Mapping
      status_arrow = case_when(
        is.na(mk_p_val) ~ "✖ Insufficient Data",
        is_low_volume ~ "✖ Low Baseline",
        is_highly_volatile ~ "≁ High Volatility", # Traps noisy trusts like RFR
        
        # Growth: Must be statistically monotonic AND fast enough AND historically worse
        mk_p_val < 0.05 & mk_tau > 0 & trend_velocity_pct > status_cutoff & exceeds_history ~ "▲ Growth",
        
        # Decline: Must be statistically monotonic AND fast enough AND historically better
        mk_p_val < 0.05 & mk_tau < 0 & trend_velocity_pct < -status_cutoff & below_history ~ "▼ Decline",
        
        TRUE ~ "■ Stable"
      )
    ) %>%
    select(org, trend_velocity_pct, mk_p_val, mk_tau, is_highly_volatile, status_arrow)
  
  # Map back to the full structural history cleanly
  left_join(ae_impacts_clean, ae_trends_final, by = "org")
})

write_csv(ae_impacts, "data/ae_impacts.csv")