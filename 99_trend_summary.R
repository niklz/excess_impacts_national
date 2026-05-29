library(tsibble)
library(feasts)
library(fable)

foo <- ae_impacts %>%
  filter(org != "Total" == "Type 1 (Major)")


ae_trends <- foo %>%
  mutate(yearmon = yearmonth(period)) %>%
  as_tsibble(index = yearmon, key = c(org))%>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(c(tot_ae_adm, dta_gt4, excess_mort, excess_beds), .direction = "down") %>%
  add_count(org) %>% 
  filter(n > 24) %>% 
  select(-n) %>%
  model(stl_decomposition = STL(excess_mort ~ trend() + season(window = "periodic"))) %>%
  components() %>% # Drop time-series restrictions for standard aggregation
  as_tibble() %>%
  group_by(org) %>%
  arrange(yearmon) %>%
  summarise(
    latest_raw_value = last(excess_mort),
    latest_pure_trend = last(trend),
    trend_mom_delta = last(trend) - nth(trend, -2),
    last_3_months_trend = list(tail(trend, 3)),
    trend_velocity = map_dbl(last_3_months_trend, ~ lm(.x ~ seq_along(.x))$coefficients[2])
  ) %>%
  mutate(
    status_arrow = case_when(
      trend_velocity > 0.5  ~ "↑ Growth",
      trend_velocity < -0.5 ~ "↓ Decline",
      TRUE                  ~ "→ Stable"
    )
  )


ae_diagnostics <- foo %>%
  mutate(yearmon = yearmonth(period)) %>%
  as_tsibble(index = yearmon, key = c(org)) %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(c(tot_ae_adm, dta_gt4, excess_mort, excess_beds), .direction = "down") %>%
  add_count(org) %>% 
  filter(n > 24) %>% 
  select(-n) %>%
  model(stl_decomposition = STL(excess_mort ~ trend() + season(window = "periodic"))) %>%
  components() %>% 
  as_tibble() %>%
  
  # --- DIAGNOSTIC WINDOW FUNCTIONS ---
  group_by(org) %>%
  arrange(yearmon) %>%
  mutate(
    # Capture the ultimate velocity of the key to tag historical rows
    final_trend_velocity = map_dbl(list(tail(trend, 3)), ~ lm(.x ~ seq_along(.x))$coefficients[2]),
    status_group = case_when(
      final_trend_velocity > 0.5  ~ "Grower",
      final_trend_velocity < -0.5 ~ "Decliner",
      TRUE                  ~ "Stable"
    )
  ) %>%
  ungroup() %>%
  # Re-convert to tsibble so feasts/ggplot understand the time dimension
  as_tsibble(index = yearmon, key = c(org))


# Visualise the underlying trend lines of all declining keys
ae_diagnostics %>%
  filter(status_group == "Decliner") %>%
  ggplot(aes(x = yearmon, y = trend, color = org)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    title = "Diagnostic View: Long-Term Decliners (Core Trend Only)",
    x = "Timeline", y = "Isolated Trend Value"
  ) +
  theme(legend.position = "none") # Hide legend if there are too many hospital names


target_org <- "Barking, Havering and Redbridge University Hospitals NHS Trust"

ae_diagnostics %>%
  filter(org == target_org) %>%
  ggplot(aes(x = yearmon)) +
  # Raw data (noisy, seasonal)
  geom_line(aes(y = excess_mort), color = "grey70", linetype = "dashed") + 
  geom_point(aes(y = excess_mort), color = "grey50", alpha = 0.6) +
  # Extracted trend (smooth dashboard baseline)
  geom_line(aes(y = trend), color = "firebrick", size = 1.2) + 
  theme_minimal() +
  labs(
    title = paste("Audit for:", target_org),
    subtitle = "Dashed Grey = Raw Monthly Value | Solid Red = Extracted Trend",
    x = "Month", y = "Excess Mortality"
  )
