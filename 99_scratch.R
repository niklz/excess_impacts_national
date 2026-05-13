ae_impacts %>%
filter(org != "TOTAL", ae_type == "Type 1 (Major)") %>%
ggplot(aes(x = excess_mort_per_adm)) +
  geom_histogram(col = "white") +
  theme_minimal() +
  scale_x_continuous(breaks = 1/c(50, 100, 200, 400, 800), labels = \(x) str_c("1 in ", round(1/x))) +
  labs(y = "", x = "Excess deaths per A&E admission")

ggplot(filter(ae_impacts, org != "TOTAL"), aes(x = dta_gt4/tot_ae_adm)) +
  geom_histogram(col = "white") +
  theme_minimal() +
  scale_x_continuous(breaks = 1/c(1, 2, 5, 10), labels = \(x) str_c("1 in ", round(1/x))) +
  labs(y = "", x = "DTA . 4hr")


ae_impacts %>%
  filter(org != "TOTAL") %>%
  filter(period > ymd("2025-01-01")) %>%
  summarise(across(c(tot_ae_adm, excess_mort), mean), .by = c(org, icb_name)) %>%
ggplot(aes(y = excess_mort, x = tot_ae_adm, col = icb_name)) +
geom_point() + 
  theme_minimal() +
  theme(legend.position = "off")


ggplot(filter(ae_impacts, org != "TOTAL", icb_code == "QUY"), aes(y = round(excess_mort), x = period)) +
  geom_col(col = "white") +
  theme_minimal() +
  facet_wrap(vars(org), scales = "free_y", ncol = 1)






# Funnel plot


make_dta_funnel_plot <- function(df, type_label = "") {
  # 1. Clean and Filter
  plot_df <- df %>%
    dplyr::mutate(
      dta_gt4 = as.numeric(dta_gt4),
      tot_ae_adm = as.numeric(tot_ae_adm),
      org = as.character(org) 
    ) %>%
    dplyr::filter(!is.na(dta_gt4), !is.na(tot_ae_adm), !is.na(org)) %>%
    dplyr::filter(tot_ae_adm > 0, dta_gt4 <= tot_ae_adm)

  # Circuit breaker for empty data
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = paste("No data for", type_label)) + ggplot2::theme_void())
  }

  # 2. Calculate Statistics
  mu <- sum(plot_df$dta_gt4, na.rm = TRUE) / sum(plot_df$tot_ae_adm, na.rm = TRUE)
  current_rate <- plot_df$dta_gt4 / plot_df$tot_ae_adm
  
  # Calculate Phi
  z_scores <- (current_rate - mu) / sqrt(mu * (1 - mu) / plot_df$tot_ae_adm)
  phi <- mean(z_scores^2, na.rm = TRUE) 
  phi <- max(1, phi, na.rm = TRUE)

  # 3. Generate Funnel Data
  x_min <- base::min(plot_df$tot_ae_adm, na.rm = TRUE)
  x_max <- base::max(plot_df$tot_ae_adm, na.rm = TRUE)
  
  line_df <- data.frame(
    tot_ae_adm = seq(x_min, x_max, length.out = 500)
  ) %>%
    dplyr::mutate(
      logit_mu = log(mu / (1 - mu)),
      logit_se = sqrt(phi) * sqrt(1 / (tot_ae_adm * mu * (1 - mu))),
      se_adj = sqrt(phi) * sqrt(mu * (1 - mu) / tot_ae_adm),
      upper_99_adj = 1 / (1 + exp(-(logit_mu + 3 * logit_se))),
      lower_99_adj = 1 / (1 + exp(-(logit_mu - 3 * logit_se))),
      upper_95_adj = 1 / (1 + exp(-(logit_mu + 1.96 * logit_se))),
      lower_95_adj = 1 / (1 + exp(-(logit_mu - 1.96 * logit_se)))
    )

  # 4. Identify Outliers
  plot_df <- plot_df %>%
    dplyr::mutate(
      rate = dta_gt4 / tot_ae_adm,
      limit_99 = mu + 3 * (sqrt(phi) * sqrt(mu * (1 - mu) / tot_ae_adm)),
      is_outlier = rate > limit_99
    )

  # 5. Plot
  ggplot2::ggplot(plot_df, ggplot2::aes(x = tot_ae_adm, y = rate)) +
    ggplot2::geom_line(data = line_df, ggplot2::aes(y = upper_95_adj), color = "red", linetype = "dashed") +
    ggplot2::geom_line(data = line_df, ggplot2::aes(y = lower_95_adj), color = "red", linetype = "dashed") +
    ggplot2::geom_line(data = line_df, ggplot2::aes(y = upper_99_adj), color = "red", alpha = 0.4) +
    ggplot2::geom_line(data = line_df, ggplot2::aes(y = lower_99_adj), color = "red", alpha = 0.4) +
    ggplot2::geom_hline(yintercept = mu, color = "black", linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(color = is_outlier), alpha = 0.6) +
    ggrepel::geom_text_repel(
      data = subset(plot_df, is_outlier),
      ggplot2::aes(label = org), size = 2.5, box.padding = 0.5, max.overlaps = 20
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::scale_color_manual(values = c("steelblue", "darkred")) +
    ggplot2::labs(
      title = paste("A&E Funnel Plot:", type_label),
      subtitle = paste0("Avg: ", round(mu * 100, 1), "% | Phi: ", round(phi, 2)),
      x = "Total Admissions", y = "Proportion Waiting > 4 Hours"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

make_mort_funnel_plot <- function(df, rate_breaks = c(1/400, 1/200, 1/100, 1/50), type_label = "") {

  breaks <- sort(unique(c(0, rate_breaks, Inf)))
  ribbon_df <- data.frame(
    ymin = breaks[-length(breaks)],
    ymax = breaks[-1],
    # Simple color ramp from green to red
    fill = colorRampPalette(c("#2ecc71", "#f1c40f", "#e74c3c"))(length(breaks)-1)
  )

  # Custom labeling function for the Y-axis
  rate_labeller <- function(x) {
    ifelse(x == 0, "0", paste0("1 in ", round(1/x)))
  }

  # 1. Clean and Filter
  plot_df <- df %>%
    dplyr::mutate(
  dta_gt4 = as.numeric(dta_gt4),
  tot_ae_adm = as.numeric(tot_ae_adm),
  org = as.character(org) 
) %>%
  dplyr::filter(!is.na(dta_gt4), !is.na(tot_ae_adm), !is.na(org)) %>%
    dplyr::filter(tot_ae_adm > 0, dta_gt4 <= tot_ae_adm)
  
  # Circuit breaker for empty data
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = paste("No data for", type_label)) + ggplot2::theme_void())
  }
  
  # Dynamic Y-limit
  y_limit <- max(max(plot_df$rate, na.rm = TRUE) * 1.2, max(rate_breaks))
  # 2. Calculate Statistics
  mu <- sum(plot_df$excess_mort, na.rm = TRUE) / sum(plot_df$tot_ae_adm, na.rm = TRUE)
  current_rate <- plot_df$excess_mort / plot_df$tot_ae_adm
  
  # Calculate Phi
  z_scores <- (current_rate - mu) / sqrt(mu * (1 - mu) / plot_df$tot_ae_adm)
  phi <- mean(z_scores^2, na.rm = TRUE) 
  phi <- max(1, phi, na.rm = TRUE)

  # 3. Generate Funnel Data
  x_min <- base::min(plot_df$tot_ae_adm, na.rm = TRUE)
  x_max <- base::max(plot_df$tot_ae_adm, na.rm = TRUE)
  
  line_df <- data.frame(
    tot_ae_adm = seq(x_min, x_max, length.out = 500)
  ) %>%
    dplyr::mutate(
      logit_mu = log(mu / (1 - mu)),
      logit_se = sqrt(phi) * sqrt(1 / (tot_ae_adm * mu * (1 - mu))),
      se_adj = sqrt(phi) * sqrt(mu * (1 - mu) / tot_ae_adm),
      upper_99_adj = 1 / (1 + exp(-(logit_mu + 3 * logit_se))),
      lower_99_adj = 1 / (1 + exp(-(logit_mu - 3 * logit_se))),
      upper_95_adj = 1 / (1 + exp(-(logit_mu + 1.96 * logit_se))),
      lower_95_adj = 1 / (1 + exp(-(logit_mu - 1.96 * logit_se)))
    )

  # 4. Identify Outliers
  plot_df <- plot_df %>%
    dplyr::mutate(
      rate = excess_mort / tot_ae_adm,
      limit_99 = mu + 3 * (sqrt(phi) * sqrt(mu * (1 - mu) / tot_ae_adm)),
      is_outlier = rate > limit_99
    )

  y_limit <- max(max(plot_df$rate, na.rm = TRUE) * 1.2, 0.02)
  
    ggplot2::ggplot(plot_df, ggplot2::aes(x = tot_ae_adm, y = rate)) +
    # Dynamic Shading Layer
    ggplot2::geom_rect(data = ribbon_df, inherit.aes = FALSE,
                       ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = fill),
                       alpha = 0.15) +
    ggplot2::scale_fill_identity() + # Uses the hex codes in the data frame
    # National average
    ggplot2::geom_hline(yintercept = mu, color = "black", linetype = 2, linewidth = 0.8, alpha = 0.4) +
    # Funnel Lines
    ggplot2::geom_line(data = line_df, ggplot2::aes(y = upper_99_adj), color = "black", alpha = 0.3) +
    ggplot2::geom_line(data = line_df, ggplot2::aes(y = lower_99_adj), color = "black", alpha = 0.3) +
    # Data points
    ggplot2::geom_point(color = "steelblue", alpha = 0.7) +
    # Formatting with Custom Labels
    ggplot2::scale_y_continuous(breaks = rate_breaks, labels = rate_labeller, expand = c(0, 0)) +
    ggplot2::coord_cartesian(ylim = c(0, y_limit)) +
    ggplot2::labs(
      title = "Funnel plot", #paste("A&E Funnel Plot:", type_label),
      subtitle = "",
      x = "Total type-1 A&E admissions", 
      y = "Risk rate (Expected excess deaths)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none", panel.grid.minor = ggplot2::element_blank())
}








make_mort_funnel_plot <- function(df, 
                                 rate_breaks = c(1/400, 1/200, 1/100, 1/50), 
                                 line_breaks = c("95%" = 1.96, "99.7%" = 3, "1 in 1000" = 3.29),
                                 type_label = "") {
  
  # --- 1. Background Ribbons ---
  breaks <- sort(unique(c(0, rate_breaks, Inf)))
  ribbon_df <- data.frame(
    ymin = breaks[-length(breaks)],
    ymax = breaks[-1],
    fill = colorRampPalette(c("#2ecc71", "#f1c40f", "#e74c3c"))(length(breaks)-1)
  )

  rate_labeller <- function(x) {
    ifelse(x == 0, "0", paste0("1 in ", round(1/x)))
  }

  # --- 2. Clean and Filter ---
  plot_df <- df %>%
    dplyr::mutate(
      excess_mort = as.numeric(excess_mort),
      tot_ae_adm = as.numeric(tot_ae_adm),
      org = as.character(org) 
    ) %>%
    dplyr::filter(!is.na(excess_mort), !is.na(tot_ae_adm), !is.na(org)) %>%
    dplyr::filter(tot_ae_adm > 0, excess_mort <= tot_ae_adm)
  
  if (nrow(plot_df) == 0) return(ggplot2::ggplot() + ggplot2::theme_void())

  # --- 3. Calculate Statistics ---
  mu <- sum(plot_df$excess_mort) / sum(plot_df$tot_ae_adm)
  current_rate <- plot_df$excess_mort / plot_df$tot_ae_adm
  z_scores <- (current_rate - mu) / sqrt(mu * (1 - mu) / plot_df$tot_ae_adm)
  phi <- max(1, mean(z_scores^2, na.rm = TRUE))

  # --- 4. Generate Dynamic Funnel Lines ---
  x_min <- min(plot_df$tot_ae_adm)
  x_max <- max(plot_df$tot_ae_adm)
  
  # Create a sequence for the lines
  line_data_base <- data.frame(tot_ae_adm = seq(x_min, x_max, length.out = 500)) %>%
    dplyr::mutate(
      logit_mu = log(mu / (1 - mu)),
      logit_se = sqrt(phi) * sqrt(1 / (tot_ae_adm * mu * (1 - mu)))
    )

  # Use map_df to create a "Long" format data frame of all requested lines
  line_breaks <- c("National average" = 0, line_breaks)
funnel_lines <- purrr::map_df(names(line_breaks), function(label) {
    z <- line_breaks[label]
    line_data_base %>%
      dplyr::mutate(
        upper = 1 / (1 + exp(-(logit_mu + z * logit_se))),
        # Only draw the lower line if it's a 'major' threshold (e.g., z > 2.5)
        lower = if(z > 2.5) 1 / (1 + exp(-(logit_mu - z * logit_se))) else NA,
        label = label
      )
})

  y_limit <- max(max(current_rate) * 1.2, 0.02)
  
  # --- 5. Plot ---
  ggplot2::ggplot(plot_df, ggplot2::aes(x = tot_ae_adm, y = excess_mort / tot_ae_adm)) +
    # Shading
    ggplot2::geom_rect(data = ribbon_df, inherit.aes = FALSE,
                       ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = fill),
                       alpha = 0.15) +
    ggplot2::scale_fill_identity() +
    # Dynamic Funnel Lines
    ggplot2::geom_line(data = funnel_lines, ggplot2::aes(y = upper, group = label), 
                       color = "black", alpha = 0.3) +
    ggplot2::geom_line(data = funnel_lines, ggplot2::aes(y = lower, group = label), 
                       color = "black", alpha = 0.3) +
    # Labels for lines (placed at the far right)
    ggplot2::geom_text(data = funnel_lines %>% dplyr::filter(tot_ae_adm == x_max),
                       ggplot2::aes(y = upper, label = label), 
                       hjust = 1.1, vjust = -0.5, size = 2.5, alpha = 0.6) +
    # Points
    ggplot2::geom_point(color = "steelblue", alpha = 0.7) +
    # Formatting
    ggplot2::scale_y_continuous(breaks = rate_breaks, labels = rate_labeller, expand = c(0, 0)) +
    ggplot2::coord_cartesian(ylim = c(0, y_limit)) +
    ggplot2::labs(
      title = "Risk-Adjusted Excess Mortality Funnel",
      subtitle = paste0("Avg Rate: ", rate_labeller(mu), " | Phi (Overdispersion): ", round(phi, 2)),
      x = "Total Type-1 A&E Admissions", 
      y = "Risk Rate (Expected Excess Deaths)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none", panel.grid.minor = ggplot2::element_blank())
}


# ae_impacts %>%
# filter(period == max(period)) %>%
# filter(org != "TOTAL") %>%
# nest(.by = ae_type) %>%
# filter(ae_type == "Type 1 (Major)") %>%
# mutate(funnel_plot = map2(data, ae_type, \(x, y) make_dta_funnel_plot(x, y))) %>%
# pull(funnel_plot) %>%
# patchwork::wrap_plots(ncol = 1)

ae_impacts %>%
filter(period == max(period)) %>%
filter(org != "TOTAL") %>%
nest(.by = ae_type) %>%
filter(ae_type == "Type 1 (Major)") %>%
mutate(funnel_plot = map2(data, ae_type, \(x, y) make_mort_funnel_plot(df = x, line_breaks = c(0.75, 0.85, 0.95) %>% 
  { purrr::set_names(x = qnorm(.), nm = scales::percent(.)) }, type_label = y))) %>%
pull(funnel_plot) %>%
patchwork::wrap_plots(ncol = 1)

library(patchwork)
library(sf)
library(rmapshaper)

icb_clusters <- read_csv("data/icb_cluster.csv") 

icb_impacts <- ae_impacts %>%
  filter(period == max(period, na.rm = TRUE), org != "TOTAL", icb_name != "") %>%
  left_join(mutate(icb_clusters, icb = str_to_upper(icb)), by = join_by(icb_name == icb)) %>%
  summarise(across(c(excess_mort, tot_ae_adm), sum), .by = c(period, icb_name, cluster))


icb_shp <- sf::st_read("data/icb_shape_files/ICB_APR_2026_EN_BFC.shp")

# setdiff(icb_clusters$icb, icb_shp %>%
  #   mutate(ICB26NM = str_replace_all(ICB26NM, pattern = "Integrated Care Board", replacement = "ICB")) %>%
  #   mutate(ICB26NM = str_remove_all(ICB26NM, pattern = "NHS")) %>%
  #   mutate(ICB26NM = str_trim(ICB26NM)) %>%
  #   pull(ICB26NM) %>%
  #   unique())

  # icb_shp %>%
  #   mutate(ICB26NM = str_replace_all(ICB26NM, pattern = "Integrated Care Board", replacement = "ICB")) %>%
  #   mutate(ICB26NM = str_remove_all(ICB26NM, pattern = "NHS")) %>%
  #   mutate(ICB26NM = str_trim(ICB26NM)) %>%
  #   pull(ICB26NM) %>%
  #   unique() %>%
  #   setdiff(icb_clusters$icb)

  breaks <- sort(unique(c(c(
    1 / 400,
    1 / 300,
    1 / 200,
    1 / 150,
    1 / 100,
    1 / 75
  )))) %>%
    {
      set_names(
        x = .,
        nm = colorRampPalette(c("#2ecc71", "#f1c40f", "#e74c3c"))(length(.))
      )
    }
  midpoints <- (head(breaks, -1) + tail(breaks, -1)) / 2

  
rate_labeller <- function(x) {
    ifelse(x == 0, "0", paste0("1 in ", round(1 / x)))
  }


icb_shp %>%
  mutate(
    ICB26NM = str_replace_all(
      ICB26NM,
      pattern = "Integrated Care Board",
      replacement = "ICB"
    )
  ) %>%
  mutate(ICB26NM = str_remove_all(ICB26NM, pattern = "NHS")) %>%
  mutate(ICB26NM = str_to_upper(ICB26NM)) %>%
  mutate(ICB26NM = str_trim(ICB26NM)) %>%
  left_join(icb_impacts, by = join_by(ICB26NM == icb_name)) %>%
  mutate(cluster = case_when(is.na(cluster) ~ ICB26NM, .default = cluster)) %>%
  group_by(cluster) %>%
  summarise(across(c(excess_mort, tot_ae_adm), sum)) %>%
  ms_simplify(keep = 0.0005, keep_shapes = TRUE) %>%
  ggplot(aes(fill = excess_mort / tot_ae_adm)) +
  geom_sf() +
scale_fill_stepsn(
    breaks = breaks,
    colors = names(breaks),
    labels = rate_labeller,
    limits = range(breaks),
    guide = guide_colorsteps(
      even.steps = TRUE, # Keeps the physical width of bins proportional to the values
      show.limits = FALSE,
      title.position = "top",
      barheight = unit(0.05, 'npc'),
      barwidth = unit(0.9, 'npc') # Adjusted for a longer bar
    )
  ) +
  theme_void() +
  labs(fill = "Risk rate (excess expected deaths)") +
  theme(legend.position = "bottom",
        legend.title.position = "top")





region_shp <- icb_shp %>%
  mutate(
    ICB26NM = str_replace_all(
      ICB26NM,
      pattern = "Integrated Care Board",
      replacement = "ICB"
    )
  ) %>%
  mutate(ICB26NM = str_remove_all(ICB26NM, pattern = "NHS")) %>%
  mutate(ICB26NM = str_to_upper(ICB26NM)) %>%
  mutate(ICB26NM = str_trim(ICB26NM)) %>%
  left_join(ae_impacts %>% distinct(org, icb_name, parent_org), by = join_by(ICB26NM == icb_name)) %>%
  mutate(parent_org = case_when(is.na(parent_org) ~ ICB26NM, .default = parent_org)) %>%
  group_by(parent_org) %>%
  summarise() 


# %>%
  region_plot <- region_shp %>%
    ms_simplify(keep = 0.0001, keep_shapes = TRUE) %>%
    ggplot(aes(fill = parent_org, col = "white")) +
    geom_sf() +
    theme_void() +
    theme(legend.position = "off")

# Region TS

(ae_impacts %>%
  filter(period > max(period)-dmonths(6), parent_org != "TOTAL") %>%
  mutate(parent_org = str_wrap(str_to_title(str_trim(str_remove_all(parent_org, "NHS ENGLAND"))), width = 15)) %>%
  summarise(across(c(excess_mort, tot_ae_adm), sum), .by = c(period, parent_org)) %>%
  ggplot(aes(x = period, y = excess_mort, col = parent_org)) +
  geom_path() +
  geom_point() +
  # scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(col = "") +
  theme(legend.position = "bottom") | region_plot) +
  plot_layout(widths = c(4, 2))


