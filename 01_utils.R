# provides all available information from the Organsational Data Services API
# based on provided health code
ods_info <- function(health_org_code) {
  
  url <- paste0(
    "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/",
    health_org_code
  )
  
  httpResponse <- httr::GET(url, httr::accept_json())
  ods_information <- jsonlite::fromJSON(
    httr::content(
      httpResponse, 
      "text", 
      encoding="UTF-8"
    )
  )
  return(ods_information)
}

ods_lookup <- function(health_org_code, table1, table2, filter_category) {
  if (is.na(health_org_code)) return(NA)
  
  ods_information <- ods_info(health_org_code)
  
  
  lkp <- purrr::pluck(
    ods_information,
    "Organisation",
    table1,
    table2
  )
  
  if (!is.null(lkp)) {
    if (filter_category == "Active") {
      lkp <- lkp |> 
        filter(
          Status == "Active"
        )
    } else if (filter_category == "Successor") {
      lkp <- lkp |> 
        filter(
          Type == "Successor"
        )
    }
    
    if (nrow(lkp) > 0) {
      lkp <- lkp |> 
        tibble() |> 
        unnest(cols = Target) |> 
        unnest(cols = OrgId) |> 
        pull(extension)
    } else {
      lkp <- NA
    }
    
  } else {
    lkp <- NA
  }
  
  return(lkp)
}


# identifies active parent organisations for health org code provided
health_org_lookup <- function(health_org_code) {
  lkp <- ods_lookup(
    health_org_code,
    table1 = "Rels",
    table2 = "Rel",
    filter_category = "Active"
  )
  
  return(lkp)
}


ods_icb_extract <- function(ods_data) {

  rels <- ods_data$Organisation$Rels$Rel
  
  # Filter for Active relationships that match ICB link types
  # Trusts use RE5 (Geography), GPs use RE4 (Commissioning)
  icb_row <- rels[rels$Status == "Active" & rels$id %in% c("RE4", "RE5"), ]
  
  # If there are multiple, the one with Target.PrimaryRoleId.id == "RO261" 
  # is almost certainly the ICB
  icb_code <- icb_row$Target$OrgId$extension[icb_row$Target$PrimaryRole$id == "RO261"]
  
  return(icb_code[1]) # Return the first match
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

rate_labeller <- function(x) {
  # Small epsilon check for zero
  ifelse(x < 1e-10, "0", paste0("1 in ", round(1 / x)))
}
