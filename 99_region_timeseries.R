source("03_estimate_impacts.R")

region_shp <- sf::st_read("data/region/region.shp")


plot_data <- ae_impacts %>%
  filter(period > max(period)-dmonths(6), parent_org != "Total") %>%
  mutate(parent_org = str_wrap(str_to_title(str_trim(str_remove_all(parent_org, "NHS England"))), width = 15)) %>%
  summarise(across(c(excess_mort, tot_ae_adm), sum), .by = c(period, parent_org)) %>%
  mutate(rate = excess_mort/tot_ae_adm, .by = c(period, parent_org))




label_data <- plot_data %>% 
  filter(period == max(period))


# 1. Map: Extreme simplification + static lines
region_plot <- region_shp %>%
  ms_simplify(keep = 0.005, keep_shapes = TRUE) %>% 
  ggplot(aes(fill = parent_org, data_id = parent_org)) +
  geom_sf_interactive(colour = "white", size = 0.5, alpha = 0.15) + 
  coord_sf(datum = NA) +
  theme_void() +
  paletteer::scale_fill_paletteer_d("MetBrewer::Hokusai1") +
  theme(legend.position = "none")

# 2. Time Series: Mix static and interactive layers
ts_plot <- ggplot(plot_data, aes(x = period, y = rate*1000, 
                                 col = parent_org, group = parent_org,
                                 data_id = parent_org)) +
  # Keep the halo STATIC (standard geom_line) to save memory
  geom_line(linewidth = 2.5, col = "white") + 
  # Only the colored line is interactive
  geom_line_interactive(linewidth = 1.2) +
  geom_point_interactive(
    aes(tooltip = scales::comma(round(rate*1000))),
    size = 2.5, 
    hover_nearest = TRUE
  ) +
  geom_text_repel_interactive(
    data = label_data, aes(label = parent_org, data_id = parent_org), 
    hjust = 0, nudge_x = 10, direction = "y", 
    segment.color = NA, size = 3.5, fontface = "bold"
  ) +
  scale_x_date(expand = expansion(mult = c(0.05, 0.3))) + 
  scale_y_continuous(labels = scales::comma) +
  paletteer::scale_color_paletteer_d("MetBrewer::Hokusai1") +
  labs(title = str_wrap("Estimated monthly excess deaths, per 1000 type-1 A&E admissions per region", 50), x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_text(angle = 0, vjust = 1, hjust = 0, face = "bold")
  )

# 3. Interactive Assembly
girafe(
  ggobj = ts_plot + inset_element(region_plot, on_top = FALSE, left = -0.15, bottom = 0, right = 0.9, top = 1),
  options = list(
    # This CSS makes the hovered region/line fully opaque and "pops" it
    opts_hover(css = "opacity:1.0; fill-opacity:1.0; stroke-width:3px; transition: all 0.3s ease-in-out;"),
    opts_tooltip(css = "background-color:white;color:black;padding:5px;border-radius:5px;font-family:sans-serif;"),
    # This dims everything else so the hovered region stands out
    opts_hover_inv(css = "opacity:0.05;"), 
    opts_toolbar(saveaspng = FALSE)
  )
)


