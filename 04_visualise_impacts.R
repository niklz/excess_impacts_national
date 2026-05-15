ae_impacts %>%
  filter(period == max(period)) %>%
  filter(org != "TOTAL") %>%
  nest(.by = ae_type) %>%
  filter(ae_type == "Type 1 (Major)") %>%
  mutate(
    funnel_plot = map2(data, ae_type, \(x, y) {
      make_mort_funnel_plot(
        df = x,
        line_breaks = c(0.75, 0.85, 0.95) %>%
          {
            purrr::set_names(x = qnorm(.), nm = scales::percent(.))
          },
        type_label = y
      )
    })
  ) %>%
  pull(funnel_plot) %>%
  patchwork::wrap_plots(ncol = 1)