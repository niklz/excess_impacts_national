source("02_scrape_data.R")


icb_shp <- sf::st_read("data/icb_shape_files/ICB_APR_2026_EN_BFC.shp")

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
  left_join(ae_data_sum %>% distinct(icb_name, parent_org) %>% mutate(icb_name = str_to_upper(icb_name)), by = join_by(ICB26NM == icb_name)) %>%
  group_by(parent_org) %>%
  summarise() 

cluster_shp <- icb_shp %>%
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
  left_join(ae_data_sum %>% distinct(icb_name, cluster) %>% mutate(icb_upper = str_to_upper(icb_name)), by = join_by(ICB26NM == icb_upper), keep = TRUE) %>%
  mutate(cluster = case_when(is.na(cluster) ~ icb_name, .default = cluster)) %>%
  group_by(cluster) %>%
  summarise() 


