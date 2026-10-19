load("C:/Users/lwilde2/Documents/Collaborations/RobbMerkle_FenceCrossings/RDH_GPSdata_HS+AC+LW+KM+MK_01.06.2026.RData")



span_per_aid <- all_rdh_data.sf %>%
  filter(Mgtry == "Long") %>%
  st_drop_geometry() %>%
  group_by(AID) %>%
  summarise(
    start_date = min(Date, na.rm = TRUE),
    end_date   = max(Date, na.rm = TRUE),
    span_days  = as.numeric(difftime(end_date, start_date, units = "days")),
    .groups = "drop"
  )

# Keep AIDs whose span exceeds 365 days
aid_gt_year_by_span <- span_per_aid %>%
  filter(span_days > 100)

# Filter the original data
long_gt_year_span_data <- all_rdh_data.sf %>%
  filter(AID %in% aid_gt_year_by_span$AID)

long_gt_year_span_data %>% distinct(id_yr)
