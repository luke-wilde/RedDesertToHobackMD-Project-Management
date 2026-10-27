load("~/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData")

deer <- c(447, 708, 722, 970)


keep <-rdh_data.sf |> filter(AID %in% deer) |> filter(Month == c(11,12))

poly <- keep |> 
  group_by(AID) |>
  mutate(
    # 2. Calculate the group centroid coordinates
    centroid_x = mean(st_coordinates(geometry)[, 1]),
    centroid_y = mean(st_coordinates(geometry)[, 2]),
    # 3. Compute the Euclidean distance of each point to its group centroid
    dist_to_centroid = sqrt((st_coordinates(geometry)[, 1] - centroid_x)^2 + 
                              (st_coordinates(geometry)[, 2] - centroid_y)^2)
  ) |>
  # 4. Keep only the closest 95% of points for each group
  filter(dist_to_centroid <= quantile(dist_to_centroid, 0.95)) |>
  # 5. Combine the remaining 95% points into a MULTIPOINT geometry per group
  summarise(geometry = st_combine(geometry), .groups = "drop") |>
  # 6. Generate the convex hull for the 95% points
  st_convex_hull()
