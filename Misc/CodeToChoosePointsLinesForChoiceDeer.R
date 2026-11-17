


load("~/RDH Database/Processed Data/RDH_AllMigrations_2011t2025_06012026.RData")

aids <- mig.SpringMigration.sf |> st_drop_geometry() |> filter(Mgtry == "Long") |> distinct(AID) |> as.data.frame()

keeps = Spring.summary |> filter(MigComplete == "Yes", AID %in% aids$AID) |> distinct(id_yr) |> sample_n(11) 

deer <- c("108_2020","255_2016","242_2020","665_2023", keeps$id_yr)

RDH_point <- mig.SpringMigration.sf |> filter(id_yr %in% deer) |> select(id_yr, POSIXct, geometry)

RDH_line <- RDH_point |> as("Spatial") |> st_as_sf() |> Points2Lines(date_name = "POSIXct", id_name = "id_yr")

mapview(RDH_line)


st_write(RDH_point, "C:/Users/lwilde2/Downloads/Kauffman_RDH_point.shp")
st_write(RDH_line, "C:/Users/lwilde2/Downloads/Kauffman_RDH_line.shp")
