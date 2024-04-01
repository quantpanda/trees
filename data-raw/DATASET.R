## code to prepare `yeg` dataset goes here
yeg <- readr::read_csv( "~/4th year/Fintech 3/trees/data-raw/trees.csv") %>%
  dplyr::mutate(Year = lubridate::year(PLANTED_DATE))
yeg_neighbourhoods <- readr::read_csv("~/4th year/Fintech 3/trees/data-raw/yeg_neighbourhood.csv")
yeg_neighbourhoods <- sf::st_as_sfc(yeg_neighbourhoods$`Geometry Multipolygon`)

unique_neighbourhoods <- unique(yeg$NEIGHBOURHOOD_NAME)
colors <- brewer.pal(n = min(length(unique_neighbourhoods), 9), name = "Set1")

if (length(unique_neighbourhoods) > 9) {
  colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(unique_neighbourhoods))}

color_df <- dplyr::data_frame(colors, unique_neighbourhoods)

indices <- base::match(yeg$NEIGHBOURHOOD_NAME, color_df$unique_neighbourhoods)

yeg$colors <- color_df$colors[indices]
