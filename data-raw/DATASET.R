## code to prepare `yeg` dataset goes here

api_endpoint <- "https://data.edmonton.ca/resource/eecg-fc54.json?$limit=500000"
yeg <- httr::GET(api_endpoint) %>% httr::content("text") %>% jsonlite::fromJSON() %>% dplyr::as_tibble()
  dplyr::mutate(Year = lubridate::year(PLANTED_DATE))
api_endpoint_n <- "https://data.edmonton.ca/resource/gihh-utrc.json"
yeg_neighbourhoods <- httr::GET(api_endpoint_n) %>% httr::content("text") %>% jsonlite::fromJSON() %>% dplyr::as_tibble()

api_endpoint_pop <- "https://data.edmonton.ca/resource/jkjx-2hix.json"
populations <- httr::GET(api_endpoint_pop) %>% httr::content("text") %>% jsonlite::fromJSON() %>% dplyr::as_tibble()

yeg_neighbourhoods <- sf::st_as_sfc(yeg_neighbourhoods$`Geometry Multipolygon`)

unique_neighbourhoods <- unique(yeg$NEIGHBOURHOOD_NAME)
colors <- brewer.pal(n = min(length(unique_neighbourhoods), 9), name = "Set1")

# assign colors to neighbourhoods
if (length(unique_neighbourhoods) > 9) {
  colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(unique_neighbourhoods))}
color_df <- dplyr::data_frame(colors, unique_neighbourhoods)
yeg$colors <- color_df$colors[base::match(yeg$NEIGHBOURHOOD_NAME, color_df$unique_neighbourhoods)]

remove(yeg_neighbourhoods, yeg)
