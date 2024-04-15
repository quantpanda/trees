## code to prepare `yeg` dataset goes here

# api_endpoint <- "https://data.edmonton.ca/resource/eecg-fc54.json?$limit=500000"
# yeg <- httr::GET(api_endpoint) %>% httr::content("text") %>% jsonlite::fromJSON() %>% dplyr::as_tibble()

# RAW DATA
yeg_raw <- readr::read_csv("https://data.edmonton.ca/api/views/eecg-fc54/rows.csv?date=20240414&accessType=DOWNLOAD")

api_endpoint_n <- "https://data.edmonton.ca/resource/65fr-66s6.json?$limit=500000"
yeg_neighbourhoods_raw <- readr::read_csv("https://data.edmonton.ca/api/views/65fr-66s6/rows.csv?date=20240414&accessType=DOWNLOAD")

api_endpoint_inc <- "https://data.edmonton.ca/resource/jkjx-2hix.json"
incomes <- httr::GET(api_endpoint_inc) %>% httr::content("text") %>% jsonlite::fromJSON() %>% dplyr::as_tibble()

api_endpoint_assessment <- "https://data.edmonton.ca/resource/q7d6-ambg.json"
assessments <- httr::GET(api_endpoint_assessment) %>% httr::content("text") %>% jsonlite::fromJSON() %>% dplyr::as_tibble()

api_endpoint_pop <- "https://data.edmonton.ca/resource/phd4-y42v.json"
population_raw <- httr::GET(api_endpoint_pop) %>% httr::content("text") %>% jsonlite::fromJSON() %>% dplyr::as_tibble()

# MANIPULATED DATA

yeg <- yeg_raw %>%
  dplyr::transmute(PLANTED_DATE,
                   neighbourhood_name = NEIGHBOURHOOD_NAME,
                   LONGITUDE, LATITUDE) %>%
  dplyr::mutate(Year = lubridate::year(PLANTED_DATE))

trees_by_n <- yeg %>%
  dplyr::group_by(neighbourhood_name) %>%
  dplyr::summarise(Count = dplyr::n()) %>%
  dplyr::arrange(Count)

population <- population_raw %>%
  dplyr::select(-ward, -neighbourhood_number, -no_response) %>%
  tidyr::pivot_longer(cols = -neighbourhood_name, names_to = "class", values_to = "popu") %>%
  dplyr::mutate(popu = as.numeric(popu)) %>%
  dplyr::arrange(neighbourhood_name)

total_population <- population %>%
  group_by(neighbourhood_name) %>%
  summarise(Population = sum(popu, na.rm = TRUE))

yeg_neighbourhoods <- yeg_neighbourhoods_raw %>%
  dplyr::transmute(coords = sf::st_as_sfc(`Geometry Multipolygon`),
                   `neighbourhood_name` = `Neighbourhood Name`) %>%
  dplyr::full_join(incomes, by = "neighbourhood_name") %>%
  dplyr::full_join(trees_by_n, by = "neighbourhood_name") %>%
  dplyr::full_join(total_population, by = "neighbourhood_name") %>%
  # if neighbourhood name has "INDUSTRIAL" in it, set count to 0
  dplyr::mutate(Count = ifelse(grepl("INDUSTRIAL", neighbourhood_name), NA, Count)) %>%
  dplyr::mutate(Count = ifelse(grepl("ANTHONY HENDAY", neighbourhood_name), NA, Count)) %>%
  dplyr::mutate(Count = ifelse(grepl("RIVER VALLEY", neighbourhood_name), NA, Count)) %>%
  dplyr::mutate(Count = ifelse(grepl("CREEK RAVINE", neighbourhood_name), NA, Count)) %>%
  dplyr::mutate(Count_prop = Count / Population)

bonnie_doon <- yeg_neighbourhoods %>%
  dplyr::filter(neighbourhood_name == "BONNIE DOON")
#plot population hist
p2 <- ggplot2::ggplot(data = bonnie_doon) +
  ggplot2::geom_histogram(mapping = ggplot2::aes(x = Population), bins = 30) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Population Distribution in Edmonton Neighbourhoods",
                x = "Population",
                y = "Count")

# Not using

unique_neighbourhoods <- unique(yeg$NEIGHBOURHOOD_NAME)
colors <- brewer.pal(n = min(length(unique_neighbourhoods), 9), name = "Set1")

# assign colors to neighbourhoods
if (length(unique_neighbourhoods) > 9) {
  colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(unique_neighbourhoods))}
color_df <- dplyr::data_frame(colors, unique_neighbourhoods)
yeg$colors <- color_df$colors[base::match(yeg$NEIGHBOURHOOD_NAME, color_df$unique_neighbourhoods)]

remove(yeg_neighbourhoods, yeg)
