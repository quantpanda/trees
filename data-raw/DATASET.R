## code to prepare `yeg` dataset goes here

# api_endpoint <- "https://data.edmonton.ca/resource/eecg-fc54.json?$limit=500000"
# yeg <- httr::GET(api_endpoint) %>% httr::content("text") %>% jsonlite::fromJSON() %>% dplyr::as_tibble()

# RAW DATA
yeg_raw <- readr::read_csv("https://data.edmonton.ca/api/views/eecg-fc54/rows.csv?date=20240414&accessType=DOWNLOAD")

api_endpoint_n <- "https://data.edmonton.ca/resource/65fr-66s6.json?$limit=500000"
yeg_neighbourhoods_raw <- readr::read_csv("https://data.edmonton.ca/api/views/65fr-66s6/rows.csv?date=20240414&accessType=DOWNLOAD")

api_endpoint_inc <- "https://data.edmonton.ca/resource/jkjx-2hix.json"
incomes_raw <- httr::GET(api_endpoint_inc) %>% httr::content("text") %>% jsonlite::fromJSON() %>% dplyr::as_tibble()

api_endpoint_assessment <- "https://data.edmonton.ca/resource/q7d6-ambg.json?$limit=500000"
assessments_raw <- httr::GET(api_endpoint_assessment) %>% httr::content("text") %>% jsonlite::fromJSON() %>% dplyr::as_tibble()

api_endpoint_pop <- "https://data.edmonton.ca/resource/phd4-y42v.json"
population_raw <- httr::GET(api_endpoint_pop) %>% httr::content("text") %>% jsonlite::fromJSON() %>% dplyr::as_tibble()


# MANIPULATED DATA

# ASSESSMENTS
assessments <- assessments_raw %>%
  dplyr::filter(mill_class_1 == "RESIDENTIAL") %>%
  transmute(neighbourhood_name = neighbourhood,
            assessed_value = as.numeric(assessed_value)) %>%
  group_by(neighbourhood_name) %>%
  summarise(mean_assessed_value = mean(assessed_value, na.rm = TRUE),
            median_assessed_value = median(assessed_value, na.rm = TRUE),
            max_assessed_value = max(assessed_value, na.rm = TRUE),
            min_assessed_value = min(assessed_value, na.rm = TRUE),
            num_properties = n())


#TREES
yeg <- yeg_raw %>%
  dplyr::transmute(PLANTED_DATE,
                   neighbourhood_name = NEIGHBOURHOOD_NAME,
                   LONGITUDE, LATITUDE) %>%
  dplyr::mutate(Year = lubridate::year(PLANTED_DATE))

# TREES SUMMARY
trees_by_n <- yeg %>%
  dplyr::group_by(neighbourhood_name) %>%
  dplyr::summarise(Count = dplyr::n()) %>%
  dplyr::arrange(Count)

# POPULATION HISTO
population <- population_raw %>%
  dplyr::select(-ward, -neighbourhood_number, -no_response) %>%
  tidyr::pivot_longer(cols = -neighbourhood_name, names_to = "class", values_to = "popu") %>%
  dplyr::mutate(popu = as.numeric(popu)) %>%
  dplyr::group_by(neighbourhood_name) %>%
  dplyr::mutate(prop = popu/sum(popu + 0.01, na.rm = T)) %>%
  #remove first char of class
  dplyr::mutate(class = substr(class, 2, nchar(class))) %>%
  dplyr::mutate(class = gsub("_", "-", class)) %>%
  dplyr::mutate(class = gsub("5-9", "05-9", class)) %>%
  dplyr::arrange(neighbourhood_name, class)




# POPULATION TOTAL
total_population <- population %>%
  group_by(neighbourhood_name) %>%
  summarise(Population = sum(popu, na.rm = TRUE))

# INCOMES
income <- incomes_raw %>%
  dplyr::select(-ward, -neighbourhood_number, -no_response) %>%
  tidyr::pivot_longer(cols = -neighbourhood_name, names_to = "class", values_to = "income") %>%
  dplyr::mutate(income = as.numeric(income)) %>%
  dplyr::group_by(neighbourhood_name) %>%
  dplyr::mutate(prop = income/sum(income + 0.01, na.rm = T)) %>%
  #assign numerical values to class
  dplyr::arrange(neighbourhood_name) %>%
  # replace "to_less_than_" by "to"
  dplyr::mutate(class = gsub("to_less_than_", "to ", class)) %>%
  # replace _ by " "
  dplyr::mutate(class = gsub("_", " ", class)) %>%
  dplyr::mutate(class_num = case_when(
    class == "less than 30 000"  ~ 1,
    class == " 30 000 to 60 000"  ~ 2,
    class == " 60 000 to 100 000" ~ 3,
    class == " 100 000 to 125 000"  ~ 4,
    class == " 125 000 to 150 000"  ~ 5,
    class == " 150 000 to 200 000" ~ 6,
    class == " 200 000 to 250 000" ~ 7,
    class == " 250 000 or more" ~ 8,
    TRUE ~ NA)) %>%
  dplyr::arrange(neighbourhood_name, class_num)


# NEIGHBOURHOODS MASTER
yeg_neighbourhoods <- yeg_neighbourhoods_raw %>%
  dplyr::transmute(coords = sf::st_as_sfc(`Geometry Multipolygon`),
                   `neighbourhood_name` = `Neighbourhood Name`) %>%
  dplyr::full_join(trees_by_n, by = "neighbourhood_name") %>%
  dplyr::full_join(total_population, by = "neighbourhood_name") %>%
  dplyr::full_join(assessments, by = "neighbourhood_name") %>%
  # if neighbourhood name has "INDUSTRIAL" in it, set count to 0
  dplyr::mutate(Count = ifelse(grepl("INDUSTRIAL", neighbourhood_name), NA, Count)) %>%
  dplyr::mutate(Count = ifelse(grepl("ANTHONY HENDAY", neighbourhood_name), NA, Count)) %>%
  dplyr::mutate(Count = ifelse(grepl("RIVER VALLEY", neighbourhood_name), NA, Count)) %>%
  dplyr::mutate(Count = ifelse(grepl("CREEK RAVINE", neighbourhood_name), NA, Count)) %>%
  dplyr::mutate(tree_prop = Count / num_properties)

# Not using

unique_neighbourhoods <- unique(yeg$NEIGHBOURHOOD_NAME)
colors <- brewer.pal(n = min(length(unique_neighbourhoods), 9), name = "Set1")

# assign colors to neighbourhoods
if (length(unique_neighbourhoods) > 9) {
  colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(unique_neighbourhoods))}
color_df <- dplyr::data_frame(colors, unique_neighbourhoods)
yeg$colors <- color_df$colors[base::match(yeg$NEIGHBOURHOOD_NAME, color_df$unique_neighbourhoods)]

remove(yeg_neighbourhoods, yeg)
