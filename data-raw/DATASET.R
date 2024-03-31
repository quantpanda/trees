## code to prepare `yeg` dataset goes here
yeg <- readr::read_csv( "~/4th year/Fintech 3/trees/data-raw/trees.csv") %>%
  dplyr::mutate(Year = lubridate::year(PLANTED_DATE))
