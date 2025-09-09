## code to prepare `soilCores` dataset goes here
soilCores <- read.csv('/Volumes/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2022/Datasets/Soil2022NonStandardized2025-09-06.csv') %>%
  dplyr::group_by(fence, plot) %>%
  dplyr::mutate(coreNum = cur_group_id()) %>%
  dplyr::ungroup() %>%
  dplyr::select(-fence, -plot)

usethis::use_data(soilCores, overwrite = TRUE)
