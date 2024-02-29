soilCores <- read.csv("/Users/emmalathrop/Documents/NAU/EES698 R Package/soilCores.csv") %>%
  dplyr::group_by(fence, plot) %>%
  dplyr::mutate(coreNum = cur_group_id()) %>%
  dplyr::ungroup() %>%
  dplyr::select(-fence, -plot)

usethis::use_data(soilCores)
