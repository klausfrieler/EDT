EDT_dict_raw <- readRDS("data_raw/EDT_dict.RDS")
#names(EDT_dict_raw) <- c("key", "DE", "EN")
EDT_dict_raw <- EDT_dict_raw[,c("key", "EN", "DE")]
EDT_dict <- psychTestR::i18n_dict$new(EDT_dict_raw)
#usethis::use_data(EDT_dict, overwrite = TRUE)
