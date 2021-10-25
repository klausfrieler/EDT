EDT_item_bank <- readRDS("data_raw/EDT_item_bank.RDS")
#usethis::use_data(EDT_item_bank, overwrite = TRUE)

EDT2_item_bank <- read.csv("data_raw/EDT2_item_bank.csv", stringsAsFactors = F)

#note 25.10.2021: inattention values are wrong and need inversion
EDT2_item_bank$inattention <- 1 - EDT2_item_bank$inattention
usethis::use_data(EDT2_item_bank, overwrite = TRUE)
