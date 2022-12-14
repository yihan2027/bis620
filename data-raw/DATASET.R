## code to prepare `DATASET` dataset goes here

ukb_accel <- readRDS("accel.rds")
usethis::use_data(ukb_accel, overwrite = TRUE)

## include Panitumumab clinical trial data in the package
folfox_path <- file.path("PDS_DSA_20050203")
ff_files <- dir(folfox_path)
ff_names <- gsub("_pds2019.sas7bdat", "", ff_files)
dl <- map(file.path(folfox_path, ff_files), ~ read_sas(.x))
names(dl) <- ff_names
usethis::use_data(dl, overwrite = TRUE)
