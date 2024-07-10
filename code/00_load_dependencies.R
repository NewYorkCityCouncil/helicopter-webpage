## Load Libraries -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c(
  "data.table", "httr", "glue", "sf", "arrow",
  "smoothr", "janitor", "lubridate", "cowplot", "leaflet", "leaflet.extras",
  "gtable", "gtExtras", "gt",
  "tidyverse", "ggpubr", "councilverse"
  )

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#councildown.check <- "councildown" %in% installed.packages()[,"Package"]
councilverse.check <- "councilverse" %in% installed.packages()[,"Package"]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)
#if(councildown.check == FALSE) remotes::install_github("newyorkcitycouncil/councildown")
if(councilverse.check == FALSE) remotes::install_github("newyorkcitycouncil/councilverse")

# packages are loaded
lapply(c(list.of.packages,"councilverse"), require, character.only = TRUE)

# remove created variables for packages
rm(list.of.packages,new.packages,councilverse.check)

CRS <- 4326

read_all_csv <- function(csv_dir, start_date = NULL, end_date = NULL){
  
  csv_files_df <- 
    tibble(
      filename = dir(csv_dir),
      date = str_remove_all(filename, ".csv")
    ) %>%
    filter(
      str_detect(filename, ".csv")
    ) %>%
    mutate(
      date = as_date(date)
    )
  
  if (!is.null(start_date)) {
    csv_files_df <- 
      csv_files_df %>%
      filter(date >= mdy(start_date))
  }
  
  if (!is.null(end_date)) {
    csv_files_df <- 
      csv_files_df %>%
      filter(date <= mdy(end_date))
  }
  
  csv_files <- glue("{csv_dir}/{csv_files_df$filename}")
  all_df_list <- lapply(csv_files, read_csv, show_col_types = F, col_types = cols(.default = "c"))
  all_df <- bind_rows(all_df_list)
  return(all_df)
}

read_all_parquet <- function(file_dir, start_date = NULL, end_date = NULL){
  
  files_df <- 
    tibble(
      filename = dir(file_dir),
      date = str_remove_all(filename, ".csv")
    ) %>%
    mutate(
      date = as_date(date)
    )
  
  if (!is.null(start_date)) {
    files_df <- 
      files_df %>%
      filter(date >= mdy(start_date))
  }
  
  if (!is.null(end_date)) {
    files_df <- 
      files_df %>%
      filter(date <= mdy(end_date))
  }
  
  files <- glue("{file_dir}/{files_df$filename}")
  all_df_list <- lapply(files, read_parquet, show_col_types = F)
  all_df <- 
    bind_rows(all_df_list) %>%
    st_as_sf(coords = c("lon", "lat"), crs = CRS) 
  
  return(all_df)
}

get_positions_csv <- function(dir) {
  list.files(path = dir,
             pattern = "\\.csv$",
             full.names = TRUE) %>% 
    set_names() %>% 
    map_dfr(~read_csv(., col_types = cols(.default = "c")), .id = "file_name") %>%
    mutate(
      flight_id = str_replace(str_extract(string = file_name, pattern = "[^_]+$"), ".csv", ""), 
      date = str_replace(str_extract(string = file_name, pattern = "[^/]+$"), "_[0-9]+.csv", "")
    )
}


