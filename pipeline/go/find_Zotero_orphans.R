# library(checkmate)
# library(magrittr)
# library(purrr)
# library(readr)
# library(stringr)

library(tidyverse)

# Export the Zotero library in a CSV file.
list_linked_files <- function(
  lib_file = file.choose(),
  basename = TRUE
) {
  checkmate::assert_file_exists(lib_file, access = "r")
  checkmate::assert_flag(basename)
  
  out <- lib_file |>
    readr::read_csv(col_types = readr::cols(.default = "c")) |>
    magrittr::extract2("File Attachments") |>
    stringr::str_split("; ") |>
    unlist() |>
    stringr::str_squish() |>
    purrr::discard(is.na)

  if (isTRUE(basename)) {
    basename(out)
  } else {
    out
  }
}

find_orphan_files <- function(
  lib_file = file.choose(),
  file_folder = "G:\\Meu Drive\\Zotero\\files"
) {
  checkmate::assert_file_exists(lib_file, access = "r")
  checkmate::assert_directory_exists(file_folder, access = "rw")

  linked_files <- list_linked_files(lib_file, basename = TRUE)
  real_files <- list.files(file_folder) |> basename()

  real_files[!real_files %in% linked_files]
}

dir("~/Desktop/") 


linked_files <- bind_rows(
  read_csv("~/Desktop/SecurityForceDatasetSources.csv") |> 
    select(`File Attachments`),
  read_csv("~/Desktop/My Library.csv") |> 
    select(`File Attachments`),
  read_csv("~/Desktop/Transitional Justice Updates.csv") |> 
    select(`File Attachments`)
) |> 
  rename(attachments = `File Attachments`) |> 
  filter(!is.na(attachments)) |> 
  mutate(attachments = str_split(attachments, "; ")) |> 
  unnest(attachments) |> 
  unlist(use.names = FALSE) |> 
  dirname() |> 
  unique()


real_files <- paste("/Users/otthoms/Zotero/storage/", list.files("/Users/otthoms/Zotero/storage/"), sep = "")

real_files[!real_files %in% linked_files]
