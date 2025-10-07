### can use this script to find Zotero items with missing attachments/snapshots
### need to first manually export the Zotero library to .csv file

read_csv("~/Desktop/libdata.csv") |> 
  rename(
    attach = `File Attachments`, 
    access = `Access Date` ) |>  
  filter(!is.na(attach)) |>
  select(Key, access, attach) |> 
  rowwise() |>  
  mutate(
    attach = str_split(attach, "; "), 
    n = length(attach),  
    attach = list(attach[attach != ""]) ) |> 
  unnest(attach) |> 
  ungroup() |> 
  mutate(exists = file.exists(attach)) |> 
  filter(!exists) |> 
  select(Key, access, attach, n) |> 
  arrange(desc(access)) |> 
  group_by(Key) |> 
  mutate(miss = n()) |> 
  ungroup() |> 
  mutate(tag = paste("#missing_attach_", ifelse(n - miss == 0, "all", miss), sep = ""))