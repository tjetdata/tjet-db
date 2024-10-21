### print 10 focus country profiles to Quarto files 
### NOT NEEDED ANYMORE 

# df <- db[["Countries"]] %>%
#   filter(include) %>%
#   select(country, country_case, ccode, ccode_case, ccode_ksg, beg, end, tjet_focus, 
#          txt_intro, txt_regime, txt_conflict, txt_TJ) %>%
#   arrange(country)
# 
# dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/focus/")
# 
# df %>% 
#   filter(tjet_focus == 1 | country == "Uganda") %>%  
#   select(country) %>%
#   unlist(use.names = FALSE) %>%  
#   map(., function(ctry) {
#     temp <- df %>% 
#       filter(country == ctry) %>%
#       mutate(txt_intro = str_replace_all(str_trim(txt_intro), "\n", "\n\n"),
#              txt_regime = str_replace_all(str_trim(txt_regime), "\n", "\n\n"),
#              txt_conflict = str_replace_all(str_trim(txt_conflict), "\n", "\n\n"),
#              txt_TJ = str_replace_all(str_trim(txt_TJ), "\n", "\n\n")) %>%
#       select(txt_intro, txt_regime, txt_conflict, txt_TJ) %>% 
#       unlist()
#     paste("---\ntitle: ", ctry, "\nformat: docx\n---", 
#           "\n\n## Introduction\n\n", 
#           temp[["txt_intro"]], 
#           "\n\n## Regime Background\n\n", 
#           temp[["txt_regime"]], 
#           "\n\n## Conflict Background\n\n", 
#           temp[["txt_conflict"]], 
#           "\n\n## Transitional Justice\n\n", 
#           temp[["txt_TJ"]], 
#           sep = "") %>% 
#       write_file(., file = paste("~/Dropbox/TJLab/TimoDataWork/country_profiles/focus/", ctry, ".qmd", sep = ""))
#     invisible()
#   })

### print all country profiles with auto summaries to Quarto files 
### NO LONGER NEEDED 

# dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/")
# dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/")
# # dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/edits/")
# dir("~/Dropbox/TJLab/TimoDataWork/country_profiles")
# 
# map(df$country_case, function(ctry) {
#   
#   df1 <- df %>%
#     filter(country_case == ctry) %>%
#     mutate(txt_intro = str_replace_all(str_trim(txt_intro), "\n", "\n\n"),
#            txt_regime = str_replace_all(str_trim(txt_regime), "\n", "\n\n"),
#            txt_conflict = str_replace_all(str_trim(txt_conflict), "\n", "\n\n"),
#            txt_TJ = str_replace_all(str_trim(txt_TJ), "\n", "\n\n")) %>%
#     select(txt_intro, txt_regime, txt_conflict, txt_TJ) %>%
#     unlist()
#   
#   new <- autotxt %>% 
#     filter(country_case == ctry) %>%
#     select(-country_case, -ccode_case) %>%
#     unlist()
#   
#   # new <- map(autotxt, function(df2) {
#   #   df2 %>%
#   #     filter(country_case == ctry) %>%
#   #     select(text) %>%
#   #     unlist(use.names = FALSE)
#   # })
#   
#   paste("---\ntitle: ", ctry, "\nformat: docx\n---\n\n",
#         new[["summary"]],
#         "\n\n## Country Background", 
#         "\n\n### Democratic Transition\n\n",
#         new[["regime"]], "\n\n", 
#         df1[["txt_regime"]],
#         "\n\n### Violent Conflict\n\n",
#         new[["conflict"]],
#         df1[["txt_conflict"]],
#         "\n\n## Transitional Justice\n\n",
#         df1[["txt_intro"]], "\n\n",
#         df1[["txt_TJ"]], "\n\n",
#         ifelse(!is.na(new[["amnesties"]]),
#                paste("### Amnesties\n\n", new[["amnesties"]], "\n\n", sep = ""),
#                ""),
#         ifelse(!is.na(new[["domestic"]]),
#                paste("### Domestic Trials\n\n", new[["domestic"]], "\n\n", sep = ""),
#                ""),
#         ifelse(!is.na(new[["intl"]]),
#                paste("### International Trials\n\n", new[["intl"]], "\n\n", sep = ""),
#                ""),
#         ifelse(!is.na(new[["foreign"]]),
#                paste("### Foreign Trials\n\n", new[["foreign"]], "\n\n", sep = ""),
#                ""),
#         ifelse(!is.na(new[["reparations"]]),
#                paste("### Reparations\n\n", new[["reparations"]], "\n\n", sep = ""),
#                ""),
#         ifelse(!is.na(new[["tcs"]]),
#                paste("### Truth Commissions\n\n", new[["tcs"]], "\n\n", sep = ""),
#                ""),
#         ifelse(!is.na(new[["un"]]),
#                paste("### UN Investigations\n\n", new[["un"]], "\n\n", sep = ""),
#                ""),
#         ifelse(!is.na(new[["vetting"]]),
#                paste("### Vetting\n\n", new[["vetting"]], "\n\n", sep = ""),
#                ""),
#         sep = "") %>% 
#     write_file(., file = paste("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/", ctry, ".qmd", sep = ""))
#   
#   invisible()
# })
# 
# file.copy(from = list.files("~/Dropbox/TJLab/TimoDataWork/country_profiles/original", full.names = TRUE),
#           to = "~/Dropbox/TJLab/TimoDataWork/country_profiles/edits/",
#           overwrite = FALSE, recursive = TRUE, copy.mode = FALSE)
