#start_header {
  # reference GIT YT kL6L2MNqPHg
  # library(usethis)
  # ?use_github
  # edit_r_environ()
  # use_github(protocol = "https", auth_token = Sys.getenv("GITHUB_PAT"))
  # install.packages("here")
}

clean.it <- function() {
  basic.packages <- c("package:stats","package:graphics",
                      "package:grDevices","package:utils",
                      "package:datasets","package:methods",
                      "package:base")
  package.list <- dplyr::setdiff( search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)] , basic.packages)
  if (length(package.list)>0)  for(package in package.list) detach(package, character.only=TRUE)
  if(!require(pacman))install.packages("pacman"); require(here)
  
  path <- here("input")
  
  pacman::p_load(here, haven, readxl, 
                 tidyverse,magrittr,purrr,
                 stringi,zoo,DataExplorer)
  
  rm(list = dplyr::setdiff( ls(envir = globalenv()),
                            c("clean.it", "path")
            ),
     envir = globalenv())
  #gc() # or sessionInfo()
  
}
clean.it()

spss_data_n158 <- read_sav("input/Datensatz_TO_TE_komplett_neu_n158.sav") %>% 
                    as_tibble() %>% 
                    janitor::clean_names() %>% 
                    rename(age_in_months = alter_monate, gender = geschlecht, weight = gewicht) %>% 
                    unite("ID", 1:3, sep = "_", remove = TRUE, na.rm = FALSE) # the ID is composed from pseudonr, data and pat_ID
  
colnames(spss_data_n158) <- colnames(spss_data_n158) %>% str_replace("x", "") %>% str_to_sentence() %>% str_replace("Id","ID")

# there are some columns with labels, store the spss labels in a list.
#labels_list <- spss_data_n158 %>% select_if(is.labelled) %>% map(function(x) attr(x, 'labels')) 


dipidolor <- spss_data_n158 %>% 
  dplyr::select(matches("ID|Dipi|dipi")) %>% 
  dplyr::select(-matches("4_|5_tag|Indikation")) %>% # just the first three days
  dplyr::select(-c("Dipi_0nein_1ja","Haufigkeit_der_dipigabe")) %>% # we don't need now these columns
  select_all(~str_replace_all(., "Awr", "0_0")) %>% 
  select_all(~str_replace_all(., "4h", "0_1")) %>% 
  select_all(~str_replace_all(., "8h", "0_2")) %>% 
  select_all(~str_replace_all(., "dipi", "")) %>% 
  select_all(~str_replace_all(., "tag_mo", "0")) %>%
  select_all(~str_replace_all(., "tag_mi", "1")) %>%
  select_all(~str_replace_all(., "tag_a", "2")) %>% 
  select_all(~str_replace_all(., "__", "_")) %>% 
  dplyr::rename('2_0_gabe_ja_nein' = '2_0_')

dipidolor_quantitative <- dipidolor %>% dplyr::select(matches("ID|mg")) %>% 
  select_all(~str_replace_all(., "_mg", "")) %>% 
  select_all(~str_replace_all(., "_15", "_second")) %>% 
  mutate(`0_0` = pmax(`0_0_nach_score2`, `0_0_pflege`, na.rm = TRUE)) %>% 
  dplyr::select(ID, `0_0`, everything()) %>% 
  dplyr::select(-c(`0_0_nach_score2`, `0_0_pflege`)) 
plot_missing(dipidolor_quantitative)
dipidolor_quantitative %<>% pivot_longer(-ID, values_to = "Dipi_quant", names_to = "time") %>% 
  arrange(ID, time)
dipidolor_quantitative$time %>% unique()
head(dipidolor_quantitative,10)


dipidolor_qualitative <- dipidolor %>% dplyr::select(matches("ID|ja")) %>% 
  select_all(~str_replace_all(., "_gabe_ja_nein", "")) %>% 
  rename(`0_0` = `0_0_1ja`, `0_0_second` = `0_0_15_1ja`) 
plot_missing(dipidolor_qualitative)
select(dipidolor_qualitative, -ID) %>% tidyr::gather() %>% pull(value) %>% unique() 
dipidolor_qualitative %<>% mutate_at(vars(-ID), as.integer) 
plot_missing(dipidolor_qualitative)
select(dipidolor_qualitative, -ID) %>% tidyr::gather() %>% pull(value) %>% unique() #  1  0 NA  = all the values found in df
dipidolor_qualitative %<>% pivot_longer(-ID, values_to = "Dipi_qual", names_to = "time") 
head(dipidolor_qualitative,10)

dipidolor_qualitative$time %>% unique()
dipidolor_quantitative$time %>% unique()


dipidolor <- full_join( dipidolor_quantitative, dipidolor_qualitative, by = c("ID", "time")) %>% 
  mutate(
    Dipi_binary = pmax(
      ifelse(Dipi_quant > 0, 1, Dipi_quant),
      Dipi_qual,
      na.rm = TRUE), 
    identity = (Dipi_qual == Dipi_binary)
  ) %>% dplyr::select(-c(identity, Dipi_qual)) %>% 
  tidyr::separate(col = "time", c("day", "time", "first_second")) %>% 
  mutate(first_second = ifelse(is.na(first_second), "first", first_second))

rm(dipidolor_qualitative, dipidolor_quantitative)
  
# dipidolor_per_time <- dipidolor %>%
#   group_by(ID, day, time) %>% 
#   summarise_at(vars(Dipi_quant, Dipi_binary), ~ sum(., na.rm = TRUE)) %>% 
#   ungroup()
# 
# dipidolor_per_day <- dipidolor %>%
#   group_by(ID, day) %>% 
#   summarise_at(vars(Dipi_quant, Dipi_binary), ~ sum(., na.rm = TRUE)) %>% 
#   ungroup()

#naniar::gg_miss_upset(dipidolor)

ains <- spss_data_n158 %>%
  dplyr::select(matches("ID|meta|nov|ibu|volt|par|per")) %>%
  dplyr::select(-matches("4_|5_|Vome|Nw")) %>% 
  mutate(Awr_novalgin_mg = as.double(Awr_novalgin_mg)) %>% 
  dplyr::select(-c(Novalgin_perioperativ_mg, matches("Abweichung"))) %>% 
  mutate(
    '0_0_Novalgin' = pmax(Awr_metamizol_mg, Awr_novalgin_mg, na.rm = TRUE) %>% 
                      pmax(Novalgin_perioperativ_ja1, na.rm = TRUE),
    '1_X_Novalgin' = pmax(Novalgin_tag_1_mg, `1_tag_novalgin_mg`, na.rm = TRUE)
         ) %>% 
  dplyr::select(-c(Awr_metamizol_mg, Awr_novalgin_mg, Novalgin_perioperativ_ja1,
                   Novalgin_tag_1_mg, `1_tag_novalgin_mg`)) %>% 
  select_all(~str_replace_all(., "paractamol", "Paracetamol")) %>% 
  select_all(~str_replace_all(., "paracetamol", "Paracetamol")) %>% 
  select_all(~str_replace_all(., "ibu", "Ibuprofen")) %>%
  select_all(~str_replace_all(., "novalgin", "Metamizol")) %>%
  select_all(~str_replace_all(., "Novalgin", "Metamizol")) %>%
  select_all(~str_replace_all(., "voltaren", "Diclofenac")) %>% 
  select_all(~str_replace_all(., "Op_tag_station", "0_X")) %>% 
  select_all(~str_replace_all(., "Op_tag", "0_X")) %>% 
  select_all(~str_replace_all(., "_mg", "")) %>% 
  select_all(~str_replace_all(., "Awr", "0_0")) %>% 
  select_all(~str_replace_all(., "tag", "X")) %>% 
  dplyr::select(sort(names(.))) %>% dplyr::select(ID, everything()) %>% 
  rowwise() %>% mutate_at( vars(-ID), ~ ifelse(is.na(.), NA, ifelse(. > 0, 1, .)) ) %>% 
  pivot_longer(-ID) %>% 
  separate(name, c("day", "time", "drug")) %>% 
  pivot_wider(id_cols = c(ID, day, time), names_from = drug, values_from = value) 


intervention <- full_join(ains, dipidolor) %>% arrange(ID, day, time) %>% 
  mutate_at(vars(day, Diclofenac:Dipidolor), as.integer)


per_day <- anti_join(intervention, dplyr::filter(intervention, day == 0 & time == 0)) %>% 
  group_by(ID, day) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE)

scores <- spss_data_n158 %>% 
  dplyr::select(matches("ID|kuss|faces|ppmd")) %>% 
  dplyr::select(-matches("4_tag|5_tag|elternteil")) %>%
  select_all(~str_replace_all(., "x", "")) %>% 
  select_all(~str_replace_all(., "_wert", "")) %>% 
  select_all(~str_replace_all(., "Awr", "0_0")) %>% 
  select_all(~str_replace_all(., "4h", "0_1")) %>% 
  select_all(~str_replace_all(., "8h", "0_2")) %>% 
  select_all(~str_replace_all(., "tag_mo", "0")) %>% 
  select_all(~str_replace_all(., "tag_mi", "1")) %>% 
  select_all(~str_replace_all(., "tag_a", "2")) 

Kuss <- scores %>% dplyr::select(matches("ID|kuss")) %>% 
  select_all(~str_replace_all(., "_kuss", "")) 

Kuss %>% map_dfr(~ round(100*mean(is.na(.)),2)) %>% pivot_longer(-ID, values_to = "percent_missing") %>%
  dplyr::select(-1) 

Kuss %<>% select(-matches("15_15|1515")) %>% pivot_longer(-ID)



  
  
  
  pivot_longer(-ID) %>%
  rename(Kuss = value) %>% 
  mutate(key = str_sub(name, 1, 3)) %>%
  group_by(ID, key) %>% 
  summarise_at(vars(Kuss), ~ sum(., na.rm = TRUE)) %>% 
  arrange(ID, key) ->
Kuss
  

scores %>% dplyr::select(matches("ID|faces")) %>% 
  group_by(ID) %>% mutate_all(as.in)
  select_all(~str_replace_all(., "_faces", "")) %>% 
  pivot_longer(-ID) %>%
  rename(Faces = value) %>% 
  mutate(key = str_sub(name, 1, 3)) %>%
  group_by(ID, key) %>% 
  summarise_at(vars(Faces), ~ sum(., na.rm = TRUE)) %>% 
  arrange(ID, key) ->
Faces



#############



















map_dfr(ains, ~ round(100*mean(is.na(.)),2)) %>%
  t() %>% as.data.frame(stringsAsFactors = FALSE) %>% rownames_to_column() %>%
  rename(column = rowname, percent_missing = V1) %>% arrange(percent_missing) %>%
  filter(percent_missing < 90) %>% pull(column) -> cols_to_keep;
medication <- select(medication, one_of(cols_to_keep)); rm(cols_to_keep)

# examine the missing values, the distinct values 
map_dfr(ains, ~ dplyr::n_distinct(.)) %>% 
  t() %>% as.data.frame(stringsAsFactors = FALSE) %>% tibble::rownames_to_column() %>%
  rename(column = rowname, distinct_values = V1) %>% 
  full_join(
    map_dfr(medication, ~ round(100*mean(is.na(.)),2)) %>% 
      t() %>% as.data.frame(stringsAsFactors = FALSE) %>% rownames_to_column() %>%
      rename(column = rowname, percent_missing = V1)
  ) -> explore

rm(admit_expression, reject_expression)


admit_expression <- paste(
  c("Pseudonym", "Geschlecht","Gewicht", "Alter_Monate", "_KUSS", "_Faces", "AWR_Dipi_1ja",
    "_PPMD_", "Gabe", "_ml", "Erbrechen","Übelkeit","Nachblutung", "Fieber", "Antibiose", "Indikation_Dipi_2._Tag_mo"),
  collapse = "|")
reject_expression <- paste(
  c("5","Station","Häufigkeit_","Eltern","_ohne_"),
  collapse = "|")



  
library(xlsx)
write.xlsx(processed_data_n158, file = "before_after.xlsx",
           sheetName = "Sheet1", 
           col.names = TRUE, append = FALSE)


cbind(colnames(processed_data_n158), colnames(test)) %>% as.data.frame()


write.xlsx(TE2, file = "TE2.xlsx", col.names = TRUE, row.names = TRUE)
  



  



