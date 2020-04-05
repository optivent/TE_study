###### header #######
# reference GIT YT kL6L2MNqPHg # library(usethis) # ?use_github # edit_r_environ() # use_github(protocol = "https", auth_token = Sys.getenv("GITHUB_PAT"))

clean.it <- function() {
  basic.packages <- c("package:stats","package:graphics",
                      "package:grDevices","package:utils",
                      "package:datasets","package:methods",
                      "package:base")
  package.list <- dplyr::setdiff( search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)] , basic.packages)
  if (length(package.list)>0)  for(package in package.list) detach(package, character.only=TRUE)
  if(!require(pacman))install.packages("pacman"); require(here)
  
  path <- here("input")
  
  pacman::p_load(here, haven, readxl, VIM,
                 tidyverse,magrittr,purrr,
                 stringi,zoo,DataExplorer)
  
  rm(list = dplyr::setdiff( ls(envir = globalenv()),
                            c("clean.it", "path")
            ),
     envir = globalenv())
  #gc() # or sessionInfo()
  
}; clean.it()

null_and_missing <- function(df) {
  result <- map_df(df, ~ sum(is.na(dplyr::na_if(.,0)))) %>% pivot_longer(everything()) %>% arrange(desc(value))
  return(result)
}

select_cutoff <- function(df, cutoff){
  cols_to_keep <- filter(pivot_longer(map_df(df, ~ sum(is.na(dplyr::na_if(.,0)))), everything()), value < cutoff)$name
  result <- dplyr::select(df, one_of(cols_to_keep))
  return(result)
}



##### import rowdata #####

#read_sav("input/Datensatz_TO_TE_komplett_neu_n158.sav") %>% select_if(is.labelled) %>% map(function(x) attr(x, 'labels')) # the SPSS labels

spss_data <- read_sav("input/Datensatz_TO_TE_komplett_neu_n158.sav") %>% 
                    as_tibble() %>% 
                    janitor::clean_names() %>% 
                    rename(Age_in_months = alter_monate,
                           Gender = geschlecht,
                           Weight = gewicht) %>% 
                    mutate(ID = paste(as.character(pseudonym), as.character(datum), sep = "::")) %>% 
                    dplyr::select(ID, everything()) %>% 
                    dplyr::select(-datum, -pseudonym, -nr)
                    
colnames(spss_data) <- colnames(spss_data) %>% str_replace("x", "") %>% str_to_sentence() %>% str_replace("Id","ID")



##### IID_measures #####
# Gender 0 = m, 1 = f

IID_measures <- spss_data %>%
  select(c(ID:Fa_assistenzarzt, Fieber, Nachblutung:Entlassungtag_post_op)) %>%
  select(-matches("Pseudonym|Ubereinstimm|Nw_zeit|Datum|Alter|Vome_|Novalgin")) %>% 
  mutate_if(is.double, ~ as.integer(.)) 

IID_measures %>% null_and_missing()

IID_measures %<>% select_cutoff(cutoff = 125)

IID_measures %<>% rename(Dipidolor_01 = Dipi_0nein_1ja,
                         Sum_Dipigabe = Haufigkeit_der_dipigabe,
                         Dipi_Indik.nicht_erhalten = Indikation_fur_dipi_aber_nicht_erhalten,
                         ) %>% 
                  select_all(~str_replace_all(., "_post_op", "")) %>%
                  select_all(~str_replace_all(., "2", "")) %>%
                  select_all(~str_replace_all(., "anz_der_umstechungen", "Umstechungen"))

                  
  
  
##### Dipidolor #####

dipidolor <- spss_data %>% 
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

# the data is too large for efficient manipulation: there are two datasets: 
# dipidolor_quantitative is on ID, day, time, first_second taken from the "mg" data

dipidolor_quantitative <- dipidolor %>% dplyr::select(matches("ID|mg")) %>% 
  select_all(~str_replace_all(., "_mg", "")) %>% 
  select_all(~str_replace_all(., "_15", "_second")) %>% 
  mutate(`0_0` = pmax(`0_0_nach_score2`, `0_0_pflege`, na.rm = TRUE)) %>% 
  dplyr::select(ID, `0_0`, everything()) %>% 
  dplyr::select(-c(`0_0_nach_score2`, `0_0_pflege`)) %>% 
  pivot_longer(-ID, values_to = "Dipi_quant", names_to = "time") %>% 
  left_join(
    IID_measures %>% select(ID, Weight) # divided by KG
  )  %>% 
  mutate(Dipi_µgperKg = as.integer(1000*Dipi_quant/Weight)) %>% 
  select(-c(Dipi_quant,Weight))


dipidolor_qualitative <- dipidolor %>% dplyr::select(matches("ID|ja")) %>% 
  select_all(~str_replace_all(., "_gabe_ja_nein", "")) %>% 
  rename(`0_0` = `0_0_1ja`, `0_0_second` = `0_0_15_1ja`) %>% 
  mutate_at(vars(-ID), as.integer) %>% 
  pivot_longer(-ID, values_to = "Dipi_qual", names_to = "time") 
  
library(hablar) # the function "sum_" returns NA when all elements are NA, in contrast to the normal base::sum
# "cum" means cumulative : the first and the second administration
# the effect of the second administration from Dipidolor is not documented

dipidolor <- full_join( dipidolor_quantitative, dipidolor_qualitative, by = c("ID", "time")) %>% 
  mutate(Dipi_binary = as.integer(ifelse(Dipi_µgperKg > 0, 1, Dipi_qual))) %>% select(-Dipi_qual) %>% 
  tidyr::separate(col = "time", c("day", "time", "first_second")) %>% 
  mutate(first_second = ifelse(is.na(first_second), "first", first_second)) %>% 
  pivot_wider(id_cols = c(ID:time), names_from = first_second, values_from = c(Dipi_µgperKg, Dipi_binary) ) %>% 
  rowwise() %>% 
    mutate(
      Dipi_µgperKg_cum = hablar::sum_(c(Dipi_µgperKg_first,Dipi_µgperKg_second), ignore_na = TRUE), 
      Dipi_binary_cum = hablar::sum_(c(Dipi_binary_first,Dipi_binary_second), ignore_na = TRUE)) %>% 
  ungroup() %>% 
  select(-matches("second")) %>% # we have the cumulative and the first dosis, so the 2nd dosis can be calculated easily 
  select_all(~str_replace_all(., "_first", "")) %>% 
  mutate(Dipi_repeat = ifelse(Dipi_binary_cum > 1, 1, 0) %>% as.integer()) %>% 
  transmute(ID, day = as.integer(day), time = as.integer(time), Dipi = Dipi_µgperKg, Dipi_repeat)
  
  
rm(dipidolor_qualitative, dipidolor_quantitative)



##### AINS #####

ains <- spss_data %>%
  dplyr::select(matches("ID|meta|nov|ibu|volt|par|per")) %>%
  dplyr::select(-matches("4_|5_|Vome|Nw|Diclofenac|voltaren")) %>% 
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
  pivot_longer(-ID, values_to = "AINS_mg") %>% 
  left_join( IID_measures %>% select(ID, Weight) ) %>% 
  transmute(ID, name, AINS_mgperkg = as.integer(AINS_mg/Weight)) %>% # the drugs are in mg/Kg body weight
  separate(name, c("day","time","AINS")) %>% 
  #mutate(AINS_binary = ifelse(AINS_mgperkg > 0, 1, AINS_mgperkg) %>% as.integer()) %>% 
  pivot_wider(id_cols = c(ID, day, time),
              names_from = AINS,
              values_from = AINS_mgperkg) %>% 
  group_by(ID, day) %>% 
    summarize_if(is.numeric, ~ sum(., na.rm = TRUE)) %>% 
  ungroup()

# exploratory AINS 
ains %>% mutate_all( ~ replace_na(.,0)) %>% 
  summarise_all(~ length(which(. != 0))) %>%
  pivot_longer(everything())
# Diclofenac has only two values, those columns may be dropped

  
ains %>% mutate_all( ~ replace_na(.,0)) %>% summarise_if(is.numeric, ~ length(which(. != 0)))
# only Metamizol was administered in AWR: 31 times


##### Scores #####

scores <- spss_data %>% 
  dplyr::select(matches("ID|kuss|faces|ppmd")) %>% 
  dplyr::select(-matches("4_tag|5_tag|elternteil")) %>%
  select_all(~str_replace_all(., "x", "")) %>% 
  select_all(~str_replace_all(., "_wert", "")) %>% 
  select_all(~str_replace_all(., "Awr", "0_0")) %>% 
  select_all(~str_replace_all(., "4h", "0_1")) %>% 
  select_all(~str_replace_all(., "8h", "0_2")) %>% 
  select_all(~str_replace_all(., "tag_mo", "0")) %>% 
  select_all(~str_replace_all(., "tag_mi", "1")) %>% 
  select_all(~str_replace_all(., "tag_a", "2")) %>% 
  mutate(`0_1_faces` = as.double(`0_1_faces`)) %>% # this column was as charachter ...
  select_all(~str_replace_all(., "1515", "15_15")) %>% 
  rename(`0_0_15_kuss` = `0_0_kuss_15`, `0_0_15_faces` = `0_0_faces_15`) %>% 
  dplyr::select(-matches("15_15")) %>% 
  select_all(~str_replace_all(., "15", "second")) %>% 
  select_all(~str_replace_all(., "_kuss", "#kuss")) %>% 
  select_all(~str_replace_all(., "_faces", "#faces")) %>% 
  select_all(~str_replace_all(., "_ppmd", "#ppmd")) %>% 
  pivot_longer(-ID) %>% 
  separate(name,  c("day_and_time", "scale"), sep = "#") %>% 
  separate(day_and_time, c("day", "time", "first_second")) %>% 
  mutate(first_second = ifelse(is.na(first_second), "first", first_second)) %>% 
  pivot_wider(id_cols = c(ID:first_second), names_from = scale, values_from = value)

#do not join those two blocks, in the next code block is a full_join with the data itself
scores <- scores %>%
  group_by(ID) %>% 
    summarise_at( vars(kuss, faces) , ~ length(unique(.)) ) %>% 
    mutate(category = ifelse(kuss > faces, "KUSS", "FACES")) %>% select(-c(kuss:faces)) %>% 
    full_join(scores) %>% # here the join with the data itself
  ungroup() %>% 
  pivot_wider(
    id_cols = c(ID, category, day, time),
    names_from = first_second,
    values_from = c(kuss, faces, ppmd)) %>% 
  rowwise() %>% 
    transmute(ID, day, time, category, 
            kuss_first, kuss_second, kuss_diff = -1* hablar::sum_(c(kuss_first, -1 * kuss_second), ignore_na = FALSE),
            faces_first, faces_second, faces_diff = -1* hablar::sum_(c(faces_first, -1 * faces_second), ignore_na = FALSE),
            ppmd = ppmd_first
            ) %>% 
  ungroup() %>% 
  split(.$category) %>% 
  map(~ .x %>% 
        select( -category ) %>% 
        select_if(~sum(!is.na(.)) > 10) %>% # drop the non-populated columns
        select(-matches("second")) %>% 
        select_all(~str_replace_all(., "_first", "")) %>% 
        select_all(~str_replace_all(., "_diff", "_repeat")) 
      ) 
  

###### Fluids and PONV ########

fluids_and_ponv <- spss_data %>%
  select(c(ID, Op_tag_trinkmennge_ml:Vome2)) %>% 
  select(-matches("Nw|4|5|achblutung|keit_erb|keit")) %>% 
  select_all(~str_replace_all(., "Vome", "Vomex_")) %>% 
  select_all(~str_replace_all(., "_ml", "")) %>%
  select_all(~str_replace_all(., "_tag", "")) %>%
  select_all(~str_replace_all(., "Op_", "0_")) %>%
  mutate_at(vars('Erbrechen_op':'Vomex_2'), ~ tidyr::replace_na(.,0)) %>%  # NA in Erbrechen means 0
  rename('0_Vomex' = 'Vomex_', '2_Vomex' = 'Vomex_2', '1_Vomex' = 'Vomex_1',
         '0_trinkmenge' = '0_trinkmennge', 
         '0_Erbrechen' = 'Erbrechen_op', '1_Erbrechen' = 'Erbrechen_1', '2_Erbrechen' = 'Erbrechen_2_uhrzeit') %>% 
  mutate_if(is.numeric, ~as.integer(.)) %>% 
  pivot_longer(-ID) %>% 
  separate(name, c("day", "category"), remove = TRUE) %>% 
  pivot_wider(id_cols = c(ID, day), names_from = category, values_from = value) %>% 
  mutate_at(vars(Erbrechen, Vomex), ~ tidyr::replace_na(.,0)) %>%  # another small "imputation" %>% 
  left_join(IID_measures %>% select(ID, Weight)) %>% 
  transmute(ID, day,
            Trinkmenge = trinkmenge/Weight, Infusion = infusion/Weight, TotalFluids = Trinkmenge + Infusion,
            Erbrechen, Vomex)
  

# original_colnames <- colnames(fluids_and_ponv)
# fluids_and_ponv %<>% rename_all( ~ LETTERS[1:length(original_colnames)])
# left_join(dplyr::select(IID_measures, c(ID, Age = Age_in_months, Weight, Fieber)), by = "ID") %>% 
# mutate(missing = ifelse(is.na(trinkmenge), "*", "")) %>% 
# dplyr::select(ID, day, missing, everything()) %>% 
# missRanger::missRanger(
#   . ~ . -ID -missing -Vomex,
#   num.trees = 1000, maxiter = 100, pmm.k = 5
# ) %>% dplyr::select(ID:Vomex) %>% dplyr::select(-missing) %>% 


TE2 <- list(ains, dipidolor, fluids_and_ponv, scores, IID_measures)
names(TE2) <- c("ains", "dipidolor", "fluids", "scores", "IID")
rm(ains, dipidolor, fluids_and_ponv, scores, IID_measures, spss_data)

TE2$ains$day %<>% as.integer() # small patch (ID, day are now as integer in all the rawlong)

TE2$ains_fluids <- full_join(rawlong$ains, rawlong$fluids, by = c("ID","day")) %>%   
  mutate_at(vars(-ID, -day), ~ ifelse(. > 0, 1, 0)) %>% 
  mutate(PONV = pmax(Erbrechen, Vomex, na.rm = TRUE)) %>% 
  select(-matches("Total|Trink|Erbr|Vome")) 

kuss_IDs <-TE2$scores$KUSS$ID %>% unique()
faces_IDs <- TE2$scores$FACES$ID %>% unique()

save.image(file = paste0(here("input"), "/TE_2_data.RData"))


########### longitudinal data from FACES, KUSS, PPMD ############
clean.it()
Sys.setenv(LANG = "en")
library(readxl)
library(janitor)

path <- here("input/Rohdaten pro Patient")

longitudinal <- list.files(here("input/Rohdaten pro Patient"), pattern = ".xlsx") %>% 
  map_dfr(~  readxl::read_xlsx(
    path = paste0(path, "/", .x),
    range = cell_cols("A:Z"),
    col_types = "text"
  ) %>% 
    rename(realID = reaI_ID, pseudoID = Pseudo) %>% janitor::clean_names() %>% 
    mutate_at(vars(p1:interv), ~ replace_na(.,0)) %>% # no entry represents 0
    mutate_at(vars(p1:interv), ~ dplyr::na_if(.,"N")) %>% # N represents NA
    mutate_at(vars(p1:interv),  ~ as.integer(.)) %>% 
    mutate(total_of_cols= select(., p1:interv) %>% rowSums(na.rm = TRUE)) %>% 
    filter(total_of_cols > 0) %>% select(- total_of_cols) %>% 
    tidyr::fill(pseudo_id, .direction = "downup") %>% 
    tidyr::fill(real_id, .direction = "downup") 
  ) %>% 
  mutate(
    time = str_remove(time, "[D]"),
    time = str_replace(time, "postOP", "01"),
    day = as.integer(substr(time, start = 1, stop = 1)),
    time = as.integer(substr(time, start = 2, stop = 2)),
    time_axis = as.integer((day*3) + time)
  ) %>% dplyr::select(matches("id"), day, time, time_axis, everything()) %>% 
  filter(time_axis <= 14) %>%  # only 4 interventions 4/241 1,6% are after time_axis 15
  mutate_at(vars(time_axis:interv), ~ as.integer(.)) %>% 
  mutate_at(vars(p1:p15), ~ ifelse(. > 0, 1, .)) %>% 
  mutate(total_of_cols= select(., p1:interv) %>% rowSums(na.rm = TRUE)) %>% 
  filter(total_of_cols > 0) %>% select(- total_of_cols) %>% # observ. with all-zeros are dropped
  distinct() %>% 
  rowwise() %>% 
  mutate(
    ppmd = hablar::sum_(c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15),ignore_na = TRUE),
    kuss_sum = hablar::sum_(c(w,g,r,b,m), ignore_na = TRUE),
    weighted_score = hablar::sum_( c(kuss, faces, as.integer(ppmd*2/3)) , ignore_na = TRUE )    
  ) %>% 
  mutate_at(vars(ppmd), ~ ifelse(time_axis == 1, NA, .)) %>% 
  ungroup() %>% 
  mutate_at(vars(day:weighted_score), ~ as.integer(.))


to_drop <- filter(longitudinal, weighted_score == 0 & interv == 1) %>% # drop obs with sum of scores zero and intervention
  bind_rows( filter(longitudinal, is.na(kuss) & is.na(faces) & is.na(ppmd)) ) 

longitudinal <- anti_join(longitudinal, to_drop) 

rm(path, to_drop)

count(longitudinal, interv) # 222 Dipidolor interventions

longitudinal %>% filter(interv == 1) %>% count(time_axis)

longitudinal %>% 
  group_by(real_id) %>% 
  summarise(nr_pseudo_id = length(unique(pseudo_id))) %>%
  arrange(desc(nr_pseudo_id)) %>% 
  ungroup() # pseudo_id is a better identifier, real_id is NA in 27 cases

longitudinal %>% map(~ head(unique(.x)))

count_scores <- list()

no_interv <- longitudinal %>% filter(interv == 0) %>% 
  select(kuss, faces) %>% 
  mutate_all( ~ as.numeric(.)) %>% 
  map(~count(data.frame(x=.x), x) %>% na.omit)
with_interv <- longitudinal %>% filter(interv == 1) %>% 
  select(kuss, faces) %>% 
  mutate_all( ~ as.numeric(.)) %>% 
  map(~count(data.frame(x=.x), x) %>% na.omit())

count_scores$kuss <- no_interv$kuss %>% rename(without_interv = n) %>%
  full_join(rename(with_interv$kuss, with_interv = n)) %>% 
  rename(kuss = x) %>% 
  mutate_at(vars(without_interv, with_interv),
            ~ tidyr::replace_na(.,0) * 100/sum(., na.rm = TRUE)
  ) 

count_scores$faces <- no_interv$faces %>% rename(without_interv = n) %>%
  full_join(rename(with_interv$faces, with_interv = n)) %>% 
  rename(faces = x) %>% 
  mutate_at(vars(without_interv, with_interv),
            ~ tidyr::replace_na(.,0) * 100/sum(., na.rm = TRUE)
  ) 

rm(no_interv, with_interv)

count_scores$ppmd <- longitudinal %>% 
  filter(time_axis > 1 & interv == 0) %>% count(ppmd) %>%
  rename(without_interv = n) %>% na.omit() %>% 
  left_join(
    longitudinal %>% 
      filter(time_axis > 1 & interv == 1) %>% count(ppmd) %>% 
      rename(with_interv = n) %>% na.omit()
  ) %>% 
  mutate_at(vars(without_interv, with_interv),
            ~ tidyr::replace_na(.,0) * 100/sum(., na.rm = TRUE)
  ) 

count_scores %>% map(~ mutate_all(.x, ~ as.integer(.))) # the procents per each scale

# interventions and faces, 128 children


contrast <- longitudinal %>% select(faces, kuss, ppmd) %>%
  map(~ tibble(pseudo_id = longitudinal$pseudo_id,
               scale = .x,
               interv = longitudinal$interv) %>% 
        na.omit() %>% 
        group_by(pseudo_id) %>% 
        summarise(levels_interv = n_distinct(interv)) %>% 
        ungroup %>% 
        filter(levels_interv == 2) 
  ) %>% 
  map(~ .x %>% left_join(longitudinal) %>% 
        dplyr::select(pseudo_id, time_axis, faces, kuss, ppmd, interv) %>% 
        mutate_at(vars(-pseudo_id), ~ as.integer(.)) %>% distinct() 
  ) 

contrast$faces %<>% drop_na(faces) %>% 
  group_split(pseudo_id, interv) %>% 
  map_dfr(~ .x %>% 
            summarise(
              pseudo_id = first(pseudo_id),
              min = min(faces),
              med = median(faces),
              max = max(faces), 
              interv = first(interv), 
            ) %>% mutate_at(vars(-pseudo_id), ~ as.integer(.))
  )
  
  
contrast$kuss %<>% drop_na(kuss) %>% 
  group_split(pseudo_id, interv) %>% 
  map_dfr(~ .x %>% 
            summarise(
              pseudo_id = first(pseudo_id),
              min = min(kuss),
              med = median(kuss),
              max = max(kuss), 
              interv = first(interv), 
            ) %>% mutate_at(vars(-pseudo_id), ~ as.integer(.))
  )


contrast$ppmd %<>% drop_na(ppmd) %>% 
  group_split(pseudo_id, interv) %>% 
  map_dfr(~ .x %>% 
            summarise(
              pseudo_id = first(pseudo_id),
              min = min(ppmd),
              med = median(ppmd),
              max = max(ppmd), 
              interv = first(interv), 
            ) %>% mutate_at(vars(-pseudo_id), ~ as.integer(.))
  )
  




  

