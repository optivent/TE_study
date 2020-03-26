## Data exploration

clean.it <- function() {
  basic.packages <- c("package:stats","package:graphics",
                      "package:grDevices","package:utils",
                      "package:datasets","package:methods",
                      "package:base")
  package.list <- dplyr::setdiff( search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)] , basic.packages)
  if (length(package.list)>0)  for(package in package.list) detach(package, character.only=TRUE)
  if(!require(pacman))install.packages("pacman"); require(here)
  
  pacman::p_load(here, VIM, janitor, missRanger,
                 tidyverse,magrittr,purrr,
                 DataExplorer)
  
  rm(list = dplyr::setdiff( ls(envir = globalenv()),
                            c("clean.it")
  ),
  envir = globalenv())
  #gc() # or sessionInfo()
  
}
clean.it()


# load data from "import_data.R"

load(paste0(here("input"), "/TE_data.RData"))


# the function that extracts the difference between "first" and "second" per day

DiffAbs <- function(x) {
  dplyr::first(x) - dplyr::last(x) 
}

# extract scores from r$scores and flatten the data with the first measure and the difference
# the ppmd scores are not taken twice, this operation is not for ppmd
# all variables are converted to integer

scores <- r$scores %>% group_by(ID) %>% 
  summarise_at( vars(kuss, faces) , ~ length(unique(.)) ) %>% 
  mutate(category = ifelse(kuss > faces, "KUSS", "FACES")) %>% select(-c(kuss:faces)) %>% 
  full_join(r$scores) %>% 
  select(category, everything()) %>% 
  group_by(category, ID,day,time) %>% 
    summarise_at(vars(kuss:ppmd), 
               list(first = first, 
                    diff = function(x) { dplyr::first(x) - dplyr::last(x) }
                    )
    ) %>%
  ungroup() %>% 
  transmute(category, ID, day, time,
            scale = pmax(kuss_first, faces_first, na.rm = TRUE),
            diff = pmax(kuss_diff, faces_diff, na.rm = TRUE) %>% replace_na(.,0),
            ppmd = ppmd_first
  ) %>% mutate_at(vars(-category), ~ as.integer(.)) %>% 
  arrange(ID, day, time,) %>% split(.$ID) %>% 
  #map_dfr( ~ .x %>% fill(names(.x), .direction = "downup")) %>%   # least observation called forward and backward
  split(.$category) #%>% 
  map(~ .x %>% select(-category))

scores_wide <- scores %>%
  map_dfr(~ pivot_wider(data = .x, id_cols = c(ID:time), names_from = time, names_prefix = "time_", values_from = c(scale:ppmd)) )
                                                     
scores %>% map


  
  
  
  




# this line is required in the next paragraph with ains_fluids_ponv for the imputation of trinkmenge 
dipidolor_and_scores <- full_join(r$dipidolor, r$scores) 

# for AINS, only the binary and not the quantitative form
ains_fluids_ponv <- r$ains %>% # take the r$ains from the list
  dplyr::select(-matches("mg|Ibuprofen|Diclofenac")) %>% ## !!!! Ibuprofen has no variance, can be excluded
  select_all(~str_replace_all(., "AINS_binary_", "")) %>%
  mutate(ID = as.integer(ID)) %>% # useful for the joining
  group_by(ID, day) %>%
    summarise_at(vars(Metamizol:Paracetamol), ~ sum(., na.rm = TRUE)) %>% 
  ungroup() %>% 
  full_join(r$fluids_and_ponv) %>% # ains is binded with fluids_ponv (they have the same daily structure)
  left_join(dplyr::select(IID_measures, c(ID, Weight))) %>% 
  mutate_at(vars(ID,day), ~ as.integer(.)) %>% 
  left_join(
    full_join(r$dipidolor, r$scores) %>% # also add dipidolor and scores also for 
      group_by(ID, day) %>% 
        summarise_at(vars(Dipi_binary:ppmd), ~ sum(., na.rm = TRUE)) %>% # the sum of Dipi and scores per day is taken only for the imputation of trinkmenge
      ungroup() %>%
      mutate_all( ~ as.integer(.))
  ) %>% 
  mutate(
    trinkmenge = trinkmenge/Weight, infusion = infusion/Weight,  #  trinkmenge, infusion, daily_fluids as mL/Kg
    NA_trinkmenge = ifelse(is.na(trinkmenge),"*","")) %>%        # mark the values that will be imputed
  missRanger::missRanger(
    trinkmenge ~ day + Erbrechen + Weight + Dipi_binary + Metamizol + kuss + faces + ppmd, # the formula for the RF imputation
    num.trees = 10000, maxiter = 100, pmm.k = 3
  ) %>% 
  mutate(daily_fluids = trinkmenge + infusion) %>% 
  mutate_if(is.numeric, ~ as.integer(.)) %>% 
  dplyr::select(c(ID:infusion, daily_fluids, Erbrechen:Vomex)) # drop the extra columns 

# the number of unique elements in each column
ains_fluids_ponv %>% map(~count(data.frame(x=.x), x)) 


# the dipidolor_and_scores was previously loaded for the ains_fluids_ponv imputation
dipidolor_and_scores %<>% mutate_at(vars(ID:time), ~ as.integer(.)) %>% 
  left_join(IID_measures %>% transmute(ID = as.integer(ID), Weight)) %>% # import the weight from IIDmeasures
  rename(Dipiµgperkg = Dipi_mg) %>% # Dipidolor in µg / KG
  mutate(Dipiµgperkg = replace_na(1000*Dipiµgperkg/Weight,0),
         Dipi_binary = replace_na(Dipi_binary,0),
         ) %>% 
  mutate_if(is.numeric, ~ as.integer(.)) %>% 
  select(-Weight) # drop the weight





# the children with kiss
Dipi_Faces_Ppmd <- dipidolor_and_scores %>% filter(ID %in% faces_children) %>% filter(first_second == "first") %>% 
  select(-kuss) %>% plot_missing()

Dipi_Faces_Ppmd <- 
# the children with faces
Dipi_Kuss_Ppmd <- dipidolor_and_scores %>% filter(ID %in% kuss_children); rm(kuss_children)










dipidolor_and_scores %>%
  group_by(ID, day, time) %>% 
  select(kuss:faces) %>% 
  summarise_all(
    list(first = first, DiffAbs = DiffAbs)
  ) %>% View()
               
  
  
  
  
pivot_wider(
    id_cols = c(ID:time),
    names_from = first_second,
    values_from = c(Dipiµgperkg:ppmd))


  


  


# the summary_per_day ! cointains only repeated measurement, no IID data
daily_summary <- r %>% # that's the list with repeated measurements
  map( ~ mutate(.x, ID = as.integer(ID), day = as.character(day))) %>% 
  map( ~ group_by(.x, ID, day)) %>% 
  map( ~ summarise_if(.x, is.numeric,
    list( #~ min(., na.rm = TRUE),
          ~ max(., na.rm = TRUE),
          ~ median(., na.rm = TRUE),
          ~ sum(., na.rm = TRUE)
          #~ diff(range(., na.rm = TRUE))
     ))) %>% 
  reduce(full_join, by = c("ID", "day")) %>% ungroup() %>% 
  dplyr::select(-matches("AINS")) %>% # remove the ains
  full_join(
    dplyr::select(r$ains, -matches("mg")) %>%
      select_all(~str_replace_all(., "AINS_binary_", "")) %>%
        mutate(ID = as.integer(ID)) %>% 
        group_by(ID, day) %>%
          summarise_at(vars(Diclofenac:Paracetamol), ~ sum(., na.rm = TRUE)) %>% 
        ungroup()
  ) %>% mutate(ID = as.factor(ID), day = as.factor(day)) %>% 
  mutate_if(is.numeric, ~ ifelse(is.infinite(.), NA, .)) %>% dplyr::na_if(NaN) %>% 
  dplyr::select(-c(kuss_sum:ppmd_sum))

  dplyr::select(-matches("Pseudonym|_mg_")) %>% 


  mutate_at(vars(Diclofenac_max:Vomex_median), )


select_all(~str_replace_all(., "AINS_binary_", "")) %>% 
  
  select_all(~str_replace_all(., "binary_", "")) %>% 

scale01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

daily_summary %>% select_at(vars(Diclofenac_max:Vomex_median)) %>% 
  mutate_all(~ scale01, na.rm = TRUE)
 
  pivot_longer(-c(ID, day)) %>% 
  separate(name, c("categ","funct")) 

daily_summary %>% group_by(categ, funct, day) %>% summarise(length(unique(.)))


  


  pivot_wider(id_cols = ID, names_from = day, values_from = -matches("ID|day")) %>% 

  
  
  

  

daily_summary %>% dplyr::select_if(is.numeric) %>% 
  map( ~ scale(.x, center = FALSE, scale = TRUE)) %>% # scaled columns
  map_dfr( ~ var(., na.rm = TRUE)) %>% pivot_longer(everything()) %>% 
  transmute(name, variance = round(value, 3) %>% replace_na(0)) %>% 
  filter(variance < 0.1) %>% pull(name) -> cols_to_drop # are dropped when the variance < 0.1
  
  
summary_per_ID %>% dplyr::transmute(
    ID, percent_missing_rows = round(100*rowSums(is.na(.))/ncol(summary_per_ID))
    ) %>% arrange(desc(percent_missing_rows)) %>% View() # the percent of missing rows per patient

summary_per_ID %>% map_dfr( ~ round(100*mean(is.na(.)),2)) %>% 
    pivot_longer(everything()) %>% arrange(desc(value)) %>% View() # the percent of missing values per column
  

summary_per_ID %<>% dplyr::select(one_of(cols_to_keep)); rm(cols_to_keep)

summary_per_ID %>%
  select_if(is.numeric) %>% 
  map_dfr( ~ round(100*mean(is.na(.)),2)) %>% 
  pivot_longer(everything()) %>% 
  filter(value > 0) %>% 
  mutate(name =  str_replace_all(name, "menge_", "menge_mL_")) %>% 
  mutate(name =  str_replace_all(name, "kuss_", "scale_kuss_")) %>% 
  mutate(name =  str_replace_all(name, "faces_", "scale_faces_")) %>% 
  mutate(name =  str_replace_all(name, "ppmd_", "scale_ppmd_")) %>% 
  separate(name, c("A","B","C","D","E")) 

  

imputed_summ_perID %>% select(matches(""))


