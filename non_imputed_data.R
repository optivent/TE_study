###### head and functions ######

source("functions.R")
clean.it(); 

pacman::p_load(here, tidyverse,magrittr,purrr, DataExplorer)


# load data from "import_data.R"

load(paste0(here("input"), "/TE_data.RData"))



####### Longitudinal: data per day-subunit #######

multiple_daily <- rawlong$scores %>%
  map( ~ .x %>% left_join(rawlong$dipidolor)) %>% 
  map( ~ .x %>% mutate(Dipi_boli = ifelse(Dipi > 1, 1, Dipi) + Dipi_repeat) ) # transform µg/Kg in 1/0 and add dipi_repeat (1/0)



####### Longitudinal: data per day-subunit #######

once_daily <- multiple_daily %>%
map( ~
  .x %>% 
    group_by(ID, day) %>% 
      summarise_at(
        vars(-matches("repeat"), -Dipi, -time),
        list(
          ~ min(.),
          ~ median(.,na.rm = T),
          ~ max(.),
          ~ sum(., na.rm = TRUE),
          ~ range(.)
        ) # here the (custom) summarisation functions
      ) %>%
    ungroup() %>% 
  left_join(
  .x %>% 
      pivot_wider(
        id_cols = c(ID,day),
        names_from = "time",
        names_prefix = "time",
        values_from = matches("faces|kuss|ppmd|Dipi")
      ) %>% # here the scores in wide_format
      select(ID, day, matches("faces_time|kuss_time|ppdm_time|Dipi_boli")) # only binary Dipi
  ) %>% 
  left_join(
    rawlong$ains %>% 
      mutate_at(vars(-ID, -day), ~ ifelse(. > 0, 1, .)) %>% 
      rename_at(vars(-ID, -day), ~ paste0(.,"_bin")) # only binary AINS
  ) %>% 
  left_join(
    rawlong$fluids %>%
      transmute(ID, day,
                Infusion = ifelse(Infusion > 0, 1, Infusion),
                PONV = pmax(Erbrechen, Vomex, na.rm = TRUE)
      )
  ) %>% 
    select_all(~str_replace_all(., "_boli", "")) %>% 
    select_all(~str_replace_all(., "_bin", "")) %>% 
    select(sort(tidyselect::peek_vars())) %>% 
    select(ID, day, 
           matches("kuss|faces|ppmd"),
           matches("Dipi"), matches("Metamizol|Paracetamol|Ibuprofen"),
           matches("Infusion|PONV"), everything()) %>%
    mutate_at(vars(-ID), ~ as.integer(.)) 
) 



####### flattened_data ############

flat <- rawlong$scores %>%
map(~
  .x %>%
    left_join(
      rawlong$dipidolor %>% mutate(Dipi = ifelse(Dipi>0,1,Dipi))
    )
) %>% 
map(~
  .x %>% 
    pivot_wider(
      id_cols = ID,
      names_from = c(day,time),
      values_from = matches("kuss|faces|ppmd|dipi")
    ) %>% 
  left_join(
    .x %>% 
      group_by(ID, day) %>% 
        summarise_at(
          vars(matches("faces|kuss|ppmd|dipi")),
          list(
            ~ min(.),
            ~ median(.,na.rm = T),
            ~ max(.),
            ~ sum(., na.rm = TRUE),
            ~ range(.)
          )
        ) %>% 
        pivot_wider(
          id_cols = ID,
          names_from = day,
          names_prefix = "day_",
          values_from = matches("faces|kuss|ppmd|dipi")
        ) %>% 
      ungroup()
  ) %>% 
  left_join(
    .x %>% 
      group_by(ID) %>% 
        summarise_at(
          vars(matches("faces|kuss|ppmd|dipi")),
          list(~ min(.),
               ~ median(.,na.rm = T),
               ~ max(.),
               ~ sum(., na.rm = TRUE),
               ~ range(.))
        ) %>%
      ungroup()
  )  
) %>% 
map(~
  .x %>%
    left_join(
      rawlong$ains_fluids %>% 
        group_by(ID) %>% 
        pivot_wider(
          id_cols = ID, 
          names_from = day,
          names_prefix = "day",
          values_from = c(Ibuprofen, Paracetamol, Metamizol, Infusion, PONV)
        ) %>% 
        ungroup()
    ) %>% 
    left_join(
      rawlong$ains_fluids %>% 
        group_by(ID) %>% 
        summarise_at(
          vars(matches("Ibu|Para|Meta|Infu|PONV")),
          list(
            ~ min(.),
            ~ median(.,na.rm = T),
            ~ max(.),
            ~ sum(., na.rm = TRUE),
            ~ range(.))
        ) %>% 
        ungroup()
    )
) %>%
map_dfr(~
  .x %>%
    left_join(
      rawlong$IID %>%
        select(-c(Dipidolor_01, Sum_Dipigabe)) %>%
        rename(Indikation_aber_nicht = "Dipi_Indik.nicht_erhalten") %>% 
        rename_at(vars(-ID), ~ paste0("NR_",.)) 
    ) %>% 
    mutate_at(vars(-ID), ~ as.integer(.))
) %>% 
select(sort(tidyselect::peek_vars())) %>%
select(
  ID, matches("kuss|faces|ppmd"),matches("Dipi"),matches("Meta|Para|Ibu"),matches("Infusion|PONV"), matches("NR_") ,everything()
) %>% 
select_cutoff(cutoff = 95) 

  