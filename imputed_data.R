####### imputed_data #######

clean.it <- function() {
  basic.packages <- c("package:stats","package:graphics",
                      "package:grDevices","package:utils",
                      "package:datasets","package:methods",
                      "package:base")
  package.list <- dplyr::setdiff( search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)] , basic.packages)
  if (length(package.list)>0)  for(package in package.list) detach(package, character.only=TRUE)
  if(!require(pacman))install.packages("pacman"); require(here)
  
  rm(list = dplyr::setdiff( ls(envir = globalenv()),
                            c("clean.it")
  ),
  envir = globalenv())
  #gc() # or sessionInfo()
  
}; clean.it()
source("functions.R")
pacman::p_load(here, tidyverse,magrittr,purrr, DataExplorer)


# load data from "import_data.R"

load(paste0(here("input"), "/TE_data.RData"))

rawlong$scores$FACES$scale <- "FACES"
rawlong$scores$KUSS$scale <- "KUSS"
cutoff <- 95

flat_imputed <- rawlong$scores %>% reduce(full_join) %>%
    select(ID, day, time, scale, ppmd, everything()) %>%
    select(-matches("repeat")) %>% 
    group_by(ID) %>%
      tidyr::fill(dplyr::matches("faces|ppmd|kuss"), .direction = "downup") %>% 
    ungroup() %>% 
left_join(
    rawlong$dipidolor %>% transmute(ID, day, time, Dipi = ifelse(Dipi > 0, 1, Dipi) %>% replace_na(., 0)) 
) %>% 
  fn_flat3(firstlevel = ID, secondlevel = day, thirdlevel = time, excludecol = scale ,cutoff = cutoff) %>% 
left_join(rawlong$ains_fluids %>% mutate_all(~ replace_na(.,0)) %>% 
  fn_flat2(firstlevel = ID, secondlevel = day, cutoff = cutoff)
) %>% 
left_join(rawlong$IID %>% 
    select(-c(Dipidolor_01, Sum_Dipigabe)) %>%
    mutate_all(~ replace_na(., mean(., na.rm = TRUE))) %>% 
    rename(Indikation_aber_nicht = "Dipi_Indik.nicht_erhalten") %>% 
    rename_at(vars(-ID), ~ paste0("NR_",.))
) %>% 
  mutate_if(is.numeric, ~ as.integer(.)) %>% 
  select(sort(tidyselect::peek_vars())) %>% select(ID, matches("ppmd|kuss|faces"), matches("Dipi"), everything()) %>% 
  split(.$scale) %>% 
  map(~ .x %>% select_cutoff(cutoff = cutoff))


flat_imputed$FACES %>% plot_missing()
flat_imputed$KUSS %>% plot_missing()

primul_test <- flat_imputed$KUSS %>% corr_RF(iter = 300)


  
  

