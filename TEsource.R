# reference GIT YT kL6L2MNqPHg
# library(usethis)
# ?use_github
# edit_r_environ()
# use_github(protocol = "https", auth_token = Sys.getenv("GITHUB_PAT"))

clean.it <- function() {
  basic.packages <- c("package:stats","package:graphics",
                      "package:grDevices","package:utils",
                      "package:datasets","package:methods",
                      "package:base")
  package.list <- dplyr::setdiff( search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)] , basic.packages)
  if (length(package.list)>0)  for(package in package.list) detach(package, character.only=TRUE)
  ll <- dplyr::setdiff( ls(envir = globalenv()), ## objects to exclude from cleaning
                        c("clean.it")) 
  rm(list = ll, envir = globalenv()); gc() # or sessionInfo()
  if(!require(pacman))install.packages("pacman")
  pacman::p_load(here, 
                 tidyverse,magrittr,
                 stringi,zoo,here)
}
clean.it()

path <- here("input")




setwd("~/OneDrive/DATA_CODE/TE")
clean.it <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- dplyr::setdiff( search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)] , basic.packages)
  if (length(package.list)>0)  for(package in package.list) detach(package, character.only=TRUE)
  ll <- dplyr::setdiff( ls(envir = globalenv()) , c("clean.it","select") ) ## objects to exclude 
  rm(list = ll, envir = globalenv()); gc() # or sessionInfo()
}
clean.it()

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, xlsx)
select <- dplyr::select

rawdata2 <- read_sav("Datensatz_TO_TE_komplett_neu_n158.sav") %>% as_tibble()

admit_expression <- paste(
  c("Pseudonym", "Geschlecht","Gewicht", "Alter_Monate", "_KUSS", "_Faces", "AWR_Dipi_1ja",
    "_PPMD_", "Gabe", "_ml", "Erbrechen","Übelkeit","Nachblutung", "Fieber", "Antibiose", "Indikation_Dipi_2._Tag_mo"),
  collapse = "|")
reject_expression <- paste(
  c("5","Station","Häufigkeit_","Eltern","_ohne_"),
  collapse = "|")

TE2 <- select(rawdata2, 
              matches(admit_expression),
             -matches(reject_expression)
              ) %>% 
      select_all(~str_replace_all(., "@", "")) %>%
      select_all(~str_replace_all(., "Tag_mo", "0")) %>%
      select_all(~str_replace_all(., "Tag_mi", "1")) %>%
      select_all(~str_replace_all(., "Tag_a", "2")) %>% 
      select_all(~str_replace_all(., "_Wert", "")) %>%
      select_all(~str_replace_all(., "Dipi__Gabe_JaNein", "Intervention")) %>% 
      select_all(~str_replace_all(., "AWR", "0.0")) %>%
      select_all(~str_replace_all(., "4h", "0.1")) %>%
      select_all(~str_replace_all(., "8h", "0.2")) %>%
      rename(pseudoID = Pseudonym,
             age_in_months = Alter_Monate,
             gender = Geschlecht,
             weight = Gewicht)
             

write.xlsx(TE2, file = "TE2.xlsx", col.names = TRUE, row.names = TRUE)
  



  



