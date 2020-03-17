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
                 tidyverse,magrittr,
                 stringi,zoo,DataExplorer)
  
  rm(list = dplyr::setdiff( ls(envir = globalenv()),
                            c("clean.it", "path")
            ),
     envir = globalenv())
  #gc() # or sessionInfo()
  
}
clean.it()

spss_data_n158 <- read_sav("input/Datensatz_TO_TE_komplett_neu_n158.sav") %>% as_tibble() 
dim(spss_data_n158)

spss_data_n158 %<>% janitor::clean_names() %>% janitor::remove_empty(c("rows", "cols")) 
dim(spss_data_n158)

spss_data_n158 %<>% rename(age_in_months = alter_monate, gender = geschlecht, weight = gewicht)

colnames(spss_data_n158) <- colnames(spss_data_n158) %>% str_replace("x", "") %>% str_to_sentence()
dim(spss_data_n158)

sapply(spss_data_n158, function(x) sum(is.na(x))) %>% enframe() %>%
  mutate(percent = (100*value)/nrow(spss_data_n158)) %>% 
  arrange(desc(percent)) %>% View()


threshold <- 72 # discard columns with more than 72 missing values

spss_data_n158  %<>% purrr::discard(~sum(is.na(.x))/length(.x)* 100 >= threshold) 
dim(spss_data_n158)

spss_data_n158 %<>% unite("ID", 1:3, sep = "_", remove = TRUE, na.rm = FALSE)
dim(spss_data_n158)

dipidolor <- spss_data_n158 %>% dplyr::select(matches("ID|Dipi|dipi"))
colnames(dipidolor)
#tail(colnames(dipidolor))

dipidolor %<>% dplyr::select(-c(2,3,42:44)) 
dipidolor %<>% dplyr::select(-matches("mg|Indikation")) 
colnames(dipidolor) %>% enframe() 

dipidolor  %<>%  
  select_all(~str_replace_all(., "tag_mo", "0")) %>%
  select_all(~str_replace_all(., "tag_mi", "1")) %>%
  select_all(~str_replace_all(., "tag_a", "2")) %>% 
  select_all(~str_replace_all(., "Awr_dipi_1ja", "0_0")) %>% 
  select_all(~str_replace_all(., "4h_dipi_gabe_ja_nein", "0_1")) %>% 
  select_all(~str_replace_all(., "8h_dipi_gabe_ja_nein", "0_2")) %>% 
  select_all(~str_replace_all(., "_dipi_gabe_ja_nein", "")) %>% 
  select_all(~str_replace_all(., "_dipi", ""))
  
dipidolor %<>% as.matrix() %>% as_tibble() 
dipidolor %<>% mutate_at(vars(`0_0`:`3_2`), as.integer) 

#dipidolor  %<>%  mutate(prob_interv = 100*rowMeans( select(., colnames(dipidolor[,-1])) , na.rm = TRUE ))
# ggplot(dipidolor, aes(x= prob_interv)) + geom_histogram()
# nrow(filter(dipidolor, prob_interv > 0))

dipidolor %>% pivot_longer(-ID, names_to = "time", values_to = "dipidolor") %>% 
  mutate(dipidolor = as.integer(dipidolor)) %>% View()


ains <- spss_data_n158 %>% dplyr::select(matches("ID|meta|nov|ibu|volt|par")) %>%
  select(-matches("4_")) 

ains %>% select_at(.vars=numericVars) %>% 
  summarise_all(.funs=var) %>%
  filter_all(any_vars(. == 0))
  
  
  
  transmute(ID = ID,
    0_0_Novalgin = Novalgin_perioperativ_ja1,
            
    )




  
ains <- ains %>% select(-3)%>%
  select_all(~str_replace_all(., "tag_", "x")) %>%
  select_all(~str_replace_all(., "Awr_", "0_1")) %>%
  select_all(~str_replace_all(., "Op_x", "0_2")) %>% 
  select_all(~str_replace_all(., "Awr_dipi_1ja", "0_0")) %>% 





processed_data_n158 <- select(spss_data_n158, 
              matches(admit_expression),
             -matches(reject_expression)
              ) %>% 

      select_all(~str_replace_all(., "tag_mo", "0")) %>%
      select_all(~str_replace_all(., "tag_mi", "1")) %>%
      select_all(~str_replace_all(., "tag_a", "2")) %>% 
      select_all(~str_replace_all(., "_Wert", "")) %>%
      select_all(~str_replace_all(., "Dipi__Gabe_JaNein", "dipidolor")) %>% 
      select_all(~str_replace_all(., "dipi_1ja", "dipidolor")) %>% 
      select_all(~str_replace_all(., "dipi_gabe_ja_nein", "dipidolor")) %>% 
      select_all(~str_replace_all(., "AWR", "0.0")) %>%
      select_all(~str_replace_all(., "4h", "0.1")) %>%
      select_all(~str_replace_all(., "8h", "0.2")) %>%
      rename(pseudoID = Pseudonym,
             age_in_months = Alter_Monate,
             gender = Geschlecht,
             weight = Gewicht)
             
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
  



  



