# reference GIT YT kL6L2MNqPHg
# library(usethis)
# ?use_github
# edit_r_environ()
# use_github(protocol = "https", auth_token = Sys.getenv("GITHUB_PAT"))
# install.packages("here")

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
  
}
clean.it()

read_sav("input/Datensatz_TO_TE_komplett_neu_n158.sav") %>% select_if(is.labelled) %>% map(function(x) attr(x, 'labels')) 

spss_data_n158 <- read_sav("input/Datensatz_TO_TE_komplett_neu_n158.sav") %>% 
                    as_tibble() %>% 
                    janitor::clean_names() %>% 
                    rename(Age_in_months = alter_monate, Gender = geschlecht, Weight = gewicht, ID = nr) 
  
colnames(spss_data_n158) <- colnames(spss_data_n158) %>% str_replace("x", "") %>% str_to_sentence() %>% str_replace("Id","ID")

IID_measures <- spss_data_n158 %>%
  select(c(ID:Fa_assistenzarzt, Fieber)) %>%
  select(-matches("Alter|Vome_|Novalgin")) %>% 
  mutate(ID = as.integer(ID), Gender = ifelse(Gender == 0, "m", "f")) %>% 
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE),.)) %>% 
  mutate_if(is.double, ~ as.integer(.)) 

IID_measures %>% 

aggr(col=c('navyblue','red'), cex.axis=.7, gap=3, numbers=TRUE, sortVars=TRUE, ylab=c("Histogram of missing data","Pattern"))

plot_missing(IID_measures)
glimpse(IID_measures)


# there are some columns with labels, store the spss labels in a list.


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
  mutate(first_second = ifelse(is.na(first_second), "first", first_second)) %>% 
  transmute(ID, day, time, first_second, Dipi_mg = Dipi_quant, Dipi_binary)

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

naniar::gg_miss_upset(dipidolor)

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
  pivot_longer(-ID, values_to = "AINS_mg") %>% 
  separate(name, c("day","time","AINS")) %>% 
  mutate(AINS_binary = ifelse(AINS_mg > 0, 1, as.integer(AINS_mg))) %>% 
  pivot_wider(id_cols = c(ID, day, time),
              names_from = AINS,
              values_from = c(AINS_mg, AINS_binary)) %>% 
  dplyr::select(sort(names(.))) %>% dplyr::select(ID, day, time, everything())


###############

# scores %>% map_df(~sum(is.na(.))) %>% pivot_longer(-ID) %>%
#   transmute(name, percent = 100*value/nrow(scores)) %>%
#   arrange(desc(percent)) %>% View() # percent of missing values
# scores %>% dplyr::select(-matches("15_15")) %>% plot_missing()

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

###################

fluids_and_ponv <- spss_data_n158 %>%
  select(c(ID, Op_tag_trinkmennge_ml:Vome2)) %>% 
  select(-matches("Nw|4|5|achblutung|keit_erb|keit")) %>% 
  select_all(~str_replace_all(., "Vome", "Vomex_")) %>% 
  select_all(~str_replace_all(., "_ml", "")) %>%
  select_all(~str_replace_all(., "_tag", "")) %>%
  select_all(~str_replace_all(., "Op_", "0_")) %>%
  mutate_at(vars('Erbrechen_op':'Vomex_2'), ~ tidyr::replace_na(.,0)) %>% 
  rename('0_Vomex' = 'Vomex_', '2_Vomex' = 'Vomex_2', '1_Vomex' = 'Vomex_1',
         '0_trinkmenge' = '0_trinkmennge', 
         '0_Erbrechen' = 'Erbrechen_op', '1_Erbrechen' = 'Erbrechen_1', '2_Erbrechen' = 'Erbrechen_2_uhrzeit') %>% 
  mutate_all(~as.integer(.)) %>% 
  mutate_at(vars('0_infusion':'3_infusion'), ~ tidyr::replace_na(.,0))

fluids_and_ponv %>% aggr(col=c('navyblue','red'), cex.axis=.7, gap=3,
     numbers=TRUE, sortVars=TRUE, #labels=names(df),
     ylab=c("Histogram of missing data","Pattern"))
fluids_and_ponv %>% filter_at(vars(ID:Fieber), any_vars(is.na(.))) %>% naniar::gg_miss_upset()

fluids_and_ponv %>% 
  full_join(
  IID_measures %>% select(ID, Age_in_months, Weight, Fieber), by = "ID"
  ) %>% 
  filter_at(vars(ID:Fieber), any_vars(is.na(.))) %>%
  View()  

# library(missRanger)
# fluids_and_ponv %<>%  full_join(IID_measures %>% select(ID, Age_in_months, Weight, Fieber), by = "ID") 
# original_colnames <- colnames(fluids_and_ponv)
# fluids_and_ponv %<>% rename_all( ~ LETTERS[1:length(original_colnames)])
# fluids_and_ponv %<>% missRanger::missRanger(
#     . ~ . -A,
#   num.trees = 1000, maxiter = 100, pmm.k = 5
#   ) %>% 
#   rename_all( ~ original_colnames) # %>% 
#   dplyr::select(-c(Age_in_months, Weight, Fieber))

fluids_and_ponv %<>%
  pivot_longer(-ID) %>% 
  separate(name, c("day", "category"), remove = TRUE) %>% 
  pivot_wider(id_cols = c(ID, day), names_from = category, values_from = value) %>% 
  mutate_at(vars(Erbrechen, Vomex), ~ tidyr::replace_na(.,0))
  
#plot_missing(fluids_and_ponv)


test <- fluids_and_ponv %>% 
  #filter(ID %in% list_of_pat_NA_trinkmenge) %>% 
  left_join(dplyr::select(IID_measures, c(ID, Age = Age_in_months, Weight, Fieber)), by = "ID") %>% 
  missRanger::missRanger(
        . ~ . -ID,
       num.trees = 1000, maxiter = 100, pmm.k = 5
       )



###################
library(mitml)
data(studentratings)

fml <- ReadAchiev + ReadDis + SchClimate ~ 1 + (1|ID)
imp <- panImpute(studentratings,
                 formula=fml,
                 n.burn=1000, n.iter=100, m=100)
summary(imp)
plot(imp, trace="all", print="beta", pos=c(1,2))

implist <- mitmlComplete(imp, "all")

library(lme4)
fit <- with(mitmlComplete(imp, "all"),
            lmer(ReadAchiev ~ 1 + ReadDis + (1|ID))
            )

test <- testEstimates(fit, var.comp=TRUE)
##############
library(jomo)
# make sure sex is a factor:
cldata<-within(cldata, sex<-factor(sex))
# we define the data frame with all the variables
data<-cldata[,c("measure","age", "sex", "city")]
# And the formula of the substantive lm model
# sex as an outcome only because it is the only binary variable in the dataset...
formula<-as.formula(sex~age+measure+(1|city))
#And finally we run the imputation function:
imp<-jomo.glmer(formula,data, nburn=2, nbetween=2, nimp=2)
# Note we are using only 2 iterations to avoid time consuming examples,
# which go against CRAN policies. In real applications we would use
# much larger burn-ins (around 1000) and at least 5 imputations.
# Check help page for function jomo to see how to fit the model and
# combine estimates with Rubin's rules