# remove any non-base package
clean.it <- function() {
  basic.packages <- c("package:stats","package:graphics",
                      "package:grDevices","package:utils",
                      "package:datasets","package:methods",
                      "package:base")
  package.list <- dplyr::setdiff( search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)] , basic.packages)
  if (length(package.list)>0)  for(package in package.list) detach(package, character.only=TRUE)
  if(!require(pacman))install.packages("pacman"); require(here)
  
  rm(list = dplyr::setdiff( ls(envir = globalenv()),
                            c("clean.it", "path")
  ),
  envir = globalenv())
  #gc() # or sessionInfo()
  
}

# custom min, max, and diff(range)
min_na <- function(x) {
  library(hablar)
  hablar::min_(x, ignore_na = TRUE)
}

max_na <- function(x) {
  library(hablar)
  hablar::max_(x, ignore_na = TRUE)
}

range_na <- function(x) {
  max(x) - min(x)
}

# this function shows the procent of missing per column
percent_na <- function(df) {
  result <- map_df(df,
                   ~ 100*sum( is.na(.) )/nrow(df)
            ) %>%
    pivot_longer(everything()) %>% arrange(desc(value)) %>% 
    transmute(name, procent_NA_and_0 = paste0(round(value,3), " %"))
  return(result)
}

# this function shows the procent of missing or null values per column
null_na <- function(df) {
  result <- map_df(df, ~ 100*sum(is.na(dplyr::na_if(.,0)))/nrow(df)) %>%
    pivot_longer(everything()) %>% arrange(desc(value)) %>% 
    transmute(name, procent_NA_and_0 = paste0(round(value,3), " %"))
  return(result)
}

# this function removes the columns that are above a cuttoff percent of missing or null values (s. null_and_missing)
select_cols <- function(df, cutoff){ # cutoff between 0 and 1 (100%)
  cols_to_keep <- filter(
    pivot_longer(
      map_df(df, ~ 100*sum(is.na(dplyr::na_if(.,0)))/nrow(df)),
      everything()),
    value <= cutoff)$name
  result <- dplyr::select(df, one_of(cols_to_keep))
  return(result)
}

############### fn_flat2 and fn_flat3 ###################

fn_flat2 <- function(df, firstlevel, secondlevel, cutoff){
  firstlevel <- enexpr(firstlevel)
  secondlevel <- enexpr(secondlevel) 
  df %>% 
      pivot_wider(
        id_cols = !!firstlevel,
        names_from = !!secondlevel,
        names_prefix = toString(secondlevel),
        values_from = -c(!!firstlevel, !!secondlevel)
      ) %>% 
  left_join(
    df %>% 
      group_by(!!firstlevel) %>% 
        summarise_at(
          vars(-toString(secondlevel)),
          list(
            ~ min_na(.),
            ~ median(.,na.rm = T),
            ~ max_na(.),
            ~ sum(., na.rm = TRUE),
            ~ range_na(.))
        ) %>% 
      ungroup(),
    by = toString(firstlevel) 
    ) %>% 
  select(sort(tidyselect::peek_vars())) %>% 
  select(!!firstlevel, matches("_min|_median|_max|_sum|_range"), everything()) %>% 
  select_cutoff(cutoff = cutoff)
}


fn_flat3 <- function(df, firstlevel, secondlevel, thirdlevel, excludecol, cutoff){
  firstlevel <- enexpr(firstlevel)
  secondlevel <- enexpr(secondlevel)
  thirdlevel <- enexpr(thirdlevel)
  excludecol <- enexpr(excludecol)
  df %>% 
    pivot_wider(
      names_from = c(!!secondlevel, !!thirdlevel), 
      values_from = -c(!!firstlevel, !!secondlevel, !!thirdlevel, !!excludecol)
    ) %>% 
  full_join(
    df %>% 
      group_by(!!firstlevel) %>% 
      dplyr::select_if(., is.numeric) %>% 
        summarise_at(
          vars(-c(!!secondlevel, !!thirdlevel )),
          list(
            ~ min_na(.),
            ~ median(.,na.rm = TRUE),
            ~ max_na(.),
            ~ sum(., na.rm = TRUE),
            ~ range_na(.)
          )
        ) %>% 
      ungroup()
    ) %>% 
  full_join(
    df %>% 
      group_by(!!firstlevel, !!secondlevel) %>% 
      dplyr::select_if(., is.numeric) %>% 
        summarise_at(
          vars(-!!thirdlevel),
          list(
            ~ min_na(.),
            ~ median(.,na.rm = TRUE),
            ~ max_na(.),
            ~ sum(., na.rm = TRUE),
            ~ range_na(.)
          )
        ) %>% 
      ungroup() %>% 
      pivot_wider(
        id_cols = !!firstlevel,
        names_from = !!secondlevel,
        names_prefix = toString(secondlevel),
        values_from = -c(!!firstlevel, !!secondlevel)
      ),
      by = toString(firstlevel)
  ) %>% 
  select(sort(tidyselect::peek_vars())) %>% 
  select(!!firstlevel, matches("_min|_median|_max|_sum|_range"), everything()) %>% 
  select_cutoff(cutoff = cutoff)
}

############### Random_forest Functions ##################

# library(mosaic)
# check_struct <- function(df){
#    suppressWarnings(
#      df <- df %>% purrr::map_dfr(suppressWarnings(mosaic::favstats)) %>%
#        cbind(names = names(df)) %>%
#        dplyr::select(names, everything()) %>%
#        mutate(NAs = 100*missing/nrow(df)) %>%
#        cbind(NA_NULL = map_dbl(df, ~ 100*sum(is.na(dplyr::na_if(.,0)))/nrow(df))) %>%
#        dplyr::select(-c(n, missing)) %>%
#        mutate_if(is.numeric, ~ round(., digits = 1)) %>% 
#        mutate(imp = as.integer((100-NA_NULL)*sd)) 
#    )
#    return(df)
#  }


impute_RF <- function(df) {
  if (!require(ranger)) install.packages("ranger")
  library(ranger)
  df %>% missRanger::missRanger(
    num.trees = 1000, maxiter = 100, pmm.k = 3)
}
  

library(tidyverse)
library(Boruta)
library(furrr)

scale01 <- function(x, ...){(x - base::min(x, ...)) / (base::max(x, ...) - base::min(x, ...))} # ... allows "na.rm = TRUE"

corr_RF <- function(df, iter){

    result <- df %>% names() %>% 
      future_map_dfr(
        ~ attStats(
          Boruta::Boruta(
            formula(
              paste0('`', ., '` ~ ', paste(names(df), collapse = " + "))
            ),
            data = df,
            mcAdj = TRUE, doTrace = 0, holdHistory = TRUE, 
            pValue = min(c(1/(ncol(df)*nrow(df)), 0.01)),
            maxRuns = iter
          )
        ), .progress = TRUE
      )

  return(result)
}

reduce_RF <- function(df, sens){
  
selection <- full_join(
    enframe(colMeans(df, na.rm = TRUE)) %>% rename(feature_wise = value),
    enframe(rowMeans(df, na.rm = TRUE)) %>% transmute(predictor_wise = value, name = colnames(df)),
    by = "name"
  ) %>%
  mutate(product = feature_wise*predictor_wise) %>% 
  top_frac(sens, product) %>% pull(name)

  
  df <- mutate(df, target = names(df)) %>% 
  column_to_rownames(var = "target") %>%
  select(one_of(selection)) %>% 
  rownames_to_column(var = "target") %>% 
  filter(target %in% selection)
  
  return(df)
}


  

####################

corr_RF <- function(df, iterations) {
  set.seed(111)
  
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(tidyverse, furrr, Boruta)
  plan(multiprocess)  
  
  suppressWarnings(
        names(df) %>% 
      future_map_dfr(
        ~ attStats(
          Boruta::Boruta(
            formula(
              paste0('`', ., '` ~ ', paste(names(df), collapse = " + "))
            ),
            data = na.omit(df),
            mcAdj = TRUE, doTrace = 0, holdHistory = TRUE, 
            pValue = base::min(c(1/(ncol(df)*nrow(df)), 0.01)),
            maxRuns = iterations
          )
        ) %>%
          rownames_to_column() %>% 
          mutate(Score = ifelse(decision == "Rejected", 0, medianImp*normHits)) %>%
          mutate_at(vars(Score), ~ (. - base::min(.,na.rm = TRUE)) / 
                      (base::max(., na.rm = TRUE) - base::min(., na.rm = TRUE))) %>% 
          mutate(Score = dplyr::na_if(Score, 1)) %>%
          dplyr::select(rowname, Score) %>% 
          pivot_wider(names_from = rowname, values_from = Score),
        .progress = TRUE
      ) 
  )
}


  




