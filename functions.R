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
min <- function(x) {
  library(hablar)
  hablar::min_(x, ignore_na = TRUE)
}

max <- function(x) {
  library(hablar)
  hablar::max_(x, ignore_na = TRUE)
}

range <- function(x) {
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
null_and_missing <- function(df) {
  result <- map_df(df, ~ 100*sum(is.na(dplyr::na_if(.,0)))/nrow(df)) %>%
    pivot_longer(everything()) %>% arrange(desc(value)) %>% 
    transmute(name, procent_NA_and_0 = paste0(round(value,3), " %"))
  return(result)
}

# this function removes the columns that are above a cuttoff percent of missing or null values (s. null_and_missing)
select_cutoff <- function(df, cutoff){ # cutoff between 0 and 1 (100%)
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
            ~ min(.),
            ~ median(.,na.rm = T),
            ~ max(.),
            ~ sum(., na.rm = TRUE),
            ~ range(.))
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
            ~ min(.),
            ~ median(.,na.rm = TRUE),
            ~ max(.),
            ~ sum(., na.rm = TRUE),
            ~ range(.)
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
            ~ min(.),
            ~ median(.,na.rm = TRUE),
            ~ max(.),
            ~ sum(., na.rm = TRUE),
            ~ range(.)
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

#library(mosaic)

check_struct <- function(df){
   suppressWarnings(
     df <- df %>% purrr::map_dfr(suppressWarnings(mosaic::favstats)) %>%
       cbind(names = names(df)) %>%
       dplyr::select(names, everything()) %>%
       mutate(NAs = 100*missing/nrow(df)) %>%
       cbind(NA_NULL = map_dbl(df, ~ 100*sum(is.na(dplyr::na_if(.,0)))/nrow(df))) %>%
       dplyr::select(-c(n, missing)) %>%
       mutate_if(is.numeric, ~ round(., digits = 1)) %>% 
       mutate(imp = as.integer((100-NA_NULL)*sd)) 
   )
   return(df)
 }


impute_RF <- function(df) {
  require(missRanger)
  df %>% missRanger::missRanger(
    num.trees = 1000, maxiter = 100, pmm.k = 3)
}
  

library(tidyverse)
library(Boruta)
library(furrr)

scale01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))} # ... allows "na.rm = TRUE"

corr_RF <- function(df, iter){
  set.seed(111)
  plan(multiprocess)
    
    result <- df %>% names() %>% 
      future_map_dfr(
        ~ attStats(
          Boruta::Boruta(
            formula(
              paste0('`', ., '` ~ ', paste(names(df), collapse = " + "))
            ),
            data = df,
            mcAdj = TRUE, doTrace = 0, holdHistory = TRUE, 
            pValue = 0.01,
            maxRuns = iter
          )
        ) %>% 
          rownames_to_column() %>% 
          mutate(
            Score = ifelse(decision == "Rejected", 0, 
                           scale01(medianImp * normHits, na.rm = TRUE)),
            Score = na_if(Score, 1)) %>%
          dplyr::select(rowname, Score) %>% 
          pivot_wider(names_from = rowname, values_from = Score),
        .progress = TRUE
      ) %>% mutate(target = colnames(test)) %>% column_to_rownames(var = "target") %>% 
      na_if(0) 

  return(result)
}

matrix_RF <- function(df, sensibility, clusters){
  full_join(enframe(colMeans(df, na.rm = TRUE)) %>% rename(feature_wise = value),
            enframe(rowMeans(df, na.rm = TRUE)) %>% transmute(predictor_wise = value, name = colnames(df))
  ) %>% mutate(product = feature_wise*predictor_wise) %>% 
    top_frac(sensibility, product) %>% pull(name) -> selection
  
  subset(df, rownames(df) %in% selection) %>% subset(select = selection) %>% 
    pheatmap::pheatmap(display_numbers = TRUE, cutree_rows = clust, cutree_cols = clust)
}




