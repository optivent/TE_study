# remove any non-base package
clean.it <- function() {
  lapply(names(sessionInfo()$otherPkgs), function(pkgs)
    detach(
      paste0('package:', pkgs),
      character.only = T,
      unload = T,
      force = T
    ))
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

############### playground ###################

fn_flat2 <- function(df, firstlevel, secondlevel, cutoff){
  firstlevel <- enexpr(firstlevel)
  secondlevel <- enexpr(secondlevel) 
  df %>% 
      pivot_wider(
        id_cols = !!firstlevel,
        names_from = !!secondlevel,
        names_prefix = paste0("_", toString(secondlevel)),
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


fn_flat3 <- function(df, firstlevel, secondlevel, thirdlevel, cutoff){
  firstlevel <- enexpr(firstlevel)
  secondlevel <- enexpr(secondlevel)
  thirdlevel <- enexpr(thirdlevel)
  df %>% 
    pivot_wider(
      id_cols = !!firstlevel,
      names_from = c(!!secondlevel, !!thirdlevel), 
      values_from = -c(!!firstlevel, !!secondlevel, !!thirdlevel)
    ) %>% 
  full_join(
    df %>% 
      group_by(!!firstlevel) %>% 
        summarise_at(
          vars(-c(!!secondlevel, !!thirdlevel )),
          list(
            ~ min(.),
            ~ median(.,na.rm = T),
            ~ max(.),
            ~ sum(., na.rm = TRUE),
            ~ range(.)
          )
        ) %>% 
      ungroup(),
    by = toString(firstlevel)
        ) %>% 
    full_join(
      df %>% 
        group_by(!!firstlevel, !!secondlevel) %>% 
          summarise_at(
            vars(-!!thirdlevel),
            list(
              ~ min(.),
              ~ median(.,na.rm = T),
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


  






