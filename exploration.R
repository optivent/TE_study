## Data exploration

clean.it <- function() {
  basic.packages <- c("package:stats","package:graphics",
                      "package:grDevices","package:utils",
                      "package:datasets","package:methods",
                      "package:base")
  package.list <- dplyr::setdiff( search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)] , basic.packages)
  if (length(package.list)>0)  for(package in package.list) detach(package, character.only=TRUE)
  if(!require(pacman))install.packages("pacman"); require(here)
  
  pacman::p_load(here, VIM,
                 tidyverse,magrittr,purrr,
                 DataExplorer)
  
  rm(list = dplyr::setdiff( ls(envir = globalenv()),
                            c("clean.it")
  ),
  envir = globalenv())
  #gc() # or sessionInfo()
  
}
clean.it()

path <- here("input")
load(paste0(path, "/TE_data.RData"))

