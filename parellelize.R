library(parallel)
library(tidyverse)

testf <- function(df){
  df = df %>% add_column(time="now")
  return(df)
}

