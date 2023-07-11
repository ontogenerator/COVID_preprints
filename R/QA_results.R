library(tidyverse)
library(readxl)
library(janitor)

results_200 <- read_excel(here("data", "processed", "preprints_200.xlsx"))


results_200 |> 
  count(field, assessment)
