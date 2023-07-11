library(tidyverse)
library(vroom)
library(here)
library(pdfRetrieve)
library(readxl)
library(janitor)
library(stringdist)

nondetection_string <- "^no.*ed."

# data from old pipeline preprint database with missing pieces, different program versions, etc.
preprints <- vroom(here("data", "raw", "preprints.csv")) |> 
  filter(!is.na(publication_date),
         publication_date < "2022-07-01") |>
  # due to multiple variants of "not required.", "no trequired." etc. remove space
  mutate(across(contains("value"), ~ifelse(str_length(.) < 16, str_remove_all(., " "), .))) |> 
  mutate(across(contains("statement"), as.logical)) |> 
  mutate(limitations_detected = limitations > 0,
         has_jet_pages = jet_pages > 0,
         randomization_detected = ifelse(str_detect(label2, "Rand"), value2, value6),
         randomization_required = !str_detect(randomization_detected, "notrequired."),
         randomization_detected = !str_detect(randomization_detected, nondetection_string),
         blinding_detected = ifelse(str_detect(label3, "Blind"), value3, value7),
         blinding_required = !str_detect(blinding_detected, "notrequired."),
         blinding_detected = !str_detect(blinding_detected, nondetection_string),
         ethics_statement_detected = !is.na(ethics_title1),
         ethics_statement_required = !str_detect(ethics_value1, "notrequired."),
         # is_human_study = str_detect(),
         # is_animal_study = str_detect(value1, "[A,a]nimals*|[M,m]ouse|[M,m]ice|[D,d]og|[M,m]onkey|[M,m]osquito|[H,h]amster|[M,m]acaque|ARRIVE|AWERB|CULATR|[V,v]eterin|IACUC"),
         # is_human_study = str_detect(value1, "(?<![N,n]on)[H,h]umans*(?!e)|[P,p]atient|[I,i]nformed|[C,c]onsent|[V,v]oluntary|[V,v]olunteers*|[P,p]erson(?!nel)|[P,p]articip.*|[P,p]eople|[C,c]linical|CONSORT|ICREC|IRB|[R,r]eview [B,b]oard|[I,i]nstitutional [R,r]eview|[E,e]thic.*[C,c]om+it+e+|[E,e]thic.*[B,b]oard|[E,e]thic[s,a]l*.*[C,c]om+is+ion|[B,b]iosafety|approval numbers*|Declaration of Helsinki|[H,h]ealth [A,a]uthority"),
         power_analysis_detected = ifelse(str_detect(label4, "Power"), value4, value8),
         power_analysis_required = !str_detect(power_analysis_detected, "notrequired."),
         power_analysis_detected = !str_detect(power_analysis_detected, nondetection_string),
         sex_variable_detected = ifelse(str_detect(label4, "Sex"), value4, value5),
         sex_variable_required = !str_detect(sex_variable_detected, "notrequired."),
         sex_variable_detected = !str_detect(sex_variable_detected, nondetection_string),
         inclusion_criteria_detected = ifelse(str_detect(label2, "Inclusion"), value2, NA_character_),
         inclusion_criteria_required = !str_detect(inclusion_criteria_detected, "notrequired."),
         inclusion_criteria_detected = !str_detect(inclusion_criteria_detected, nondetection_string),
         attrition_detected = ifelse(str_detect(label3, "Attrition"), value3, NA_character_),
         attrition_required = !str_detect(attrition_detected, "notrequired."),
         attrition_detected = !str_detect(attrition_detected, nondetection_string),
         subject_demographics_detected = ifelse(str_detect(label5, "Subject"), value5, NA_character_),
         subject_demographics_required = !str_detect(subject_demographics_detected, "notrequired."),
         subject_demographics_detected = !str_detect(subject_demographics_detected, nondetection_string),
         cla_detected = ifelse(str_detect(label6, "Cell"), value6, NA_character_),
         cla_required = !str_detect(cla_detected, "notrequired."),
         cla_detected = !str_detect(cla_detected, nondetection_string))

preprints <- preprints |>
  rowwise() |>
  mutate(ethics_titles = paste(c(ethics_title1, ethics_title2, ethics_title3), collapse = ","),
         has_consent = str_detect(ethics_titles, "Consent"),
         has_IRB = str_detect(ethics_label, "IRB"),
         has_IRB_or_consent = has_IRB | has_consent,
         is_animal_study = str_detect(ethics_titles, "IACUC|Euthanasia") |
           str_detect(ethics_value1, "[A,a]nimals*|[V,v]eterin"),
         has_IRB_only = str_detect(ethics_titles, "IRB,NA,NA") &
           is_animal_study == FALSE) |> 
  ungroup()



set.seed(123)

preprints_test_sample <- preprints |>
  filter(publication_date > "2022-04-01") |> 
  slice_sample(n = 200)

# 
# preprints_test_sample |> 
#   write_csv2(here("data", "processed", "preprints_200.csv"))


preprints_test_sample <- read_csv2(here("data", "processed", "preprints_200.csv"))

brz_only <- vroom(here("data", "processed", "Barzooka_200.csv"))


brz_study_results <- vroom(here("data", "processed", "study_results.csv"))


brz_flowcharts <- brz_study_results |> 
  filter(flowno > 0 | flowyes > 0)

brz_flowno <- brz_flowcharts |> 
  filter(flowno > 0)

set.seed(123)

flownos <- brz_flowno |> 
  filter(!is.na(doi)) |> 
  slice_sample(n = 2000)


# download papers
# 
# pdf_folder <- "C:/Users/nachevv/OneDrive - Charité - Universitätsmedizin Berlin/COVID Preprints/PDFs/"
pdf_folder <- "C:/Users/nachevv/OneDrive - Charité - Universitätsmedizin Berlin/Tool_Comparison/barzooka_paper_data/flowno/"

# tb_dois <- preprints_test_sample
tb_dois <- flownos
 
# dois <- tb_dois |>
#   pull(doi)

dois_to_download <- tb_dois |>
  mutate(doi = tolower(doi),
         found = paste0(gsub("/", "+", doi), ".pdf") %in%
           tolower(list.files(pdf_folder))) |>
  filter(found == FALSE)

dois <- dois_to_download |>
  pull(doi)

pdf_retrieve(dois, email = Sys.getenv("EMAIL"), pdf_folder, overwrite_files = FALSE, sleep = 1,
             repository_pdf = TRUE, check_website = TRUE,
             check_crossref = FALSE,
             use_fulltext = TRUE,
             elsevier_api_key = Sys.getenv("ELSEVIER_API_KEY"),
             wiley_api_key = Sys.getenv("WILEY_API_KEY"))
# 
# 
# 

flownos |> 
  write_excel_csv2(here("data", "processed", "brz_study_2000.csv"))


downloaded_dois <- tolower(list.files(pdf_folder)) |> str_remove(".pdf") |> str_replace_all("\\+", "/")

flownos_n <- vroom(here("data", "processed", "brz_study_2000.csv")) |> 
  mutate(found = tolower(doi) %in% downloaded_dois)

count(flownos_n, found)

2 * 350000 / 60 / 24 # estimate for total length in 24h-days 
# duration in hours:
dur_hs <- 5336.29382109642 / 60 / 60
n_papers <- 46

rate <- round(n_papers / dur_hs) # papers per hour

n_total <- 350000

# estimated time in hours
n_total / rate
# estimated time in days!
n_total / rate / 24


# dealing with large files?
# data from 200 papers (not all successfully scanned) rerun with new pipeline version (March 2023)
# to compare and qa with old pipeline tools


preprints_qa <- vroom(here("data", "raw", "preprints_qa_200.csv")) |> # results from new pipeline
  # due to multiple variants of "not required.", "no trequired." etc. remove space
  mutate(across(contains("value"), \(x) ifelse(str_length(x) < 16, str_remove_all(x, " "), x))) |> 
  mutate(across(contains("statement"), as.logical)) %>%
  replace(.=="NULL", NA) |> 
  mutate(filename = filename |> str_replace_all("\\+", "/") |> str_remove(".pdf"),
         limitations_detected = limitations > 0,
         has_jet_pages = jet_pages > 0,
         randomization_detected = ifelse(str_detect(label2, "Rand"), value2, value6),
         randomization_required = !str_detect(randomization_detected, "notrequired."),
         randomization_detected = !str_detect(randomization_detected, nondetection_string),
         blinding_detected = ifelse(str_detect(label3, "Blind"), value3, value7),
         blinding_required = !str_detect(blinding_detected, "notrequired."),
         blinding_detected = !str_detect(blinding_detected, nondetection_string),
         ethics_statement_detected = !is.na(ethics_title1),
         ethics_statement_required = !str_detect(ethics_value1, "notrequired."),
         # is_human_study = str_detect(),
         # is_animal_study = str_detect(value1, "[A,a]nimals*|[M,m]ouse|[M,m]ice|[D,d]og|[M,m]onkey|[M,m]osquito|[H,h]amster|[M,m]acaque|ARRIVE|AWERB|CULATR|[V,v]eterin|IACUC"),
         # is_human_study = str_detect(value1, "(?<![N,n]on)[H,h]umans*(?!e)|[P,p]atient|[I,i]nformed|[C,c]onsent|[V,v]oluntary|[V,v]olunteers*|[P,p]erson(?!nel)|[P,p]articip.*|[P,p]eople|[C,c]linical|CONSORT|ICREC|IRB|[R,r]eview [B,b]oard|[I,i]nstitutional [R,r]eview|[E,e]thic.*[C,c]om+it+e+|[E,e]thic.*[B,b]oard|[E,e]thic[s,a]l*.*[C,c]om+is+ion|[B,b]iosafety|approval numbers*|Declaration of Helsinki|[H,h]ealth [A,a]uthority"),
         power_analysis_detected = ifelse(str_detect(label4, "Power"), value4, value8),
         power_analysis_required = !str_detect(power_analysis_detected, "notrequired."),
         power_analysis_detected = !str_detect(power_analysis_detected, nondetection_string),
         sex_variable_detected = ifelse(str_detect(label4, "Sex"), value4, value5),
         sex_variable_required = !str_detect(sex_variable_detected, "notrequired."),
         sex_variable_detected = !str_detect(sex_variable_detected, nondetection_string),
         inclusion_criteria_detected = ifelse(str_detect(label2, "Inclusion"), value2, NA_character_),
         inclusion_criteria_required = !str_detect(inclusion_criteria_detected, "notrequired."),
         inclusion_criteria_detected = !str_detect(inclusion_criteria_detected, nondetection_string),
         attrition_detected = ifelse(str_detect(label3, "Attrition"), value3, NA_character_),
         attrition_required = !str_detect(attrition_detected, "notrequired."),
         attrition_detected = !str_detect(attrition_detected, nondetection_string),
         subject_demographics_detected = ifelse(str_detect(label5, "Subject"), value5, NA_character_),
         subject_demographics_required = !str_detect(subject_demographics_detected, "notrequired."),
         subject_demographics_detected = !str_detect(subject_demographics_detected, nondetection_string),
         cla_detected = ifelse(str_detect(label6, "Cell"), value6, NA_character_),
         cla_required = !str_detect(cla_detected, "notrequired."),
         cla_detected = !str_detect(cla_detected, nondetection_string))

preprints_qa <- preprints_qa |>
  filter(sciscore > 0) |> 
  rowwise() |>
  mutate(ethics_titles = paste(c(ethics_title1, ethics_title2, ethics_title3), collapse = ","),
         has_consent = str_detect(ethics_titles, "Consent"),
         has_IRB = str_detect(ethics_label, "IRB"),
         has_IRB_or_consent = has_IRB | has_consent,
         is_animal_study = str_detect(ethics_titles, "IACUC|Euthanasia") |
           str_detect(ethics_value1, "[A,a]nimals*|[V,v]eterin"),
         has_IRB_only = str_detect(ethics_titles, "IRB,NA,NA") &
           is_animal_study == FALSE,
         source = "source_1") |> 
  ungroup() |> 
  rename(doi = filename) |> 
  arrange(doi) |> 
  select(doi, source, limitations, jet_pages, trial_n_detected, everything(),
         -contains("bar"), -contains("flow"), -approp, -pie, -hist, -dot, -box, -violin)

preprints_old_200 <- preprints_test_sample |> 
  filter(doi %in% preprints_qa$doi) |> 
  mutate(source = "source_2") |> 
  arrange(doi) |> 
  select(doi, source, limitations, jet_pages, trial_n_detected, everything(),
         -contains("bar"), -contains("flow"), -publication_date, -approp, -pie, -hist, -dot, -box, -violin)


trial_comp <- preprints_qa |> 
  select(doi, trial_n_detected) |> 
  bind_cols(preprints_old_200 |> 
              select(trial_n_detected) |> 
              rename(trial_old = trial_n_detected))

trial_comp |> 
  count(trial_n_detected == trial_old)

trial_comp |> 
  filter(trial_old != trial_n_detected)


comp <- union(preprints_qa, preprints_old_200) |> 
  get_dupes(doi) |> 
  select(-dupe_count)



comp <- comp |> 
  mutate(across(everything(), as.character)) |> 
  pivot_longer(limitations:has_IRB_only, names_to = "type", values_to = "value") |> 
  pivot_wider(names_from = source, values_from = c(type, value)) |> 
  unnest(type_source_1:value_source_2) |> 
  filter(!(str_detect(value_source_1, "^MATERIAL")|str_detect(value_source_1, "^Material")),
         !str_detect(type_source_1, "required")) |> 
  mutate(match = value_source_1 == value_source_2) |> 
  filter(match == FALSE) |> 
  select(-match, -type_source_2)

comp <- comp |> 
  mutate(type_source_1 = case_when(
    type_source_1 == "value2" ~ "Inclusion and Exclusion Criteria",
    type_source_1 == "value3" ~ "Attrition",
    type_source_1 == "value4" ~ "Sex as a biological variable",
    type_source_1 == "value5" ~ "Subject Demographics",
    type_source_1 == "value6" ~ "Randomization",
    type_source_1 == "value7" ~ "Blinding",
    type_source_1 == "value8" ~ "Power Analysis",
    .default = type_source_1
  )) |> 
  rename(field = type_source_1)

comp2 <- comp |> 
  rowwise() |> 
  mutate(dist = amatch(value_source_1, value_source_2, maxDist = 9),
         is_sentence = str_length(value_source_1) + str_length(value_source_2) > 24,
         is_match = is_sentence & (dist == 1),
         is_limitation = field == "limitations",
         has_zero = value_source_1 == "0" | value_source_2 == "0",
         lim_to_remove = !has_zero & is_limitation) |> 
  ungroup()

comp2 <- comp2 |> 
  filter(!is_match | is.na(is_match),
         !lim_to_remove,
         !str_detect(field, "is_open")) |> 
  select(!dist:lim_to_remove)
  
comp2 |> 
  write_excel_csv2(here("data", "processed", "QA_Parya.csv"))


eth_test <- comp |> 
  filter(doi == "10.1101/2022.06.07.22276117", 
         field == "ethics_value1") 

amatch(eth_test$value_source_1, eth_test$value_source_2, maxDist = 9)


#### Barzooka QA pipeline vs pdf folder on the cluster
# it seems the pipeline does figure extraction, whereas Barzooka works page by page
# compare output from two versions

barzooka_directly <- vroom(here("data", "raw", "Barzooka_200.csv")) |> 
  rename(doi = paper_id) |> 
  select(-other, -text) 

preprints_qa_brz <- vroom(here("data", "raw", "preprints_qa_200.csv")) |> 
  rename(doi = filename) |> 
  select(names(barzooka_directly)) |> 
  mutate(doi = doi |> str_replace_all("\\+", "/") |> str_remove(".pdf"),
         source = "source_1") |> 
  arrange(doi)

barzooka_directly <- barzooka_directly |> 
  filter(doi %in% preprints_qa_brz$doi) |> 
  arrange(doi) |> 
  mutate(source = "source_2")


all.equal(barzooka_directly, preprints_qa_brz)

brz_qa_200 <- barzooka_directly |> 
  full_join(preprints_qa_brz) |> 
  get_dupes(doi) |> 
  replace_na(list(source = "source_1")) |> 
  select(-dupe_count)


brz_long <- brz_qa_200 |> 
  pivot_longer(approp:violin, names_to = "type", values_to = "value") |> 
  pivot_wider(names_from = source, values_from = c(value))

brz_long <- brz_long |> 
  mutate(mismatched = source_1 != source_2)


brz_long |> 
  write_excel_csv2(here("data", "processed", "brz_qa_200.csv"))

