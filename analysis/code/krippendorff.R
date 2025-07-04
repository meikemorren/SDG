library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(icr)

df <- read.csv("analysis/data/annotated_texts.csv",
               stringsAsFactors = FALSE)
names(df)[2]  <- "text_id"
names(df)[11] <- "agreement"
df <- df %>%
  rowwise() %>%
  mutate(
    Nannotators = sum(!is.na(c_across(c(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan))))
  ) %>%
  ungroup() %>%
  # as an input we need the columns of all annotators
  rename(
    annotator1 = Finn,
    annotator2 = Steve,
    annotator3 = Meike,
    annotator4 = Gib,
    annotator5 = `Jean.Baptiste`,
    annotator6 = Ivan
  ) 

osdg <- read_excel("analysis/data/OSDG/osdg-community-data-v2023-04-01.xlsx") %>%
  mutate(annotators = rowSums(select(., labels_negative, labels_positive), 
                              na.rm = TRUE))

df_unagreed <- df %>%
  filter(is.na(Consensus)) %>%
  left_join(osdg %>% select(text_id, agreement, annotators),
            by = "text_id")
  
df_unagreed <- df_unagreed %>%
  group_by(SDG) %>%
  mutate(
    alpha.x = krippalpha(
      as.matrix(select(cur_data_all(), starts_with('annotator'), agreement.x)),
      metric = "interval"
    )$value,
    alpha.y = krippalpha(
      as.matrix(select(cur_data_all(), starts_with('annotator'), agreement.y)),
      metric = "interval"
    )$value
  ) %>% select(starts_with('alpha'))
  ungroup()

df_final <- df_unagreed %>%
  unite(
    col = "remarks",
    Notes,
    Rationale,
    sep = " // ",
    na.rm = TRUE,
    remove = TRUE
  ) %>%
  mutate(
    remarks = na_if(remarks, "")
  ) %>%
  select(
    sdg = SDG,
    text_id,
    text = Text,
    group = Group,
    remarks,
    agreement_internal  = agreement.x,
    annotators_internal = annotators.x,
    agreement_external  = agreement.y,
    annotators_external = annotators.y#,
    # alpha_internal = alpha.x,
    # alpha_external = alpha.y
  )

