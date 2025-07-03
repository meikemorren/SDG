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
    annotators = sum(!is.na(c_across(c(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan))))
  ) %>%
  ungroup()

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
      as.matrix(select(cur_data_all(), annotators.x, agreement.x)),
      metric = "interval"
    )$value,
    alpha.y = krippalpha(
      as.matrix(select(cur_data_all(), annotators.y, agreement.y)),
      metric = "interval"
    )$value
  ) %>%
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

