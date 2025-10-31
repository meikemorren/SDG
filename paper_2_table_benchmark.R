rm(list=ls())
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(icr)
library(gt)
library(janitor)
library(stringr)
library(readr)
library(tidyverse)

df <- read.csv("https://raw.githubusercontent.com/SDGClassification/benchmark/main/benchmark.csv")
df %>% filter(duplicated(text, sdg)) %>% select(text)-> dups_text
df %>% filter(text %in% dups_text$text) %>% arrange(text) %>% select(sdg,label, text) # same text, diff sdg

df <- read.csv("analysis/data/annotated_texts.csv",
               stringsAsFactors = FALSE)
names(df)[2]  <- "text_id"
names(df)[11] <- "agreement"

df <- df %>%
  rowwise() %>%
  mutate(
    annotators = sum(!is.na(c_across(c(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan)))),
    ## JOSUA: i recalculate the agreement (alignment) bc this was not correct (when 2 out of 3 agreed it was .333)
    agreement_true = sum(c_across(c(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan)), na.rm=T),
    agreement_false = sum(!c_across(c(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan)), na.rm=T),
    agreement = max(agreement_true, agreement_false) / annotators,
    Consensus = ifelse(agreement == 1, ifelse(agreement_true == annotators, T, F), NA),
    across(where(is.logical), as.character),        # avoid logical/character clashes
    across(any_of(c("label", "annotator", "source", "note", "difficulty", "text_id","SDG")), as.character),
    ) %>%   arrange(SDG, text_id) %>% 
  ungroup()

# add label definite
path  <- "analysis/data/annotation"
files <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE)
read_output_safe <- function(f) {
  # read sheet
  x <- read_excel(f, sheet = "Output")
  # normalize names + coerce tricky columns to stable types
  x %>%
    clean_names() %>%
    rename(text_id=osdg_text_id,
           SDG=sdg) %>% 
    mutate(
      across(where(is.logical), as.character),        # avoid logical/character clashes
      across(any_of(c("label", "text_id","SDG")), as.character),
      filename = basename(f)
    )
}
annotations <- map_dfr(files, read_output_safe) %>% filter(!is.na(text_id)) #%>% filter(!duplicated(text, SDG)) #1247

# add difficulty
read_source_safe <- function(f) {
  # read sheet
  x <- read_excel(f, sheet = "Source")
  # normalize names + coerce tricky columns to stable types
  x %>%
    clean_names() %>%
    rename(SDG_osdg=sdg) %>% 
    mutate(
      SDG =  parse_number(f),
      across(where(is.logical), as.character),        # avoid logical/character clashes
      across(any_of(c("label", "difficulty", "text_id","SDG")), as.character),
      filename = basename(f)) %>% 
    select(doi, text_id, text, SDG, SDG_osdg, labels_positive, labels_negative, agreement, difficulty) 
}
difficulty<-map_dfr(files, read_source_safe) %>% filter(!is.na(text_id)) 


df<-df %>%
  left_join(annotations %>% select(text_id, SDG, label) %>% mutate(bench=1) , by = c("text_id", "SDG")) %>%
  left_join(difficulty %>% select(text_id, SDG, SDG_osdg, difficulty), by = c("text_id", "SDG")) %>% 
  mutate(bench=case_when(is.na(bench)~0, TRUE~bench)) 
table(df$bench)


osdg <- read_excel("analysis/data/OSDG/osdg-community-data-v2023-04-01.xlsx") %>%
  mutate(annotators = rowSums(select(., labels_negative, labels_positive), 
                              na.rm = TRUE),
         consensus = ifelse(agreement == 1, ifelse(labels_positive == annotators|labels_negative == annotators, T, F), NA)
  )

## calculate alpha for each SDG, only for our annotation
#  nominal or interval gets same value?

for(i in unique(df$SDG)){
  df$alpha_all[df$SDG==i]<-krippalpha(df %>% 
                                    filter(SDG==i) %>% 
                                    select(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan) %>% 
                                    as.matrix() %>% 
                                    t(), 
                                  metric = 'interval', bootstrap = TRUE, bootnp = TRUE)$alpha
  df$alpha_bench[df$SDG==i]<-krippalpha(df %>% 
                                        filter(SDG==i, bench==1) %>% 
                                        select(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan) %>% 
                                        as.matrix() %>% 
                                        t(), 
                                      metric = 'interval', bootstrap = TRUE, bootnp = TRUE)$alpha
}

table(df$alpha_all)

df_final <- df %>%
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
    difficulty,
    remarks,
    consensus = label, # take label from the output sheets (instead of consensus where 80+ are missing)
    agreement,
    agreement_true,
    agreement_false,
    annotators,
    alpha_all, alpha_bench
  )


df_final %>%
  group_by(sdg) %>%
  reframe(positive=sum(consensus=='TRUE', na.rm=T),
          negative=sum(consensus=='FALSE', na.rm=T),
          undecided=100-(positive+negative),
          annotators=round(mean(annotators),1),
          agreement=round(mean(agreement),3),
          alpha_bench=round(mean(alpha_bench),3),
          alpha_all=round(mean(alpha_all),3),
          sdg=as.numeric(sdg)
  ) %>%
  merge(.,
        osdg %>%
          group_by(sdg) %>%
          filter(text_id %in% df_final$text_id) %>% #nrow() # 1647 ipv 1700!
          reframe(annotators_osdg=round(mean(annotators),1),
                  # positive=sum(consensus=='TRUE', na.rm=T),
                  # negative=sum(consensus=='FALSE', na.rm=T),
                  # undecided=100-(positive+negative),
                  agreement_osdg=round(mean(agreement),3),
                  sdg=as.numeric(sdg)
          ), by='sdg', all = T) %>%
  distinct(.) %>%
  pivot_longer(-sdg) %>%
  pivot_wider(names_from=name, values_from=value) %>%
  arrange(sdg) %>%
  # adorn_totals("row") %>%
  gt(.) %>% 
  gtsave(str_c('./analysis/output/sdg.tex'))

# we argue against OSDG in 31 texts, primarily with SDG17:
df %>% filter(SDG!=SDG_osdg) %>% select(SDG, SDG_osdg, label) %>% filter(!is.na(label), label==TRUE) %>% print(n=100)
df %>% filter(SDG!=SDG_osdg) %>% select(SDG, SDG_osdg, label) %>% filter(!is.na(label), label==TRUE) %>% tabyl(SDG_osdg)

# to understand (dis)agreement:
x<-df %>% select(Text, text_id) %>% filter(text_id=='4c567911242e96a326b0d95605fa3573')
x$Text
df %>% filter(text_id=='4c567911242e96a326b0d95605fa3573') %>% select(label, SDG, Finn, Steve, Meike, SDG_osdg)

