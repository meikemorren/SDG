library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(icr)
library(gt)
library(janitor)
library(stringr)
library(readr)

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
    Consensus = ifelse(agreement == 1, ifelse(agreement_true == annotators, T, F), NA)
  ) %>%   arrange(SDG, text_id) %>% 
  ungroup()

# add difficulty
path  <- "analysis/data/annotation"
files <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE)
read_source_safe <- function(f) {
  # read sheet
  x <- read_excel(f, sheet = "Source")
  # normalize names + coerce tricky columns to stable types
  x %>%
    clean_names() %>%
    mutate(
      across(where(is.logical), as.character),        # avoid logical/character clashes
      across(any_of(c("label", "annotator", "source", "note", "difficulty", "text_id")), as.character),
      filename = basename(f)
    )
}
annotations <- map_dfr(files, read_source_safe)
df <- df %>%
  left_join(annotations %>% select(text_id, difficulty), by = "text_id")

osdg <- read_excel("analysis/data/OSDG/osdg-community-data-v2023-04-01.xlsx") %>%
  mutate(annotators = rowSums(select(., labels_negative, labels_positive), 
                              na.rm = TRUE),
         consensus = ifelse(agreement == 1, ifelse(labels_positive == annotators, T, F), NA)
         )

## calculate alpha for each SDG, only for our annotation
#  nominal or interval gets same value?
for(i in unique(df$SDG)){
  df$alpha[df$SDG==i]<-krippalpha(df %>% 
               filter(SDG==i) %>% 
               select(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan) %>% 
               as.matrix() %>% 
               t(), 
             metric = 'interval', bootstrap = TRUE, bootnp = TRUE)$alpha
}

table(df$alpha)



# df<-df %>%
#   group_by(SDG) %>%
#   mutate(
#     alpha.x = krippalpha(df_unagreed %>% select(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan, annotators.x, agreement.x) %>%
#                            as.matrix(),
#                          # annotators.x, agreement.x,
#                          metric = "interval", bootstrap = F, bootnp = TRUE
#     )$alpha,
#     alpha.x = krippalpha(
#       as.matrix(select(cur_data_all(), annotators.x, agreement.x)),
#       metric = "interval"
#     )$alpha,
#     alpha.y = krippalpha(
#       as.matrix(select(cur_data_all(), annotators.y, agreement.y)),
#       metric = "interval"
#     )$alpha
#   ) %>%
#   ungroup() 


# df_unagreed <- df %>%
#   filter(is.na(Consensus)) %>%
#   left_join(osdg %>% select(text_id, agreement, annotators),
#             by = "text_id") %>% 
#   arrange(SDG, text_id)
# 
# df_unagreed <- df_unagreed %>%
#   group_by(SDG) %>%
#   select(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan, 
#          starts_with('annotators'), starts_with('agreement')) %>% 
#   # mutate()
#   mutate(
#     # alpha.x = krippalpha(df_unagreed %>% select(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan, annotators.x, agreement.x) %>% 
#     #                        as.matrix(), 
#     #                      # annotators.x, agreement.x, 
#     #                      metric = "interval", bootstrap = F, bootnp = TRUE
#     # )$alpha,
#     alpha.x = krippalpha(
#       as.matrix(select(cur_data_all(), annotators.x, agreement.x)),
#       metric = "interval"
#     )$alpha,
#     alpha.y = krippalpha(
#       as.matrix(select(cur_data_all(), annotators.y, agreement.y)),
#       metric = "interval"
#     )$alpha
#   ) %>%
#   ungroup() %>% print(n=400)
# 
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
    consensus = Consensus,
    agreement,
    agreement_true,
    agreement_false,
    annotators,
    alpha
  )
# only comments about 12 undecided texts, other we did make decision
write.csv(df_final %>% filter(!is.na(sdg))%>% select(text_id, sdg),
          './analysis/data/benchmark_texts.csv')

write.csv(df_final, "analysis/data/sdg_alphas.csv", row.names = F)

df_final %>%
  group_by(sdg) %>%
  reframe(positive=sum(consensus=='TRUE', na.rm=T),
          negative=sum(consensus=='FALSE', na.rm=T),
          undecided=100-(positive+negative),
          annotators=round(mean(annotators),1),
          agreement=round(mean(agreement),3),
          alpha=round(mean(alpha),3),
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

difficulties <- c("very easy", "easy", "medium", "hard", "very hard")

make_sdg_table_for_difficulty <- function(dlvl) {
  # subset to the difficulty of interest
  df_sub <- df_final %>% filter(difficulty == dlvl)
  
  # base metrics per sdg from df_sub
  left_side <- df_sub %>%
    group_by(sdg) %>%
    reframe(
      positive   = sum(consensus %in% c(TRUE, "TRUE"), na.rm = TRUE),
      negative   = sum(consensus %in% c(FALSE, "FALSE"), na.rm = TRUE),
      undecided  = 100 - (positive + negative),
      annotators = round(mean(annotators, na.rm = TRUE), 1),
      agreement  = round(mean(agreement,  na.rm = TRUE), 3),
      alpha      = round(mean(alpha,      na.rm = TRUE), 3),
      sdg        = as.numeric(sdg)
    )
  
  # align osdg to the same texts (and difficulty, via text_id linkage)
  osdg_aligned <- osdg %>%
    semi_join(df_sub %>% distinct(text_id), by = "text_id") %>%
    group_by(sdg) %>%
    reframe(
      annotators_osdg = round(mean(annotators, na.rm = TRUE), 1),
      agreement_osdg  = round(mean(agreement,  na.rm = TRUE), 3),
      sdg             = as.numeric(sdg)
    )
  
  # merge + reshape like your original
  out_tbl <- merge(left_side, osdg_aligned, by = "sdg", all = TRUE) %>%
    distinct(.) %>%
    pivot_longer(-sdg) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    arrange(sdg)
  
  # pretty title + file name
  title_txt <- paste0("SDG metrics (difficulty: ", dlvl, ")")
  file_txt  <- str_c("./analysis/output/sdg_", str_replace_all(dlvl, " ", "_"), ".tex")
  
  gt(out_tbl) %>%
    tab_header(title = title_txt) %>%
    gtsave(file_txt)
  
  message("Saved: ", file_txt)
  invisible(out_tbl)
}

# run for all five difficulty levels
results_list <- lapply(difficulties, make_sdg_table_for_difficulty)

# osdg %>%
#   group_by(sdg) %>%
#   filter(text_id %in% df_final$text_id) %>% #nrow() # 1647 ipv 1700!
#   reframe(annotators=round(mean(annotators),3),
#           # positive=sum(consensus=='TRUE', na.rm=T),
#           # negative=sum(consensus=='FALSE', na.rm=T),
#           # undecided=100-(positive+negative),
#           agreement=round(mean(agreement),3),
#           sdg=as.numeric(sdg)
#   ) %>%
#   distinct(.) %>%
#   pivot_longer(-sdg) %>%
#   pivot_wider(names_from=name, values_from=value) %>%
#   arrange(sdg) %>%
#   adorn_totals("row") %>%
#   gt(.) %>% 
#   gtsave(str_c('./analysis/output/osdg.tex'))
