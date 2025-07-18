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
    annotators = sum(!is.na(c_across(c(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan)))),
    ## JOSUA: i recalculate the agreement (alignment) bc this was not correct (when 2 out of 3 agreed it was .333)
    agreement_true = sum(c_across(c(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan)), na.rm=T),
    agreement_false = sum(!c_across(c(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan)), na.rm=T),
    agreement = max(agreement_true, agreement_false)
  ) %>%   arrange(SDG, text_id) %>% 
  ungroup()

osdg <- read_excel("analysis/data/OSDG/osdg-community-data-v2023-04-01.xlsx") %>%
  mutate(annotators = rowSums(select(., labels_negative, labels_positive), 
                              na.rm = TRUE))

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

## JOSUA: now redo this for the unagreed texts and for OSDG



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


df_unagreed <- df %>%
  filter(is.na(Consensus)) %>%
  left_join(osdg %>% select(text_id, agreement, annotators),
            by = "text_id") %>% 
  arrange(SDG, text_id)

df_unagreed <- df_unagreed %>%
  group_by(SDG) %>%
  select(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan, 
         starts_with('annotators'), starts_with('agreement')) %>% 
  # mutate()
  mutate(
    # alpha.x = krippalpha(df_unagreed %>% select(Finn, Gib, `Jean.Baptiste`, Meike, Steve, Ivan, annotators.x, agreement.x) %>% 
    #                        as.matrix(), 
    #                      # annotators.x, agreement.x, 
    #                      metric = "interval", bootstrap = F, bootnp = TRUE
    # )$alpha,
    alpha.x = krippalpha(
      as.matrix(select(cur_data_all(), annotators.x, agreement.x)),
      metric = "interval"
    )$alpha,
    alpha.y = krippalpha(
      as.matrix(select(cur_data_all(), annotators.y, agreement.y)),
      metric = "interval"
    )$alpha
  ) %>%
  ungroup() %>% print(n=400)

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
    agreement_internal = agreement.x,
    annotators_internal = annotators.x,
    agreement_external = agreement.y,
    annotators_external = annotators.y,
    alpha_internal = alpha.x,
    alpha_external = alpha.y
  )

write.csv(df_final, "analysis/data/sdg_alphas.csv", row.names = F)

## JOSUA: adjust this latex table so that alpha is in last column
## we should have a table for all texts (like below) 
annotate<-read_csv('./analysis/data/annotated_texts.csv')
annotate %>% # here i use the annotated_texts.csv, 
  rowwise() %>% 
  mutate(Annotators=sum(!is.na(Meike), !is.na(Steve), !is.na(Finn), !is.na(Ivan),
                        !is.na(`Jean-Baptiste`),!is.na(Gib)),
         Positive=sum(Meike, Steve, Finn, Ivan,`Jean-Baptiste`,Gib, na.rm=T),
         Negative=Annotators-Positive,
         Undecided=sum(Consensus)) %>%
  ungroup() %>%
  group_by(SDG) %>%
  reframe(Annotators=round(mean(Annotators),3),
          Positive=sum(Consensus=='TRUE', na.rm=T),
          Negative=sum(Consensus=='FALSE', na.rm=T),
          # Undecided=sum(Consensus=='UNDECIDED', na.rm=T)
          Undecided=100-(Positive+Negative),
          Alignment=round(mean(Alignment),3), # JOSUA: this should be the new agreement
          SDG=as.numeric(SDG)
  ) %>%
  distinct(.) %>%
  pivot_longer(-SDG) %>%
  pivot_wider(names_from=name, values_from=value) %>%
  arrange(SDG) %>% #colMeans() %>% 
  # Total & 3.34 & 617 & 637 & 446 & 0.7892451 \\ 
  adorn_totals("row") %>%
  # tibble::rownames_to_column() %>%  
  # pivot_longer(-rowname) %>% 
  # pivot_wider(names_from=rowname, values_from=value) %>% 
  # rotate_df() %>% 
  gt(.) %>% 
  gtsave(str_c('./analysis/output/curated_data.tex'))