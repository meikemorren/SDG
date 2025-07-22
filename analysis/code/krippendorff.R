library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(icr)
library(gt)
library(janitor)
library(stringr)

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
    remarks,
    consensus = Consensus,
    agreement,
    agreement_true,
    agreement_false,
    annotators,
    alpha
  )

write.csv(df_final, "analysis/data/sdg_alphas.csv", row.names = F)

annotate<-df_final
annotate %>%
  group_by(sdg) %>%
  reframe(annotators=round(mean(annotators),3),
          positive=sum(consensus=='TRUE', na.rm=T),
          negative=sum(consensus=='FALSE', na.rm=T),
          undecided=100-(positive+negative),
          agreement=round(mean(agreement),3),
          alpha=mean(alpha),
          sdg=as.numeric(sdg)
  ) %>%
  distinct(.) %>%
  pivot_longer(-sdg) %>%
  pivot_wider(names_from=name, values_from=value) %>%
  arrange(sdg) %>%
  adorn_totals("row") %>%
  gt(.) %>% 
  gtsave(str_c('./analysis/output/sdg.tex'))

annotate2<-osdg
annotate2 %>%
  group_by(sdg) %>%
  reframe(annotators=round(mean(annotators),3),
          positive=sum(consensus=='TRUE', na.rm=T),
          negative=sum(consensus=='FALSE', na.rm=T),
          undecided=100-(positive+negative),
          agreement=round(mean(agreement),3),
          sdg=as.numeric(sdg)
  ) %>%
  distinct(.) %>%
  pivot_longer(-sdg) %>%
  pivot_wider(names_from=name, values_from=value) %>%
  arrange(sdg) %>%
  adorn_totals("row") %>%
  gt(.) %>% 
  gtsave(str_c('./analysis/output/osdg.tex'))
