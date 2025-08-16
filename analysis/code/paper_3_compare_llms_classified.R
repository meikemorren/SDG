library(dplyr)
library(stringr)

annotation <- read.csv("analysis/data/annotated_texts.csv")
# source_llama <- read.csv("datasets/results_llama-3-70b-instruct(3.1).csv")
source_llama2 <- read.csv("datasets/results_llama-3.3-70b-versatile.csv")
source_gpt <- read.csv("datasets/results_gpt4o-mini.csv")
source_deepseek <- read.csv("datasets/results_deepseekv3.csv")

results <- data.frame(
  "ID" = source_gpt$Text.ID,
  "text" = source_gpt$Text,
  "SDG" = source_gpt$SDG,
  "is_expected" = NA,
  "predicted_llama" = NA,
  "is_match_llama" = NA,
  "is_expected_llama" = NA,
  "predicted_gpt" = source_gpt$predicted_sdgs,
  "is_match_gpt" = ifelse(source_gpt$predicted_sdgs==source_gpt$SDG,T,F),
  "is_expected_gpt" = NA,
  "predicted_deepseek" = source_deepseek$predicted_sdgs,
  "is_match_deepseek" = ifelse(source_deepseek$predicted_sdgs==source_gpt$SDG,T,F),
  "is_expected_deepseek" = NA,
  "is_predicted_diff" = NA,
  "is_match_diff" = NA
)

results <- results %>%
  left_join(
    annotation %>%
      select(Text, Consensus),
    by = c("text" = "Text")
  ) %>%
  mutate(
    is_expected = as.logical(coalesce(is_expected, Consensus))
  ) %>%
  select(-Consensus)

results <- results %>%
  left_join(
    source_llama2 %>%
      select(Text, predicted_sdgs),
    by = c("text" = "Text")
  ) %>%
  mutate(
    predicted_llama = coalesce(predicted_llama, predicted_sdgs)
  ) %>%
  select(-predicted_sdgs)

results <- results %>%
  mutate(
    is_match_llama = ifelse(
      !is.na(predicted_llama) & str_detect(predicted_llama, paste0("\\b", SDG, "\\b")),
      TRUE, FALSE
    )
  )

results <- results %>%
  mutate(
    is_expected_llama   = is_expected == is_match_llama,
    is_expected_gpt     = is_expected == is_match_gpt,
    is_expected_deepseek= is_expected == is_match_deepseek
  )

results <- results %>%
  mutate(
    is_predicted_diff = !(predicted_llama == predicted_gpt &
                            predicted_llama == predicted_deepseek),
    
    is_match_diff = !(is_match_llama == is_match_gpt &
                        is_match_llama == is_match_deepseek)
  )

write.csv(results, "datasets/results_all.csv", row.names = FALSE)

model_accuracy <- data.frame(
  "SDG" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,NA),
  "accuracy_llama" = NA,
  "accuracy_gpt" = NA,
  "accuracy_deepseek" = NA
)

acc_by_sdg <- results %>%
  group_by(SDG) %>%
  summarise(
    accuracy_llama    = mean(is_expected_llama,    na.rm = TRUE),
    accuracy_gpt      = mean(is_expected_gpt,      na.rm = TRUE),
    accuracy_deepseek = mean(is_expected_deepseek, na.rm = TRUE),
    .groups = "drop"
  )

acc_overall <- results %>%
  summarise(
    SDG               = NA_integer_,
    accuracy_llama    = mean(is_expected_llama,    na.rm = TRUE),
    accuracy_gpt      = mean(is_expected_gpt,      na.rm = TRUE),
    accuracy_deepseek = mean(is_expected_deepseek, na.rm = TRUE)
  )

acc_all <- bind_rows(acc_by_sdg, acc_overall)

model_accuracy <- model_accuracy %>%
  select(SDG) %>%
  left_join(acc_all, by = "SDG")

write.csv(model_accuracy, "datasets/model_accuracy.csv", row.names = FALSE)
