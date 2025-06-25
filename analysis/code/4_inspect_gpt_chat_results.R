rm(list=ls())

library(readxl)
library(tidyverse)
source('./analysis/code/helper_functions.r')
library(xtable)
path<-'./analysis/data/'

#### usees the test set sample
# i have recreated the sample as the sample in pycharm was no longer the same
# as created by the R script
# GPT <- read.csv(paste0(path,'predicted/osdg_test_prompt_gpt35.csv'), sep=',')
# GPT <- read.csv(paste0(path,'predicted/osdg_test_highposagreement_prompt_gpt35.csv'), sep=',')



#### add sdgs to courses texts:
# GPT <- read.csv(paste0(path,'predicted/courses_clean_eng_topic.csv'), sep=',')
# table(GPT$SDG1)
# table(GPT$SDG2)
# table(GPT$SDG3)
# all <- read_excel('C:/Users/Gebruiker/OneDrive - Vrije Universiteit Amsterdam/ASI and SO/MatchMaking/courses_clean_sust.xlsx')
# table(is.na(all$course_content_eng))
# table(duplicated(all$course_content_eng))
# all<- all %>% 
#   merge(.,GPT %>%  select(course_content, course_content_eng, SDG1, SDG2, SDG3), by='course_content_eng')
# table(all$SDG1)
# openxlsx::write.xlsx(all,'C:/Users/Gebruiker/OneDrive - Vrije Universiteit Amsterdam/ASI and SO/MatchMaking/course_clean_sust.xlsx')


### needed for older samples in which we only saved text and sdg
# GPT <- read.csv(paste0(path,'predicted/osdg_valid_highposagreement_prompt_gpt35.csv'), sep=',')
# GPT$text <- gsub("\n\n###\n\n", "", GPT$prompt)
# GPT$sdg <- as.numeric(str_trim(gsub("###", "", GPT$completion),side = "both"))
# all<- read_excel("./analysis/data/osdg-community-data-v2023-04-01.xlsx")
# all %>%
#   mutate(labels_n = labels_negative+labels_positive,
#          neg_prop = labels_negative/labels_n,
#          pos_prop = labels_positive/labels_n,
#          highpos = case_when(agreement > .8 & pos_prop > .9 & labels_positive > 5 & labels_negative == 0~ 1, TRUE~0),
#          lowpos = case_when(agreement < .4 & labels_negative > 3 ~ 1, TRUE~0)) -> all
# # all<- read.csv(paste0(getwd(),"/datasets/osdg/osdg_test.csv"), sep=",")
# all %>% select(-sdg)->all
# # GPT<-GPT %>% select(-X) %>% merge(.,all, by='text') %>% rename(sdg=sdg_tested) # paris
# GPT<-GPT %>% select(-X) %>% merge(.,all, by='text') # valid


table(GPT$highpos)
table(GPT$lowpos)
table(GPT$highpos[GPT$sdg == GPT$SDG1])
table(GPT$highpos[GPT$sdg == GPT$SDG2])
table(GPT$highpos[GPT$sdg == GPT$SDG3])

table(GPT$lowpos[GPT$sdg == GPT$SDG1])
table(GPT$lowpos[GPT$sdg == GPT$SDG2])
table(GPT$lowpos[GPT$sdg == GPT$SDG3])

## valid (N=1000) # mainly positive labels, all higher prop of .9 (90% of labelers agreed that it was SDG)
(420+59+9)/500 #.976 correctly identified
# no lowpos, but 500 out of 1000 are not highpos
(406+61+8)/500 #.95 correctly identified

## paris osdg test (N=237)
(82+30+14)/237 # .53 correctly identified
## not so many high and low pos as number of labels not taken into account
40/54 # .74 correctly identified
31/73 # .42 wrongly identified

## osdg test (N=240)
146/160 # .9125 correctly identified
40/80 # .5 wrongly identified

# next step: train chatbot

