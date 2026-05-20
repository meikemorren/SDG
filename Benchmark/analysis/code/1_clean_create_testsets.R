library(readxl)
library(tidyverse)

rm(list=ls())

## Functions
source('./analysis/code/helper_functions.r')

## read all data
df.all<-read.csv("./datasets/osdg/osdg-community-data-v2023-10-01.csv",sep="\t")# 42355 (october)


df.all %>% 
  mutate(labels_n = labels_negative+labels_positive,
         neg_prop = labels_negative/labels_n,
         pos_prop = labels_positive/labels_n,
         # highpos = case_when(agreement > .8 & pos_prop > .9 & labels_negative ==0 ~ 1, TRUE~0),
         highpos = case_when(agreement > .8 & pos_prop > .9 & labels_positive > 5 & labels_negative == 0~ 1, TRUE~0),
         high = case_when(agreement > .8  & pos_prop > .9 ~ 1, TRUE~0),
         lowpos = case_when(agreement < .4 & labels_negative > 3 ~ 1, TRUE~0)) -> df.all

## create testsample
set.seed(1234)
testsample <- rbind(
  df.all %>% 
    filter(lowpos==1) %>% 
    group_by(sdg) %>% 
    sample_n(5),
  df.all %>% 
    filter(highpos==1) %>% 
    group_by(sdg) %>% 
    sample_n(10))

df.all %>% 
  mutate(test = case_when(text %in% testsample$text ~ 1, TRUE~0)) -> df.all

set.seed(1234)
df.all %>% 
  filter(test==0, highpos==1) %>% 
  group_by(sdg) %>% 
  sample_n(.,100) -> df.traintest

df.all %>% filter(test==0, highpos==1) -> df.all.highpos

df.all %>% filter(test==0, high==1) -> df.all.high

table(df.all.highpos$labels_positive-df.all.highpos$labels_negative)
table(df.all.high$labels_positive-df.all.high$labels_negative)
summary(df.all.high$pos_prop)


write.csv(df.all[,c('text','sdg')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/all/osdg.csv",
          row.names = F)
write.csv(df.all.highpos[,c('text','sdg')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/highpos_restricted/osdg_highpos.csv",
          row.names = F)
write.csv(df.all.high[,c('text','sdg')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/high/osdg_high.csv",
          row.names = F)
write.csv(df.traintest[,c('text','sdg')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/high_low_OpenAI/osdg_highagreement_balanced.csv",
          row.names = F)
write.csv(testsample[c('text','sdg')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/osdg_test.csv", row.names = F)

## create predicted testsample aurora (i have estimated for april full dataset)
df.all <- read_excel("./analysis/data/osdg-community-data-v2023-04-01.xlsx")
aurora<-read_excel("./datasets/aurora/predictions_osdg_policy_2023_april.xlsx")
aurora['prompt']<-df.all$text
aurora['Label']<-df.all$sdg
aurora['agreement']<-df.all$agreement
tail(aurora[aurora$agreement>.8,], n=10)

ids<-which(df.all$text %in% testsample$text)
aurora<-aurora[ids,]

## reconstruct data set used for ADA
df.all <- read_excel("./analysis/data/osdg-community-data-v2023-04-01.xlsx")
testsample1<- read_excel("./analysis/data/SDG reduced data for Paris.xlsx")
ids<-which(df.all$text %in% testsample1$text)
df.all<-df.all[-ids,] # 41452
df.all %>% 
  mutate(labels_n = labels_negative+labels_positive,
         neg_prop = labels_negative/labels_n,
         pos_prop = labels_positive/labels_n,
         highpos = case_when(agreement > .8 & pos_prop > .9 ~ 1, TRUE~0)) -> df.all
table(df.all$highpos) # 14683
write.csv(df.all[df.all$highpos==1,c('text','sdg')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/highpos/osdg_highpos_ada.csv",
          row.names = F)
#### per SDG goal
# 500 texts, add 500 random other goals
# for goals less than 500, add from the less high agreement texts

testsamples<-create_sdg_testsample(df.all, 1:16, n=300)
for(i in 1:16) print(nrow(testsamples[[i]])) # check
table(testsamples[[12]]$sdg) # check

for(i in 1:16) write.csv(testsamples[[i]],paste0("D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/high_low_OpenAI/sdg",as.character(i),".csv"))
