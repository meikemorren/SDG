library(readxl)
library(tidyverse)

rm(list=ls())

## note:
#1. fine tune model only on hihg pos agreement samples ( no SDG 17!)
#2. compare models ada without training with aurora without finetuning
#3. fine tune aurora using osdg dataset (dit i use the 17 models?)
getwd()
## create test set

## read all data
df.all<- read_excel("./analysis/data/osdg-community-data-v2023-04-01.xlsx")
# 41689 (april)
# df.all<-read.csv("./datasets/osdg/osdg-community-data-v2023-10-01.csv",sep="\t") 
# 42355 (october)
df.all %>% 
  mutate(labels_n = labels_negative+labels_positive,
         neg_prop = labels_negative/labels_n,
         pos_prop = labels_positive/labels_n,
         highpos = case_when(agreement > .8 & pos_prop > .9 & labels_positive > 5 & labels_negative == 0~ 1, TRUE~0),
         lowpos = case_when(agreement < .4 & labels_negative > 3 ~ 1, TRUE~0)) -> df.all


set.seed(1) # doesnt work
testsample2 <- rbind(
  df.all %>% 
    filter(lowpos==1) %>% 
    group_by(sdg) %>% 
    sample_n(5),
  df.all %>% 
    filter(highpos==1) %>% 
    group_by(sdg) %>% 
    sample_n(10))

testsample<- read_excel("./analysis/data/SDG reduced data for Paris.xlsx")
testsample<- testsample[-(238:nrow(testsample)),] # calculations in excel


df.all %>% 
  mutate(test2 = case_when(text %in% testsample2$text ~ 1, TRUE~0),
         test = case_when(text_id %in% testsample$text_id ~ 1, TRUE~0)) -> df.all


# df.all<-merge(df.all, df.all1[c("text","sdg")], by="text", all.x=T)
# table(df.all$sdg.x==df.all$sdg.y) # nothing has changed, just new examples


table(df.all$labels_negative)
table(df.all$labels_positive) # how can there by 100+ annotators for some text?

# df$low_agree <- ifelse(df$agreement>.2 & df$agreement <.8, 1, 0)
# table(df$low_agree, df$sdg)
# df %>% 
#   ggplot(aes(x=agreement))+
#   geom_histogram()+
#   facet_grid(~sdg)


# testsample <- merge(testsample, df.all[c('text_id','highpos')], by='text_id')
set.seed(1)
df.all %>% 
  filter(test2==0, highpos==1) %>% 
  # filter(test==0, highpos==1) %>% 
  group_by(sdg) %>% 
  sample_n(.,150) -> df.traintest

df.all %>% 
  filter(test2==0, highpos==1) -> df.all.traintest

## unbalanced, all texts with high agreement
# df.all %>% 
#   mutate(test2 = case_when(text %in% testsample2$text ~ 1, TRUE~0)) %>% 
#   filter(test2 == 0, highpos==1)-> df.traintest

# (april) 14758 # older dataset
# (october) 14875 # oct dataset agreement>.8 & pos_prop>.9
table(df.all$highpos) # oct dataset (november) 7683: agreement>.8 & pos_prop>.9 & labels_positive>5 & labels_negative==0
table(df.all$lowpos) # oct dataset (november) 3854: agreement<.4 & labels_negative>3 
table(df.all$test2)




# write.csv(df[df$agreement>.8,c('text','sdg')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/highagreement/osdg_highagreement.csv")

write.csv(df.traintest[,c('text','sdg')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/high_low_OpenAI/osdg_highagreement_balanced_nov.csv",
          row.names = F)
write.csv(df.all.traintest[,c('text','sdg')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/high_low_OpenAI/osdg_highagreement_unbalanced_nov.csv",
          row.names = F)
write.csv(testsample2[c('text','sdg')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/high_low_OpenAI/osdg_test.csv", row.names = F)
write.csv(testsample2[c('text','sdg','highpos','lowpos','labels_n','agreement')],"./datasets/osdg/osdg_test.csv", row.names = F)


#### per SDG goal
# 500 texts, add 500 random other goals
# for goals less than 500, add from the less high agreement texts


add_texts<-function(df, sample, i){

  df %>% 
    filter(test==1, sdg=={{i}}) %>% 
    nrow(.) -> n
  
  df %>% 
    filter(sdg=={{i}}, 
           !text_id %in% sample$text_id,
           test==0, highpos==0,
           agreement <=.8, agreement > .6,
           pos_prop>.7)  %>% 
    mutate(label = sdg) %>% 
    arrange(desc(agreement), desc(pos_prop)) -> extra

  return(extra[1:(500-n),])
  
}


create_sdg_testsample<-function(df, sdgs){
  list_testsamples<-list()
  for(i in sdgs){

    df %>% 
      filter(sdg=={{i}}, highpos==1) %>% 
      mutate(label = sdg) -> sdgsample
    
    if(nrow(sdgsample) <500){
      sdgsample <- rbind(sdgsample,add_texts(df, sdgsample, i))
    }
    if(nrow(sdgsample)> 500){
      set.seed(1234)
      sdgsample[sample(500),] -> sdgsample
    }

    df %>% 
      filter(sdg!={{i}}) %>% 
      mutate(label= case_when(sdg=={{i}}~1, TRUE~0)) -> random
    
    random_selected<-NULL
    for(j in sdgs[sdgs!=i]){
      print(j)
      temp<-random[random$sdg==j,]
  
      set.seed(1234)
      temp<-temp[sample(ceiling(500/(length(sdgs)-1))),]
      random_selected<-rbind(random_selected, temp)
    }
    
    # if i dont shuffle, it will just do 1:500 below with sample()
    random_selected<-random_selected[sample(1:nrow(random_selected)),]
    set.seed(1234)
    random_sample <- random_selected[sample(500),] 
    
    # shuffle cases so order doesnt matter
    sdgsample<-rbind(sdgsample, random_sample)
    list_testsamples[[i]]<-sdgsample[sample(1:nrow(sdgsample)),]
  }
  return(list_testsamples)
}

testsamples<-create_sdg_testsample(df, 1:16)
for(i in 1:16) print(nrow(testsamples[[i]])) # check
table(testsamples[[12]]$sdg) # check

for(i in 1:16) write.csv(testsamples[[i]],paste0("D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/highposagreement_sdg/sdg",as.character(i),".csv"))
