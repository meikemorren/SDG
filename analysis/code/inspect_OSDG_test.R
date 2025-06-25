library(readxl)
library(tidyverse)


## note:
#1. fine tune model only on hihg pos agreement samples ( no SDG 17!)
#2. compare models ada without training with aurora without finetuning
#3. fine tune aurora using osdg dataset (dit i use the 17 models?)

## create test set
df<-read_excel("./analysis/data/osdg-community-data-v2023-04-01.xlsx") # 41689
testsample<- read_excel("./analysis/data/SDG reduced data for Paris.xlsx")
testsample<- testsample[-(238:nrow(testsample)),]
table(df$labels_negative)
table(df$labels_positive) # how can there by 100+ annotators for some text?

# df$low_agree <- ifelse(df$agreement>.2 & df$agreement <.8, 1, 0)
# table(df$low_agree, df$sdg)
# df %>% 
#   ggplot(aes(x=agreement))+
#   geom_histogram()+
#   facet_grid(~sdg)

df %>% 
  mutate(labels_n = labels_negative+labels_positive,
         neg_prop = labels_negative/labels_n,
         pos_prop = labels_positive/labels_n) %>% 
  filter(agreement>.8, pos_prop>.9) -> df # 14758

nrow(df[!(df$text_id %in% testsample$text_id),]) # 14683
# write.csv(df[df$agreement>.8,c('text','sdg')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/highagreement/osdg_highagreement.csv")
write.csv(df[!(df$text_id %in% testsample$text_id),c('text','sdg')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/highposagreement/osdg_highagreement_notest.csv")
write.csv(testsample[c('text','sdg_tested')],"D:/Data/Users/Meike/ChatGPT/SDG/input/v2023/highposagreement/osdg_test.csv")

## get ADA labels
get_n_labels<-function(df, intervals, x){
  df %>% 
    mutate(sdg = as.numeric({{x}})) %>% 
    filter(agreement< intervals[1]) %>% 
    group_by(sdg) %>% 
    summarize(n=n()) -> low
  df %>% 
    mutate(sdg = as.numeric({{x}})) %>% 
    filter(agreement>=intervals[1] & agreement <intervals[2]) %>% 
    group_by(sdg) %>% 
    summarize(n=n()) -> mid
  df %>% 
    mutate(sdg = as.numeric({{x}})) %>% 
    filter(agreement>=intervals[2]) %>% 
    group_by(sdg) %>% 
    summarize(n=n()) %>% 
    arrange()-> high
  return(list(low, mid, high))
}

OSDG<- get_n_labels(testsample, c(.2,.8), sdg_tested)
OSDG1<- get_n_labels(testsample, c(.2,.8), `OSDG 1`)

# add column with multiple sdg goals, and select one with highest prob (what i did now?)
Aurora<- get_n_labels(testsample, c(.2,.8), `Aurora`)

# get probabilities from ada
# train each sdg separately
testchatgpt<-read.csv('./analysis/data/osdg_test_label.csv', sep=";")
testchatgpt<- testchatgpt[-(238:nrow(testchatgpt)),]
testsample['ChatGPT (ada2)']<-testchatgpt['label']
# sum(testchatgpt['text']==testsample['text'])
rm(testchatgpt)
ChatGPT<- get_n_labels(testsample, c(.2,.8), `ChatGPT (ada2)`)

# multiple SDGs per text!
Prospector<-read_excel('./analysis/data/VU_request_prospector.xlsx')
# first assign most likely label (same as sdgtested, if not)
# create column with multiple labels (like OSDG column)
# then ask jeanpaul the probabilities -> assign highest probability
# testsample['Prospector']<-Prospector['label']

df<-as.data.frame(c(Aurora, ChatGPT, OSDG, OSDG1))
colnames(df)[1]<-"Freq"
df['Model'] <- c(rep("Aurora", 17),
                 rep("ChatGPT (ada)", 17),
                 rep("OSDG (initial)", 17),
                 rep("OSDG (estimated)", 17))
df["SDG"] <- rep(1:17,4)
df["low"] <- c(Aurora[[1]], ChatGPT[[1]], OSDG[[1]], OSDG1[[1]])
df["high"] <- c(Aurora[[2]], ChatGPT[[2]], OSDG[[2]], OSDG1[[2]])
df["highpos"] <- c(Aurora[[3]], ChatGPT[[3]], OSDG[[3]], OSDG1[[3]])

ID = 1:17

df %>% 
  # filter(Model == "Aurora") %>%
  ggplot(aes(SDG,Freq))+
  geom_col()+
  theme(plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, size=8, vjust = 0.5),
        axis.text.y = element_text(size=10),
        axis.title.y = element_blank()) +
  scale_x_continuous("SDG", labels = as.character(ID), breaks = ID)+
  facet_grid(~Model)+
  theme(strip.text = element_text(
    size = 15, color = "dark green"))
  

df %>% 
  # filter(Model == "Aurora") %>%
  ggplot(aes(SDG,low))+
  geom_col()+
  theme(plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, size=8, vjust = 0.5),
        axis.text.y = element_text(size=10),
        axis.title.y = element_blank()) +
  scale_x_continuous("SDG", labels = as.character(ID), breaks = ID)+
  facet_grid(~Model)+
  theme(strip.text = element_text(
    size = 15, color = "dark green"))


df %>% 
  # filter(Model == "Aurora") %>%
  ggplot(aes(SDG,high))+
  geom_col()+
  theme(plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, size=8, vjust = 0.5),
        axis.text.y = element_text(size=10),
        axis.title.y = element_blank()) +
  scale_x_continuous("SDG", labels = as.character(ID), breaks = ID)+
  facet_grid(~Model)+
  theme(strip.text = element_text(
    size = 15, color = "dark green"))

df %>% 
  # filter(Model == "Aurora") %>%
  ggplot(aes(SDG,highpos))+
  geom_col()+
  theme(plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, size=8, vjust = 0.5),
        axis.text.y = element_text(size=10),
        axis.title.y = element_blank()) +
  scale_x_continuous("SDG", labels = as.character(ID), breaks = ID)+
  facet_grid(~Model)+
  theme(strip.text = element_text(
    size = 15, color = "dark green"))
