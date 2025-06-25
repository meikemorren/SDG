## run 1_create test sample first!

source('./analysis/code/helper_functions.r')
library(xtable)
# path<- 'D:/Data/Users/Meike/ChatGPT/SDG/output/high_low_OpenAI/'
path<-'./analysis/data/predicted/'


## to compare, order all texts acc to alphabet
# testsample<-testsample[order(testsample$text),]

# create table for paper
f1_table<-as.data.frame(matrix(0, nrow=16, ncol=1))
for(i in 1:16) f1_table$V1[i]<-paste0('SDG',i)
# colnames(f1_table)<-c('ADA easy','ADA diff','ADA easy','ADA diff',
#                       'Babbage easy','Babbage diff','Babbage easy','Babbage diff',
#                       'Davinci (ML) easy','Davinci (ML) diff','Davinci easy','Davinci diff',
#                       'Prospector easy','Prospector diff',
#                       'Aurora (ML) easy','Aurora (ML) diff','Aurora easy','Aurora diff')
# for(i in 1:16) rownames(f1_table)[i]<-paste0('SDG',i)

###### Multi Label model
#### ADA ML
# used all data to train the model (all highagreement texts!)
ADA <- read.csv(paste0(path,'test_ada (multilabel).csv'), sep=',')
ls<-get_predicted_values(testsample, ADA, ml=TRUE, openai = TRUE, OpenAI = FALSE)
testsample['ADA']<-ls[[1]]
f1_table[,'ADA easy']<-ls[[2]]
f1_table[,'ADA diff']<-ls[[3]]

#### babbage ML
# I use the same data used for the ADA model above
BAB<-read.csv(paste0(path,'test_babbage_8UZwuiwR.csv'), sep=',')
ls<-get_predicted_values(testsample, BAB, ml=TRUE, openai=TRUE, OpenAI=TRUE)
testsample['Babbage A (ADA, random)']<-ls[[1]]
f1_table[,'Babbage A easy (ADA, random)']<-ls[[2]]
f1_table[,'Babbage A diff (ADA, random)']<-ls[[3]]

#### Babbage ML
# same data as above
# take 1000, but acc to distribution
BAB<-read.csv(paste0(path,'test_babbage_8Uy0Y35M.csv'), sep=',')
ls<-get_predicted_values(testsample, BAB, ml=TRUE, openai=TRUE, OpenAI=TRUE)
testsample['Babbage A (ADA, distr)']<-ls[[1]]
f1_table[,'Babbage A easy (ADA, distr)']<-ls[[2]]
f1_table[,'Babbage A diff (ADA, distr)']<-ls[[3]]

#### Babbage ML
# same data as above
# take 20% as valid sample


#### Babbage ML (1) # 14715; all high pos agreement, 1000 random valid set
BAB<-read.csv(paste0(path,'/test_babbage_8SoZQIiA.csv'), sep=',')
ls<-get_predicted_values(testsample, BAB, ml=TRUE, openai=TRUE, OpenAI=TRUE)
testsample['Babbage B (14715, random)']<-ls[[1]]
f1_table[,'Babbage B easy (14715, random)']<-ls[[2]]
f1_table[,'Babbage B diff (14715, random)']<-ls[[3]]

#### babbage ML (3) # unbalanced 7532; valid = 1000, from restricted high pos agreement
BAB<-read.csv(paste0(path,'/test_babbage_8SpfH6rR.csv'), sep=',')
ls<-get_predicted_values(testsample, BAB, ml=TRUE, openai=TRUE, OpenAI=TRUE)
testsample['Babbage C (2400, random)']<-ls[[1]]
f1_table[,'Babbage C easy (2400, random)']<-ls[[2]]
f1_table[,'Babbage C diff (2400, random)']<-ls[[3]]


#### babbage ML (2) # 7532; restricted high pos agreement: those with few negative labels and many positive labels
# the train and valid sets are fixed number per sdg (N=7532 in total), 20% valid 
# different number of texts per sdg, same distribution of sdgs between train and test
# the babbage model predicts the next word, which is here the completion, which is the sdg
BAB<-read.csv(paste0(path,'/test_babbage_8NHAgY5i.csv'), sep=',')
ls<-get_predicted_values(testsample, BAB, ml=TRUE, openai=TRUE, OpenAI=TRUE)
testsample['Babbage D (7532, fixed)']<-ls[[1]]
f1_table[,'Babbage D easy (7532, fixed)']<-ls[[2]]
f1_table[,'Babbage D diff (7532, fixed)']<-ls[[3]]

#### babbage ML
## for each SDG i took randomly 10 texts about which many annotators agreed, and 5 about which they disagreed
# the train and valid sets are fixed number per sdg (N=2400 in total), valid 20%
# the babbage model predicts the next word, which is here the completion, which is the sdg
BAB<-read.csv(paste0(path,'/test_babbage_8MwaGTe4.csv'), sep=',')
ls<-get_predicted_values(testsample, BAB, ml=TRUE, openai=TRUE, OpenAI=TRUE)
testsample['Babbage E (2400, fixed)']<-ls[[1]]
f1_table[,'Babbage E easy (2400, fixed)']<-ls[[2]]
f1_table[,'Babbage E diff (2400, fixed)']<-ls[[3]]



#### Davinci ML
## training and valid = 2400
# DAV<-read.csv('D:/Data/Users/Meike/ChatGPT/SDG/output/high_low_OpenAI/test_davinci_8NInsTdJ.csv', sep=',')
# ls<-get_predicted_values(testsample, DAV, ml=TRUE, openai=TRUE, OpenAI=TRUE)
# testsample['Davinci (ML)']<-ls[[1]]
# f1_table[,'Davinci (ML) easy']<-ls[[2]]
# f1_table[,'Davinci (ML) diff']<-ls[[3]]
sample_models<-colnames(f1_table)[grep("Babbage", colnames(f1_table))]
# sample_models <- sample_models[-grep("ADA", sample_models)]
easy_sample_models<-sample_models[grep("easy", sample_models)]
diff_sample_models<-sample_models[grep("diff", sample_models)]
valid_models<-colnames(f1_table)[grep("ADA", colnames(f1_table))]
easy_valid_models<-valid_models[grep("easy", valid_models)]


tab<-as.data.frame(f1_table[easy_sample_models])
for(i in 1:16) rownames(tab)[i]<-paste0('SDG',i)
tab<-rbind(tab,
          colMeans(tab),
          c(14683, 14683, 14715, 2400, 7532,2400),
          c(1000,1000,1000,1000,1506,480))
rownames(tab)[17:nrow(tab)]<-c('Avg F1','N','valid')
print(xtable(round(tab,2), type = "latex"), file = "./analysis/output/table_samples_babbage_easy.tex") 

tab<-as.data.frame(f1_table[diff_sample_models])
for(i in 1:16) rownames(tab)[i]<-paste0('SDG',i)
tab<-rbind(tab,
                    colMeans(tab),
                    c(14683, 14683, 14715, 2400, 7532,2400),
                    c(1000,1000,1000,1000,1506,480))
rownames(tab)[17:nrow(tab)]<-c('Avg F1','N','valid')
print(xtable(round(tab,2), type = "latex"), file = "./analysis/output/table_samples_babbage_diff.tex") 

tab<-as.data.frame(f1_table[easy_valid_models])
for(i in 1:16) rownames(tab)[i]<-paste0('SDG',i)
tab<-rbind(tab, colMeans(tab))
print(xtable(round(tab,2), type = "latex"), file = "./analysis/output/valid_samples_easy.tex") 



f1_table[,'Davinci (ML) diff']<-ls[[3]]

############ Single Models (one per SDG) #############
#### ADA 
## single models: train each sdg separately
ADA <- read.csv('D:/Data/Users/Meike/ChatGPT/SDG/output/high_low_OpenAI/test_ada (single models).csv', sep=';') # ;
# ls<-get_predicted_values(testsample, ADA, ml=FALSE, openai = TRUE, OpenAI = TRUE)
# testsample['ADA']<-ls[[1]]
# f1_table[,'ADA easy']<-ls[[2]]
# f1_table[,'ADA diff']<-ls[[3]]


#### babbage 16 models
## for each SDG i took randomly 100 texts about which many annotators agreed, and 100 about which they disagreed
# the babbage model predicts the next word, which is here the completion, which is the sdg





















#### aurora

aurora<-read_excel('./datasets/aurora/predictions_osdg_policy_2023_april.xlsx')
df.all <- read_excel("./analysis/data/osdg-community-data-v2023-04-01.xlsx")
df.all %>% 
  mutate(labels_n = labels_negative+labels_positive,
         neg_prop = labels_negative/labels_n,
         pos_prop = labels_positive/labels_n,
         highpos = case_when(agreement > .8 & pos_prop > .9 ~ 1, TRUE~0)) -> df.all
aurora['text']<-df.all$text
aurora['sdg']<-df.all$sdg
aurora['agreement']<-df.all$agreement
aurora['highpos']<-df.all$highpos

testsample$text[!(testsample$text %in% aurora$text)]
ids<-which(testsample$text %in% aurora$text)

aurora[c('sdg1','sdg2','sdg3')]<-get_SDG(aurora[,2:(2+17)], prob=TRUE)
aurora['sdg_pred'] <- select_SDG(aurora, true_label=sdg)
aurora['multiple'] <- multiple_SDGs(aurora, 1:9)
table(aurora$multiple)
testsample[ids,'Aurora'] <- aurora$sdg_pred


ls<-get_predicted_values(testsample, aurora, ml=TRUE, openai=FALSE, OpenAI=FALSE)
testsample1<- read_excel("./analysis/data/SDG reduced data for Paris.xlsx")
ids<-which(testsample$text %in% df.all$text)





#### PROSPECTOR
# select test items
Prospector<-read_excel('./analysis/data/VU_request_prospector.xlsx')
Prospector<-Prospector[,-1]
ids<-which(Prospector$text %in% testsample$text)
Prospector<-Prospector[ids,]
Prospector<-Prospector[order(Prospector$text),]
# Prospector<-Prospector[which(testsample$text %in% Prospector$text),]
testsample$text[!(testsample$text %in% Prospector$text)]
ids<-which(testsample$text %in% Prospector$text)
Prospector$highpos <- testsample$highpos[ids]

Prospector[c('sdg1','sdg2','sdg3')]<-get_SDG(Prospector[,8:(8+17)])
Prospector['sdg_pred'] <- select_SDG(Prospector, true_label=sdg)
Prospector['multiple'] <- multiple_SDGs(Prospector, SDG1_MODEL_TL:SDG17_MODEL_TL)
table(Prospector$multiple)
Prospector$sdg_pred[is.na(Prospector$sdg_pred)]<-0
colSums(Prospector[Prospector$multiple>1,8:(8+16)]) # SDG 17,2, 8 most
table(predicted=Prospector$sdg_pred, true=Prospector$sdg)
sum(Prospector$sdg==Prospector$sdg_pred) # 154 well predicted
table(Prospector$highpos)
table(Prospector$sdg[Prospector$highpos==0]==Prospector$sdg_pred[Prospector$highpos==0]) # 9
table(Prospector$sdg[Prospector$highpos==1]==Prospector$sdg_pred[Prospector$highpos==1]) # 145 out of 160
testsample[ids,'Prospector'] <- Prospector$sdg_pred


tab<-as.data.frame(f1_table[c('ADA','Aurora','Prospector','Babbage A (ada sample')])
for(i in 1:16) rownames(tab)[i]<-paste0('SDG',i)
tab<-rbind(tab, colMeans(tab))
print(xtable(round(tab,2), type = "latex"), file = "./analysis/output/models_easy.tex") 



#### comparing models
sum(testsample$sdg[ids]==testsample$Prospector[ids]) # 154 well predicted
sum(testsample$sdg==testsample$Babbage) # 184 well predicted

sum(testsample$sdg==testsample$ADAm) # 111 well predicted
sum(testsample$sdg==testsample$Aurora) # 61 well predicted

sum(testsample$sdg==testsample$`ChatGPT (ada)`) # 140 out of 237 well predicted

## only highpos examples (n=53)
sum(testsample$sdg_tested[testsample$highpos==1]==testsample$ADA[testsample$highpos==1]) # 111 well predicted
sum(testsample$sdg_tested[testsample$highpos==1]==testsample$Aurora[testsample$highpos==1]) # 61 well predicted
sum(testsample$sdg_tested[testsample$highpos==1]==testsample$Prospector[testsample$highpos==1]) # 74 well predicted
sum(testsample$sdg_tested[testsample$highpos==1]==testsample$`ChatGPT (ada)`[testsample$highpos==1]) # 140 out of 237 well predicted

## babbage # 111 well predicted

# ADA does equally well on low and high agreement (around 57-60%)
sum(testsample$sdg_tested[testsample$agreement<.2]==testsample$`ChatGPT (ada)`[testsample$agreement<.2]) # 32 out of 55 well predicted
sum(testsample$sdg_tested[testsample$agreement<.8]==testsample$`ChatGPT (ada)`[testsample$agreement<.8]) # 57 out of 94 well predicted
sum(testsample$sdg_tested[testsample$agreement>.8]==testsample$`ChatGPT (ada)`[testsample$agreement>.8]) # 82 out of 142 well predicted
















# assess whether the model has predicted the sdg
ADA$sdg<-ADA$sdg1
ADA$agreement <-testsample$agreement
Prospector$sdg<-Prospector$sdg1

ADAS<- get_n_labels(ADA[ADA$multiple==1,], `sdg`, c(.2,.8))
ProspectS<- get_n_labels(Prospector[Prospector$multiple==1,], `sdg`, c(.2,.8))
OSDG<- get_n_labels(testsample, sdg_tested, c(.2,.8))
OSDG1<- get_n_labels(testsample, `OSDG 1`, c(.2,.8))
Aurora<- get_n_labels(testsample, `Aurora`, c(.2,.8))

## fill empty sdg's
for(i in 1:3) OSDG[[i]]<-add_rows_missing_sdg(OSDG[[i]])
for(i in 1:3) OSDG1[[i]]<-add_rows_missing_sdg(OSDG1[[i]])
for(i in 1:3) ADAS[[i]]<-add_rows_missing_sdg(ADAS[[i]])
for(i in 1:3) Aurora[[i]]<-add_rows_missing_sdg(Aurora[[i]])
for(i in 1:3) ProspectS[[i]]<-add_rows_missing_sdg(ProspectS[[i]])

## how many are accurately predicted?
# per model, per level of difficulty (i.e. agreement)
# inspect texts that are not (what is it in this text that is hard?)


## dit werkt niet:
df<-as.data.frame(c(Aurora, ADAS, ProspectS, OSDG, OSDG1))
colnames(df)[1]<-'Freq'
df['Model'] <- c(rep('Aurora', 17),
                 rep('ADA (gpt3.5)', 17),
                 rep('Prospector', 17),
                 rep('OSDG (initial)', 17),
                 rep('OSDG (estimated)', 17))
df['SDG'] <- rep(1:17,4)
df['low'] <- c(Aurora[[1]], ChatGPT[[1]], OSDG[[1]], OSDG1[[1]])
df['high'] <- c(Aurora[[2]], ChatGPT[[2]], OSDG[[2]], OSDG1[[2]])
df['highpos'] <- c(Aurora[[3]], ChatGPT[[3]], OSDG[[3]], OSDG1[[3]])

ID = 1:17

df %>% 
  # filter(Model == 'Aurora') %>%
  ggplot(aes(SDG,Freq))+
  geom_col()+
  theme(plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, size=8, vjust = 0.5),
        axis.text.y = element_text(size=10),
        axis.title.y = element_blank()) +
  scale_x_continuous('SDG', labels = as.character(ID), breaks = ID)+
  facet_grid(~Model)+
  theme(strip.text = element_text(
    size = 15, color = 'dark green'))
  

df %>% 
  # filter(Model == 'Aurora') %>%
  ggplot(aes(SDG,low))+
  geom_col()+
  theme(plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, size=8, vjust = 0.5),
        axis.text.y = element_text(size=10),
        axis.title.y = element_blank()) +
  scale_x_continuous('SDG', labels = as.character(ID), breaks = ID)+
  facet_grid(~Model)+
  theme(strip.text = element_text(
    size = 15, color = 'dark green'))


df %>% 
  # filter(Model == 'Aurora') %>%
  ggplot(aes(SDG,high))+
  geom_col()+
  theme(plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, size=8, vjust = 0.5),
        axis.text.y = element_text(size=10),
        axis.title.y = element_blank()) +
  scale_x_continuous('SDG', labels = as.character(ID), breaks = ID)+
  facet_grid(~Model)+
  theme(strip.text = element_text(
    size = 15, color = 'dark green'))

df %>% 
  # filter(Model == 'Aurora') %>%
  ggplot(aes(SDG,highpos))+
  geom_col()+
  theme(plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 90, size=8, vjust = 0.5),
        axis.text.y = element_text(size=10),
        axis.title.y = element_blank()) +
  scale_x_continuous('SDG', labels = as.character(ID), breaks = ID)+
  facet_grid(~Model)+
  theme(strip.text = element_text(
    size = 15, color = 'dark green'))





# ## Prospector
# # avoid this one observation lacking in original testsample
# Prospector <-read_excel('./datasets/prospector/VU_request_prospector.xlsx')
# colnames(Prospector)[c(5,9:24)]<-c('completion','SDG1','SDG2','SDG3','SDG4','SDG5','SDG6','SDG7','SDG8',
#                                    'SDG9','SDG10','SDG11','SDG12','SDG13','SDG14','SDG15','SDG16')
# ls<-get_predicted_values(testsample, Prospector, ml=TRUE, openai=FALSE, OpenAI=FALSE)
# testsample['Prospector']<-ls[[1]]
# f1_table[,'Prospector']<-ls[[2]]
# f1_table[,'Prospector']<-ls[[3]]
# 
# 
# 
# # Aurora
# aurora<-read_excel('./datasets/aurora/predictions_osdg_policy_2023_april.xlsx')
# df.all <- read_excel("./analysis/data/osdg-community-data-v2023-04-01.xlsx")
# df.all %>% 
#   mutate(labels_n = labels_negative+labels_positive,
#          neg_prop = labels_negative/labels_n,
#          pos_prop = labels_positive/labels_n,
#          highpos = case_when(agreement > .8 & pos_prop > .9 ~ 1, TRUE~0)) -> df.all
# aurora['text']<-df.all$text
# aurora['Label']<-df.all$sdg
# aurora['agreement']<-df.all$agreement
# aurora['highpos']<-df.all$highpos
# 
# ls<-get_predicted_values(testsample, aurora, ml=TRUE, openai=FALSE, OpenAI=FALSE)
# testsample1<- read_excel("./analysis/data/SDG reduced data for Paris.xlsx")
# ids<-which(testsample$text %in% df.all$text)
# 
# testsample[ids,'Aurora']<-ls[[1]]
# f1_table[ids,'Aurora']<-ls[[2]]
# f1_table[ids,'Aurora']<-ls[[3]]
# 
# testsample['Davinci (ML)']<-ls[[1]]
# f1_table[,'Davinci (ML) easy']<-ls[[2]]
