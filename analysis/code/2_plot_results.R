
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
