library(gt)
library(janitor)
library(tidyverse)
annotate<-read_csv('./analysis/data/annotated_texts.csv')

annotate %>%
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
            Alignment=round(mean(Alignment),3),
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

# library(sjmisc)
# mtcars %>% rotate_df()

  #### tokens
library(stringr)
library(tidytext)

series <- tibble()
for(i in seq_along(annotate$`Text ID`)) {
  
  clean <- tibble(id = annotate$`Text ID`[i],
                  text = annotate$Text[i],
                  sdg = annotate$SDG[i],
                  label = ifelse(is.na(annotate$Consensus[i]), 'Undecided', 
                                 ifelse(annotate$Consensus[i]==TRUE, 'Confirmed','Rejected')))%>%
    unnest_tokens(word, text, strip_punct = TRUE) %>%
    mutate(word = str_replace(word, "('s)", "")) %>% 
    filter(nchar(word) > 2) %>% 
    anti_join(stop_words, by = c("word" = "word"))
  series <- rbind(series, clean)
}

# head(series)
# table(series$label)
# mean(tapply(series$word, series$id, length))





# get the info per text
df.char<-series %>% 
  group_by(sdg, id, label) %>% 
  summarise(n=length(word),
            )
# df.char<-NULL
# df.char<-as.data.frame(
#   cbind(n=tapply(series$word, series$id, length), 
#         id=names(tapply(series$word, series$id, length)),
#         sdg=aggregate(series$sdg, list(series$id),first)$x, # it is the same per id, so take the first
#         label=aggregate(series$label, list(series$id),first)$x))
# rownames(df.char)<-NULL

#### test differences between texts
library(rstatix)
aov_n<-df.char %>% mutate(n=as.numeric(n)) %>%  group_by(sdg) %>% anova_test(n ~ label)
cols <-c('F','average_Confirmed', 'average_Rejected', 'average_Undecided','n_Confirmed', 'n_Rejected', 'n_Undecided')
df.char %>% 
  group_by(sdg, label) %>% 
  summarise(average=mean(as.numeric(n)),
            n=length(n)) %>%
  pivot_wider(names_from=label, values_from=c(average, n)) %>%
  merge(.,aov_n, by='sdg') %>% 
  select(-c(Effect, DFn, DFd,ges)) %>% 
  arrange(sdg) %>% 
  mutate(across(cols, round, 1),
         Confirmed = paste0(as.character(average_Confirmed), ' (',as.character(n_Confirmed),')'),
         Rejected = paste0(as.character(average_Rejected), ' (',as.character(n_Rejected),')'),
         Undecided = paste0(as.character(average_Undecided), ' (',as.character(n_Undecided),')'),
         F = paste0(as.character(F),`p<.05`)
         )%>% 
  select(sdg, Confirmed, Rejected, Undecided, F) %>% 
  gt(.) %>% 
  gtsave(str_c('./analysis/output/texts_test.tex'))

# # get summary of n words per label
# df.char %>% 
#   mutate(n=as.numeric(n),
#          sdg = factor(sdg,levels=c('1','2','3','4','5','6','7','8','9',
#                                    '10','11','12','13','14','15','16','17'))) %>% 
#   group_by(label, sdg) %>% 
#   summarise(n=mean(as.numeric(n)))->df.char.summary

# appendix: show how sdgs do not differ much in length of texts
df.char %>% 
  ggplot(aes(x=as.numeric(n), fill=sdg))+
  geom_histogram()+
  scale_fill_manual(values=c('#E5233D','#DDA73A','#4CA146','#C5192D','#EF402C','#27BFE6',
                             '#FBC412','#A31C44','#F26A2D','#E01483','#F89D2A','#BF8D2C','#407F46','#1F97D4','#59BA48',
                             '#126A9F','#13496B'))+
  facet_wrap(~sdg, scales='fixed')+
  theme(legend.position="none", 
        axis.title = element_blank())

df.char %>% 
  ggplot(aes(x=as.numeric(n), color=label, fill=sdg))+
  geom_histogram()+
  scale_color_manual(values=c("black","grey",'lightgrey'))+
  scale_fill_manual(values=c('#E5233D','#DDA73A','#4CA146','#C5192D','#EF402C','#27BFE6',
                             '#FBC412','#A31C44','#F26A2D','#E01483','#F89D2A','#BF8D2C','#407F46','#1F97D4','#59BA48',
                             '#126A9F','#13496B'))+
  facet_wrap(~sdg, scales='fixed')+
  theme(legend.position="none", 
        axis.title = element_blank())

df.char %>% 
  mutate(label=case_when(label=='Undecided'~ 'Undecided', TRUE~'Agreed')) %>% 
  ggplot(aes(x=as.numeric(n), fill=label))+
  geom_histogram()+
  scale_fill_manual(values=c("black","red"))+
  facet_wrap(~sdg, scales='fixed')+
  theme(legend.position="none", 
        axis.title = element_blank())


# # # across labels, how often most popular words are mentioned
series  %>%
  group_by(sdg) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>% #tabyl(sdg)%>%
  mutate(sdg = factor(sdg,levels=c('1','2','3','4','5','6','7','8','9',
                                   '10','11','12','13','14','15','16','17')),
         name = reorder_within(word, -n, sdg),
         name = str_replace(name, '___(\\d+)','')
         ) %>% 
  # arrange(sdg, desc(n), desc(name)) %>% 
  # group_by(sdg,n) %>% mutate(text_order = 1:length(sdg)) %>% 
  # mutate(name = fct_inorder(name)) %>% 
  # ggplot(aes(reorder(word, text_order), n, fill = sdg)) +
  # ggplot(aes(reorder_within(word, n, sdg),n,fill=sdg))+
  ggplot(aes(reorder_within(name,n,sdg), n, fill = sdg)) +
  geom_col(show.legend = FALSE) +
  # geom_text(aes(label = n), hjust = 1) +
  # geom_bar(stat = "identity") +
  facet_wrap(~ sdg, scales = "free_y", ncol = 4) +
  scale_fill_manual(values=c('#E5233D','#DDA73A','#4CA146','#C5192D','#EF402C','#27BFE6',
  '#FBC412','#A31C44','#F26A2D','#E01483','#F89D2A','#BF8D2C','#407F46','#1F97D4','#59BA48',
  '#126A9F','#13496B'))+
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none", 
        axis.title = element_blank())

# differentiate between texts that are and are not about SDG

### inspect differences in words per label
series  %>%
  group_by(sdg, label) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>% #tabyl(sdg)%>%
  mutate(sdg = factor(sdg,levels=c('1','2','3','4','5','6','7','8','9',
                                   '10','11','12','13','14','15','16','17')),
         name = reorder_within(word, -n, sdg),
         name = str_replace(name, '___(\\d+)','')
  )%>% filter(sdg==7) %>% arrange(label) %>% print(n=50)
series  %>%
  group_by(sdg, label) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>% #tabyl(sdg)%>%
  mutate(sdg = factor(sdg,levels=c('1','2','3','4','5','6','7','8','9',
                                   '10','11','12','13','14','15','16','17')),
         name = reorder_within(word, -n, sdg),
         name = str_replace(name, '___(\\d+)','')
  )%>% filter(sdg==17) %>% arrange(label) %>% print(n=50)

series  %>%
  filter(label=='Rejected') %>%
  group_by(sdg, label) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>% #tabyl(sdg)%>%
  mutate(sdg = factor(sdg,levels=c('1','2','3','4','5','6','7','8','9',
                                   '10','11','12','13','14','15','16','17')),
         name = reorder_within(word, -n, sdg),
         name = str_replace(name, '___(\\d+)','')
  ) %>% 
  ggplot(aes(reorder_within(name,n,sdg), n, fill = sdg)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sdg, scales = "free_y", ncol = 4) +
  scale_fill_manual(values=c('#E5233D','#DDA73A','#4CA146','#C5192D','#EF402C','#27BFE6',
                             '#FBC412','#A31C44','#F26A2D','#E01483','#F89D2A','#BF8D2C','#407F46','#1F97D4','#59BA48',
                             '#126A9F','#13496B'))+
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none", 
        axis.title = element_blank())


series  %>%
  group_by(sdg) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>% #tabyl(sdg)%>%
  mutate(sdg = factor(sdg,levels=c('1','2','3','4','5','6','7','8','9',
                                   '10','11','12','13','14','15','16','17')),
         text_order = nrow(.):1
         
  ) %>% 
  ggplot(aes(reorder(word, text_order), n, fill = sdg)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ sdg, scales = "free_y",, ncol = 4) +
  scale_fill_manual(values=c('#E5233D','#DDA73A','#4CA146','#C5192D','#EF402C','#27BFE6',
                             '#FBC412','#A31C44','#F26A2D','#E01483','#F89D2A','#BF8D2C','#407F46','#1F97D4','#59BA48',
                             '#126A9F','#13496B'))+
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none", 
        axis.title = element_blank())

# ggplot(df_summary_country,aes(factor(ideology), 
#                                 mean_cons_country, fill=country)) + 
#   geom_col(fill='lightgray') + geom_bar(stat='identity',
#                                         position='dodge')+
#   scale_fill_manual(values = hcl.colors(n=4,palette = 'berlin', 
#                                         alpha = .8)) + 
#   ylim(0,12)+
#   labs(x=NULL, y=NULL, title="")+
#   scale_y_continuous(breaks=c(0,1,2,3),
#                      limits = c(0,12),"Per country", 
#                      sec.axis = sec_axis(~ . / 4, breaks=NULL, name=NULL))+
#   theme(legend.position="none") + 
#   theme(plot.title = element_text(hjust = .5))+
#   geom_signif(comparisons=list(c("0", "10")), 
#               annotations="Conservation \n t = -3.060***",
#               y_position = 11.5, tip_length = 0.2, vjust = .5)+
#   theme(axis.text.y.right = element_blank(),
#         axis.title.y.left = element_text(hjust=0.1))