library(gt)
library(janitor)
library(tidyverse)
annotate<-read.csv('./analysis/data/annotated_texts.csv')

annotate %>% 
  mutate(Finn=as.logical(case_when(Finn=='NOT SDG 10'~ 'FALSE',
                   Finn=='SDG 10'~ 'TRUE',
                   Finn=='NOT SDG 7'~ 'FALSE',
                   Finn=='SDG 7'~ 'TRUE',
                   Finn=='SKIP'~ NA,Finn=='TO DO'~ NA, TRUE~Finn)),
         Gib=as.logical(case_when(Gib=='NOT SDG 10'~ 'FALSE',
                       Gib=='SDG 10'~ 'TRUE',
                       Gib=='NOT SDG 7'~ 'FALSE',
                       Gib=='SDG 7'~ 'TRUE',
                       Gib=='SKIP'~ NA, Gib=='TO DO'~ NA, TRUE~Gib)),
         `Jean-Baptiste`=as.logical(case_when(`Jean-Baptiste`=='NOT SDG 10'~ 'FALSE',
                       `Jean-Baptiste`=='SDG 10'~ 'TRUE',
                       `Jean-Baptiste`=='NOT SDG 7'~ 'FALSE',
                       `Jean-Baptiste`=='SDG 7'~ 'TRUE',
                       `Jean-Baptiste`=='SKIP'~ NA, `Jean-Baptiste`=='TO DO'~ NA, TRUE~`Jean-Baptiste`)),
         Meike=as.logical(case_when(Meike=='NOT SDG 10'~ 'FALSE',
                         Meike=='SDG 10'~ 'TRUE',
                         Meike=='SKIP'~ NA, Meike=='TO DO'~ NA, TRUE~Meike)),
         Ivan=as.logical(case_when(Ivan=='SKIP'~ NA, Ivan=='TO DO'~ NA, TRUE~Ivan)),
         Steve=as.logical(case_when(Steve=='NOT SDG 10'~ 'FALSE',
                         Steve=='SDG 10'~ 'TRUE',
                         Steve=='NOT SDG 7'~ 'FALSE',
                         Steve=='SDG 7'~ 'TRUE',
                         Steve=='SKIP'~ NA,Steve=='TO DO'~ NA, TRUE~Steve)),
         Consensus=as.logical(case_when(Consensus=='NOT SDG 10'~ 'FALSE',
                                    Consensus=='SDG 10'~ 'TRUE',
                                    Consensus=='NOT SDG 7'~ 'FALSE',
                                    Consensus=='SDG 7'~ 'TRUE',
                                    Consensus=='IGNORE'~ NA,TRUE~Consensus))) %>% 
  rowwise() %>% 
  mutate(Annotators=sum(!is.na(Meike), !is.na(Steve), !is.na(Finn), !is.na(Ivan),
                        !is.na(`Jean-Baptiste`),!is.na(Gib)),
         Positive=sum(Meike, Steve, Finn, Ivan,`Jean-Baptiste`,Gib, na.rm=T),
         Negative=Annotators-Positive,
         Undecided=sum(Consensus))%>%
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
  gt(.) %>% 
  gtsave(str_c('./analysis/output/curated_data.tex'))

  #### tokens
library(stringr)
library(tidytext)

series <- tibble()
for(i in seq_along(annotate$`Text ID`)) {
  
  clean <- tibble(id = annotate$`Text ID`[i],
                  text = annotate$Text[i],
                  sdg = annotate$SDG[i],
                  label = Consensus[i]) %>%
    unnest_tokens(word, text, strip_punct = TRUE) %>%
    mutate(word = str_replace(word, "('s)", "")) %>% 
    filter(nchar(word) > 2) %>% 
    anti_join(stop_words, by = c("word" = "word"))
   
  series <- rbind(series, clean)
}

head(series)
mean(tapply(series$word, series$id, length))

df.char<-cbind(n=tapply(series$word, series$id, length), 
               id=names(tapply(series$word, series$id, length)),
               sdg=aggregate(series$sdg, list(series$id),first))
rownames(df.char)<-NULL

df.char %>% 
  as.data.frame() %>% 
  rename(sdg=sdg.x) %>% 
  mutate(n=as.numeric(n),
         sdg = factor(sdg,levels=c('1','2','3','4','5','6','7','8','9',
                                          '10','11','12','13','14','15','16','17'))) %>% 
  ggplot(aes(x=n, fill=sdg))+geom_histogram()+
  scale_fill_manual(values=c('#E5233D','#DDA73A','#4CA146','#C5192D','#EF402C','#27BFE6',
                             '#FBC412','#A31C44','#F26A2D','#E01483','#F89D2A','#BF8D2C','#407F46','#1F97D4','#59BA48',
                             '#126A9F','#13496B'))+
  facet_wrap(~sdg)+
  theme(legend.position="none", 
        axis.title = element_blank())

# differentiate between texts that are and are not about SDG
series  %>%
  group_by(sdg) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>% #tabyl(sdg)%>%
  mutate(sdg = factor(sdg,levels=c('1','2','3','4','5','6','7','8','9',
                                   '10','11','12','13','14','15','16','17')),
         # text_order = nrow(.):1
         
         ) %>% #tabyl(sdg)
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

