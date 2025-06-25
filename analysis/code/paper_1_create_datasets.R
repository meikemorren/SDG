# prepare table for paper
library(tidyverse)
library(readxl)
files<-list.files('./analysis/data/annotation')


df<-NULL
annotate<-NULL

for(i in seq(1, length(files))){
  df<-rbind(df, readxl::read_excel(paste0('./analysis/data/annotation/', files[i]), sheet = 'Output'))

  if(i %in% c(1,3:6,11:14,16,17)){
    annotate<-bind_rows(annotate,readxl::read_excel(paste0('./analysis/data/annotation/', files[i]),
                                  sheet = 'Final Review') %>%
      rename(Finn=`Finn...4`,
             Gib=`Gib...5`,
             `Jean-Baptiste`=`Jean-Baptiste...6`,
             Meike=`Meike...7`,
             Steve=`Steve...8`,
             Comments=`Comments...15`) %>%
      mutate(Finn=as.character(Finn),
             Gib=as.character(Gib),
             Meike=as.character(Meike),
             Steve=as.character(Steve),
             `Jean-Baptiste`=as.character(`Jean-Baptiste`),
             SDG=as.character(parse_number(gsub('_',' ',files[i])))) %>%
      select(`Text ID`, SDG, Text, Group, Finn, Gib, `Jean-Baptiste`, Meike, Steve, Alignment, Consensus, Rationale, Notes)
    )
  }
  if(i==2){
    annotate<-bind_rows(annotate, readxl::read_excel(paste0('./analysis/data/annotation/', files[i]),
                       sheet = 'Final Review') %>%
      mutate(Finn=as.character(Finn),
             Gib=as.character(Gib),
             Meike=as.character(Meike),
             Steve=as.character(Steve),
             `Jean-Baptiste`=as.character(`Jean-Baptiste`),
             Group=toupper(group),
             SDG=as.character(parse_number(gsub('_',' ',files[i])))) %>%
        select(`Text ID`, SDG, Text, Group, Finn, Gib, `Jean-Baptiste`, Meike, Steve, Alignment, Consensus, Rationale, Notes)
    )
  }
  if(i==7){
    annotate$Ivan<-NA
    annotate<-bind_rows(annotate, readxl::read_excel(paste0('./analysis/data/annotation/', files[i]),
                                                     sheet = 'Final Review') %>%
                          rename(Finn=`Finn...4`,
                                 Gib=`Gib...5`,
                                 `Jean-Baptiste`=`Jean-Baptiste...7`,
                                 Meike=`Meike...8`,
                                 Steve=`Steve...9`,
                                 Comments=`Comments...16`) %>%
                          mutate(Meike=as.character(Meike),
                                 Steve=as.character(Steve),
                                 `Jean-Baptiste`=as.character(`Jean-Baptiste`),
                                 SDG=as.character(parse_number(gsub('_',' ',files[i])))) %>%
                          select(`Text ID`, SDG, Text, Finn, Gib, Ivan, `Jean-Baptiste`, Meike, Steve, Alignment, Consensus, Rationale, Notes)
    )
  }
  if(i %in% c(8,9)){
    annotate<-bind_rows(annotate, readxl::read_excel(paste0('./analysis/data/annotation/', files[i]),
                                                     sheet = 'Final Review') %>%
                          rename(Finn=`Finn...4`,
                                 Gib=`Gib...5`,
                                 Ivan=`Ivan...6`,
                                 `Jean-Baptiste`=`Jean-Baptiste...7`,
                                 Meike=`Meike...8`,
                                 Steve=`Steve...9`,
                                 Comments=`Comments...17`) %>%
                          mutate(Finn=as.character(Finn),
                                 Gib=as.character(Gib),
                                 Meike=as.character(Meike),
                                 Steve=as.character(Steve),
                                 `Jean-Baptiste`=as.character(`Jean-Baptiste`),
                                 SDG=as.character(parse_number(gsub('_',' ',files[i])))) %>%
                          select(`Text ID`, SDG, Text, Finn, Gib, Ivan, `Jean-Baptiste`, Meike, Steve, Alignment, Consensus, Rationale, Notes)
    )
  }
  if(i ==10){
    annotate<-bind_rows(annotate, readxl::read_excel(paste0('./analysis/data/annotation/', files[i]),
                                                     sheet = 'Final Review') %>%
                          rename(Finn=`Finn...4`,
                                 Gib=`Gib...5`,
                                 `Jean-Baptiste`=`Jean-Baptiste...6`,
                                 Meike=`Meike...7`,
                                 Steve=`Steve...8`,
                                 Comments=`Comments...16`) %>%
                          mutate(Finn=as.character(Finn),
                                 Gib=as.character(Gib),
                                 Meike=as.character(Meike),
                                 Steve=as.character(Steve),
                                 `Jean-Baptiste`=as.character(`Jean-Baptiste`),
                                 SDG=as.character(parse_number(gsub('_',' ',files[i])))) %>%
                          select(`Text ID`, SDG, Text, Finn, Gib, `Jean-Baptiste`, Meike, Steve, Alignment, Consensus, Rationale, Notes)
    )
  }
  if(i==15){
    annotate<-bind_rows(annotate, readxl::read_excel(paste0('./analysis/data/annotation/', files[i]),
                                                     sheet = 'Final Review') %>%
                          mutate(SDG=as.character(parse_number(gsub('_',' ',files[i])))) %>%
                          select(`Text ID`, SDG, Text, Finn, Gib, `Jean-Baptiste`, Steve, Alignment, Consensus, Rationale) 
    )
  }
}

table(annotate$Consensus)

annotate<-annotate %>% 
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
                                              `Jean-Baptiste`=='SKIP'~ NA, 
                                              `Jean-Baptiste`=='TO DO'~ NA, TRUE~`Jean-Baptiste`)),
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
                                        Consensus=='IGNORE'~ NA,TRUE~Consensus)))

table(annotate$Consensus)

write.csv(annotate,'./analysis/data/annotated_texts.csv')
write.csv(annotate %>% filter(is.na(Consensus)) %>% select(`Text ID`, SDG, Text),
          './analysis/data/unclassified_texts.csv')
write.csv(df %>% filter(!is.na(df$sdg)),'./analysis/data/benchmark_texts.csv')
