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


write.csv(annotate,'./analysis/data/annotated_texts.csv')
write.csv(df %>% filter(!is.na(df$sdg)),'./analysis/data/benchmark_texts.csv')
