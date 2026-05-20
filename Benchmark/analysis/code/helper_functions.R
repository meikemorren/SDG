# to select texts of each sdg, even though agreement is below threshold
add_texts<-function(df, sdgsample, i, n){
  "if not enough good texts (high agreement), then
  pick among the second best texts (moderate agreement)"
  df %>% 
    filter(test==0, sdg=={{i}}, highpos==1) %>% 
    nrow(.) -> nhighpos
  print(n)
  
  df %>% 
    filter(sdg=={{i}}, 
           # !text_id %in% sample$text_id,
           test==0, highpos==0,
           agreement <=.8, agreement > .6,
           pos_prop>.7)  %>% 
    mutate(label = sdg) %>% 
    arrange(desc(agreement), desc(pos_prop)) -> extra
  
  return(extra[1:(n-nhighpos),])
  
}


create_sdg_testsample<-function(df, sdgs, n){
  list_testsamples<-list()
  for(i in sdgs){
    # get easy examples
    df %>% 
      filter(sdg=={{i}}, highpos==1) %>% 
      mutate(label = sdg) -> sdgsample
    
    # add second best easy examples until n=500
    if(nrow(sdgsample) <n){
      sdgsample <- rbind(sdgsample, add_texts(df, sdgsample, i, n))
    }
    if(nrow(sdgsample)> n){
      set.seed(1234)
      sdgsample[sample(n),] -> sdgsample
    }
    
    # get random examples that are not sdg i
    df %>% 
      filter(sdg!={{i}}) %>% 
      mutate(label= case_when(sdg=={{i}}~1, TRUE~0)) -> random
    
    random_selected<-NULL
    for(j in sdgs[sdgs!=i]){
      temp<-random[random$sdg==j,]
      
      set.seed(1234)
      temp<-temp[sample(ceiling(n/(length(sdgs)-1))),]
      random_selected<-rbind(random_selected, temp)
    }
    
    # if i dont shuffle, it will just do 1:500 below with sample()
    set.seed(1234)
    random_selected<-random_selected[sample(1:nrow(random_selected)),]
    set.seed(1234)
    random_sample <- random_selected[sample(n),] 
    
    # shuffle cases so order doesnt matter
    sdgsample<-rbind(sdgsample, random_sample)
    set.seed(1234)
    list_testsamples[[i]]<-sdgsample[sample(1:nrow(sdgsample)),]
  }
  return(list_testsamples)
}


## get ADA labels
get_n_labels<-function(df, x, intervals){
  df %>% 
    mutate(sdg = case_when({{x}}==0 ~NA, TRUE~ as.numeric({{x}}))) %>% 
    filter(agreement < intervals[1], !is.na(sdg)) %>% 
    group_by(sdg) %>% 
    summarize(n=n()) -> low
  df %>% 
    # mutate(sdg = as.numeric({{x}})) %>% 
    mutate(sdg = case_when({{x}}==0 ~NA, TRUE~ as.numeric({{x}}))) %>% 
    filter(agreement>=intervals[1] & agreement <intervals[2], !is.na(sdg)) %>% 
    group_by(sdg) %>% 
    summarize(n=n()) -> mid
  df %>% 
    # mutate(sdg = as.numeric({{x}})) %>% 
    mutate(sdg = case_when({{x}}==0 ~NA, TRUE~ as.numeric({{x}}))) %>% 
    filter(agreement>=intervals[2], !is.na(sdg)) %>% 
    group_by(sdg) %>% 
    summarize(n=n()) %>% 
    arrange()-> high
  return(list(low, mid, high))
}

add_rows_missing_sdg<- function(incomplete.df){
  complete.df<-NULL
  k<-1
  for(i in 1:16){
    if(i %in% incomplete.df$sdg){
      complete.df<-rbind(complete.df, incomplete.df[k,])
      k<-k+1
    }else{
      complete.df<-rbind(complete.df, c(sdg=i, n=0))
    }
  }
  return(complete.df)
}

multiple_SDGs<-function(df, cols){
  
  df %>% 
    select({{cols}}) %>% 
    mutate(across({{cols}}, ~ case_when(
      .x==0 ~ 0,
      .x!=0 ~ 1)))->df
  
  df %>% 
    rowwise() %>% 
    mutate(multiple = sum(c_across({{cols}}), na.rm=T)) %>% 
    select(multiple) -> multiple
  
  return(multiple)
}


get_SDG<-function(df, prob=FALSE){
  'assumes sdgs are ordered in dataframe, last columns,
  only takes up to 3 sdgs, creates separate columns'
  sdg1<-rep(0, nrow(df))
  sdg2<-rep(0, nrow(df))
  sdg3<-rep(0, nrow(df))
  if(prob==FALSE){
    for(k in seq(1, nrow(df))){
      for(i in seq(1, ncol(df))){
        sdg3[k]<- ifelse(df[k,i]>0 & sdg1[k]!=0 & sdg2[k]!=0, i, sdg3[k])
        if(sdg3[k]==0) sdg2[k]<- ifelse(df[k,i]>0 & sdg1[k]!=0, i, sdg2[k])
        if(sdg2[k]==0 & sdg3[k]==0) sdg1[k]<- ifelse(df[k,i]!=0 & sdg1[k]==0, i, sdg1[k]) 
      }
    }
  }
  if(prob==TRUE){
    for(k in seq(1, nrow(df))){
      for(i in seq(1, ncol(df))){
        sdg3[k]<- ifelse(df[k,i]>=.98 & sdg1[k]>=.98 & sdg2[k]>=.98, i, sdg3[k])
        if(sdg3[k]<.98) sdg2[k]<- ifelse(df[k,i]>=.98 & sdg1[k]>=.98, i, sdg2[k])
        if(sdg2[k]<.98 & sdg3[k]<.98) sdg1[k]<- ifelse(df[k,i]>=.98 & sdg1[k]<.98, i, sdg1[k]) 
      }
    }
  }
  return(cbind(sdg1, sdg2, sdg3))
}

select_SDG<-function(df, true_label){
  'finds the overlap between sdg tested and sdg,
  inputs first sdg if no overlap 
  Warning: lower SDGs are more likely to appear'
  df %>% 
    rowwise() %>% 
    mutate(sdg = case_when({{true_label}} %in% c(sdg1, sdg2, sdg3) ~ {{true_label}}, TRUE~ sdg1)) %>% 
    select(sdg) -> sdg
  print(sdg)
  return(sdg)
}


get_predicted_values <-function(df, pred.df, ml=TRUE, openai=TRUE, OpenAI=TRUE){
  
  if(openai==TRUE) for(i in seq(1, nrow(pred.df))) pred.df$text[i]<-strsplit(pred.df$prompt[i],' ->')[[1]][1]
  ids<-which(pred.df$text %in% df$text)
  print(length(ids)==nrow(df))
  pred.df<-pred.df[ids,]
  pred.df<-pred.df[order(pred.df$text),]
  
  if(length(ids) < nrow(df)){
    print(sprintf('deleted the following observation(s) due to missing match text: %s',df$text[!(df$text %in% pred.df$text)]))
    ids<-which(df$text %in% pred.df$text)
    all<-1:nrow(df)
    print(sprintf('this text is on row: %i',all[!(all %in% ids)]))
  }
  
  pred.df$highpos <- df$highpos[ids]
  pred.df$textori <- df$text[ids]
  print(table(pred.df$highpos, pred.df$completion))
  
  print('@_@_@_@_@_@_@_@ checks @_@_@_@_@_@_@_@')
  print(sprintf('Number of text matches: %i (should be %i).',table(pred.df$text==pred.df$textori), nrow(pred.df))) # check
  
  if(ml==TRUE){
    
    if(openai==TRUE) for(i in seq(1, nrow(pred.df))) pred.df$Label[i]<-gsub('###','',pred.df$Label[i]) # check
    pred.df$Label <- as.numeric(pred.df$Label)
    # print(table(pred.df$Label,pred.df$completion))
    print(table(pred.df$highpos, pred.df$Label))
    print(sprintf('Number of label matches: %i.',table(pred.df$completion==pred.df$Label)['TRUE'])) #
    print('@_@_@_@_@_@_@_@ inspection @_@_@_@_@_@_@_@')
    print('Number of easy texts (1 means high agreement, by many annotaters):')
    print(table(pred.df$highpos))
    
    print('well-predicted labels easy texts:')
    print(table(pred.df$completion[pred.df$highpos==1]==pred.df$Label[pred.df$highpos==1])) #
    print('well-predicted labels difficult texts: ')
    print(table(pred.df$completion[pred.df$highpos==0]==pred.df$Label[pred.df$highpos==0])) #
    print('inspect which labels do not overlap: ')
    print(table(true=pred.df$completion[pred.df$highpos==1], predicted=pred.df$Label[pred.df$highpos==1])) #
    
    print('@_@_@_@_@_@_@_@ evaluation @_@_@_@_@_@_@_@')
    pred.df.easy<-pred.df[pred.df$highpos==1,]
    print(sprintf('proportion of EASY texts correct: %f', 
        table(pred.df.easy$Label==pred.df.easy$completion)['TRUE']/nrow(pred.df.easy)
    ))

    print('@_@_@_@_@_@_@_@ F1 statistics @_@_@_@_@_@_@_@')
    print('f1 easy texts:')
    f1s_easy<-f1_multilabel(pred.df$completion[pred.df$highpos==1],pred.df$Label[pred.df$highpos==1])
    print('f1 difficult texts: ')
    print(table(pred.df$completion[pred.df$highpos==0],pred.df$Label[pred.df$highpos==0]))
    f1s_diff<-f1_multilabel(pred.df$completion[pred.df$highpos==0],pred.df$Label[pred.df$highpos==0])
    # print(sprintf('number of EASY texts correct: %f', f1))
    return(list(pred.df$Label, f1s_easy, f1s_diff))
  }
  
  if(ml==FALSE){
    # c <- which(colnames(pred.df) %in% 'SDG1')
    # print(c)
    pred.df[c('sdg1','sdg2','sdg3')]<-get_SDG(pred.df %>% select(starts_with('SDG'))) ## somehow only give SDG columns!
    pred.df['sdg_pred']<-select_SDG(pred.df, true_label=completion)
    # print(pred.df[c('sdg1','sdg2','sdg3','completion','sdg_pred')])
    # print(table(pred.df$sdg_pred))
    pred.df['multiple']<- multiple_SDGs(pred.df, SDG1:SDG16)
    
    print('@_@_@_@_@_@_@_@ inspection @_@_@_@_@_@_@_@')
    print('Number of multiple labels per text: ')
    print(table(pred.df$multiple))
    
    print('well-predicted labels easy texts:')
    print(table(pred.df$completion[pred.df$highpos==1]==pred.df$sdg_pred[pred.df$highpos==1])) #
    print('well-predicted labels difficult texts: ')
    print(table(pred.df$completion[pred.df$highpos==0]==pred.df$sdg_pred[pred.df$highpos==0])) #
    
    print('inspect which labels do not overlap: ')
    print(table(predicted=pred.df$sdg_pred, true=pred.df$completion))
    
    pred.df.easy<-pred.df[pred.df$highpos==1,]
    print('@_@_@_@_@_@_@_@ evaluation @_@_@_@_@_@_@_@')
    print(sprintf('number of easy texts correct: %f', 
        table(pred.df.easy$sdg_pred==pred.df.easy$completion)['TRUE']/nrow(pred.df.easy)
    ))
    f1s_easy<-f1(pred.df$completion[pred.df$highpos==1],pred.df$sdg_pred[pred.df$highpos==1])
    f1s_diff<-f1(pred.df$completion[pred.df$highpos==0],pred.df$sdg_pred[pred.df$highpos==0])
    # print(sprintf('number of EASY texts correct: %f', f1))
    return(list(pred.df$sdg_pred, f1s_easy, f1_diff))
    return(pred.df$sdg_pred)
  }
}

# tab<-table(BAB$completion==1, BAB$Label==1)
f1<-function(tab, class){
  f1<-NA
  # print("yes")
  if(ncol(tab)==2 & nrow(tab)==2){
    precision<- tab[2,2]/sum(tab[,2]) # correct positive to total positive predictions
    recall<-tab[2,2]/sum(tab[2,]) # correct positive to total correct positive
    f1<-2*(precision*recall)/(precision+recall)
    if(is.nan(f1)) f1<-0.000 # CHECK: no correct positive labels
    print(sprintf('F1 of label %g is %f', class ,f1))
  }else{
    message("An Error Occurred")
    f1<-0.000 # CHECK: no positive labels
    # print(class)
    # print(tab)
  }
  
  # if(class){
  #   print(sprintf('F1 of label %g is %f', class ,f1))
  #   }else{
  #   print(sprintf('F1 of single label model is %f', f1))
  # }
  return(f1)
}

f1_multilabel<-function(classes, predictions){
  f1s <- NULL
  for(class in unique(classes)){
    print(class)
    tab<-table(classes==class, predictions==class)
    print(tab)
    f1<-f1(tab, class)
    f1s <- c(f1s, f1)
  }
  return(f1s)
}

# f1s<-f1_multilabel(BAB$completion, BAB$Label)

