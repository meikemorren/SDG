source('./analysis/code/helper_functions.r')

# create table for paper
f1_table<-as.data.frame(matrix(0, nrow=9*2, ncol=16))
rownames(f1_table)<-c('ADA','Babbage','Davinci','Prospector',
                      'Aurora')
for(i in 1:16) colnames(f1_table)[i]<-paste0('SDG',i)

test_babbage_8UZwuiwR.csv