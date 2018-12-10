
#Robustness check on variable first trial.

cd <- "DataFiles/" ### Put your own current directory here.
q4="FirstTrial.csv" #leave name intact

### File extensions
file4=paste(cd, q4, sep = "")

FT=read.csv(file4)

#Visualinspection
ggplot(data=FT, aes(x=popsbeforefirstcash,y=Alone)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm)

#Correlation matrix
x=FT[,c("Alone","popsbeforefirstcash","firstcash")] 
rc=rcorr(as.matrix(x),type="pearson")  
rc

#Descriptives
FT$popsbeforefirstcash=factor(FT$popsbeforefirstcash)
aggregate(FT$Alone, by=list(group=FT$popsbeforefirstcash),mean) 
aggregate(FT$firstcash, by=list(group=FT$popsbeforefirstcash),mean) 
