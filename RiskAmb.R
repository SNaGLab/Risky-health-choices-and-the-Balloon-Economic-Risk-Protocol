

#Only change cd on line 27!

# Risk-ambiguity data

countAS=0
countRS=0
Participant=0
sw1=0
sw2=0
SwitchRisk=0
SwitchAmb=0

for (i in 1:158) 
  
{
  
  id=c(1:2,4:6,8:9,11:14,16:22,24:163) 
  p=id[i]
  
  MRIpart <- ifelse(p == 8 || p==16 || p==24 || p==30 || p==36 || p==45 || p==47 || p==54 || p==60 || p==65 || p==72 || p==74 || p==80 || p==83 || p==86 || p==90 || p==103 || p==105 || p==114 || p==115 || p==118 || p==123 || p==127 || p==131 || p==138 || p==140 || p==150 || p==156 || p==160, 
                    c(1), c(0))
  p=toString(p)
  
  ###put here the cd where you have the folder with data behavioral participants and MRI participants
  s1 <- ifelse(MRIpart==0,c("/Users/Kimmie/Dropbox/Papers/BART/PaperVersions/EconBARTalone/JOEP/DataFiles/BehData/"), c("/Users/Kimmie/Dropbox/Papers/BART/PaperVersions/EconBARTalone/JOEP/DataFiles/MRI/"))
  s2 <- "/RiskAmb.csv"
  file=paste(s1, p, s2, sep = "")
  
  RA=read.csv(file, header=FALSE)
  
  RA <- rename(RA, c(V1="TypeUnc"))
  RA <- rename(RA, c(V2="Row"))
  RA <- rename(RA, c(V3="SureAmount"))
  RA <- rename(RA, c(V4="Choice"))
  
  RA$Row=RA$Row+1
  
  RA$Choice <- ifelse(RA$Choice == "Gamble", 
                      c(1), c(2))
  
  for (x in 1:20)
  {
    sw1[x]=ifelse(x==1, c(0),c(abs(RA$Choice[x]-RA$Choice[x-1])))
  }
  countswitchrisk=sum(sw1)
  
  for (y in 1:20)
  {
    sw2[y]=ifelse(y==1, c(0),c(abs(RA$Choice[y+20]-RA$Choice[y+19])))
  }
  countswitchamb=sum(sw2)
  
  countswitchrisk <- ifelse(RA$TypeUnc[1]=="Risk", c(sum(sw1)), c(sum(sw2)))
  countswitchamb <- ifelse(RA$TypeUnc[1]=="Ambiguity", c(sum(sw1)), c(sum(sw2)))
  
  countAS[i]=with(RA, c(sum(Choice==1 & TypeUnc=="Ambiguity")))
  countRS[i]=with(RA, c(sum(Choice==1 & TypeUnc=="Risk")))
  Participant[i]=id[i]
  SwitchRisk[i]=countswitchrisk
  SwitchAmb[i]=countswitchamb
  
}

RiskAmb=cbind.data.frame(Participant,countAS,countRS,SwitchRisk,SwitchAmb)
RiskAmb$AA=ifelse(countAS==0 & countRS==0, c(0),c((countRS-countAS)/(countAS+countRS)))
RiskAmb$RA=ifelse(countRS==0, c(0),c(1-countRS/20)) #if 10 then RA = 0.5, which is risk aversion

