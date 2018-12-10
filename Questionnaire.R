

# Questionnaire data analyses

cd <- "/Users/Kimmie/Dropbox/Papers/BART/PaperVersions/EconBARTalone/JOEP/DataFiles/" ### Put your own current directory here.
q1="Questionnaire.csv" #leave name intact

### Upload file: stored in R as Q.
file1=paste(cd, q1, sep = "")
Q=read.csv(file1)
Q$Participant <- Q$What.is.your.subject.ID.

## Initialize count variable
count=0

## Code impulsivity and sensation seeking

# Barratt impulsiveness scale
BIS1=Q$I.plan.tasks.carefully.
BIS2=Q$I.do.things.without.thinking.
BIS3=Q$I.make.up.my.mind.quickly.
BIS4=Q$I.am.happy.go.lucky.
BIS5=Q$I.don.t.pay.attention.
BIS6=Q$I.have.racing.thoughts.
BIS7=Q$I.plan.trips.well.ahead.of.time.
BIS8=Q$I.am.self.controlled.
BIS9=Q$I.concentrate.easily.
BIS10=Q$I.save.regularly.
BIS11=Q$I..squirm..at.plays.or.lectures.
BIS12=Q$I.am.a.carefull.thinker.
BIS13=Q$I.plan.for.job.security.
BIS14=Q$I.say.things.without.thinking.
BIS15=Q$I.like.to.think.about.complex.problems.
BIS16=Q$I.change.jobs.
BIS17=Q$I.act..on.impulse..
BIS18=Q$I.get.easily.bored.when.solving.thought.problems.
BIS19=Q$I.act.on.the.spur.of.the.moment.
BIS20=Q$I.am.a.steady.thinker.
BIS21=Q$I.change.where.I.live.
BIS22=Q$I.buy.things.on.impulse.
BIS23=Q$I.can.only.think.about.one.problem.at.a.time.
BIS24=Q$I.change.hobbies.
BIS25=Q$I.spend.or.charge.more.than.I.earn.
BIS26=Q$I.have.outside.thoughts.when.thinking.
BIS27=Q$I.am.more.interested.in.the.present.than.the.future.
BIS28=Q$I.am.restless.at.lectures.or.talks.
BIS29=Q$I.like.puzzles.
BIS30=Q$I.plan.for.the.future.

BIS=data.frame(BIS1,BIS2,BIS3,BIS4,BIS5,BIS6,BIS7,BIS8,BIS9,BIS10,BIS11,BIS12,BIS13,BIS14,BIS15,BIS16,BIS17,BIS18,BIS19,BIS20,BIS21,BIS22,BIS23,BIS24,BIS25,BIS26,BIS27,BIS28,BIS29,BIS30) #Make a BIS data frame
BIS[is.na(BIS)] <- 0

for (i in 1:(nrow(Q))) {
  count[i]=with(BIS[i,], c(sum(BIS[i,] == 0)))
}

BIS$countna=count

#Reverse coding: 1, 7, 8, 9, 10, 12, 13, 15, 20, 29, 30 (1-4 = 4-1)

RCBIS<-c('BIS1','BIS7','BIS8','BIS9','BIS10','BIS12','BIS13','BIS15','BIS20','BIS29','BIS30')
y <- length(c(RCBIS))

for (k in 1:y) {
  z <- RCBIS[k]  
  x <- length(BIS[,z])
  for (i in 1:x) {
    if(BIS[i,z]==4)  {
      BIS[i,z]=1
    } else if(BIS[i,z]==3)  {
      BIS[i,z]=2
    } else if(BIS[i,z]==2)  {
      BIS[i,z]=3
    } else if(BIS[i,z]==1) {
      BIS[i,z]=4
    } else BIS[i,z]=0
  }
}

# Summing the numbers before the zeros have been replaced with the average per part score
BIS$BISscore <- rowSums(BIS[,1:30],na.rm = T)
BIS$BISavscore <- round(BIS$BISscore/(30-BIS$countna))

for (i in 1:(nrow(Q))) {
  for (q in 1:30) {
    BIS[i,q] <- ifelse(BIS[i,q]==0, BIS$BISavscore[i], BIS[i,q])
  }
}

# Now make composite score (after reverse coding and after solving some NA's)
BIS$BISscore <- rowSums(BIS[,1:30])
# Add participant as to combine it with another data frame later.
BIS$Participant=Q$Participant

# Brief Sensation seeking scale

BSS1=Q$I.would.like.to.explore.strange.places.
BSS2=Q$I.would.like.to.take.off.on.a.trip.with.no.pre.planned.routes.or.timetables.
BSS3=Q$I.get.restless.when.I.spend.too.much.time.at.home..
BSS4=Q$I.prefer.friends.who.are.excitingly.unpredictable..
BSS5=Q$I.like.to.do.frightening.things..
BSS6=Q$I.would.like.to.try.bungee.jumping..
BSS7=Q$I.like.wild.parties..
BSS8=Q$I.would.love.to.have.new.and.exciting.experiences..even.if.they.are.illegal..

BSSS=data.frame(BSS1,BSS2,BSS3,BSS4,BSS5,BSS6,BSS7,BSS8)

#Solve those na's...

BSSS[is.na(BSSS)] <- 0

for (i in 1:(nrow(Q))) {
  count[i]=with(BSSS[i,], c(sum(BSSS[i,] == 0)))
}

BSSS$BSSSscore <- rowSums(BSSS,na.rm = T)
BSSS$countna=count
BSSS$avscoreBSSS <- round(BSSS$BSSSscore/(8-BSSS$countna))

for (i in 1:(nrow(Q))) {
  for (q in 1:8) {
    BSSS[i,q] <- ifelse(BSSS[i,q]==0, BSSS$avscoreBSSS[i], BSSS[i,q])
  }
}

# Now make composite and subset scale scores (after solving some NA's)
BSSS$BSSSscore <- rowSums(BSSS[,1:8])
BSSS$experienceseeking <- BSSS$BSS1+BSSS$BSS2
BSSS$boredom <- BSSS$BSS3+BSSS$BSS4
BSSS$thrill <- BSSS$BSS5+BSSS$BSS6
BSSS$disinhibition <- BSSS$BSS7+BSSS$BSS8

# Add participant as to combine it with another data frame later.
BSSS$Participant=Q$Participant


#Audit: relabel the Questions for ease of coding
AUDIT1 = Q$How.often.do.you.have..a.drink.containing.alcohol..
AUDIT2A = Q$How.many.drinks.containing.alcohol.do.you.have.on.a.typical.day.when.you.are.drinking.by.yourself..
AUDIT2S = Q$How.many.drinks.containing.alcohol.do.you.have.on.a.typical.day.when.you.are.drinking.with.others.
AUDIT3A = Q$How.often.do.you.have.six.or.more.drinks.on.one.occasion.when.you.are.drinking.alone.
AUDIT3S = Q$How.often.do.you.have.six.or.more.drinks.on.one.occasion.when.you.are.drinking.with.others.
AUDIT4A = Q$How.often.during.the.last.year.have.you.found.that.you.were.not.able.to.stop.drinking.once.you.ha...
AUDIT4S = Q$How.often.during.the.last.year.have.you.found.that.you.were.not.able.to.stop.drinking.once.you.ha....1
AUDIT5A = Q$How.often.during.the.last..year.have.you.failed.to.do.what.was.normally.expected.of.you.because.a...
AUDIT5S = Q$How.often.during.the.last..year.have.you.failed.to.do.what.was.normally.expected.of.you.because.a....1
AUDIT6 = Q$How.often.during.the.last.year.have.you.needed.a.first.drink.in.the.morning.to.get.yourself.going...
AUDIT7A = Q$How.often.during.the.last.year.have.you.had.a.feeling.of.guilt.or.remorse.after.drinking.by.yours...
AUDIT7S = Q$How.often.during.the.last.year.have.you.had.a.feeling.of.guilt.or.remorse.after.drinking.with.oth...
AUDIT8 = Q$How.often.during.the.last.year.have.you.been.unable.to.remember.what.happened.the.night.before.be...
AUDIT9 = Q$Have.you.or.someone.else.been.injured.because.of.your.drinking..
AUDIT10 = Q$Has.a.relative..friend..doctor..or.other.health.care.worker.been.concerned.about.your.drinking.or...

#Audit: originally for the 3-option questions scored to 0,2 and 4.
AUDIT9 <- ifelse(AUDIT9==2, 4, AUDIT9)
AUDIT9<- ifelse(AUDIT9==1, 2, AUDIT9)

AUDIT10 <- ifelse(AUDIT10==2, 4, AUDIT10)
AUDIT10<- ifelse(AUDIT10==1, 2, AUDIT10)

# New data frame based on recalculated questions
AuditA <- data.frame(AUDIT1,AUDIT2A,AUDIT3A,AUDIT4A,AUDIT5A,AUDIT6,AUDIT7A,AUDIT8,AUDIT9,AUDIT10)
AuditA[is.na(AuditA)] <- 0
AuditA$AAscore <- rowSums(AuditA,na.rm = T)
AuditA$Participant=Q$Participant
AuditS <- data.frame(AUDIT1,AUDIT2S,AUDIT3S,AUDIT4S,AUDIT5S,AUDIT6,AUDIT7S,AUDIT8,AUDIT9,AUDIT10)
AuditS[is.na(AuditS)] <- 0
AuditS$ASscore <- rowSums(AuditS,na.rm = T)
AuditS$Participant=Q$Participant

# Centers for Disease Control Youth Risk Behaviour Surveillance System (CDC, 2001)

CDCrisk1=Q$Smoked.a.cigarette..even.a.puff.
CDCrisk2=Q$Drank.alcohol..even.one.drink..
CDCrisk3=Q$Used.any.illegal.drug
CDCrisk4=Q$Gambled.for.real.money.
CDCrisk5=Q$Had.sexual.intercourse.without.a.condom.
CDCrisk6=Q$Stolen.anything.from.a.store.
CDCrisk7=Q$Carried.a.weapon.such.as.a.gun..knife..or.club.outside.of.your.home
CDCrisk8=Q$Been.in.a.physical.fight.
CDCrisk9=Q$Ridden.in.a.car.without.wearing.your.seatbelt..even.once.
CDCrisk10=Q$Ridden.a.bicycle.or.motorcycle.without.a.helmet..even.once.

CDCrisk=data.frame(CDCrisk1,CDCrisk2,CDCrisk3,CDCrisk4,CDCrisk5,CDCrisk6,CDCrisk7,CDCrisk8,CDCrisk9,CDCrisk10)
CDCrisk[is.na(CDCrisk)] <- 0

CDCrisk$CDCscore=rowSums(CDCrisk)
CDCrisk$CDCviolence=rowSums(CDCrisk[6:8])
CDCrisk$CDCharmfuloneself=rowSums(CDCrisk[,c(5,9,10)])
CDCrisk$Participant=Q$Participant

#drugs

Marijuana_A=Q[,40]
Stimulants_A=Q[,41]
Cocaine_A=Q[,42]
Hallucinogens_A=Q[,43]
Opiates_A=Q[,44]
Sedatives_A=Q[,45]
Nodrugs_A=Q[,46]

Marijuana_O=Q[,49]
Stimulants_O=Q[,50]
Cocaine_O=Q[,51]
Hallucinogens_O=Q[,52]
Opiates_O=Q[,53]
Sedatives_O=Q[,54]
Nodrugs_O=Q[,55]

Druguse=data.frame(Marijuana_A,Stimulants_A,Cocaine_A,Hallucinogens_A,Opiates_A,Sedatives_A,Nodrugs_A,Marijuana_O,Stimulants_O,Cocaine_O,Hallucinogens_O,Opiates_O,Sedatives_O,Nodrugs_O)
Druguse[is.na(Druguse)] <- 0

Druguse$Drugstotal_A=rowSums(Druguse[,1:6])
Druguse$Drugstotal_O=rowSums(Druguse[,8:13])
Druguse$hitup_A=Q[,58]
Druguse$hitup_O=Q[,59]
Druguse[is.na(Druguse)] <- 0

# Incl heroine use in the drugs score
Heroine_O <- ifelse(Druguse$hitup_O>0, 1, 0)
Druguse$Drugstotal_O = Druguse$Drugstotal_O + Heroine_O

Druguse$Participant=Q$Participant

#Sex

Sexpartners=Q[,62] #0: none, 1=1, 2=2, 3=3, 4=4, 5=5, 6: 5-10, 7: >10 in past year
Regularpartners=Q[,63] #0: none, 1=1, 2=2, 3=3, 4=4, 5=5, 6: 5-10, 7: >10 in past year
Casualpartners=Q[,66]
STDregpartner=Q[,64] #0=no, 1=yes, 
STDcaspartner=Q[,67] #0=never, 1=sometimes, 2=often, 3=always
Condomregpartner=Q[,65] #0=never, 1=rarely, 2=sometimes, 3=often, 4=every time
Condomcaspartner=Q[,68] #0=never, 1=rarely, 2=sometimes, 3=often, 4=every time

Sex=data.frame(Sexpartners, Regularpartners, Casualpartners, STDregpartner, STDcaspartner, Condomregpartner, Condomcaspartner)
Sex[is.na(Sex)] <- 0 #many NA's here, eg question about sex behavior with reg partner, but you don't have them.
# Code carefully.

Sex$Participant=Q$Participant

# Demographics
Age=Q[,171]
Gender=Q[,172] #1=female, 2=male, 0=notreported
Ethnicity=Q[,173] #0=unknown/unreported, 1=hispanic/latino, 2=nothispanic/latino
Race=Q[,174] #0=unknown/unreported,1=american indian or alaska native, 2=asian, 3=black, 4=pacific islander, 5=white, 6=more than one race
Student=Q[,175] #0=neither, 1=studentCU, 2=studentnocu, 3=employee
Education=Q[,176] #1=high school, 2=high school equiv, 3=college, 4=ass degree, 5=bachelor, 6=master, 7=PhD, 8=prof degree
Towardsmen=Q[,177] #1-7: 1=very strongly avoid, 7=very strongly prefer
Towardswomen=Q[,178] #1-7: 1=very strongly avoid, 7=very strongly prefer

Demographics=data.frame(Age,Gender,Ethnicity,Race,Student,Education,Towardsmen,Towardswomen)

# Add participant as to combine it with another data frame later.
Demographics$Participant=Q$Participant