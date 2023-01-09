#install.packages("pacman")
pacman::p_load(RCurl,multcomp, Epi,foreign, downloader, survey, srvyr, ggplot2, dplyr,sqldf,tableone)
library(tigris)
library(spdep)
library(RColorBrewer)
library(rsatscan)
library(plyr)

setwd()

#2011
brfss11<-read.csv("brfss11.csv", header = T,stringsAsFactors = F)

brfss11$hp[brfss11$BPHIGH4 %in% c(3)]<-1
brfss11$hp[brfss11$BPHIGH4 %in% c(1,2,4)]<-0
table(brfss11$hp,useNA = "ifany")

brfss11$diab[brfss11$DIABETE3 %in% c(3)]<-1
brfss11$diab[brfss11$DIABETE3 %in% c(1,2,4)]<-0
table(brfss11$diab,useNA = "ifany")

brfss11$chole[brfss11$TOLDHI2 %in% 2]<-1
brfss11$chole[brfss11$TOLDHI2 %in% 1]<-0
table(brfss11$chole,useNA = "ifany")

brfss11$smoke<-0
brfss11$smoke[brfss11$SMOKE100 %in% 2 ]<-1 #smoked <100
brfss11$smoke[brfss11$SMOKE100 %in% 1 & brfss11$SMOKDAY2 %in% 3]<-1 #smoked >100 but not currently smoking
brfss11$smoke[brfss11$SMOKE100 %in% c(7,9) | brfss11$SMOKDAY2 %in% c(7,9)]<-NA
table(brfss11$smoke,useNA = "ifany")

brfss11$bmicont<-brfss11$WTKG3/(brfss11$HTM4*brfss11$HTM4/10000)/100
brfss11$bmi[brfss11$bmicont >=18.5 & brfss11$bmicont < 25 ]<-1
brfss11$bmi[brfss11$bmicont <18.5 | brfss11$bmicont >=25 ]<-0
table(brfss11$bmi,useNA = "ifany")

brfss11$pa[brfss11$X_PA150R1 %in% 1]<-1
brfss11$pa[brfss11$X_PA150R1 %in% c(2,3)]<-0
table(brfss11$pa,useNA = "ifany")

brfss11$diet_sum<-rowSums(brfss11[,c("FTJUDA1_","FRUTDA1_","BEANDAY_","GRENDAY_","ORNGDAY_","VEGEDA1_")], na.rm = T) 
brfss11$dietmiss<-is.na(brfss11$FTJUDA1_)+is.na(brfss11$FRUTDA1_)+is.na(brfss11$BEANDAY_)+is.na(brfss11$GRENDAY_)+is.na(brfss11$ORNGDAY_)+is.na(brfss11$VEGEDA1_)
brfss11$diet[brfss11$diet_sum/100 >=5]<-1
brfss11$diet[brfss11$diet_sum/100 <5 & brfss11$dietmiss == 0]<-0
table(brfss11$diet,useNA = "ifany")

brfss11$race[brfss11$X_RACEGR2 %in% c(1)]<-1
brfss11$race[brfss11$X_RACEGR2 %in% c(2)]<-2
brfss11$race[brfss11$X_RACEGR2 %in% c(5)]<-3
brfss11$race[brfss11$X_RACEGR2 %in% c(3,4)]<-4
table(brfss11$race,useNA = "ifany")

#2013
brfss13<-read.csv("brfss13.csv", header = T,stringsAsFactors = F)

brfss13$hp[brfss13$BPHIGH4 %in% c(3)]<-1
brfss13$hp[brfss13$BPHIGH4 %in% c(1,2,4)]<-0
table(brfss13$hp,useNA = "ifany")

brfss13$diab[brfss13$DIABETE3 %in% c(3)]<-1
brfss13$diab[brfss13$DIABETE3 %in% c(1,2,4)]<-0
table(brfss13$diab,useNA = "ifany")

brfss13$chole[brfss13$TOLDHI2 %in% 2]<-1
brfss13$chole[brfss13$TOLDHI2 %in% 1]<-0
table(brfss13$chole,useNA = "ifany")

brfss13$smoke<-0
brfss13$smoke[brfss13$SMOKE100 %in% 2 ]<-1 #smoked <100
brfss13$smoke[brfss13$SMOKE100 %in% 1 & brfss13$SMOKDAY2 %in% 3]<-1 #smoked >100 but not currently smoking
brfss13$smoke[brfss13$SMOKE100 %in% c(7,9) | brfss13$SMOKDAY2 %in% c(7,9)]<-NA
table(brfss13$smoke,useNA = "ifany")

brfss13$bmicont<-brfss13$WTKG3/(brfss13$HTM4*brfss13$HTM4/10000)/100
brfss13$bmi[brfss13$bmicont >=18.5 & brfss13$bmicont < 25 ]<-1
brfss13$bmi[brfss13$bmicont <18.5 | brfss13$bmicont >=25 ]<-0
table(brfss13$bmi,useNA = "ifany")

brfss13$pa[brfss13$X_PA150R2 %in% 1]<-1
brfss13$pa[brfss13$X_PA150R2 %in% c(2,3)]<-0
table(brfss13$pa,useNA = "ifany")

brfss13$diet_sum<-rowSums(brfss13[,c("FTJUDA1_","FRUTDA1_","BEANDAY_","GRENDAY_","ORNGDAY_","VEGEDA1_")], na.rm = T) 
brfss13$dietmiss<-is.na(brfss13$FTJUDA1_)+is.na(brfss13$FRUTDA1_)+is.na(brfss13$BEANDAY_)+is.na(brfss13$GRENDAY_)+is.na(brfss13$ORNGDAY_)+is.na(brfss13$VEGEDA1_)
brfss13$diet[brfss13$diet_sum/100 >=5]<-1
brfss13$diet[brfss13$diet_sum/100 <5 & brfss13$dietmiss == 0]<-0
table(brfss13$diet,useNA = "ifany")

brfss13$race[brfss13$X_RACEGR3 %in% c(1)]<-1
brfss13$race[brfss13$X_RACEGR3 %in% c(2)]<-2
brfss13$race[brfss13$X_RACEGR3 %in% c(5)]<-3
brfss13$race[brfss13$X_RACEGR3 %in% c(3,4)]<-4
table(brfss13$race,useNA = "ifany")

colnames(brfss13)[30]<-c("AGE")


#2015
brfss15<-read.csv("brfss15.csv", header = T,stringsAsFactors = F)

brfss15$hp[brfss15$BPHIGH4 %in% c(3)]<-1
brfss15$hp[brfss15$BPHIGH4 %in% c(1,2,4)]<-0
table(brfss15$hp,useNA = "ifany")

brfss15$diab[brfss15$DIABETE3 %in% c(3)]<-1
brfss15$diab[brfss15$DIABETE3 %in% c(1,2,4)]<-0
table(brfss15$diab,useNA = "ifany")

brfss15$chole[brfss15$TOLDHI2 %in% 2]<-1
brfss15$chole[brfss15$TOLDHI2 %in% 1]<-0
table(brfss15$chole,useNA = "ifany")

brfss15$smoke<-0
brfss15$smoke[brfss15$SMOKE100 %in% 2 ]<-1 #smoked <100
brfss15$smoke[brfss15$SMOKE100 %in% 1 & brfss15$SMOKDAY2 %in% 3]<-1 #smoked >100 but not currently smoking
brfss15$smoke[brfss15$SMOKE100 %in% c(7,9) | brfss15$SMOKDAY2 %in% c(7,9)]<-NA
table(brfss15$smoke,useNA = "ifany")

brfss15$bmicont<-brfss15$WTKG3/(brfss15$HTM4*brfss15$HTM4/10000)/100
brfss15$bmi[brfss15$bmicont >=18.5 & brfss15$bmicont < 25 ]<-1
brfss15$bmi[brfss15$bmicont <18.5 | brfss15$bmicont >=25 ]<-0
table(brfss15$bmi,useNA = "ifany")

brfss15$pa[brfss15$X_PA150R2 %in% 1]<-1
brfss15$pa[brfss15$X_PA150R2 %in% c(2,3)]<-0
table(brfss15$pa,useNA = "ifany")

brfss15$diet_sum<-rowSums(brfss15[,c("FTJUDA1_","FRUTDA1_","BEANDAY_","GRENDAY_","ORNGDAY_","VEGEDA1_")], na.rm = T) 
brfss15$dietmiss<-is.na(brfss15$FTJUDA1_)+is.na(brfss15$FRUTDA1_)+is.na(brfss15$BEANDAY_)+is.na(brfss15$GRENDAY_)+is.na(brfss15$ORNGDAY_)+is.na(brfss15$VEGEDA1_)
brfss15$diet[brfss15$diet_sum/100 >=5]<-1
brfss15$diet[brfss15$diet_sum/100 <5 & brfss15$dietmiss == 0]<-0
table(brfss15$diet,useNA = "ifany")

brfss15$race[brfss15$X_RACEGR3 %in% c(1)]<-1
brfss15$race[brfss15$X_RACEGR3 %in% c(2)]<-2
brfss15$race[brfss15$X_RACEGR3 %in% c(5)]<-3
brfss15$race[brfss15$X_RACEGR3 %in% c(3,4)]<-4
table(brfss15$race,useNA = "ifany")

colnames(brfss15)[19]<-c("AGE")

#2017
brfss17<-read.csv("brfss17.csv", header = T,stringsAsFactors = F)

brfss17$hp[brfss17$BPHIGH4 %in% c(3)]<-1
brfss17$hp[brfss17$BPHIGH4 %in% c(1,2,4)]<-0
table(brfss17$hp,useNA = "ifany")

brfss17$diab[brfss17$DIABETE3 %in% c(3)]<-1
brfss17$diab[brfss17$DIABETE3 %in% c(1,2,4)]<-0
table(brfss17$diab,useNA = "ifany")

brfss17$chole[brfss17$TOLDHI2 %in% 2]<-1
brfss17$chole[brfss17$TOLDHI2 %in% 1]<-0
table(brfss17$chole,useNA = "ifany")

brfss17$smoke<-0
brfss17$smoke[brfss17$SMOKE100 %in% 2 ]<-1 #smoked <100
brfss17$smoke[brfss17$SMOKE100 %in% 1 & brfss17$SMOKDAY2 %in% 3]<-1 #smoked >100 but not currently smoking
brfss17$smoke[brfss17$SMOKE100 %in% c(7,9) | brfss17$SMOKDAY2 %in% c(7,9)]<-NA
table(brfss17$smoke,useNA = "ifany")

brfss17$bmicont<-brfss17$WTKG3/(brfss17$HTM4*brfss17$HTM4/10000)/100
brfss17$bmi[brfss17$bmicont >=18.5 & brfss17$bmicont < 25 ]<-1
brfss17$bmi[brfss17$bmicont <18.5 | brfss17$bmicont >=25 ]<-0
table(brfss17$bmi,useNA = "ifany")

brfss17$pa[brfss17$X_PA150R2 %in% 1]<-1
brfss17$pa[brfss17$X_PA150R2 %in% c(2,3)]<-0
table(brfss17$pa,useNA = "ifany")

brfss17$diet_sum<-rowSums(brfss17[,c("FTJUDA2_","FRUTDA2_","GRENDA1_","FRNCHDA_","POTADA1_","VEGEDA2_")], na.rm = T) 
brfss17$dietmiss<-is.na(brfss17$FTJUDA2_)+is.na(brfss17$FRUTDA2_)+is.na(brfss17$GRENDA1_)+is.na(brfss17$FRNCHDA_)+is.na(brfss17$POTADA1_)+is.na(brfss17$VEGEDA2_)
brfss17$diet[brfss17$diet_sum/100 >=5]<-1
brfss17$diet[brfss17$diet_sum/100 <5 & brfss17$dietmiss == 0]<-0
table(brfss17$diet,useNA = "ifany")

brfss17$race[brfss17$X_RACEGR3 %in% c(1)]<-1
brfss17$race[brfss17$X_RACEGR3 %in% c(2)]<-2
brfss17$race[brfss17$X_RACEGR3 %in% c(5)]<-3
brfss17$race[brfss17$X_RACEGR3 %in% c(3,4)]<-4
table(brfss17$race,useNA = "ifany")
colnames(brfss17)[19]<-c("AGE")

#2019
brfss19<-read.csv("brfss19.csv", header = T,stringsAsFactors = F)
colnames(brfss19)

brfss19$hp[brfss19$BPHIGH4 %in% c(3)]<-1
brfss19$hp[brfss19$BPHIGH4 %in% c(1,2,4)]<-0
table(brfss19$hp,useNA = "ifany")

brfss19$diab[brfss19$DIABETE4 %in% c(3)]<-1
brfss19$diab[brfss19$DIABETE4 %in% c(1,2,4)]<-0
table(brfss19$diab,useNA = "ifany")

brfss19$chole[brfss19$TOLDHI2 %in% 2]<-1
brfss19$chole[brfss19$TOLDHI2 %in% 1]<-0
table(brfss19$chole,useNA = "ifany")

brfss19$smoke<-0
brfss19$smoke[brfss19$SMOKE100 %in% 2 ]<-1 #smoked <100
brfss19$smoke[brfss19$SMOKE100 %in% 1 & brfss19$SMOKDAY2 %in% 3]<-1 #smoked >100 but not currently smoking
brfss19$smoke[brfss19$SMOKE100 %in% c(7,9) | brfss19$SMOKDAY2 %in% c(7,9)]<-NA
table(brfss19$smoke,useNA = "ifany")

brfss19$bmicont<-brfss19$WTKG3/(brfss19$HTM4*brfss19$HTM4/10000)/100
brfss19$bmi[brfss19$bmicont >=18.5 & brfss19$bmicont < 25 ]<-1
brfss19$bmi[brfss19$bmicont <18.5 | brfss19$bmicont >=25 ]<-0
table(brfss19$bmi,useNA = "ifany")

brfss19$pa[brfss19$X_PA150R3 %in% 1]<-1
brfss19$pa[brfss19$X_PA150R3 %in% c(2,3)]<-0
table(brfss19$pa,useNA = "ifany")

brfss19$diet_sum<-rowSums(brfss19[,c("FTJUDA2_","FRUTDA2_","GRENDA1_","FRNCHDA_","POTADA1_","VEGEDA2_")], na.rm = T) 
brfss19$dietmiss<-is.na(brfss19$FTJUDA2_)+is.na(brfss19$FRUTDA2_)+is.na(brfss19$GRENDA1_)+is.na(brfss19$FRNCHDA_)+is.na(brfss19$POTADA1_)+is.na(brfss19$VEGEDA2_)
brfss19$diet[brfss19$diet_sum/100 >=5]<-1
brfss19$diet[brfss19$diet_sum/100 <5 & brfss19$dietmiss == 0]<-0
table(brfss19$diet,useNA = "ifany")

brfss19$race[brfss19$X_RACEGR3 %in% c(1)]<-1
brfss19$race[brfss19$X_RACEGR3 %in% c(2)]<-2
brfss19$race[brfss19$X_RACEGR3 %in% c(5)]<-3
brfss19$race[brfss19$X_RACEGR3 %in% c(3,4)]<-4
table(brfss19$race,useNA = "ifany")
colnames(brfss19)[19]<-c("AGE")



#combine all data cycles

dat<-rbind(brfss11[,c("X_STATE","IYEAR","X_PSU","X_LLCPWT","AGE","race","X_EDUCAG","X_INCOMG","MARITAL","PREGNANT","hp","diab","chole","smoke","bmi","pa","diet","CVDINFR4","CVDCRHD4","CVDSTRK3","RCSRLTN2","HLTHPLN1")],brfss13[,c("X_STATE","IYEAR","X_PSU","X_LLCPWT","AGE","race","X_EDUCAG","X_INCOMG","MARITAL","PREGNANT","hp","diab","chole","smoke","bmi","pa","diet","CVDINFR4","CVDCRHD4","CVDSTRK3","RCSRLTN2","HLTHPLN1")],brfss15[,c("X_STATE","IYEAR","X_PSU","X_LLCPWT","AGE","race","X_EDUCAG","X_INCOMG","MARITAL","PREGNANT","hp","diab","chole","smoke","bmi","pa","diet","CVDINFR4","CVDCRHD4","CVDSTRK3","RCSRLTN2","HLTHPLN1")],brfss17[,c("X_STATE","IYEAR","X_PSU","X_LLCPWT","AGE","race","X_EDUCAG","X_INCOMG","MARITAL","PREGNANT","hp","diab","chole","smoke","bmi","pa","diet","CVDINFR4","CVDCRHD4","CVDSTRK3","RCSRLTN2","HLTHPLN1")],brfss19[,c("X_STATE","IYEAR","X_PSU","X_LLCPWT","AGE","race","X_EDUCAG","X_INCOMG","MARITAL","PREGNANT","hp","diab","chole","smoke","bmi","pa","diet","CVDINFR4","CVDCRHD4","CVDSTRK3","RCSRLTN2","HLTHPLN1")])

colnames(dat)[c(1:4)]<-c("state","year","psu","weight")

save(dat,file="brfss11_19.Rda")

dat$year[dat$year %in% c(2012)]<-2011
dat$year[dat$year %in% c(2014)]<-2013
dat$year[dat$year %in% c(2016)]<-2015
dat$year[dat$year %in% c(2018)]<-2017
dat$year[dat$year %in% c(2020)]<-2019
table(dat$year,useNA = "ifany")

dat<-dat[!dat$state %in% c(66,72,78),] #1,308,981  23116
dat<-dat[dat$AGE>=15 & dat$AGE<=49,] #425,892  883089
dat<-dat[!is.na(dat$race),] #420,665  5227
dat<-dat[!is.na(dat$hp) & !is.na(dat$diab) & !is.na(dat$chole) & !is.na(dat$bmi) & !is.na(dat$smoke) & !is.na(dat$pa) & !is.na(dat$diet), ] #269,564  151101

#age group
dat$ageg[dat$AGE<=24]<-1               #18-24
dat$ageg[dat$AGE>=25 & dat$AGE<=34]<-2 #25-34
dat$ageg[dat$AGE>=35 & dat$AGE<=44]<-3 #35-44
dat$ageg[dat$AGE>=45 & dat$AGE<=49]<-4 #45-49
table(dat$ageg,useNA = "ifany")

#education
dat$edu[dat$X_EDUCAG %in% c(1)]<-1 #<high school
dat$edu[dat$X_EDUCAG %in% c(2)]<-2 #high school
dat$edu[dat$X_EDUCAG %in% c(3)]<-3 #some college
dat$edu[dat$X_EDUCAG %in% c(4)]<-4 #college or above
table(dat$edu,useNA = "ifany")

#income(1: <25000, 2: 25000-50000, 3: >50000)
dat$income[dat$X_INCOMG %in% c(1,2)]<-1
dat$income[dat$X_INCOMG %in% c(3,4)]<-2
dat$income[dat$X_INCOMG %in% c(5)]<-3
table(dat$income,useNA = "ifany")

#marital
dat$marry[dat$MARITAL %in% c(1,6)]<-1 #married, member of unmarried couple
dat$marry[dat$MARITAL %in% c(2,3,4)]<-2 #divorced,widowed,separated
dat$marry[dat$MARITAL %in% c(5)]<-3 #never married
table(dat$marry,useNA = "ifany")

#pregnancy
dat$pregnancy[dat$PREGNANT %in% c(1)]<-1 #currently pregnant
dat$pregnancy[dat$PREGNANT %in% c(2)]<-3 #never pregnant
dat$pregnancy[!dat$PREGNANT %in% c(1) & dat$RCSRLTN2 %in% c(1)]<-2 #ever pregnant
table(dat$pregnancy,useNA = "ifany")

#histroy of CVD
dat$cvd<-NA
dat$cvd[dat$CVDCRHD4 %in% c(1) | dat$CVDINFR4 %in% c(1) | dat$CVDSTRK3 %in% c(1) ]<-1
dat$cvd[dat$CVDCRHD4 %in% c(2) & dat$CVDINFR4 %in% c(2) & dat$CVDSTRK3 %in% c(2) ]<-0
table(dat$cvd,useNA = "ifany")

#access to health care
dat$care[dat$HLTHPLN1 %in% 1]<-1 #yes
dat$care[dat$HLTHPLN1 %in% 2]<-0 #no
table(dat$care,useNA = "ifany")

#cvh score
dat$cvhscore<-rowSums(dat[,c("hp","diab","chole","smoke","bmi","diet","pa")]) 
table(dat$cvhscore,useNA = "ifany")

#ideal cvh
dat$cvh[dat$cvhscore ==7]<-1
dat$cvh[dat$cvhscore <7]<-0
table(dat$cvh,useNA = "ifany")
13800/269564

dat<-dat[,c("state","year","psu","weight","ageg","race","edu","income","marry","pregnancy","cvd","hp","diab","chole","smoke","bmi","diet","pa","cvh","cvhscore","care")]
table(dat$cvh, dat$year,useNA = "ifany")
table(dat$cvhscore, dat$year,useNA = "ifany")

save(dat,file="brfss_cleaned.Rda")

dat$edu[is.na(dat$edu)]<-99
dat$income[is.na(dat$income)]<-99
dat$marry[is.na(dat$marry)]<-99
dat$pregnancy[is.na(dat$pregnancy)]<-99
dat$cvd[is.na(dat$cvd)]<-99
dat$care[is.na(dat$care)]<-99

dat$race2[dat$race %in% c(1,3,4,99)]<-0
dat$race2[dat$race %in% c(2)]<-1
table(dat$race2)

#account for survey design
design1<-svydesign(id = ~ psu, strata = ~ state, nest = TRUE, weight = ~ weight, data = dat)
state<-unique(dat$state)

############################
#    Descriptive tables    #
############################

# describe each metric
comp<-function(var){
  n<-table(dat[,var],useNA = "ifany")
  p<-svyCreateCatTable(vars = c(var),data = design1)
  
  np<-cbind(paste0(n[2]," (",round(p[[1]][[1]]$percent,1)[2],")"),
            paste0(n[3]," (",round(p[[1]][[1]]$percent,1)[3],")"))
  np
}

vars<-c("hp","diab","chole","smoke","bmi","pa","diet")
out<-NULL
for (i in 1:7){
  table<-comp(vars[i])
  out<-rbind(out,table,NA)
  print(paste("Working on",vars[i]))
}
out
write.csv(out,file="metric_percent.csv",row.names = F)

### Table 1 ###
t1<-svyCreateCatTable(vars = c("cvh"),data = design1)
t2<-svyCreateContTable(vars = c("cvhscore"),data = design1)

describe<-function(var,strata){
  n1<-table(dat[,var],dat$cvh,useNA = "ifany")
  p1<-svyCreateCatTable(vars = c(var),strata = c(strata),data = design1)
  n2<-table(dat[,var],useNA = "ifany")
  p2<-svyCreateCatTable(vars = c(var),data = design1)
  n3<-table(dat[,var])
  mean<-svyCreateContTable(vars = c("cvhscore"),strata = c(var),data = design1)
  
  out<-NULL
  for(i in 1:length(unique(dat[,var]))){
    np<-cbind(paste0(prettyNum(n1[i,2],big.mark = ",")," (",sprintf("%.1f",p1[[2]][[1]]$percent[i]),")"), 
              paste0(prettyNum(n1[i,1],big.mark = ",")," (",sprintf("%.1f",p1[[1]][[1]]$percent[i]),")"),
              paste0(prettyNum(n2[i],big.mark = ",")," (",sprintf("%.1f",p2[[1]][[1]]$percent[i]),")"),
              paste0(sprintf("%.2f",mean[[i]][4]), "±", sprintf("%.2f",mean[[i]][5])),
              sprintf("%.2f",mean[[i]][4]),
              paste0(sprintf("%.2f",(mean[[i]][4]-mean[[i]][5]/sqrt(n3[i])))," - ",sprintf("%.2f",(mean[[i]][4]+mean[[i]][5]/sqrt(n3[i]))))
    )
    colnames(np)<-c("Ideal","Non-ideal","Total","Mean±SD","Mean","95% CI")
    out<-rbind(out,np)
  }
  return(out)
}

vars<-c("ageg","race","edu","income","marry","cvd","pregnancy","year")
out<-NULL
for (i in 1:8){
  print(paste("Working on",vars[i]))
  table<-describe(vars[i],"cvh")
  out<-rbind(out,table,NA)
}
out
write.csv(out,file="outputs/brfss_describe.csv")

#calculate 95% CIs
out2<-NULL
for (j in c(1,0)){
  testdat<-dat[dat$cvh %in% j,]
  testdesign<-svydesign(id = ~ psu, strata = ~ state, nest = TRUE, weight = ~ weight, data = testdat)
  out1<-NULL
  for (i in 1:8){
    print(paste("Working on",j,vars[i]))
    n1<-table(testdat[,vars[i]],useNA = "ifany")
    f<-as.formula(paste0("~factor(",vars[i],")"))
    p<-round(svymean(f,testdesign)*100,1)
    ci<-round(confint(svymean(f,testdesign))*100,1)
    
    out<-NULL
    for(k in 1:length(unique(dat[,vars[i]]))){
      row1<-prettyNum(n1[k],big.mark = ",")
      row2<-paste0(sprintf("%.1f",p[[k]])," (",sprintf("%.1f",ci[k,1]),", ",sprintf("%.1f",ci[k,2]),")")
      cell<-rbind(row1,row2)
      out<-rbind(out,cell)
    }
    out1<-rbind(out1,out,NA)
  }
  out2<-cbind(out2,out1)
}

out3<-NULL
for (i in 1:8){
  print(paste("Working on",vars[i]))
  n1<-table(dat[,vars[i]],useNA = "ifany")
  f<-as.formula(paste0("~factor(",vars[i],")"))
  p<-round(svymean(f,design1)*100,1)
  ci<-round(confint(svymean(f,design1))*100,1)
  
  out<-NULL
  for(k in 1:length(unique(dat[,vars[i]]))){
    row1<-prettyNum(n1[k],big.mark = ",")
    row2<-paste0(sprintf("%.1f",p[[k]])," (",sprintf("%.1f",ci[k,1]),", ",sprintf("%.1f",ci[k,2]),")")
    cell<-rbind(row1,row2)
    out<-rbind(out,cell)
  }
  out3<-rbind(out3,out,NA)
}

out4<-NULL
for (i in 1:8){
  print(paste("Working on",vars[i]))
  f<-as.formula(paste0("~",vars[i]))
  mean<-svyby(~cvhscore,f,design1,svymean)
  ci<-round(confint(mean),2)
  
  out<-NULL
  for(k in 1:length(unique(dat[,vars[i]]))){
    row1<-sprintf("%.2f",mean$cvhscore[k])
    row2<-paste0("(",sprintf("%.2f",ci[k,1]),", ",sprintf("%.2f",ci[k,2]),")")
    cell<-rbind(row1,row2)
    out<-rbind(out,cell)
  }
  out4<-rbind(out4,out,NA)
}

table1<-cbind(out2,out3,out4)
write.csv(table1,file="outputs/brfss_describe2.csv")

svymean(~factor(cvh),design1)
confint(svymean(~factor(cvh),design1))
svymean(~cvhscore,design1)
confint(svymean(~cvhscore,design1))

### Table 2 and S2 racial disparities###
# 4 racial categories
# Ideal CVH
cont<-function(out,pred){
  f1<-paste0(out,"~",pred)
  lm1<-svyglm(formula = f1, design=design1, family=quasibinomial(link = "logit"))
  est1<-sprintf("%.2f",exp(coef(lm1)[2:4]))
  ci1<-sprintf("%.2f",exp(confint(lm1)[2:4,]))
  m1<-rbind(paste0(est1[1]," (",ci1[1],", ",ci1[4],")"),paste0(est1[2]," (",ci1[2],", ",ci1[5],")"),paste0(est1[3]," (",ci1[3],", ",ci1[6],")"))
  
  f2<-paste0(out,"~",pred,"+factor(ageg)")
  lm2<-svyglm(formula = f2, design=design1, family=quasibinomial(link = "logit"))
  est2<-sprintf("%.2f",exp(coef(lm2)[2:4]))
  ci2<-sprintf("%.2f",exp(confint.default(lm2)[2:4,]))
  m2<-rbind(paste0(est2[1]," (",ci2[1],", ",ci2[4],")"),paste0(est2[2]," (",ci2[2],", ",ci2[5],")"),paste0(est2[3]," (",ci2[3],", ",ci2[6],")"))
  
  f3<-paste0(out,"~",pred,"+factor(ageg)+factor(edu)+factor(income)+factor(marry)+factor(pregnancy)+factor(cvd)+factor(year)+factor(care)")
  lm3<-svyglm(formula = f3, design=design1, family=quasibinomial(link = "logit"))
  est3<-sprintf("%.2f",exp(coef(lm3)[2:4]))
  ci3<-sprintf("%.2f",exp(confint.default(lm3)[2:4,]))
  m3<-rbind(paste0(est3[1]," (",ci3[1],", ",ci3[4],")"),paste0(est3[2]," (",ci3[2],", ",ci3[5],")"),paste0(est3[3]," (",ci3[3],", ",ci3[6],")"))
  
  ors<-cbind(unadj=m1,age_adj=m2,fully_adj=m3)
  return(ors)
}
#cont("cvh","factor(race)")
ls7<-c("cvh","hp","diab","chole","smoke","bmi","pa","diet")
out<-NULL
for (i in 1:8){
  row<-cbind(ls7[i],cont(ls7[i],"factor(race)"))
  out<-rbind(out,row)
}
out
write.csv(out,file="racialdis.csv",row.names = F)

#CVH score
cont3<-function(out,pred){
  f1<-paste0(out,"~",pred)
  lm1<-svyglm(formula = f1, design=design1, family=quasi(link = "identity"))
  est1<-sprintf("%.2f",coef(lm1)[2:4])
  ci1<-sprintf("%.2f",confint(lm1)[2:4,])
  m1<-rbind(paste0(est1[1]," (",ci1[1],", ",ci1[4],")"),paste0(est1[2]," (",ci1[2],", ",ci1[5],")"),paste0(est1[3]," (",ci1[3],", ",ci1[6],")"))
  
  f2<-paste0(out,"~",pred,"+factor(ageg)")
  lm2<-svyglm(formula = f2, design=design1, family=quasi(link = "identity"))
  est2<-sprintf("%.2f",coef(lm2)[2:4])
  ci2<-sprintf("%.2f",confint(lm2)[2:4,])
  m2<-rbind(paste0(est2[1]," (",ci2[1],", ",ci2[4],")"),paste0(est2[2]," (",ci2[2],", ",ci2[5],")"),paste0(est2[3]," (",ci2[3],", ",ci2[6],")"))
  
  f3<-paste0(out,"~",pred,"+factor(ageg)+factor(edu)+factor(income)+factor(marry)+factor(pregnancy)+factor(cvd)+factor(year)+factor(care)")
  lm3<-svyglm(formula = f3, design=design1, family=quasi(link = "identity"))
  est3<-sprintf("%.2f",coef(lm3)[2:4])
  ci3<-sprintf("%.2f",confint(lm3)[2:4,])
  m3<-rbind(paste0(est3[1]," (",ci3[1],", ",ci3[4],")"),paste0(est3[2]," (",ci3[2],", ",ci3[5],")"),paste0(est3[3]," (",ci3[3],", ",ci3[6],")"))
  
  beta<-cbind(unadj=m1,age_adj=m2,fully_adj=m3)
  return(beta)
}
cont3("cvhscore","factor(race)")
write.csv(cont3("cvhscore","factor(race)"),file="racialdis2.csv",row.names = F)

###Supplemental table 3###
#Overall
#poor cvh
lm1<-svyglm(cvh~factor(ageg)*factor(race), design=design1, family=quasibinomial(link="logit"))
pred1<-predict(lm1,dat,type="response")
pred1<-as.data.frame(pred1)
dat<-cbind(dat,adjcvh=pred1[,1])
#cvh score
lm2<-svyglm(cvhscore~factor(ageg)*factor(race), design=design1, family=quasi(link = "identity"))
pred2<-predict(lm2,dat)
pred2<-as.data.frame(pred2)
dat<-cbind(dat,adjscore=pred2[,1])
designs1<-svydesign(id = ~ psu, strata = ~ state, nest = TRUE, weight = ~ weight, data = dat)

nstate<-table(dat$state,useNA = "ifany")

#overall ideal cvh prevalence by state
test<-svyby(~adjcvh,~state,designs1,svymean)
ci<-confint(svyby(~adjcvh,~state,designs1,svymean))

out1<-NULL
for (i in 1:51){
  row1<-prettyNum(nstate[i],big.mark = ",")
  row2<-paste0(sprintf("%.2f",test$adjcvh[i]*100)," (",sprintf("%.2f",ci[i,1]*100),", ",sprintf("%.2f",ci[i,2]*100),")")
  cell<-rbind(row1,row2)
  out1<-rbind(out1,cell)
}
#overall average cvh score by state
test2<-svyby(~adjscore,~state,designs1,svymean)
ci2<-confint(svyby(~adjscore,~state,designs1,svymean))

out2<-NULL
for (i in 1:51){
  row1<-sprintf("%.2f",test2$adjscore[i])
  row2<-paste0(sprintf("%.2f",ci2[i,1]),", ",sprintf("%.2f",ci2[i,2]))
  cell<-rbind(row1,row2)
  out2<-rbind(out2,cell)
}

#Yearly
#ideal cvh by state and year 
test3<-svyby(~adjcvh,~state+year,designs1,svymean)
row235<-cbind(state=34,year=2019,adjcvh=0,se=0) # new jersey blank data
test3<-rbind(test3[1:234,],row235,test3[235:254,]) 
ci3<-confint(svyby(~adjcvh,~state+year,designs1,svymean))
row235_2<-cbind(0,0)# new jersey blank data
ci3<-rbind(ci3[1:234,],row235_2,ci3[235:254,]) 

t<-table(dat$state,dat$year)
t<-as.vector(t)

out3<-NULL
for (i in 1:255){
  row1<-prettyNum(t[i],big.mark = ",")
  row2<-paste0(sprintf("%.2f",test3$adjcvh[i]*100)," (",sprintf("%.2f",ci3[i,1]*100),", ",sprintf("%.2f",ci3[i,2]*100),")")
  cell<-rbind(row1,row2)
  out3<-rbind(out3,cell)
}
out3<-cbind(out3[1:102,],out3[103:204,],out3[205:306,],out3[307:408,],out3[409:510,])

#cvh score by state and year
test4<-svyby(~adjscore,~state+year,designs1,svymean)
row235<-cbind(state=34,year=2019,adjscore=0,se=0) # new jersey blank data
test4<-rbind(test4[1:234,],row235,test4[235:254,])
ci4<-confint(svyby(~adjscore,~state+year,designs1,svymean))
row235_2<-cbind(0,0)# new jersey blank data
ci4<-rbind(ci4[1:234,],row235_2,ci4[235:254,]) 

out4<-NULL
for (i in 1:255){
  row1<-sprintf("%.2f",test4$adjscore[i])
  row2<-paste0(sprintf("%.2f",ci4[i,1]),", ",sprintf("%.2f",ci4[i,2]))
  cell<-rbind(row1,row2)
  out4<-rbind(out4,cell)
}
out4<-cbind(out4[1:102,],out4[103:204,],out4[205:306,],out4[307:408,],out4[409:510,])

supp1<-cbind(out3,out1,out4,out2)
head(supp1)

write.csv(supp1,file="outputs/supp1.csv",row.names = F)

#test variations
for (i in 1:51){
  print(state[i])
  tmpdat<-dat[dat$state %in% state[i],]
  tmpdesign<-svydesign(id = ~ psu, strata = ~ state, nest = TRUE, weight = ~ weight, data = tmpdat)
  res<-svyCreateTableOne(vars = "adjcvh",strata = "year",tmpdesign)
  print(res$ContTable)
}

for (i in 1:51){
  print(state[i])
  tmpdat<-dat[dat$state %in% state[i],]
  tmpdesign<-svydesign(id = ~ psu, strata = ~ state, nest = TRUE, weight = ~ weight, data = tmpdat)
  res<-svyCreateTableOne(vars = "adjscore",strata = "year",tmpdesign)
  print(res$ContTable)
}

#Table S4#
out2<-NULL
for (i in 1:51){
  row<-paste0(sprintf("%.2f",test2$adjscore[i])," (",sprintf("%.2f",ci2[i,1]),", ",sprintf("%.2f",ci2[i,2]),")")
  out2<-rbind(out2,row)
}
out4<-NULL
for (i in 1:255){
  row<-paste0(sprintf("%.2f",test4$adjscore[i])," (",sprintf("%.2f",ci4[i,1]),", ",sprintf("%.2f",ci4[i,2]),")")
  out4<-rbind(out4,row)
}
out4<-cbind(out4[1:51,],out4[52:102,],out4[103:153,],out4[154:204,],out4[205:255,])
write.csv(cbind(out4,out2),file="outputs/supp4.csv",row.names = F)

#Table S5#
vars<-c("hp","diab","chole","smoke","bmi","pa","diet")
outs5<-NULL
for (i in 1:7){
  f<-paste0(vars[i],"~factor(ageg)*factor(race)")
  lm<-svyglm(f, design=design1, family=quasibinomial(link="logit"))
  pred<-predict(lm,dat,type="response")
  pred<-as.data.frame(pred)
  dat1<-cbind(dat,adj=pred[,1])
  designs1<-svydesign(id = ~ psu, strata = ~ state, nest = TRUE, weight = ~ weight, data = dat1)
  test<-svyby(~adj,~state,designs1,svymean)
  ci<-confint(svyby(~adj,~state,designs1,svymean))
  
  outs<-NULL
  for (j in 1:51){
    row<-paste0(sprintf("%.2f",test$adj[j]*100)," (",sprintf("%.2f",ci[j,1]*100),", ",sprintf("%.2f",ci[j,2]*100),")")
    outs<-rbind(outs,row)
  }
  outs5<-cbind(outs5,outs)
}
write.csv(outs5,file="outputs/tableS5.csv",row.names = F)

######## 3. Individual CVH metrics prevalence (Supplemental Table 4) ##########
vars<-c("hp","diab","chole","smoke","bmi","pa","diet")
for (i in 1:7){
  f<-paste0(vars[i],"~factor(ageg)*factor(race)")
  lm<-svyglm(f, design=design1, family=quasibinomial(link="logit"))
  pred<-predict(lm,dat,type="response")
  pred<-as.data.frame(pred)
  dat1<-cbind(dat,adj=pred[,1])
  designs1<-svydesign(id = ~ psu, strata = ~ state, nest = TRUE, weight = ~ weight, data = dat1)
  test<-svyCreateTableOne(vars = "adj",strata = "state",data = designs1)
  
  outs<-NULL
  for (j in 1:51){
    pop<-round(test[[1]][[j]][1],0)
    rate<-test[[1]][[j]][4]*100 
    cas<-round(pop*rate/100,0)
    row<-cbind(state=unique(dat$state)[j],pop,rate,cas)
    outs<-rbind(outs,row)
  }
  outs<-as.data.frame(outs)
  q<-quantile(outs$rate,probs = seq(0,1,0.2))
  print(q)
  outs$level<-cut(outs$rate, breaks = c(0,q[2],q[3],q[4],q[5],100),labels = c("5","4","3","2","1"))
  
  filename<-paste0("satscan/",vars[i],".csv")
  write.csv(outs,file=filename,row.names = F)
  assign(vars[i],outs)
}

quantile(hp$rate,probs = seq(0,1,0.2)) 
quantile(diab$rate,probs = seq(0,1,0.2)) 
quantile(chole$rate,probs = seq(0,1,0.2)) 
quantile(smoke$rate,probs = seq(0,1,0.2)) 
quantile(obesity$rate,probs = seq(0,1,0.2)) 
quantile(pa$rate,probs = seq(0,1,0.2)) 
quantile(diet$rate,probs = seq(0,1,0.2)) 




################################
# Disease clustering (SatScan) #
################################
# 1. Overall
#ideal cvh
lm1<-svyglm(cvh~factor(ageg)*factor(race), design=design1, family=quasibinomial(link="logit"))
pred1<-predict(lm1,dat,type="response")
pred1<-as.data.frame(pred1)
dat1<-cbind(dat,adjcvh=pred1[,1])
designs1<-svydesign(id = ~ psu, strata = ~ state, nest = TRUE, weight = ~ weight, data = dat1)
test1<-svyCreateContTable(vars = "adjcvh",strata = "state",data = designs1)

#cvh score
lm2<-svyglm(cvhscore~factor(ageg)*factor(race), design=design1, family=quasi(link = "identity"))
pred2<-predict(lm2,dat)
pred2<-as.data.frame(pred2)
dat2<-cbind(dat1,adjscore=pred2[,1])
designs2<-svydesign(id = ~ psu, strata = ~ state, nest = TRUE, weight = ~ weight, data = dat2)
test2<-svyCreateContTable(vars = "adjscore",strata = "state",data = designs2)

t<-table(dat$state,useNA = "ifany")
predicted_overall<-NULL
for (i in 1:51){
  pop<-round(test1[[i]][1],0)
  rate<-test1[[i]][4]*100 
  cas<-round(pop*rate/100,0)
  avg_score<-test2[[i]][4]
  n<-t[i]
  row<-cbind(state=unique(dat$state)[i],pop,rate,cas,avg_score,n)
  predicted_overall<-rbind(predicted_overall,row)
}
predicted_overall<-as.data.frame(predicted_overall)

# 2. Stratified by data cycle
#ideal cvh
test3<-svyby(~adjcvh,~state+year,designs1,svymean)
row235<-cbind(state=34,year=2019,adjcvh=0,se=0)
test3<-rbind(test3[1:234,],row235,test3[235:255,]) # new jersey blank data
t<-table(dat$state,dat$year)
t<-as.vector(t)

out3<-cbind(test3[1:255,],pop=t*100000,n=t)
out3$cas<-round(out3$pop*out3$adjcvh/100,0)
out3$rate<-out3$adjcvh*100
out3<-out3[,c("state","year","pop","rate","cas","n")]

#cvh score
test4<-svyby(~adjscore,~state+year,designs2,svymean)
row235<-cbind(state=34,year=2019,adjscore=0,se=0)
test4<-rbind(test4[1:234,],row235,test4[235:255,]) # new jersey blank data

predicted_year<-cbind(out3,avg_score=test4[1:255,"adjscore"])
predicted_year<-as.data.frame(predicted_year)

#Categorize the rate and score into quintiles
quantile(c(predicted_overall$rate,predicted_year$rate),probs = seq(0,1,0.2)) 
quantile(c(predicted_overall$avg_score,predicted_year$avg_score),probs = seq(0,1,0.2)) 

predicted_overall$level1<-cut(predicted_overall$rate, breaks = c(0, 4.66, 4.91, 5.17, 5.37, 10),labels = c("5","4","3","2","1"))
predicted_overall$level2<-cut(predicted_overall$avg_score, breaks = c(0, 4.50, 4.53, 4.57, 4.59, 10),labels = c("5","4","3","2","1"))

predicted_year$level1<-cut(predicted_year$rate, breaks = c(0, 4.66, 4.91, 5.17, 5.37, 10),labels = c("5","4","3","2","1"))
predicted_year$level2<-cut(predicted_year$avg_score, breaks = c(0, 4.50, 4.53, 4.57, 4.59, 10),labels = c("5","4","3","2","1"))

write.csv(predicted_year,file="satscan/predicted_year.csv",row.names = F)
write.csv(predicted_overall,file="satscan/predicted_overall.csv",row.names = F)

#changes
dat1<-read.csv("satscan/predicted_year.csv",header = T, stringsAsFactors = F)
dat2<-read.csv("satscan/predicted_overall.csv",header = T, stringsAsFactors = F)

dats<-cbind(dat1[dat1$year %in% 2011,c("state","rate","avg_score")],dat1[dat1$year %in% 2013,c("rate","avg_score")],dat1[dat1$year %in% 2015,c("rate","avg_score")],dat1[dat1$year %in% 2017,c("rate","avg_score")],dat1[dat1$year %in% 2019,c("rate","avg_score")])
colnames(dats)<-c("state","r1","s1","r2","s2","r3","s3","r4","s4","r5","s5")

dats$change<-NA
dats$change[(dats$r1 >= dats$r2) & (dats$r2 >= dats$r3) & (dats$r3 >= dats$r4) & (dats$r4 >= dats$r5)]<-1 #decreasing
dats$change[(dats$r1 <= dats$r2) & (dats$r2 <= dats$r3) & (dats$r3 <= dats$r4) & (dats$r4 <= dats$r5)]<-2 #increasing
table(dats$change, useNA = "ifany")

dats$change2<-NA
dats$change2[(dats$s1 >= dats$s2) & (dats$s2 >= dats$s3) & (dats$s3 >= dats$s4) & (dats$s4 >= dats$s5)]<-1 #decreasing
dats$change2[(dats$s1 <= dats$s2) & (dats$s2 <= dats$s3) & (dats$s3 <= dats$s4) & (dats$s4 >= dats$s5)]<-2 #increasing
table(dats$change2, useNA = "ifany")
dats$state[dats$change2 %in% 2]

dat1$rate<-round(dat1$rate,2)
dat1$avg_score<-round(dat1$avg_score,2)
dat2$rate<-round(dat2$rate,2)
dat2$avg_score<-round(dat2$avg_score,2)