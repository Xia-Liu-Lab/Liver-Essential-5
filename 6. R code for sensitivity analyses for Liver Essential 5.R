# Sensitivity analyses for Liver Essential 5
SCC<-read.csv() #load SCC data

#(1) Weighted Liver Essential 5
weight<-glm(MAFLD~age+gender+education+marriage+income+HL_smoking+HL_moderate+HL_diet+HL_PA+HL_sleep,family=binomial,data=SCC)
a<-summary(weight)

SCC$weighted_score<-(SCC$HL_smoking*abs(a$coefficients[7,1])+
                       SCC$HL_moderate*abs(a$coefficients[8,1])+
                       SCC$HL_PA*abs(a$coefficients[10,1])+
                       SCC$HL_diet*abs(a$coefficients[9,1])+
                       SCC$HL_sleep*abs(a$coefficients[11,1]))/
  (abs(a$coefficients[7,1])+abs(a$coefficients[8,1])+abs(a$coefficients[9,1])+abs(a$coefficients[10,1])+abs(a$coefficients[11,1]))*5

SCC$weighted_group<-NA
SCC<-within(SCC,{
  weighted_group[weighted_score<1]<-0
  weighted_group[weighted_score<2 & weighted_score >=1]<-1
  weighted_group[weighted_score<3 & weighted_score >=2]<-2
  weighted_group[weighted_score<4 & weighted_score >=3]<-3
  weighted_group[weighted_score >=4]<-4
})

summary(glm(MAFLD~age+gender+education+marriage+income+weighted_group,family=binomial(),data=SCC))
summary(glm(MAFLD~age+gender+education+marriage+income+weighted_score,family=binomial(),data=SCC))
summary(glm(MAFLD~age+gender+education+marriage+income+factor(weighted_group),family=binomial(),data=SCC))

#(2) Continuous Liver Essential 5
SCC$bscore<-(SCC$b_smoking+SCC$b_drinking+SCC$b_PA+SCC$b_diet+SCC$b_sleep)/5 #the score of each lifestyle factor and sleep quality was ranged from 0 to 100
SCC$bscore_per<-SCC$bscore/10
SCC$bscore_group<-NA

SCC<-within(SCC,{
  bscore_group[bscore<60]<-1
  bscore_group[bscore>=60 & bscore <75]<-2
  bscore_group[bscore>=75 & bscore <100]<-3
})

summary(glm(MAFLD~age+gender+education+marriage+income+bscore_group,family=binomial,data=SCC))
summary(glm(MAFLD~age+gender+education+marriage+income+factor(bscore_group),family=binomial,data=SCC))
summary(glm(MAFLD~age+gender+education+marriage+income+bscore_per,family=binomial,data=SCC))
