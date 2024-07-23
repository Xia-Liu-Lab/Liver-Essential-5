#reclassify and association of Liver Essential 5 with MAFLD
SCC<-read.csv() #load SCC data

#(1) reclassify
#unfavorable lifestyle pattern
least<-subset(SCC,lifepattern==0)
a<-table(least$total_pattern,least$MAFLD) #total pattern means categorical variable evualauted by Liver Essential 5, which incoporating sleep quality
prop.table(a,1)
#moderate lifestyle pattern
moderate<-subset(SCC,lifepattern==1)
table(moderate$total_pattern,moderate$MAFLD)
chisq.test(moderate$total_pattern,moderate$MAFLD)
#favorable lifestyle pattern
most<-subset(SCC,lifepattern==2)
table(most$total_pattern,most$MAFLD)
chisq.test(most$total_pattern,most$MAFLD)

#(2) Association with MAFLD
summary(glm(MAFLD~age+gender+education+marriage+income+total_pattern,family=binomial,data=SCC))
summary(glm(MAFLD~age+gender+education+marriage+income+factor(total_pattern),family=binomial,data=SCC))
summary(glm(MAFLD~age+gender+education+marriage+income+factor(total_score),family=binomial,data=SCC))
summary(glm(MAFLD~age+gender+education+marriage+income+total_score,family=binomial,data=SCC))