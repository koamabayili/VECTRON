require(dplyr)
require(binom)
require(questionr)
getwd()
rp<-"C:/Users/hp/Desktop/Planet/assay/2016_04/Evaluation"
setwd(rp)
readLines("eval123.txt",n=5)
evl<-read.table("eval123.txt",header=T,dec=",",sep="\t")
View(evl)
dim(evl)
dput(colnames(evl))
##################################################
old.v<-c("id", "nbe","Num.Periode","numcase", "traitdos", "typmur", "iddor", "iddor1",
"gresp", "aesp", "spcomp", "gvmp", "gmimp", "ajvmp", "ajmimp",
"gm24hmp", "gv24hmp", "ajm24hmp", "ajv24hmp", "gm48hmp", "gv48hmp",
"ajm48hmp", "ajv48hmp", "gm72hmp", "gv72hmp", "ajm72hmp", "ajv72hmp")

new.v<-c("Id_recording", "Nb_Species","Round","Num_Hut", "Treatments", "type_Wall",
"Id_sleeper", "Other_sleeper","Species", "Other_species", "Section",
"Blood_fed_live", "Blood_fed_dead", "Unfed_live", "Unfed_dead",
"Blood_fed_dead_24h", "Blood_fed_live_24h", "Unfed_dead_24h", "Unfed_live_24h",
"Blood_fed_dead_48h", "Blood_fed_live_48h", "Unfed_dead_48h", "Unfed_live_48h",
"Blood_fed_dead_72h", "Blood_fed_live_72h", "Unfed_dead_72h", "Unfed_live_72h")
###############################################################################
evl<-rename.variable(evl,old.v,new.v)
View(evl)
###################################################
"Id_recording"
"Round"
"Nb_Species"
"Num_Hut"
"Treatments"
"type_Wall"
"Id_sleeper"
"Other_sleeper"
"Species"
"Other_species"
"Section"
"Blood_fed_live"
"Blood_fed_dead"
"Unfed_live"
"Unfed_dead"
"Blood_fed_dead_24h"
"Blood_fed_live_24h"
"Unfed_dead_24h"
"Unfed_live_24h"
"Blood_fed_dead_48h"
"Blood_fed_live_48h"
"Unfed_dead_48h"
"Unfed_live_48h"
"Blood_fed_dead_72h"
"Blood_fed_live_72h"
"Unfed_dead_72h"
"Unfed_live_72h"
####################################################
evl$Treatments<-factor(evl$Treatments,levels=c(1,2,3,4,5),
labels=c("Control","MAI-B2-100mg/m²","MAI-B3-100mg/m²","MAI-B3-150mg/m²",
"Actellic-1g/m²"))

evl$type_Wall<-factor(evl$type_Wall,levels=c(1,2),
labels=c("Concrete","Mud"))

evl$Species<-factor(evl$Species,levels=c(1,2),
labels=c("An.gambiae s.l","Other"))

evl$Section<-factor(evl$Section,levels=c(2,3,4),
labels=c("Ceiling","Hut","Veranda"))

evl$Num_Hut<-factor(evl$Num_Hut,levels=c(1,2,3,4,7,9,10,11),labels=c("Hut-01","Hut-02","Hut-03","Hut-04","Hut-07","Hut-09","Hut-10","Hut-11"))
###################################################
View(evl)
############   Sous_Base  An.gambiae s.l ##########
sous.evl<-subset(evl,Species=="An.gambiae s.l")
View(sous.evl)
##################################################
sous.evl<-transform(sous.evl,Tested=(Blood_fed_live+Blood_fed_dead+Unfed_live+Unfed_dead))
sous.evl<-transform(sous.evl,Dead=(Blood_fed_dead+Unfed_dead+Blood_fed_dead_24h+Unfed_dead_24h+Blood_fed_dead_48h+Unfed_dead_48h+Blood_fed_dead_72h+Unfed_dead_72h))

sous.evl<-transform(sous.evl,M_rate=Dead/Tested*100)
########### La base sous evl sans zero teste #####
sous.evl1<-subset(sous.evl,Tested != 0 & Dead != 0)
View(sous.evl1)
sous.evl1.sel<-select(sous.evl1,Round,type_Wall,Treatments,Tested,Dead,M_rate)
evl_gp<-group_by(sous.evl1.sel,Round,type_Wall,Treatments)

res15 <- summarise(evl_gp,
            Nb.Tested = sum(Tested),
            Nb.Dead = sum(Dead),
          Mortality_rate_72h= round((Nb.Dead/Nb.Tested)*100,2),
            SD = sd(M_rate),
            N = n(),
            SE=(SD/sqrt(N)))
View(res15)

##################################################

veranda<-subset(sous.evl,Section=="Veranda")
veranda<-transform(veranda,Exi.veranda= Blood_fed_live+Blood_fed_dead+Unfed_live+Unfed_dead)
View(veranda)
###################################################
s.evl.sel<-select(sous.evl,Round,Id_recording,Treatments,type_Wall,
Num_Hut,Section,Blood_fed_live,Blood_fed_dead,Unfed_live,
Unfed_dead,Blood_fed_dead_24h,Blood_fed_live_24h,
Unfed_dead_24h,Unfed_live_24h,Blood_fed_dead_48h,
Blood_fed_live_48h,Unfed_dead_48h,Unfed_live_48h,
Blood_fed_dead_72h,Blood_fed_live_72h,Unfed_dead_72h,Unfed_live_72h)
##############summary per days ########
s.evl.gp<-group_by(s.evl.sel,Round,Treatments,type_Wall,Num_Hut)

res1<-summarise(s.evl.gp,
N=n(),
T.bld.fed=(sum(Blood_fed_live)+sum(Blood_fed_dead)),
T.Unfed.Caught=(sum(Unfed_live)+sum(Unfed_dead)),
T.bld.fed.dead=(sum(Blood_fed_dead)+sum(Blood_fed_dead_24h)+sum(Blood_fed_dead_48h)+sum(Blood_fed_dead_72h)),
T.Unfed.dead=(sum(Unfed_dead)+sum(Unfed_dead_24h)+sum(Unfed_dead_48h)+sum(Unfed_dead_72h)),
T.Caught=(sum(Blood_fed_live)+sum(Blood_fed_dead)+sum(Unfed_live)+sum(Unfed_dead)),
T.dead=(sum(Blood_fed_dead)+sum(Unfed_dead)+sum(Blood_fed_dead_24h)+sum(Unfed_dead_24h)+sum(Blood_fed_dead_48h)+sum(Unfed_dead_48h)+sum(Blood_fed_dead_72h)+sum(Unfed_dead_72h)))
View(res1)
######### Exiting per round #####
ver_gp<-group_by(veranda,Round,Treatments,type_Wall,Num_Hut)

res4<- summarise(ver_gp,
              T.exiting=sum(Exi.veranda))

View(res4)

res9<-merge(res1,res4,by=c("Round","Treatments","type_Wall","Num_Hut"))
View(res9)

######################################################
res1$Exophily<-(veranda$Exi.veranda/res1$T.Caught)*100

s.evl.gp1<-group_by(res1,Round,Treatments,type_Wall,Num_Hut)

res2<-summarise(s.evl.gp1,
N=n(),
Global.Mortality.rate=(T.dead/T.Caught)*100,
Unfed.dead.rate=(T.Unfed.dead/T.Caught)*100,
Blood.dead.rate=(T.bld.fed.dead/T.Caught)*100)
View(res2)
###############  Global Summary ##################
s.evl.gp<-group_by(s.evl.sel,Id_recording,Treatments,type_Wall,Num_Hut)

res15<-summarise(s.evl.gp,
N=n(),
T.bld.fed=(sum(Blood_fed_live)+sum(Blood_fed_dead)),
T.Unfed.Caught=(sum(Unfed_live)+sum(Unfed_dead)),
T.bld.fed.dead=(sum(Blood_fed_dead)+sum(Blood_fed_dead_24h)+sum(Blood_fed_dead_48h)+sum(Blood_fed_dead_72h)),
T.Unfed.dead=(sum(Unfed_dead)+sum(Unfed_dead_24h)+sum(Unfed_dead_48h)+sum(Unfed_dead_72h)),
T.Caught=(sum(Blood_fed_live)+sum(Blood_fed_dead)+sum(Unfed_live)+sum(Unfed_dead)),
T.dead=(sum(Blood_fed_dead)+sum(Unfed_dead)+sum(Blood_fed_dead_24h)+sum(Unfed_dead_24h)+sum(Blood_fed_dead_48h)+sum(Unfed_dead_48h)+sum(Blood_fed_dead_72h)+sum(Unfed_dead_72h)))
View(res15)
###################################################
res15<-subset(res15,T.Caught != 0 & T.dead != 0)
View(res15)
res15<-transform(res15,Mortality=T.dead/T.Caught*100)
######## Standard error ###########################
se.gp<-group_by(res15,type_Wall,Num_Hut,Treatments)
res0<-summarise(se.gp,
           N=n(),
           sdev=sd(Mortality),
           se=(sdev/sqrt(N)))
View(res0)
##################################################

View(s.evl.sel)
s.evl.gp<-group_by(s.evl.sel,Treatments,type_Wall,Num_Hut)

res3<-summarise(s.evl.gp,
N=n(),
T.bld.fed=(sum(Blood_fed_live)+sum(Blood_fed_dead)),

T.Unfed.Caught=(sum(Unfed_live)+sum(Unfed_dead)),
T.bld.fed.dead=(sum(Blood_fed_dead)+sum(Blood_fed_dead_24h)+sum(Blood_fed_dead_48h)+sum(Blood_fed_dead_72h)),
T.Unfed.dead=(sum(Unfed_dead)+sum(Unfed_dead_24h)+sum(Unfed_dead_48h)+sum(Unfed_dead_72h)),
T.Caught=(sum(Blood_fed_live)+sum(Blood_fed_dead)+sum(Unfed_live)+sum(Unfed_dead)),
T.dead=(sum(Blood_fed_dead)+sum(Unfed_dead)+sum(Blood_fed_dead_24h)+sum(Unfed_dead_24h)+sum(Blood_fed_dead_48h)+sum(Unfed_dead_48h)+sum(Blood_fed_dead_72h)+sum(Unfed_dead_72h)))
View(res3)
####################################################################### Exiting ####################
ver_gp<-group_by(veranda,Round,Treatments,type_Wall,Num_Hut)

res5<- summarise(ver_gp,
              T.exiting=sum(Exi.veranda))

View(res5)
############ Merge les deux bases ###########

res6<-merge(res3,res5,by=c("Treatments","type_Wall","Num_Hut"))
View(res6)
##################################################
se<-read.table("ertype.txt",header=T,dec=",",sep="\t")
View(se)




##################################################


s.evl.gp1<-group_by(res6,Treatments,type_Wall,Num_Hut)

res7<-summarise(s.evl.gp1,
N=n(),
Global.Mortality.rate=(T.dead/T.Caught)*100,
Unfed.dead.rate=(T.Unfed.dead/T.Caught)*100,
Blood.dead.rate=(T.bld.fed.dead/T.Caught)*100,
Exiting.rate=(T.exiting/T.Caught)*100)

View(res7)
##################################################
write.table(evl,"Final.data.txt",row.names=F,sep="\t",dec=",")
write.table(sous.evl,"An.gamb.data.txt",row.names=F,sep="\t",dec=",")
write.table(res1,"res1.txt",row.names=F,sep="\t",dec=",")
write.table(veranda,"veranda.txt",row.names=F,sep="\t",dec=",")
write.table(res6,"global.sum_R123.txt",row.names=F,sep="\t")
write.table(res7,"global.mort_R123.txt",row.names=F,sep="\t")
write.table(res0,"SE_evaluation.txt",dec=",",row.names=F,sep="\t")
write.table(res9,"Exiting.txt",dec=",",row.names=F,sep="\t")
#################################################