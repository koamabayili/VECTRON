require(dplyr)
require(questionr)
getwd()
rp<-"C:/Users/HP/Desktop/Eagles of death metal/Planet/All_Studies/Test_insecticide/2016_04/B3"
setwd(rp)
readLines("mespar.txt",n=5)
mp<-read.table("mespar.txt",header=T,dec=",",sep="\t")
View(mp)
### Divisons la variable numcone en replique et numero de cone##
mp$numcone<- as.character(mp$numcone)
mp$Replicate<- substring(mp$numcone,1,1)
table(mp$Replicate)
mp$Block_Number<-substring(mp$numcone,3,3)
table(mp$Block_Number)
############################################
dput(names(mp))
## nom des variables actuelles ###
old.v<-c("id", "Mosquitoes_species_strain","Time.after_Spray","Type_hut","ttrait", "numcone", "kd60min", "mort24h", "mort48h", "mort72h", "tested", "Replicate", "Block_Number")
new.v<- c("Test.date_Project.code", "Mosquitoes_species_strain","Time.after_Spray","Type_hut","Treatments","numcone", "kd60min","Total_death_24h","Total_death_48h", "Total_death_72h","Number_tested","Replicate", "Block_Number")
### rename.variable ### 
mp<-rename.variable(mp,old.v,new.v)
#recodage de la variable traitement #
mp$Replicate <- as.factor(mp$Replicate)
levels(mp$Replicate)<-c("R.1","R.2")
mp$Treatments<-as.factor(mp$Treatments)
levels(mp$Treatments)<-c("Control","B3-50mg/m²","B3-100mg/m²","B3-200mg/m²")
#######
write.table(mp,
            "Phase_1_raw_data_OMS.txt",
            dec = ".",
            sep="\t",
            row.names = F)


# les operation  sur la base de donnee #
mp <- transform(mp,T.kd60min=kd60min)
mp <- transform(mp, per.kd60min=(T.kd60min/Number_tested)*100)

mp <- transform(mp,T.death24=Total_death_24h)
mp <- transform(mp, per.death.24h=(T.death24/Number_tested)*100)

mp <- transform(mp,T.death48=Total_death_24h+Total_death_48h)
mp <- transform(mp, per.death.48h=(T.death48/Number_tested)*100)

mp <- transform(mp,T.death72=Total_death_24h+Total_death_48h+Total_death_72h)
mp <- transform(mp, per.death.72h=(T.death72/Number_tested)*100)
#################################################
mp<-select(mp,
           Test.date_Project.code,
           Mosquitoes_species_strain,
           Type_hut,Treatments,
           Time.after_Spray,
           Number_tested,
           T.kd60min,
           per.kd60min,
           T.death24,
           per.death.24h,
           T.death48,
           per.death.48h,
           T.death72,
           per.death.72h)
##############################################
View(mp)
dim(mp)
str(mp)
#################################################
mpgb <- group_by(mp,
                 Test.date_Project.code,
                 Mosquitoes_species_strain,
                 Time.after_Spray,
                 Type_hut,
                 Treatments)
#################################################
res1 <- summarise(mpgb,
                  N = n(),
                  nb.expo = sum(Number_tested),
                  nb.kd = sum(T.kd60min),
                  nb.dead24 = sum(T.death24),
                  nb.dead48 = sum(T.death48),
                  nb.dead72 = sum(T.death72),
                  kd_rate= round((nb.kd/nb.expo)*100,2),
                  per.mort24= round((nb.dead24/nb.expo)*100,2),
                  per.mort48= round((nb.dead48/nb.expo)*100,2),
                  per.mort72= round((nb.dead72/nb.expo)*100,2),
                  se_kd = sd(per.kd60min)/sqrt(N),
                  se_24 = sd(per.death.24h)/sqrt(N),
                  se_48 = sd(per.death.48h)/sqrt(N),
                  se_72 = sd(per.death.72h)/sqrt(N))
            
View(res1)
##################################################
write.table(res1,"Resume_OMS.txt",dec = ",",sep="\t",row.names = F)    
############ installation des packages ####
pk<-c("binom", "boot", "car","confint", "Design", "Epi", "epicalc", "gplots", "Hmisc","lme4", "ltm", "MASS", "mice", "mgcv", "nnet", "plotrix","prettyR", "psy", "psych", "relaimpo", "RODBC", "survey","sem")
install.packages(pk)  
##### Fin ######      