require(dplyr)
require(questionr)
getwd()
repertoire <-"C:/Users/hp/Desktop/Planet/assay/2016_04/cone in situ/Ceiling+Hut"
setwd(repertoire)
readLines("insitu3.txt",n=5)
colu_field <- read.table("insitu0.txt",header=T,dec=".",sep="\t")
View(colu_field)
################################################
colu_field$id<-as.character(colu_field$id)
colu_field$Huts_number<-substring(colu_field$id,18,23)
colu_field$Type_Hut<-substring(colu_field$id,25,32)
####################################################
dim(colu_field)

dput(colnames(colu_field))
####################################################
"id"
"ttrait"
"Time_after_spray"
"Time_exposure"
"numside"
"kd60min"
"mort24h"
"mort48h"
"mort72h"
"Tested"
"Huts_number"
"Type_Hut"
###################################################

old.v<-c("id", "Mosquitoes_species_strain","ttrait", "Time_after_spray", "Time_exposure", "numside","kd60min", "mort24h", "mort48h", "mort72h", "Tested", "Huts_number","Type_Hut")

new.v<-c("Id_recording", "Mosquitoes_species_strain","Treatments", "Time_after_spray", "Time_exposure", "Num_side","kd60min", "Mortality_24h","Mortality_48h", "Mortality_72h", "Tested", "Huts_number","Type_Hut")

colu_field<-rename.variable(colu_field,old.v,new.v)
####################################################
colu_field <- transform(colu_field,Total.dead72= Mortality_24h+Mortality_48h+ Mortality_72h)
colu_field <- transform(colu_field,Total.dead24h= Mortality_24h)
#################################################
dim(colu_field)
colu_field<-transform(colu_field,mort.rate.72h=(Total.dead72/Tested)*100)
colu_field<-transform(colu_field,mort.rate.24h=(Total.dead24h/Tested)*100)
############### Base separer #######

Ceil.bd<-subset(colu_field,Num_side ==c(8,9))
rest.bd<-subset(colu_field,Num_side !=c(8,9))
####################################################
Ceil.bd$Treatments<-factor(Ceil.bd$Treatments,levels=c(1,2,3,4,5),
labels=c("Control","Actellic_1000mg/m²","MAI7316_B2_100mg/m²","MAI7316_B3_100mg/m²","MAI7316_B3_150mg/m²"))

rest.bd$Treatments<-factor(rest.bd$Treatments,levels=c(1,2,3,4,5),
labels=c("Control",
         "Actellic_1000mg/m²",
         "MAI7316_B2_100mg/m²",
         "MAI7316_B3_100mg/m²",
         "MAI7316_B3_150mg/m²"))

########### recodage de la base  ###########
Ceil.bd$Num_side<-factor(Ceil.bd$Num_side,levels=c(8,9),
labels=c("Ceiling 1","Ceiling 2"))

rest.bd$Num_side<-factor(rest.bd$Num_side,
                         levels=c(0,1,2,3,4,5,6,7),
labels=c("Length1-1","Length1-2","Width 1-1","Width 1-2","Length2-1","Length2-2",
"Width 2-1","Width 2-2"))
#############################################

Ceil.bd.sel <- select(Ceil.bd,Time_after_spray,Huts_number,Mosquitoes_species_strain,Type_Hut,Treatments,Tested,Total.dead24h,mort.rate.24h,Total.dead72,mort.rate.72h)
dim(Ceil.bd.sel)

rest.bd.sel <- select(rest.bd,Time_after_spray,Huts_number,Mosquitoes_species_strain,Type_Hut,Treatments,Tested,Total.dead24h,mort.rate.24h,Total.dead72,mort.rate.72h)
dim(rest.bd.sel)

#### Ceilling data ######
Ceil.bd_gb <- group_by(Ceil.bd.sel,Time_after_spray,Mosquitoes_species_strain,Type_Hut,Treatments)

res1 <- summarise(Ceil.bd_gb,
            N = n(),
            Nb.Tested = sum(Tested),
            Dead24 = sum(Total.dead24h),
            Mortality_24h= round((Dead24/Nb.Tested)*100,2),
            SE24=(sd(mort.rate.24h)/sqrt(N)),
            Dead72 = sum(Total.dead72),
            Mortality_72h= round((Dead72/Nb.Tested)*100,2),
            SE72=(sd(mort.rate.72h)/sqrt(N)))
          View(res1) 

######### rest data ################
rest.bd_gb <- group_by(rest.bd.sel,Time_after_spray,Mosquitoes_species_strain,Type_Hut,Treatments)

res2 <- summarise(rest.bd_gb,
            N = n(),
            Nb.Tested = sum(Tested),
            Dead24 = sum(Total.dead24h),
            Mortality_24h= round((Dead24/Nb.Tested)*100,2),
            SE24=(sd(mort.rate.24h)/sqrt(N)),
            Dead72 = sum(Total.dead72),
            Mortality_72h= round((Dead72/Nb.Tested)*100,2),
            SE72=(sd(mort.rate.72h)/sqrt(N)))
          View(res2)
####################################################
write.table(res1,"resul_Ceiling3.txt",dec=",",sep="\t",row.names=F)
write.table(res2,"resul_rest3.txt",dec=",",sep="\t",row.names=F)




