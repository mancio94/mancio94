rm(list = ls())
require(foreign)
require(tidyverse)
library(doBy)

setwd("/Users/enrico/Desktop/valdovar")
#------------------- DATI CHE MI SERVONO ----------------------#
lattazioni2001_2010.xlsx  ANA0719.DBF 
testday_ok5_VPR_soglie.xlsx lattazioni2001_2010.xlsx
ped_firstparity_alive_comb_red4_matr.xlsx
testday_ok5_VPR_soglie.xlsx
#-------------------------------------------------------------#
data.table::fread("firstparity_Pkgl_combcl.dat",sep= " ")

#---------------------------------------------------------#
#   parte 1 lavoro sulla varianza , pulizia dato ecc      #
#--------------------------------------------------------#
df = readxl::read_excel("lattazioni2001_2010.xlsx")
# modello MATRFACT NL AZNDANNO MESE 
nrow(df)   #number of data
length(unique(df$MATR)) #numbero of animals 
# c'e differenza tra razze ?
df %>% group_by(RAZZA) %>% summarize (me=mean(KGLATEFF,na.rm=T) ,SD=sd(KGLATEFF,na.rm=T))
# no 
# provare con kg latte KGLATCONV


df = df %>% dplyr::select(MATR,LATT_PROGR_, Azienda_anno, ALLUSL,Inizio_Latt_mese ,Inizio_Latt_anno,KGLATEFF,KGLATCONV)
df=as.data.frame(df)
df = df[apply(df,1,function(x) !any(is.na(x))),]
nrow(df) #dati completi 
# conservare i dati da 1 a 5
df$LATT_PROGR_ = as.character(df$LATT_PROGR_)

# rimuovo quelle che cambinao azienda durate la lattazione 
for (i in df$MATR){
  if (length(unique(df[df$MATR==i,'ALLUSL'])) > 1){
    df=df[df$MATR !=i ,]
  }}

#   qua decido se solo 4 lattazioni pulite o metterlo nel modello 
#  non senno perdo troppi dati 
if (FALSE) {
ndf=data.frame()

for (i in unique(df$MATR)) {
    g = df[df$MATR == i & order(df$LATT_PROGR_),]
if ((nrow(g)>=4)) {
  if (g[1,'LATT_PROGR_']==1 & g[2,'LATT_PROGR_']==2 &
	  g[3,'LATT_PROGR_']==3	& g[4,'LATT_PROGR_']==4	) {

	 	ndf = rbind(ndf,g[1:4,])  }	
} }
}

df$LATT_PROGR_ = as.numeric(as.character(df$LATT_PROGR_))
# tengo fino alle sei lattazioni #
df %>% count(LATT_PROGR_)
df = df %>% filter(LATT_PROGR_ < 9)

hist(df$KGLATCONV)
hist(df$KGLATEFF,20)

#----# tolgo 4 u.ds.  #-------
u1 = mean(df$KGLATEFF) + (sd(df$KGLATEFF)*3.5) 
u2 = mean(df$KGLATEFF) - (sd(df$KGLATEFF)*3.5)
u1;u2
df = df[df$KGLATEFF < u1 & df$KGLATEFF > u2,]
hist(df$KGLATEFF,20)
#----->#  rimuovo quele che hanno meno di 3 dati  #<---------#
1000/305
5000/305
low_p=

if (FLASE) {
  
  df_x = df[df$KGLATEFF < 4000 & df$KGLATEFF >  2000 ,]
  
}
nrow(df_x)
df=df_x
namali = df %>% group_by(MATR) %>% tally()
namali = namali[namali$n < 2,]
namali %>% arrange(n)
nrow(namali)
nrow(df)
df = anti_join(df,namali)
nrow(df)

# RIMUOVO CAPI SINGOLI 
l = df %>% group_by(Azienda_anno) %>% tally()
togli = l[l$n<2,'Azienda_anno']  # 4 tengo !!!!!
df=anti_join(df,togli)  
length(unique(df$MATR))
nrow(df)

nrow(df)
save.image("termine_pulizia.RData")


#-------------------------------------
#head(df)
load("termine_pulizia.RData")
aa=df %>% group_by(LATT_PROGR_) %>% summarize(mean=mean(KGLATEFF))
#aa
plot(aa$LATT_PROGR_,aa$mean,xlab = "N_LATT",ylab = "mean")
head(aa)
df$LATT_PROGR_=as.numeric(as.character(df$LATT_PROGR_))
# se non le telogo lo metto cosi 
df = as.data.frame(df)
head(df)
df$LATT_PROGR_=as.factor(df$LATT_PROGR_)
anova(lm(df$KGLATEFF ~ df$LATT_PROGR_))
df %>% count(N_LATT)
p <-ggplot(data=df,aes(x=LATT_PROGR_,y=KGLATEFF))+
 geom_boxplot()
 
setwd("/Users/enrico/Desktop/valdovar")
save.image("termine_pulizia.RData")
#-----| PULIZIA DATI |------#

#-----------------------------------------------------------------------#
#                   MODELLI DI PRECORREZIONE
#-----------------------------------------------------------------------#     

nrow(df)
setwd("/Users/enrico/Desktop/valdovar")
load("termine_pulizia.RData")
head(df)
df$Azienda_anno = as.factor(df$Azienda_anno)
df$Inizio_Latt_mese = as.factor(df$Inizio_Latt_mese)
#model = lm(KGLATEFF ~ LATT_PROGR_ +Azienda_anno + 	  Inizio_Latt_mese, df)
anova(model)
summary(model)$	r.squared
plot(df$KGLATEFF)


#-----------------| try to plot |---------------------
plot(predict(model),df$KGLATEFF,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)
#--------------------------------------------------------

#---------------| se usom il secodo modello anno mese --------#
#----------------| ALMENO 4 PER GRUPPO |---------------------#

hist(df$KGLATEFF)

for (x in unique(df$LATT_PROGR_)) {
dd=df[df$LATT_PROGR_==x,]
  quantile=quantile(dd$KGLATEFF, c(.25, .50, .75))

attributes(quantile) <- NULL

for (i in 1:nrow(df)){
  if(df[i,"KGLATEFF"] < quantile[1] & df$LATT_PROGR_==x )                                    {df[i,"CORREGO_LATTE"]=1 }
  if(df[i,"KGLATEFF"] < quantile[2]  & df[i,"KGLATEFF"] > quantile[1] &  df$LATT_PROGR_==x  )  {df[i,"CORREGO_LATTE"]=2 }
  if(df[i,"KGLATEFF"] < quantile[3]  & df[i,"KGLATEFF"] > quantile[2] & df$LATT_PROGR_==x )  {df[i,"CORREGO_LATTE"]=3 }
  if(df[i,"KGLATEFF"] > quantile[3] & df$LATT_PROGR_==x )                                    {df[i,"CORREGO_LATTE"]=4 }
}
}
# idea se il quantile cambia tra una una lattazione e l' altra posso mettere treshold 
# come la differenza tra quantilei 

average = doBy::summaryBy(KGLATEFF ~ Azienda_anno  + LATT_PROGR_,  #+ Inizio_Latt_mese
data = df ,keep.names=T, FUN=c(mean))

average = doBy::summaryBy(KGLATEFF ~ LATT_PROGR_,
                           # Azienda_anno #+ Inizio_Latt_mese  ,
                          data=df ,keep.names=T, FUN=c(mean))

df %>% count(Azienda_anno) %>% arrange(n)
#  precorrego solo pe razienda annon

for (i in 1: nrow(df)) {
	
 # anno_aznd = df[i,'Azienda_anno']
  nl =       df[i,'LATT_PROGR_'] 
#  mese =     df[i,'Inizio_Latt_mese']
   
 tolgo = 
  average[     #average$Azienda_anno == anno_aznd & 
          average$LATT_PROGR_ ==         nl      
 	#				average$Inizio_Latt_mese == mese
 					, 'KGLATEFF'] 
                 
  #print(df[i,'KGLATEFF'])
  #print(tolgo)
  df[i,'KGLATEFF1'] = df[i,'KGLATEFF'] - tolgo
}

  
hist(df$KGLATEFF,breaks=100,main = "latte",col="RED")
hist(df$KGLATEFF1,breaks=100,main = "latte_corretto",col="RED")
nrow(df[df$KGLATEFF1==0,])
df$KGLATEFF1 = df$KGLATEFF1 + (min(df$KGLATEFF1)*-1)
df$Azienda_anno=as.character(df$Azienda_anno)
# azienda anno al primoparto 
for ( i in unique(df$MATR)) {
  df[df$MATR==i,"prima_aziendaanno"] = df[df$MATR==i & order(df$LATT_PROGR_),"Azienda_anno"][1]
}


#---------------| adessocalcola le varianze |--------------# 
MATRICO=df %>% group_by(MATR) %>% tally()
nrow(MATRICO)
uniche = MATRICO[MATRICO$n<2,]
nrow(df)
df = df[!df$MATR %in% uniche$MATR,]
nrow(df)
df$KGLATEFF1 = df$KGLATEFF


pvar <- function(x) {
  sqrt(sum((x - mean(x))**2) / (length(x)+1))
}

#View(df)

for (i in unique(df$MATR)) {
  df[df$MATR==i,"ds"]=pvar((df[df$MATR==i,"KGLATEFF1"]))
  df[df$MATR==i,"media"]= mean((df[df$MATR==i,"KGLATEFF1"]))
}

quantile=quantile(df$media, c(.25, .50, .75))

attributes(quantile) <- NULL


# dividere pweer questa media !
for (i in 1:nrow(df)) {
  if(df[i,"media"] <  quantile[1]  )                                  {df[i,"CORREGO_LATTE1"]=1 }
  if(df[i,"media"] < quantile[2]  & df[i,"media"] >= quantile[1])     {df[i,"CORREGO_LATTE1"]=2 }
  if(df[i,"media"] < quantile[3]  & df[i,"media"] >= quantile[2])     {df[i,"CORREGO_LATTE1"]=3 }
  if(df[i,"media"] >= quantile[3] )                                   {df[i,"CORREGO_LATTE1"]=4 }
}

df = df[!is.na(df$ds),]
nrow(df)
hist(df$ds,breaks=100,col="RED")
hist(df$media,breaks=100,col="RED")
nrow(df[df$media==0,])
df$CV = df$ds / df$media
hist(df$ds,10,col="RED")
# posso approsimarla a normale ?
save.image("pulizia2.RData")
setwd("/Users/enrico/Desktop/valdovar")
load("pulizia2.RData")
str(df)
df$Azienda_anno = as.character(df$Azienda_anno)
for ( i in unique(df$MATR)) {
  #print( df[df$MATR == i & order(df$LATT_PROGR_),"Azienda_anno"])
  df[df$MATR == i,"aznd_anno_born"] =  df[df$MATR == i & order(df$LATT_PROGR_),"Azienda_anno"][1]
}
str(df)
df$Inizio_Latt_mese=as.numeric(as.character(df$Inizio_Latt_mese))

i="IT007000422637"

for ( i in unique(df$MATR)) {
  df[df$MATR == i,"mese_1p"] =  df[df$MATR == i & order(df$LATT_PROGR_),"Inizio_Latt_mese"][1]
}
head(df)


# prova tempranea elimino il mese #
#df = df %>% select(MATR,mese_1p,aznd_anno_born,CORREGO_LATTE,ds,CV,media) %>% distinct()
nrow(df)
length(unique(df$MATR))
df = df %>% distinct()
cor(df$ds,df$CV)
cor(df$ds,df$media)
df$Aaznd_anno_born = as.factor(df$aznd_anno_born)
#df$mese_1p = as.factor(df$mese_1p)
#model = lm(media ~ aznd_anno_born + 	  mese_1p, df)
#anova(model)
#summary(model)$	r.squared


setwd("/Users/enrico/Desktop/valdovar")
#save.image("creazione_df_variabilita.RData")
df = df[complete.cases(df), ]
length(unique(df$MATR))
df = df %>% distinct()
nrow(df)
head(df)

#--------------------------
#       analisi generica    angiugere efffetti 
#------------------------
setwd("/Users/enrico/Desktop/valdovar")
salva=df
norw(salva)
save.image("pulizia3.RData")
load("pulizia3.RData")

library(tidyverse)
setwd("/Users/enrico/Desktop/valdovar")

#load("lancio_modellif90.Rdata")
dir.create("analisi_sing");setwd("analisi_sing")
head(df)
ana = foreign::read.dbf("../ANA0719.DBF")[,1:3]
#data.table::fwrite(ana[,c(1,2,3)],file = "ped.txt",sep = " ",row.names = FALSE,col.names = FALSE,quote = FALSE)
nrow(ped);nrow(dfn)
sdf=df
system('echo \"DATAFILE
dbraw.txt
TRAITS
5
FIELDS_PASSED TO OUTPUT

WEIGHT(S)
0
RESIDUAL_VARIANCE
1.49
EFFECT
2 cross alpha
EFFECT
3 cross alpha
EFFECT
1 cross alpha
RANDOM
animal   # pruned ped!
FILE
ped.txt
FILE_POS
 1 2 3
PED_DEPTH
15
(CO)VARIANCES
0.118 "\ > par.txt')


# CALCOLO GRASSO DS 
tabella=data.frame()
y = "CV"
x = c("MATR","mese_1p", "aznd_anno_born")
for (i in y) {
  i=y
 
  lancio = df %>% dplyr::select(x,y) %>% as.data.frame() # change y with i
  head(lancio)
  summary(lancio[,i])
  lancio = lancio[complete.cases(lancio), ]
  length(unique(lancio$MATR))
  lancio = lancio %>% distinct()
  nrow(lancio)
  df_prova1=lancio 
  data.table::fwrite(lancio,"dbraw.txt",quote = FALSE,sep = " ",row.names = FALSE,col.names = FALSE)
  nrow(lancio)
  system("ln -f ~/Documents/blupf90_family/renumf90 .") 
  system("ln -f ~/Documents/blupf90_family/airemlf90 .")
  system("chmod 755 ./*")
  system("echo par.txt  | ./renumf90" )                                                            
  system("echo renf90.par  | ./airemlf90")    
  
  remlf90 <- readr::read_csv("airemlf90.log") 
  # predere solution per  la fattoriale --> scrivermi 
  
  tabella[ss,"character"]    = i
  tabella[ss,"additivo"]    = as.numeric(remlf90[3,])
  tabella[ss,"residua"]     = as.numeric(remlf90[5,])
  tabella[ss,"h2"]          =  tabella$additivo[ss] / (tabella$additivo[ss] + tabella$residua[ss])
  tabella
  
  file.rename("renf90.par", paste0(i,".par",sep=""))
  file.rename("remlf90.log",paste0(i,".log",sep=""))
  file.rename("renf90.dat", paste0(i,".dat",sep=""))
  file.rename("renf90.tables",paste0(i,".taable",sep=""))
  file.rename("renadd04.ped","fattoriale.ped")
  #-------#
  ss = ss+1   
  
}
setwd("..")
save.image("lancio_ds.RData")

View(df %>% arrange(MATR))
save.image("animlisi_singlemade.RData")
#------------------------------------------#
#-------------| bitraits  |-------------------#  dati sull intera caariera 
#------------------------------------------#
setwd("/Users/enrico/Desktop/valdovar")
#load("creazione_df_variabilita.RData")
#load("lancio_modellif90.Rdata")
dir.create("analisi_bit");setwd("analisi_bit")
head(df)
x="MATR"
y=c("CV","media")
lancio = df %>% dplyr::select(MATR,x,y) %>% as.data.frame() # change y with i
head(lancio)
summary(lancio[,i])
lancio = lancio[complete.cases(lancio), ]
lancio = lancio %>% distinct()
head(lancio)
nrow(lancio)
data.table::fwrite(lancio,"dbraw.txt",quote = FALSE,sep = " ",row.names = FALSE,col.names = FALSE)
#  aznd_anno_born perche gi coretto

system('echo \"DATAFILE
dbraw.txt
TRAITS
2 3 
FIELDS_PASSED TO OUTPUT
     
WEIGHT(S)
0
RESIDUAL_VARIANCE
1.00  0.99
0.99  1.00
EFFECT
1 1  cross alpha
RANDOM
animal   # pruned ped!
FILE
ped.txt
FILE_POS
 1 2 3
PED_DEPTH
15
(CO)VARIANCES
.71 .2
.2 .71"\ > par2.txt')
getwd()
head(df)
head(lancio)
  
  system("ln -f ../analisi_sing/ped.txt .") 
  system("ln -f ~/Documents/blupf90_family/renumf90 .") 
  system("ln -f ~/Documents/blupf90_family/airemlf90 .")
  system("chmod 755 ./*")
  system("echo par2.txt  | ./renumf90" )                                                            
  system("echo renf90.par  | ./airemlf90")    
  
  remlf90 <- readr::read_csv("airemlf90.log") 
  # predere solution per  la fattoriale --> scrivermi 
  
  tabella[ss,"character"]    = i
  tabella[ss,"additivo"]    = as.numeric(remlf90[3,])
  tabella[ss,"residua"]     = as.numeric(remlf90[5,])
  tabella[ss,"h2"]          =  tabella$additivo[ss] / (tabella$additivo[ss] + tabella$residua[ss])
  tabella
  
  file.rename("renf90.par", paste0(i,".par",sep=""))
  file.rename("remlf90.log",paste0(i,".log",sep=""))
  file.rename("renf90.dat", paste0(i,".dat",sep=""))
  file.rename("renf90.tables",paste0(i,".taable",sep=""))
  file.rename("renadd04.ped","fattoriale.ped")
  #-------#
  ss = ss+1   
  
#}

system("ln -f ~/Documents/blupf90_family/renumf90 .") 
system("ln -f ~/Documents/blupf90_family/airemlf90 .")
system("chmod 755 ./*")

system("ln -f ../analisi_gentica/ped.txt .");system("ln -f ../analisi_geentica/ped.txt .")
system("echo par2.txt  | ./renumf90" )                                                            
system("echo renf90.par  | ./airemlf90") 

#----------------------------------------------------------
# correlation del 0.5 % genetica e nulla quella residua 
# quindi si vacche piu prodttive piu variabili (MEDIA e SD)
#-----------------------------------------------------------
# ok non e troppo attendbile perche la h2 e del 40 &
#-----------------------------------------------------------
# correlation del 0.3 % genetica e -45 %  quella residua 
# quindi si vacche piu prodttive piu variabili (MEDIA e SD)
#-----------------------------------------------------------
remlf90_bit <- readr::read_csv("airemlf90.log") 

#----------------------------------------------------------------------------------------------
setwd("~/Desktop/valdovar/")
rm(list = ls())
save.image("finoallavarianza.RData")
df = df1
require(foreign)
require(tidyverse)
library(doBy)
#---------------------------------------------------------------

#-------------------------------------------------------------------
#   CORREGO prova longeviata DATI CRSTINA   
#   V1= annooazienda V7=ANIMALE  V15= COMATTIVITA, v12= latet azienda
#   V10= LONGEVITASFUNZIOANLE V9=HERDLIFE, V11=NPARTI
#------------------------------------------------------------
#                 dimostrazione che non combaciano
#---------------------------------------------------------------
#getwd();load("animlisi_singlemade.RData")
getwd()
setwd('~/Desktop/valdovar/')
load("lancio_ds.RData");library(tidyverse)
#assegno nomi alle matricole
load("pulizia2.RData")
setwd("~/Desktop/valdovar/materialecristina/per_Enrico")
#load("../../lancio_modellif90.Rdata") # MI SERVE PER CARICARE IL DATASET DF

ana = readxl::read_excel("ped_firstparity_alive_comb_red4_matr.xlsx")
data = data.table::fread("firstparity_Pkgl_combcl.dat",sep= " ")
DATA=merge(data,ana[,c("MATR","ANIMAL")],by.x = "V7",by.y = "ANIMAL",all.x = FALSE ,all.y = FALSE)
df = df_prova1
nrow(data)
nrow(DATA) # olo
nrow(df)
#View(DATA)
head(DATA)
nrow(DATA);nrow(data)
DATA = DATA %>% dplyr::select(MATR,V1,V15,V12,V10,V9,V11)
head(df)
head(DATA)
colnames(DATA) = c("MATR","anno_aznd","combcor","latte_cor","longfunz",
                    "herdlife","nparti")

head(DATA)
max(DATA$anno_aznd)==length(unique(DATA$anno_aznd))
apply(DATA,2,function(x) length(unique(x)))
# CARICARE QUESTO DATASET 
#df=df %>% dplyr::select(MATR,ALLUSL,ds, estd) 
nrow(df)
df=df %>% distinct()
nrow(df)


setwd('~/Desktop/valdovar/')
longevita=DATA
save.image("fino_alla_longrvita.RData")



#-------------------------------------------------------
#          breve analisi single straits               # 
#-----------------------------------------------------


setwd('~/Desktop/valdovar/')
load("fino_alla_longrvita.RData");library(tidyverse)
# creare la cartella 
dir.create("analisi_longevita");setwd("analisi_longevita")
getwd()

x = c('MATR','anno_aznd','combcor','latte_cor')
y = c('longfunz', 'herdlife', 'nparti')
tail(DATA)
#  intanto elimino i censered 
DATA = DATA %>% filter(herdlife >= 0) 


ss=1;tabella=data.frame()
for (i in y) {
  i='longfunz'
  print(i)
  lancio = DATA %>% dplyr::select(x,i) %>% as.data.frame()
  head(lancio)
  summary(lancio[,i])
  lancio = lancio[complete.cases(lancio), ]
  nrow(lancio)
  data.table::fwrite(lancio,"dbraw.txt",quote = FALSE, sep = " ",row.names = FALSE,col.names = FALSE)
  
  system('echo \"DATAFILE
         dbraw.txt
         TRAITS
         5
         FIELDS_PASSED TO OUTPUT
         
         WEIGHT(S)
         0
         RESIDUAL_VARIANCE
         1.49
         EFFECT
         2 cross alpha
         EFFECT
         3 cross alpha   #cov o corss
         EFFECT
         4 cross alpha   #cov o corss
         EFFECT
         1 cross alpha
         RANDOM
         animal   # pruned ped!
         FILE
         ped.txt
         PED_DEPTH
         15
         (CO)VARIANCES
         0.118 "\ > par.txt')
  
  system("ln -f ../analisi_sing/ped.txt .")
  system("ln -f ~/Documents/blupf90_family/renumf90 .") 
  system("ln -f ~/Documents/blupf90_family/airemlf90 .")
  
  system("echo par.txt | ./renumf90")                                                            
  system("echo renf90.par  | ./airemlf90")    
  
  remlf90 <- readr::read_csv("airemlf90.log") 
  # predere solution per  la fattoriale --> scrivermi 
  
  
  tabella[ss,"character"]    = i
  tabella[ss,"additivo"]    = as.numeric(remlf90[5,])
  tabella[ss,"residua"]     = as.numeric(remlf90[7,])
  tabella[ss,"h2"]          =  tabella$additivo[ss] / (tabella$additivo[ss] + tabella$residua[ss])
  tabella
  
  file.rename("renf90.par", paste0(i,".par",sep=""))
  file.rename("remlf90.log",paste0(i,".log",sep=""))
  file.rename("renf90.dat", paste0(i,".dat",sep=""))
  file.rename("renf90.tables",paste0(i,".taable",sep=""))
  file.rename("renadd04.ped","fattoriale.ped") #sistemare questo
  #-------#
  ss = ss+1   
}
setwd("..")
save.image("../fino_a_long.RData")



#--------------------------------------------------------------------------------#
#                                      BITRAITS                                 # 
#-------------------------------------------------------------------------------#


#load("/fino_a_long.RData")
dir.create("analisi_bit_longevita_x_ds");setwd("analisi_bit_longevita_x_ds")
getwd()

#  intanto elimino i censered 
DATA = DATA %>% filter(herdlife >= 0) 
save_ = df_prova1

head(df)

ndf_1 = merge(longevita,df,by.x = "MATR",by.y = "MATR",all.x = TRUE,all.y = TRUE)
# meglio questo
#ndf_1 = plyr::rbind.fill(salva,DATA) 
nrow(ndf_1);nrow(DATA);nrow(df);nrow(df) + nrow(DATA) == nrow(ndf_1)
nrow(ndf_1[complete.cases(ndf_1), ])
#View(ndf_1)

colnames(ndf_1)
colnames(df)
colnames(DATA)

#-----------------------------------------------
x_1 = c("mese_1p","aznd_anno_born","CORREGO_LATTE1")
x_2 = c("anno_aznd","combcor","latte_cor")
y_1 = c("ds","CV" ,"media")
y_2 = c("longfunz","herdlife","nparti")
#-----------------------------------------------
ndf_1 = ndf_1 %>% mutate_if(sapply(ndf_1, is.factor), as.character)
ndf_1[is.na(ndf_1)]=-999
i=y_1[2]
ii=y_2[1]

head(DATA)
head(df_prova1)

nrow(DATA[DATA$MATR %in% df_prova1$MATR,])  # se provassi a tenere solo mata degli animali con entrambi i dati !?!?
nrow(DATA)
nrow(df_prova1[df_prova1$MATR %in% DATA$MATR,])  
nrow(df_prova1)
#-----------------------------------------------#




for (i in y_1) {
  
  for (ii in y_2) {
    
    print(i)
    print(ii)
#View(lancio)
    ndf_1$ds = ndf_1$ds/100
lancio = ndf_1 %>% dplyr::select(MATR,x_1,x_2,i,ii) %>% as.data.frame()
lancio=lancio %>% distinct
data.table::fwrite(lancio,"dbraw.txt",quote = FALSE, sep = " ",row.names = FALSE,col.names = FALSE)
#lancio[is.na(lancio)]=0
head(lancio)
system('echo \"DATAFILE
         dbraw.txt
         TRAITS
         7 8
         FIELDS_PASSED TO OUTPUT
         
         WEIGHT(S)
         0
         RESIDUAL_VARIANCE
         1.49 0.8
         0.8 1.49 
         EFFECT    #-->PRIMA COSA EFFFETTI PER LA VARIABILITA <---#
         2  0 cross alpha
         EFFECT
         3  0  cross alpha 
         #EFFECT 
         #4   0  cross alpha
         EFFECT 
         0  5 cross alpha
         EFFECT 
         0  6 cov
         EFFECT 
         0  7 cov
         EFFECT
         1 1 cross alpha
         RANDOM
         animal   # pruned ped!
         FILE
         ped.txt
         PED_DEPTH
         15
         (CO)VARIANCES
         .118 .02
         .02  .118
         OPTION missing -999"\ > par.txt')
getwd()

# troppo pesante invia al sever 


system("ln -f ../../analisi_sing/ped.txt .")

system("sshpass -p enrico! ssh -p 22 enrico@dodo128.ads.uga.edu 'cd valdovarnuovo;  ls' ")
system("sshpass -p enrico! scp  par.txt  enrico@dodo128.ads.uga.edu:valdovarnuovo/")
system("sshpass -p enrico! scp   ped.txt  enrico@dodo128.ads.uga.edu:valdovarnuovo/")
system("sshpass -p enrico! scp   dbraw.txt enrico@dodo128.ads.uga.edu:valdovarnuovo/")
system("sshpass -p enrico! ssh -p 22 enrico@dodo128.ads.uga.edu 'cd valdovarnuovo; echo par.txt | renumf90 ' ")
system("sshpass -p q ssh -p 9998 enrico.mancin@147.162.139.238 'cd valdovarnuovo; chmod 775 ./*; echo renf90.par | ./airemlf90 ' ")
system('sshpass -p enrico! scp  -r enrico@dodo128.ads.uga.edu:valdovarnuovo/go.sh ./Desktop')
system('pwd')
#---------------------------------------------------------------------------------------------------    #
#      secondo prova per il bit traits limetto tuttti insime  meto tutti bittraits iseme !!           ??
##---------------------------------------------------------------------------------------------------??
setwd("/Users/enrico/Desktop/valdovar/")
setwd("analisi_bit_longevita_x_ds")


nnnd= merge(salva,DATA,by.x="MATR",by.y="MATR",all.x=TRUE,all.y=TRUE)
nrow(nnnd)

nrow(ndf_1)
ndf_1<- nnnd
#------------------------------------------------------------
x_1 = c("mese_1p","aznd_anno_born")
x_2 = c("anno_aznd","combcor","latte_cor")
y_1 = c("ds","CV" ,"media")
y_2 = c("longfunz","herdlife","nparti")
#-----------------------------------------------
ndf_1 = ndf_1 %>% mutate_if(sapply(ndf_1, is.factor), as.character)
NDF=ndf_1[!is.na(ndf_1),]
NDF = ndf_1[!is.na(ndf_1$ds) & !is.na(ndf_1$herdlife) , ]
NDF = ndf_1[!is.na(ndf_1$ds)  , ]
nrow(NDF)
head(NDF)
#ndf_1$ds=as.character(ndf_1$ds)
#ndf_1[is.na(ndf_1)]=-999
i=y_1[2]
ii=y_2[1]
head(ndf_1)
head(DATA)
tail(ndf_1)
#ndf_1 = NDF


#------------------------------------------------------------------
lancio = ndf_1 %>% select('MATR',x_1,x_2,y_1[1],y_2[1]) %>% as.data.frame()
#lancio = ndf_1[,c("MATR","mese_1p","anno_aznd","combcor","latte_cor","ds","longfunz")]
head(lancio)

lancio$ds=lancio$ds/100
tail(lancio)
ndf_1$ds=as.character(ndf_1$ds)
lancio[is.na(lancio)]="-999"
nrow(lancio)
head(lancio)
tail(lancio)
data.table::fwrite(lancio,"dbraw.txt",quote = FALSE, sep = " ", row.names = FALSE,col.names = FALSE)
#lancio[is.na(lancio)]=0
head(lancio)

lancio %>% filter(mese_1p != "-999" & anno_aznd !="-999") %>% nrow()
lancio %>% filter(mese_1p != "-999" & anno_aznd !="-999") %>% head()

head(lancio)

system('echo \"DATAFILE
       dbraw.txt
       TRAITS
       6 7 
       FIELDS_PASSED TO OUTPUT
       
       WEIGHT(S)
       0
       RESIDUAL_VARIANCE
       1.49 0.8
       0.8 1.49 
       EFFECT    #-->PRIMA COSA EFFFETTI PER LA VARIABILITA <---#
       2  0 cross alpha
       EFFECT
       3  0 cross alpha 
       EFFECT
       0  4 cross alpha 
       EFFECT 
       0  5  cov 
       EFFECT 
       0  6 cov
       EFFECT
       1 1 cross alpha
       RANDOM
       animal   # pruned ped!
       FILE
       ped.txt
       PED_DEPTH
       15
       (CO)VARIANCES
       .118 .02
       .02  .118
       OPTION missing -999
       OPTION yams
       "\ > par.txt')

3205.6      -10.533    
-10.533      0.68705 


system("ln -f ../analisi_sing/ped.txt .")

system("sshpass -p enrico! ssh -p 22 enrico@dodo128.ads.uga.edu 'cd valdovarnuovo;  ls' ")
system("sshpass -p enrico! scp  par.txt  enrico@dodo128.ads.uga.edu:valdovarnuovo/")
system("sshpass -p enrico! scp   ped.txt  enrico@dodo128.ads.uga.edu:valdovarnuovo/")
system("sshpass -p enrico! scp   dbraw.txt enrico@dodo128.ads.uga.edu:valdovarnuovo/")
system("ln -f ../analisi_sing/ped.txt .")
system("ln -f ~/Documents/blupf90_family/UNIX/renumf90 .") 
system("ln -f ~/Documents/blupf90_family/UNIX/airemlf90 .")
system("ln -f ~/Documents/blupf90_family/renumf90 .") 
system("ln -f ~/Documents/blupf90_family/airemlf90 .")


system("echo par.txt | ./renumf90")
system("echo renf90.par | ./airemlf90")



??

system("sshpass -p abote4700 ssh -p 9998 enrico.mancin@147.162.139.238 'mkdir valdovarnuovo; cd valdovarnuovo;  ls' ")
system("sshpass -p abote4700  scp  par.txt  enrico.mancin@147.162.139.238:valdovarnuovo/")
system("sshpass -p abote4700  scp   ped.txt  enrico.mancin@147.162.139.238:valdovarnuovo/")
system("sshpass -p abote4700  scp   dbraw.txt enrico.mancin@147.162.139.238:valdovarnuovo/")
system("sshpass -p abote4700  scp  airemlf90  enrico.mancin@147.162.139.238:valdovarnuovo/")
system("sshpass -p abote4700  scp  renumf90  enrico.mancin@147.162.139.238:valdovarnuovo/")

system("sshpass -p abote4700  ssh -p 9998 enrico.mancin@147.162.139.238 'cd valdovarnuovo;  ls' ")

system("sshpass -p abote4700 ssh -p 9998 enrico.mancin@147.162.139.238 'cd valdovarnuovo; echo par.txt | ./renumf90 ' ")
system("sshpass -p abote4700 ssh -p 9998 enrico.mancin@147.162.139.238 'cd valdovarnuovo; chmod 775 ./*; echo renf90.par | ./airemlf90 ' ")

#-----------------------------------------------------------------------------
#-----------------------
remlf90 <- readr::read_csv("airemlf90.log") 
# predere solution per  la fattoriale --> scrivermi 
remlf90 <- as.data.frame(remlf90[1:24,])

C_1=strsplit(as.character(remlf90[5,])," ")[[1]][1]
COV=strsplit(as.character(remlf90[5,])," ")[[1]][length(strsplit(as.character(remlf90[5,])," ")[[1]])]
C_2=strsplit(as.character(remlf90[6,])," ")[[1]][length(strsplit(as.character(remlf90[6,])," ")[[1]])]

CORR=strsplit(as.character(remlf90[9,])," ")[[1]][1]

CE_1=strsplit(as.character(remlf90[16,])," ")[[1]][1]
CE_2=strsplit(as.character(remlf90[17,])," ")[[1]][length(strsplit(as.character(remlf90[17,])," ")[[1]])]


tabella[1:2,"character"]    =                c(i,ii)
tabella[1:2,"additivo"]    =                 c(C_1,C_2)
tabella[1:2,"residua"]    =                 c(CE_1,CE_2)
tabella[1:2,paste("h2",i,sep= "_")]          =  as.numeric(tabella$additivo[1]) / (as.numeric(tabella$additivo[1]) + as.numeric(tabella$residua[1]))
tabella[1:2,paste("h2",ii,sep= "_")]          =  tabella$additivo[2] / (tabella$additivo[2] + tabella$residua[2])
tabella[ss,paste("h2",ii,sep= "_")]          =  tabella$additivo[2] / (tabella$additivo[2] + tabella$residua[2])

assign( paste(i,ii,sep="_"),tabella)

file.rename("renf90.par", paste0(i,"-",ii,".par",sep=""))
file.rename("airemlf90.log",paste0(i,"-",ii,".log",sep=""))
file.rename("renf90.dat", paste0(i,"-",ii,".dat",sep=""))
file.rename("renf90.tables",paste0(i,"-",ii,".taable",sep=""))
#file.rename("renadd04.ped","fattoriale.ped"

system("mv renadd*  pedix")
file.rename("pedix",paste0(i,"_",ii,".ped",sep=""))
  }

#-----------------------
#    CALCOLO LA MEDI DELLA LATTAZIONI 
#-----------------------

latte_nonthd = readxl::read_excel("/Users/enrico/Desktop/valdovar/lattazioni2001_2010.xlsx")
head(latte_nonthd)

# modello MATRFACT NL AZNDANNO MESE 

latte_nonthd %>% group_by(RAZZA) %>% summarize (me=mean(KGLATEFF,na.rm=T) ,SD=sd(KGLATEFF,na.rm=T))

latte_nonthd = latte_nonthd %>% dplyr::select(MATR,LATT_PROGR_, Azienda_anno, ALLUSL,Inizio_Latt_mese ,Inizio_Latt_anno,KGLATEFF)
latte_nonthd=as.data.frame(latte_nonthd)
latte_nonthd = latte_nonthd[apply(latte_nonthd,1,function(x) !any(is.na(x))),]
nrow(latte_nonthd)

# RIMUOVO COAPI SINGOLI 
l = latte_nonthd %>% group_by(Azienda_anno) %>% tally()
togli = l[l$n<2,'Azienda_anno']
latte_nonthd=anti_join(latte_nonthd,togli)  
nrow(latte_nonthd) 

l = latte_nonthd %>% group_by(Inizio_Latt_mese) %>% tally()
l[l$n<2,]

latte_nonthd %>% group_by(LATT_PROGR_) %>% tally()
latte_nonthd = latte_nonthd[latte_nonthd$LATT_PROGR_ <= 9,]
nrow(latte_nonthd)
#........ analisi single traits  .............#
  
  
latte_nonthd = latte_nonthd %>% mutate_if(sapply(latte_nonthd, is.factor), as.character)
latte_nonthd[is.na(latte_nonthd)]=-999
head(latte_nonthd)


setwd("/Users/enrico/Desktop/valdovar/")
#------------------------------------------------------------------
dir.create("latte_produzione");setwd("latte_produzione")
lancio = latte_nonthd %>% dplyr::select('MATR','LATT_PROGR_', 'Azienda_anno','Inizio_Latt_mese','KGLATEFF') %>% as.data.frame()
data.table::fwrite(lancio,"dbraw.txt",quote = FALSE, sep = " ",row.names = FALSE,col.names = FALSE)
#lancio[is.na(lancio)]=0
head(lancio)

system('echo \"DATAFILE
       dbraw.txt
       TRAITS
       5
       FIELDS_PASSED TO OUTPUT
       
       WEIGHT(S)
       0
       RESIDUAL_VARIANCE
       1.49 
       EFFECT    #-->PRIMA COSA EFFFETTI PER LA VARIABILITA <---#
       2 cross alpha
       EFFECT
       3 cross alpha 
       EFFECT 
       4  cross alpha 
       EFFECT
       1 1 cross alpha
       RANDOM
       animal   # pruned ped!
       FILE
       ped.txt
       PED_DEPTH
       15
       (CO)VARIANCES
       .118 
       OPTION missing -999"\ > par.txt')

system("ln -f ../analisi_sing/ped.txt .")

system("sshpass -p enrico! ssh -p 22 enrico@dodo128.ads.uga.edu 'cd valdovarnuovo;  ls' ")
system("sshpass -p enrico! scp  par.txt  enrico@dodo128.ads.uga.edu:valdovarnuovo/")
system("sshpass -p enrico! scp   ped.txt  enrico@dodo128.ads.uga.edu:valdovarnuovo/")
system("sshpass -p enrico! scp   dbraw.txt enrico@dodo128.ads.uga.edu:valdovarnuovo/")

system("ln -f ../analisi_sing/ped.txt .")
system("ln -f ~/Documents/blupf90_family/UNIX/renumf90 .") 
system("ln -f ~/Documents/blupf90_family/UNIX/airemlf90 .")

  
  
#--------------------------------  
#   parte due lavoro nel testday
#---------------------------------
  
  
  
  
  

#------------------------------------------------------------------#
#  SECONDO BIT TRAITS PRODUZIONE LATTE (non testday)  E VARIANZA   #
#------------------------------------------------------------------#
# ripescare il vecchio dataset 
# rimuovo quelle che cambinao azienda durate la lattazione 

#---------------------------------------------------
# dataset 3
#-----------------------------------------------------------
#  LATTE TESTDAYY
#-----------------------------------------------------
setwd("/Users/enrico/Desktop/valdovar/");library(tidyverse)
setwd("LATTE")
getwd()
#--------->                      <------------#
#--------->   INTANTO VPR        <------------#
#--------->                      <------------#

#setwd("C:/Users/Mancin/Desktop/valdovar/")
dfd = readxl::read_excel("testday_ok5_VPR_soglie.xlsx")
# modello MATRFACT NL AZNDANNO MESE 
colnames(dfd)
dfl = as.data.frame(dfd %>% select(MATR,htdnl,ClGrav,apc_num,mpc_num,l1,W1,W2,W3,LATTEKG,GRASSOKG,PROTEINEKG,SCS))
head(dfd)

#---------- PRIMA LANCIO LE ANALISI NORMALMENTE 
save.image("caricatifilelatte.RData")
head(dfl)
data.table::fwrite(dfl,"latte.db",quote = FALSE, sep = " ",row.names = FALSE,col.names = FALSE)
system("sshpass -p abote4700  scp -P  9998 latte.db enrico.mancin@147.162.139.238:valdovarnuovo/")
system("sshpass -p abote4700  ssh -p 9998 enrico.mancin@147.162.139.238 'ls valdovarnuovo' ")

n            # se voglio trnere la riga cosi mi ricordo 

system("echo 'DATAFILE
latte.db
TRAITS
10
FIELDS_PASSED TO OUTPUT
       
WEIGHT(S)
 0
RESIDUAL_VARIANCE
 9.                                                                  
EFFECT
2  cross alpha #htdnl                                                          
EFFECT
3   cross alpha #clgrav  
EFFECT 
6 cov
NESTED 
4 cross alpha
EFFECT 
7 cov
NESTED 
4 cross alpha
EFFECT 
8 cov
NESTED 
4 cross alpha
EFFECT 
9 cov
NESTED 
4  cross alpha
EFFECT 
6 cov
NESTED 
5 cross alpha
EFFECT 
7 cov
NESTED 
5 cross alpha
EFFECT 
8 cov
NESTED 
5 cross alpha
EFFECT 
9 cov
NESTED 
5  cross alpha
EFFECT 
1 cross alpha
RANDOM
animal   # pruned ped!
OPTIONAL
pe
FILE
ped.txt
PED_DEPTH
15
(CO)VARIANCES
.118                                                      
OPTION sol se
OPTION solution 0 '> par_renum ") 


system("sshpass -p abote4700  scp -P  9998 par_renum  enrico.mancin@147.162.139.238:valdovarnuovo/")
system("ln -f ../analisi_sing/ped.txt .")
system("sshpass -p abote4700  scp -P  9998 ped.txt  enrico.mancin@147.162.139.238:valdovarnuovo/")
system("sshpass -p abote4700 ssh -p 9998 enrico.mancin@147.162.139.238 'ls valdovarnuovo' ")
system("ln -f ~/Documents/blupf90_family/UNIX/renumf90 .") 
system("ln -f ~/Documents/blupf90_family/UNIX/airemlf90 .")
system("sshpass -p abote4700  scp -P  9998 airemlf90  enrico.mancin@147.162.139.238:valdovarnuovo/")
system("sshpass -p abote4700  scp -P  9998 renumf90  enrico.mancin@147.162.139.238:valdovarnuovo/")
system("sshpass -p abote4700 ssh -p 9998 enrico.mancin@147.162.139.238 'ls valdovarnuovo' ")
system("sshpass -p abote4700 ssh -p 9998 enrico.mancin@147.162.139.238 'cd valdovarnuovo; chmod 775 ./*; echo par_renum | ./renumf90 ' ")
system("sshpass -p abote4700 ssh -p 9998 enrico.mancin@147.162.139.238 'cd valdovarnuovo; chmod 775 ./*; echo renf90.par | ./airemlf90 ' ")

#-------------------------------------------
#--------------------------------------------

system("sshpass -p enrico! scp  par_renum  enrico@dodo128.ads.uga.edu:valdovarnuovo/")
system("sshpass -p enrico! scp   ped.txt  enrico@dodo128.ads.uga.edu:valdovarnuovo/")
system("sshpass -p enrico! scp   latte.db enrico@dodo128.ads.uga.edu:valdovarnuovo/")
system("sshpass -p enrico! ssh -p 22 enrico@dodo128.ads.uga.edu 'cd valdovarnuovo; echo par_renum | renumf90' ")




# PARTE TRE CREO LA VARINAZA 
#--------------------------------------------------
#--------| creo daset latte   |-------------------# 

latte=dfd[ dfd$NL==1  , c("DIM","LATTEKG","NL","azienda_num","anno")]
cc= dfd[dfd$NL==1 & dfd$DIM> 90 & dfd$DIM<200 ,c("DIM","LATTEKG","MATR")]
plot(dfd$DIM,y=dfd$LATTEKG)
plot(x=cc$DIM,y=cc$LATTEKG)

# ggplot(data = cc, aes(x=DIM,y=LATTEKG,color=MATR))+
# geom_point()


pvar <- function(x) {
  sqrt(sum((x - mean(x))**2) / (length(x)+1))
}

#----- piccola prova tengo solo i testday 100 -200 ---------#
pp = dfd[dfd$DIM> 90 & dfd$DIM<200 ,]  # questo e un modo per farli intra lattazione
nrow(pp)
#----------cazzo pero adeso devo risclare tutto --------------#
latte$AZIENDAANNO = paste(latte$azienda_num,latte$anno)
latte %>% count (AZIENDAANNO)
latte = latte[latte$AZIENDAANNO=="1 2008"  , c("DIM","LATTEKG","NL")]
head(latte)

head()
# fare uno script che tenga conto di questo 
# devo rifare tutti i campi

pp %>% count(ClGrav) %>% arrange(n)      # RIMOVERE QUESTI ANIMLI
pp %>% count(count_HTDNL) %>% arrange(n)  # RIMUOERE QUESTI ANIMALI 
pp %>% count(mpc_num) %>% arrange(n)
pp %>% count(apc_num) %>% arrange(n)




# --|  modello genetico  per la varinza #-----|
# --------|   AZIENDA NL mESE  #--------------|
pp$animal_nl = paste(pp$MATR,pp$NL,sep="_")
pp=as.data.frame(pp)

#   almeno tre lattzioni in questi valori #
pp %>% count(animal_nl) %>% filter(n < 3) %>% nrow()
pp %>% count(animal_nl) %>% filter(n > 3) %>% nrow()

pp %>% count(animal_nl) %>% filter(n < 3)
pp %>% count(animal_nl) %>% filter(n > 3)


pp %>% nrow()
pp %>% count(animal_nl) %>% filter(n > 3) %>% nrow() / pp %>% nrow()

which_animal_nl = pp %>% count(animal_nl) %>% filter(n > 3) %>% pluck("animal_nl")
head(pp %>% count(animal_nl) %>% arrange(n));tail(pp %>% count(animal_nl) %>% arrange(n))

pp = pp %>%  filter( animal_nl %in% which_animal_nl) 
nrow(pp)
i="IT007000623649"
for (i in pp$animal_nl) {
  latte=as.numeric(pp[pp$animal_nl==i,"LATTEKG"])
  pp[pp$animal_nl==i,"var-latte"] =  pvar( latte)
}
hist(pp$`var-latte`,200)
save.image("Pulizia_varianze2.Rdata")
#dfd[dfd$MATR=="IT007000525034" & dfd$NL==3,]
getwd()
setwd("/Users/enrico/Desktop/valdovar/")
load("Pulizia_varianze2.Rdata")

# qua # 
head(pp %>% select("MATR","AZIENDA","NL","mpc_num","anno","var-latte" ),10)


pp = pp %>% select("MATR","AZIENDA","NL","mpc_num","anno","var-latte" )

pp$azndanno = paste(pp$AZIENDA,pp$anno,sep="_")
pp %>% count(azndanno) %>% arrange(n)
pp %>% nrow() 
pp %>% count(azndanno) %>% filter(n==1) %>% nrow()
which_herd =pp %>% count(azndanno) %>% filter(n==1) %>% pluck("azndanno")
pp = pp %>%  filter( !azndanno %in% which_herd) 
head(pp)
pp = pp %>% distinct()
nrow(pp)

just_to_check =  pp
just_to_check$MATRNL = paste( just_to_check$MATR,just_to_check$NL)
nrow(pp) == length(unique(just_to_check$MATRNL))

just_to_check %>% count(MATRNL) %>% arrange(desc(n))
# non  funziona perche ci sono almeno animali nella stessa lazione conmese diverso 
colnames(pp)
pp= pp %>% select("MATR","NL","azndanno","var-latte" )
pp = pp %>% distinct()
nrow(pp)

just_to_check =  pp
just_to_check$MATRNL = paste(just_to_check$MATR,just_to_check$NL)
just_to_check %>% count(MATRNL) %>% arrange(desc(n))
nrow(pp) == length(unique(just_to_check$MATRNL))
#---------# cambia anche l azienda anno #--------------#
# elimino questi valori 
pp$MATRNL = paste(just_to_check$MATR,just_to_check$NL)
just_to_check %>% count(MATRNL) %>% filter(n>1) %>% nrow()
which_herd =pp %>% count(MATRNL) %>% filter(n>1) %>% pluck("MATRNL")
pp = pp %>%  filter( !MATRNL %in% which_herd) 
head(pp)
pp = pp %>% distinct()


nrow(pp) == length(unique(pp$MATRNL))
head(pp,200)


save.image("secondo_tipodivarianza_2.RData")
#----------------------------------------------------------
# ---------|       start analisis   |----------------------
#----------------------------------------------------------
setwd("/Users/enrico/Desktop/valdovar/");library(tidyverse)
load("secondo_tipodivarianza_2.RData")
head(pp)
pp[is.na(pp)]=-999
#lancio = pp %>% select(  MATR,mpc_num, NL,azndanno,"var-latte")
lancio = pp %>% select(  MATR, NL,azndanno,"var-latte")
lancio = lancio %>% distinct()
pp_bit=lancio
head(lancio)
dir.create("analisi_longevita2");setwd("analisi_longevita2")
data.table::fwrite(lancio,"latte.db",quote = FALSE, sep = " ",row.names = FALSE,col.names = FALSE)
hist(lancio$`var-latte`,40)

system("echo 'DATAFILE
       latte.db
       TRAITS
       4
       FIELDS_PASSED TO OUTPUT
       
       WEIGHT(S)
       0
       RESIDUAL_VARIANCE
       9.00
       EFFECT    
       2  cross alpha
     #  EFFECT    
       3  cross alpha
    #   EFFECT
     # 4 cross alpha
       EFFECT
       1 cross alpha
       RANDOM
       animal   # pruned ped!
      OPTIONAL
       pe
       FILE
       ped.txt
       PED_DEPTH
       15
       (CO)VARIANCES
       0.118
       OPTION missing -999
       '  > par_renum ")
# non capisco perche mi dice che ci sono dei dati mancanti rocntrollo 

system("ln -f ../analisi_sing/ped.txt .")
system("ln -f ~/Documents/blupf90_family/renumf90 .") 
system("ln -f ~/Documents/blupf90_family/airemlf90 .")

system("sshpass -p enrico! scp  par_renum  enrico@dodo128.ads.uga.edu:valdovarnuovo/valdonuovo2")
system("sshpass -p enrico! scp  ped.txt  enrico@dodo128.ads.uga.edu:valdovarnuovo/valdonuovo2")
system("sshpass -p enrico! scp  latte.db enrico@dodo128.ads.uga.edu:valdovarnuovo/valdonuovo2")
system("sshpass -p enrico! ssh -p 22 enrico@dodo128.ads.uga.edu 'cd valdovarnuovo/valdonuovo2; ls' ")
system("sshpass -p enrico! ssh -p 22 enrico@dodo128.ads.uga.edu 'cd valdovarnuovo/valdonuovo2; echo par_renum | renumf90' ")

system("echo par_renum | ./renumf90")                                                            
system("echo renf90.par  | ./airemlf90")    
system("cat renf90.par") 

awk "NR == 4152" renf90.dat 
grep "5856" renadd03.ped 
grep "IT007000588931" latte.db 



Genetic variance(s) for effect  3       
11.140    
Genetic variance(s) for effect  4       
43.656    
Residual variance(s)
54.674  

11.140/(11.140    + 43.656  +54.674 ) # good  

#-------------------------------#
#   bitratits con lengevita     #
#-------------------------------#
setwd("/Users/enrico/Desktop/valdovar/");library(dplyr)
load("secondo_tipodivarianza_2.RData")
dir.create("analisi_longevita2_BIT");setwd("analisi_longevita2_BIT")
pp = pp %>% select(  MATR, NL,azndanno,"var-latte")
head(pp);head(pp)
DATA1 = DATA %>% filter(herdlife >= 0)
mix = plyr::rbind.fill(pp,DATA1) 
head(mix)  # elimino i censored perche sono meno 1 
mix[is.na(mix)]=-999
head(mix,40)
tail(mix,40)
# probela che zero non vuol dire missing 

x_1=c( "NL","azndanno")   # cambiare con azienda anno !!!!!!!
x_2=c("anno_aznd", "combcor", "latte_cor")
y_1=c("var-latte")
y_2=c("longfunz", "herdlife", "nparti")
#---------------------------------

i=y_1
ii=y_2[1]
head(pp_bit)
lancio = mix %>% dplyr::select(MATR,x_1,x_2,i,ii) %>% as.data.frame()
nrow(pp);nrow(DATA);nrow(lancio)
head(lancio)
#View(pp)
data.table::fwrite(lancio,"dbraw.txt",quote = FALSE, sep = " ",row.names = FALSE,col.names = FALSE)
#lancio[is.na(lancio)]=0
head(lancio)

system('echo \"DATAFILE
         dbraw.txt
         TRAITS
         7 8  
         FIELDS_PASSED TO OUTPUT
         
         WEIGHT(S)
         0
         RESIDUAL_VARIANCE
         1.49 0.8
         0.8 1.49 
         EFFECT    #-->PRIMA COSA EFFFETTI PER LA VARIABILITA <---#
         2  0 cross alpha
         EFFECT
         3  0  cross alpha   
         EFFECT 
         0  4  cross alpha 
         EFFECT 
         0  5  cov 
        # EFFECT 
        # 0  6  cov 
         EFFECT
         1 1 cross alpha
         RANDOM
         animal   # pruned ped!
        #OPTIONAL
       # pe
         FILE
         ped.txt
         PED_DEPTH
         15
         (CO)VARIANCES
         .118 .02
         .02  .118
         OPTION missing -999"\ > par.txt')
getwd()

system("ln -f ../analisi_sing/ped.txt .")
system("ln -f ~/Documents/blupf90_family/renumf90 .") 
system("ln -f ~/Documents/blupf90_family/airemlf90 .")


system("echo par.txt | ./renumf90")  
system("head dbraw.txt")  
#system("echo renf90.par  | ./airemlf90")    
#system("cat renf90.par") 


latte %>% count(azienda_num)
plot(x=latte$DIM,y=latte$LATTEKG)
hist(latte$PROTEINEKG)

df %>% group_by(RAZZA) %>% summarize (me=mean(KGLATEFF,na.rm=T) ,SD=sd(KGLATEFF,na.rm=T))

df = df %>% dplyr::select(MATR,LATT_PROGR_, Azienda_anno, ALLUSL,Inizio_Latt_mese ,Inizio_Latt_anno,KGLATEFF)
df=as.data.frame(df)
df = df[apply(df,1,function(x) !any(is.na(x))),]
nrow(df)

# RIMUOVO COAPI SINGOLI 
l = df %>% group_by(Azienda_anno) %>% tally()
togli = l[l$n<2,'Azienda_anno']
df=anti_join(df,togli)  
nrow(df) 

l = df %>% group_by(Inizio_Latt_mese) %>% tally()
l[l$n<2,]

df %>% group_by(LATT_PROGR_) %>% tally()
df = df[df$LATT_PROGR_ <= 9,]
nrow(df)

#------------------------
#  View(df)
#--------------------
df=df %>% dplyr::select(MATR,LATT_PROGR_,Azienda_anno,Inizio_Latt_mese,KGLATEFF)

#--------------------
head(df1) #e il datset buono
ddf=df1
#-----------------
head(df) # nuovo dataset 

jointdf = plyr::rbind.fill(df,df1) #
head(ddf)
head(df)

length(unique(ddf$MATR)) #MATRICOLE  DDF
length(unique(df$MATR)) #



nrow(ddf[ddf$MATR %in% c(df$MATR),])  #|  tutte le matricole sono incluse 
nrow(df[df$MATR %in% c(ddf$MATR),])  #|  tutte le matricole sono incluse 
# OVERLAPPING PERFETTO #

head(jointdf)
length(jointd)
tail(jointdf)
df = jointdf
length(unique(df$ALLUSL))
length(unique(df1$ALLUSL))
#----------------------------------------
# SONO ARRIVATO QUA !!!!!!!!!!!!1
#------------------------------------

ana = foreign::read.dbf("ANA0719.DBF")
ana = Filter(function(x)!all(is.na(x)), ana)
ana = as.data.frame(apply(ana,2,as.character))
ana$MATR = as.character(ana$MATR)
#-------------| FRO PED FOX PRO |----------------
df$MATRFACT = as.numeric(as.factor(df$MATR))  #in numeric 
animal_ped=ana[ana$MATR %in% c(df$MATR),]
head(animal_ped)
length(unique((df$MATR)))==nrow(animal_ped)
animal_ped=merge(animal_ped,df[,c("MATR","MATRFACT")],all.x = F,all.y = F)
head(animal_ped)

animal_ped=animal_ped %>% dplyr::select("MATR","MATRFACT","PADRE","MADRE")
animal_ped$MATRFACT=as.integer(animal_ped$MATRFACT);names(animal_ped)=c("Matr","Animal","Padre","Madre")
animal_ped=animal_ped %>%  distinct()
nrow(animal_ped)
print(getwd());foreign::write.dbf(animal_ped,"cows.dbf")
head(animal_ped)
#------------------------------------------
head(df);tail(df)
savejdf =  df
df$MATR  = as.numeric(as.factor(df$MATR ))
df$LATT_PROGR_ = as.numeric(as.factor(df$LATT_PROGR_))
df$Azienda_anno = as.numeric(as.factor(df$Azienda_anno))
df$ALLUSL = as.numeric(as.factor(df$ALLUSL))
head(df);tail(df)
df$KGLATEFF=df$KGLATEFF/1000 
# RICORDARMI IL OPTION -99

df[,c('ds')] = df[,c('ds')]/10

df = df %>% dplyr::select(MATR, LATT_PROGR_, Azienda_anno,  Inizio_Latt_mese,KGLATEFF  , ALLUSL,     ds,
                     VAR ,ETAMAX   ,   estd, mean_correto   )


df$estd =df$estd*10
head(df)
tail(df)


df$Azienda_anno=as.numeric(as.factor(df$Azienda_anno))
df$Inizio_Latt_anno=as.numeric(as.factor(df$Inizio_Latt_anno))
df$Inizio_Latt_mese=as.numeric(as.factor(df$Inizio_Latt_mese))

#-------------------------------
df[is.na(df)] = 0
df[is.na(df)] =-999  # nb decider
#---------------------------
head(df)
#df = df[,-(which((colnames(df)) %in% c("MATRFACT")))]

getwd()
cor(df$ds,df$estd)


apply(df,2,function(x) length(unique(x)))

data.table::fwrite("variancexmedia.dat",x = df,quote = F,sep = " ",row.names = F,col.names = F)

data.table::fwrite("variancexmedia.dat",x = df,quote = F,sep = " ",row.names = F)

View(animal_ped)


#----------------
head(dfl)
head(df)
setwd("/Users/enrico/Desktop/valdovar/")
load("secondo_tipodivarianza_2.RData")
dir.create("analisi_longevita2_BITHTD");setwd("analisi_longevita2_BITHTD")

df$mese_1p=as.character( df $mese_1p)
df$Aaznd_anno_born=as.character( df $Aaznd_anno_born)
mix = plyr::rbind.fill(dfl,df) 
head(mix)  # elimino i censored perche sono meno 1 
mix[is.na(mix)]=-999
head(mix,4)
tail(mix,4)
# probela che zero non vuol dire missing 
colnames(df)

x_1=c("htdnl" ,"ClGrav" ,"apc_num" ,"mpc_num","l1","W1","W2","W3" )
x_2=c( "aznd_anno_born", "mese_1p")   # cambiare con azienda anno !!!!!!!
y_1=c("LATTEKG" ,"GRASSOKG","PROTEINEKG","SCS"          )
y_2=c("ds")

#---------------------------------


i=y_1[1]
ii=y_2[1]
head(pp_bit)
lancio = mix %>% dplyr::select(MATR,x_1,x_2,i,ii) %>% as.data.frame()
nrow(pp);nrow(DATA);nrow(lancio)
lancio$htdnl=as.numeric(as.factor(lancio$htdnl))

head(lancio)
system("echo 'DATAFILE
      dbraw.txt
       TRAITS
       11 12 
       FIELDS_PASSED TO OUTPUT
       
       WEIGHT(S)
       0
       RESIDUAL_VARIANCE
       9. 5
       5   9
       EFFECT
       2 0  cross alpha #htdnl                                                          
       EFFECT
       3 0   cross alpha #clgrav  
       EFFECT 
        6 0 cov
       NESTED 
       4 cross alpha
       EFFECT 
       7 0 cov
       NESTED 
       4 cross alpha
       EFFECT 
       8 0 cov
       NESTED 
       4 cross alpha
       EFFECT 
       9 0 cov
       NESTED 
       4  cross alpha
       EFFECT 
       6 0 cov
       NESTED 
       5 cross alpha
       EFFECT 
       7 0 cov
       NESTED 
       5 cross alpha
       EFFECT 
       8 0 cov
       NESTED 
       5 cross alpha
       EFFECT 
       9 0 cov
       NESTED 
       5  cross alpha
       EFFECT 
       0 10 cross alpha
       EFFECT 
       0 11 cross  alpha
       EFFECT 
       1 1 cross alpha
       RANDOM
       animal   # pruned ped!
       OPTIONAL
       pe
       FILE
       ped.txt
       PED_DEPTH
       15
       (CO)VARIANCES
       .118 0.03
       .03   .118 
       OPTION sol se
       OPTION solution 0 
       OPTION missing -999
       OPTION max_string_readline 273536 
       OPTION alpha_size 52 '> par_renum ") 

lancio$W1= round(lancio$W1,3)
lancio$W2= round(lancio$W2,3)
lancio$W3= round(lancio$W3,3)
data.table::fwrite(lancio,"dbraw.txt",quote = FALSE, sep = " ",row.names = FALSE,col.names = FALSE)
#lancio[is.na(lancio)]=0
system("ln -f ../analisi_sing/ped.txt .")
#system("ln -f ~/Documents/blupf90_family/renumf90 .") 
#system("ln -f ~/Documents/blupf90_family/airemlf90 .")

system("sshpass -p enrico! scp  par_renum  enrico@dodo128.ads.uga.edu:valdovarnuovo/valdonuovo2")
system("sshpass -p enrico! scp  ped.txt  enrico@dodo128.ads.uga.edu:valdovarnuovo/valdonuovo2")
system("sshpass -p enrico! scp  dbraw.txt enrico@dodo128.ads.uga.edu:valdovarnuovo/valdonuovo2")
system("sshpass -p enrico! ssh -p 22 enrico@dodo128.ads.uga.edu 'cd valdovarnuovo/valdonuovo2; ls' ")
system("sshpass -p enrico! ssh -p 22 enrico@dodo128.ads.uga.edu 'cd valdovarnuovo/valdonuovo2; echo par_renum | renumf90' ")




#############################################################################################################
############################################################################################################
##########################################################################################################
#   dataset creato a mano  #
#--------------------------------------------------------------
#------------------------------#
#------ #    CREO DATA SET LONGEVITA   #-------#
#------------------------------#                       
#--------------------------------------------------------------#

# creo un data set ad hoc per la LONGEVITA'
setwd("~/Desktop/valdovar")
library(tidyverse)
ldf = readxl::read_excel("lattazioni2001_2010.xlsx")
head(ldf)
ldf = ldf %>% dplyr::select(MATR,LATT_PROGR_, Azienda_anno, ALLUSL,Inizio_Latt_mese ,Inizio_Latt_anno,KGLATEFF)
ldf=as.data.frame(ldf)
head(ldf)
ldf = ldf[complete.cases(ldf), ]
head(ldf)

# pulizia 1 
u1 = mean(ldf$KGLATEFF) + (sd(ldf$KGLATEFF)*4)  # chidere se ha senso ! 
u2 = mean(ldf$KGLATEFF) - (sd(ldf$KGLATEFF)*4)

df = ldf[ldf$KGLATEFF < u1 & ldf$KGLATEFF > u2,]


l = df %>% group_by(Azienda_anno) %>% tally()
togli = l[l$n<2,'Azienda_anno']  # 4 tengo !!!!!
df=anti_join(ldf,togli)  
length(unique(ldf$MATR))

# ulteriore pulizia chidere a cristina 

head(ldf)
# modello 
colnames(ldf)
ldf = ldf %>% select("MATR","LATT_PROGR_","Azienda_anno" ,   
                     "Inizio_Latt_mese" ,"KGLATEFF"   )
head(ldf)
str(ldf)
ldf$LATT_PROGR_      = as.factor(ldf$LATT_PROGR_) 
ldf$Azienda_anno     = as.factor(ldf$Azienda_anno) 
ldf$Inizio_Latt_mese = as.factor(ldf$Inizio_Latt_mese)   

#--------
#model = lm(KGLATEFF ~ LATT_PROGR_ +Azienda_anno + 	  Inizio_Latt_mese, ldf)
anova(model)
summary(model)$	r.squared
#-------------|  CALCOLOLO L'ETA MASSIMA 

ldf$LATT_PROGR_ = as.numeric(ldf$LATT_PROGR_) 

for (i in unique(ldf$MATR)){
  ldf[ldf$MATR==i,"ETAMAX"]=(max(ldf[ldf$MATR==i,"LATT_PROGR_"]))
}

#------------------------
ggplot(ldf, aes(x=ETAMAX)) + geom_histogram() # approsimato alla normlita 

ldf %>% group_by(ETAMAX)  %>% tally()


#------| analisi single 

ldf = ldf %>% dplyr::select( MATR, LATT_PROGR_ ,aznd_anno_born, mese_1p,ETAMAX)
head(ldf)
nrow(ldf)
ldf=ldf %>% distinct()
nrow(ldf)
ldf1=ldf
head(ldf)
gg=ldf1 %>% group_by(aznd_anno_born) %>% tally()
soli=c(gg[gg$n==1,"aznd_anno_born"])
soli = soli$aznd_anno_born
ldf1 = ldf1[!ldf1$aznd_anno_born %in% soli ,]
nrow(ldf1)



#---------------------------------------------------
# ldf1 #SAVE AS DATSET !!!!!!!!!!!!!!!!!!!!!!!!
#----------------------------------------------
# adesso faccio che anzienda annno primo parto ??
for ( i in unique(ldf$MATR)) {
  ldf[ldf$MATR == i,"aznd_anno_born"] =  ldf[ldf$MATR == i & order(ldf$LATT_PROGR_),"Azienda_anno"][1]
}
for ( i in unique(ldf$MATR)) {
  ldf[ldf$MATR == i,"mese_1p"] =  ldf[ldf$MATR == i & order(ldf$LATT_PROGR_),"Inizio_Latt_mese"][1]
}
head(ldf)

dir.create("analisi_long");setwd("analisi_long")
ldf = ldf %>% select(MATR,aznd_anno_born,mese_1p,ETAMAX)
ana = foreign::read.dbf("../ANA0719.DBF")[,1:3]
data.table::fwrite(ana,file = "ped.txt",sep = " ",row.names = FALSE,col.names = FALSE,quote = FALSE)

system('echo \"DATAFILE
       dbraw.txt
       TRAITS
       4
       FIELDS_PASSED TO OUTPUT
       
       WEIGHT(S)
       0
       RESIDUAL_VARIANCE
       1.49
       EFFECT
       2 cross alpha
       EFFECT
       3 cross alpha
       EFFECT
       1 cross alpha
       RANDOM
       animal   # pruned ped!
       FILE
       ped.txt
       PED_DEPTH
       15
       (CO)VARIANCES
       0.118 "\ > par.txt')



y = "ETAMAX"
x = c("MATR","aznd_anno_born", "mese_1p")
#i=y
lancio = ldf %>% dplyr::select(x,y) %>% as.data.frame() # change y with i
head(lancio)
summary(lancio[,i])
lancio = lancio[complete.cases(lancio), ]
lancio = lancio %>% distinct()
nrow(lancio)
head(lancio)
data.table::fwrite(lancio,"dbraw.txt",quote = FALSE,sep = " ",row.names = FALSE,col.names = FALSE)

View(lancio)
system("ln -f ../analisi_sing/ped.txt .")
system("ln -f ~/Documents/blupf90_family/renumf90 .") 
system("ln -f ~/Documents/blupf90_family/airemlf90 .")
system("chmod 755 ./*")
system("echo par.txt  | ./renumf90" ) 

#--------| se ci mette tanto lancio le analisi nel sever |---------------------#

# system("sshpass -p abote4700 scp outpurenum enrico.mancin@147.162.139.238:valdostana_vatianza") #caico i file nei sever
# system("sshpass -p abote4700 ssh -l enrico.mancin@147.162.139.238:valdostana_vatianza" "\" ./airemlf90 "\" ")

system("echo renf90.par  | ./airemlf90")    
0.29275/(2.8668+0.29275)  # mi viene fuori questa ereditabilita 

remlf90 <- readr::read_csv("airemlf90.log") 

# faccio lo stesso mettendo come covariata la produzione  

#-----------------------------------------------------------------------
# DATASET 2
#---------------------------------------------------------------------------------------
#  NB DATO CHE ID DATASET NON CORRSIPONDONO COME DUE DATSET LI PRENDO SEPARATI E LI TRATTO 
#                                    COME DUE DASET DIVERSI
#-----------------------------------------------------------------------------------------

# finire qua !!
ndf = merge(df,ldf,by.x="MATR" ,by.y="MATR" ,all.x=TRUE,all.y=TRUE)
head(ndf)
ndf[!is.na(ndf$ETAMAX),]



#--------------------------------------------------------------
postgibbs_samples <- read.table("~/Desktop/postgibbs_samples", quote="\"", comment.char="")

plot(x=postgibbs_samples$V2,y=postgibbs_samples$V4)
plot(x=postgibbs_samples$V2,y=postgibbs_samples$V5)
plot(x=postgibbs_samples$V2,y=postgibbs_samples$V6)
plot(x=postgibbs_samples$V2,y=postgibbs_samples$V7)
plot(x=postgibbs_samples$V2,y=postgibbs_samples$V8)
plot(x=postgibbs_samples$V2,y=postgibbs_samples$V9)

plot(x=postgibbs_samples$V2,y=(postgibbs_samples$V6/postgibbs_samples$V9))


plink <- read.csv("~/Desktop/plink.qassoc", sep="")

plot(plink$BP,plink$P)


snpB <- read.table("~/Desktop/snpB.txt", quote="\"", comment.char="")
bHat=as.numeric(snpB$V1)
plot(bHat, ylab='bayes b',type='p',cex=.5,col=1,main='Marker Effects')

# linear models
test <- read.table("~/Desktop/snp_lm.txt", quote="\"", comment.char="",header = TRUE)
test$pos=c(1:nrow(test))
gaston::manhattan(test)

# linear model 
a <- read.table("~/Desktop/snpB.qtxt", quote="\"", comment.char="",header = TRUE)
test2 <- read.table("~/Desktop/gaston.txt", quote="\"", comment.char="",header = TRUE)
test2$pos=c(a$POS)
test2$chr=c(a$CHR)
gaston::manhattan(test2,thinning = TRUE,)

(2000/100)
