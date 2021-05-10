rm(list=ls())

library(rmarkdown)
library(RODBC)
library(plyr)
library(gplots)#for function plotCI (making graphs with confidence interval)
library(Hmisc)
library(stringr)#for function "str-replace"
library(pbapply)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(hrbrthemes)

#set working directory

#fullpath=knitr::current_input(dir = TRUE)
fullpath=rstudioapi::getActiveDocumentContext()$path
path=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir = path)

#define db
db="enex_access.accdb"


############
###define functions
######################################################################
#replace  "NA" by zero in a dataframe
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

###Suivi annuel des points
##lister points avec fiche manquante/complets
diag.points=function(db,cantonnements,secteurs,years){
  
  library(dplyr)
  ##process
  if(T){
    channel<-odbcConnectAccess2007(db)#define acces to db
    #import data's
    dat_point=sqlQuery(channel = channel, query="SELECT cantons.name AS cantonnement, enex_point.placette, Format(enex_fiche.Date,'yyyy') AS annee, enex_fiche.enc_detruit, enex_fiche.ex1_detruit, enex_fiche.ex2_detruit, enex_fiche.id, enex_fiche.cause_perturb, enex_dico_perturb.name AS cause
                       FROM cantons INNER JOIN (enex_dico_perturb INNER JOIN (enex_point INNER JOIN enex_fiche ON enex_point.id = enex_fiche.id_point) ON enex_dico_perturb.id = enex_fiche.cause_perturb) ON cantons.id = enex_point.id_canton;")
    
    #select only desired cantonnements 
    if(!missing(cantonnements)){dat_point=subset(dat_point,cantonnement%in%cantonnements)}
    
    
    #points sans encodage détruit (aucune année)
    pl.dest=subset(dat_point,enc_detruit==1|ex1_detruit+ex2_detruit==2)$placette
    piedat_dest=c("détruit",as.numeric(length(pl.dest)))
    ue.sains=dat_point[-which(dat_point$placette%in%pl.dest),]
    
    #fiche ue*anne (mesure 0 - 1)
    pl_fct=ddply(ue.sains,.(annee,placette,cantonnement),nb=length(placette),summarise)#sum nb of fiche/year
    
    pl_fct=pl_fct%>%spread(annee,nb)#spread (one column by year)
    pl_fct=na.zero(pl_fct)#replace na's by 0
    colnames(pl_fct)[-c(1,2)]=paste('y',colnames(pl_fct)[-c(1,2)],sep="")#non numeric names of col years
    colnames(pl_fct)[dim(pl_fct)[2]]="nodata"#name of col nodata
    
    #nombre de fiches doublons
    #manage cases non fiches for one year
    y.f=do.call(rbind,strsplit(colnames(pl_fct)[-c(1,2,which(colnames(pl_fct)=="nodata"))],"y"))[,2]#years present
    year.ok=which(y.f%in%years)#index of correct year columns
    year.wrong=which(!y.f%in%years)#index of nodata & uncorrect years
    
    supp=do.call(cbind,lapply(pl_fct[,2+year.ok],function(x)ifelse(x>1,x-1,0)))#replace by nb of supplementary fiches
    supp=apply(supp,1,sum)+apply(as.data.frame(pl_fct[,2+year.wrong]),1,sum)+pl_fct$nodata#add 1 if year out of range
    pl_fct$supp=supp
    
    
    #nombre d'années manquantes
    miss=do.call(cbind,lapply(pl_fct[,2+year.ok],function(x)ifelse(x==0,1,0)))
    miss=apply(miss,1,sum)+length(years)-length(year.ok)
    pl_fct$miss=miss
    
    #doubles-manquants
    pl_fct$diff=pl_fct$miss-pl_fct$supp
    
    #ventilation des points complets/doublons/années manquantes
    pl_fct$fiche=""
    pl_fct$fiche=ifelse(pl_fct$miss==0,"complet","?")
    pl_fct$fiche=ifelse(pl_fct$fiche=="?"&pl_fct$miss!=0&pl_fct$diff==0,"manquant = doublons",pl_fct$fiche)
    pl_fct$fiche=ifelse(pl_fct$fiche=="?"&pl_fct$miss!=0&pl_fct$diff>0,"manquant > doublons",pl_fct$fiche)
    pl_fct$fiche=ifelse(pl_fct$diff<0,"manquant < doublons",pl_fct$fiche)
    
    piedat=ddply(pl_fct,.(fiche),nb=length(fiche),summarise)
    
    
    #jointure des fiches détruites
    piedat=rbind(piedat,piedat_dest)
    piedat$nb=as.numeric(piedat$nb)
    
    #ventilation causes de destructions
    ue_dest=subset(dat_point,enc_detruit==1|ex1_detruit+ex2_detruit==2)
    ue_dest=ddply(ue_dest,.(cause),nb=length(cause_perturb),summarise)
    levels(ue_dest$cause)[levels(ue_dest$cause)=="NULL"]="non encodé"
    
    #nombre de placettes requises
    nb_requis=dim(pl_fct)[1]*3
    
    #liste placettes requises
    list_requis=as.vector(outer(pl_fct$placette,years,paste,sep="_"))
  }
  
  ##graphs 
  if(T){
    
    #ventilation des destructions encodées
    #rem : plus de fiches peuvent avoir une cause destruction manquante car si un! exclos détruit: pas comptabilisé ici...
    # g2=ggpie(ue_dest,x="nb",fill="cause",palette="jco",lab.pos="out",lab.font = list(color = "white"))+
    #   theme(legend.position="right")+
    #   labs(fill="Causes de destruction")
    ue_dest$cause=ordered(ue_dest$cause,levels=ue_dest$cause[order(ue_dest$nb,decreasing=F)])#order decreasing
    g2<-ue_dest %>%
      ggplot( aes(x=cause, y=nb) ) +
      geom_segment( aes(x=cause ,xend=cause, y=0, yend=nb), color="grey") +
      geom_point(size=3, color="#69b3a2") +
      coord_flip() +
      #theme_ipsum() +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none"
      ) +
      xlab("") +
      ylab("Nombre de points Enclos-Exclos détruits")
    
    
    
    # library(TraMineR)
    # library(sunburstR)
    # library(pipeR)
    
    
    
    
    
    #ventilation des points par fiches manquantes/doubles
    # g1=ggpie(piedat,x="nb",fill="fiche",palette="jco",lab.pos="out",lab.font = list(color = "white"))+
    #   theme(legend.position="right")+
    #   labs(fill="Point Enclos-Exclos")
    # 
    piedat$fiche=ordered(piedat$fiche,levels=piedat$fiche[order(piedat$nb,decreasing=F)])#order decreasing
    g1<-piedat %>%
      ggplot( aes(x=fiche, y=nb) ) +
      geom_segment( aes(x=fiche ,xend=fiche, y=0, yend=nb), color="grey") +
      geom_point(size=3, color="#69b3a2") +
      coord_flip() +
      #theme_ipsum() +
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position="none"
      ) +
      xlab("") +
      ylab("Nombre de points Enclos-exclos")
    
    #donut gg
    if(T){
      dico.col=as.data.frame(cbind(fiche=c("complet","manquant > doublons","manquant = doublons","manquant < doublons","détruit"),colors=c("#33FF00","#CC6600","#FF9933","#CC0033","#3300CC")))    # colors & labels
      df=piedat
      df$labs <- paste0(df$fiche, " \n(", round(df$nb/sum(df$nb)*100,1), "%)")
      
      df= left_join(df,dico.col,by="fiche")   # colors & labels
      g1.1<-ggdonutchart(df, "nb", label = "labs",lab.adjust = 0,#plot
                         lab.pos = "out", lab.font = "black",
                         fill = "labs", color = "black",
                         palette = df$colors[order(df$labs)])+ 
        theme(legend.position="none")
    }
    
    #donut googlevis
    if(T){
      library(googleVis)
      g1.2<-gvisPieChart(df[,c(1,2)],numvar="nb",options=list(width=500,
                                                              height=500,
                                                              slices="{0: {offset: 0.2},
                          1: {offset: 0.2},
                          2: {offset: 0.2}}", title='Encodage des fiches',
                                                              legend='none',
                                                              pieSliceText='label',
                                                              pieHole=0.6),
                         chartid="doughnut")
      
    }
  }
  
  #destruction sangliers : exclos seulement (auraient du être continués)
  subset(dat_point,enc_detruit==0&ex1_detruit+ex2_detruit==2&cause_perturb==2)[,which(colnames(dat_point)%in%c("id","placette","annee"))]
  return(list(g1,g2,plot(g1.1),plot(g1.2),pl_fct))  
  #ue douteuses, à enlever?? ==> inspecter remarques...
}



#nb ue measured in each sect
channel<-odbcConnectAccess2007(db)
dat=sqlQuery(channel = channel, query="SELECT secteurs.name, enex_point.placette,   Format(enex_fiche.date, 'yyyy') As annee
FROM secteurs INNER JOIN (enex_point INNER JOIN enex_fiche ON enex_point.id = enex_fiche.id_point) ON secteurs.id = enex_point.id_secteur;")
str(dat)
dat=ddply(dat,.(annee,name),nb_ue=length(placette),summarise)
dat_min=ddply(subset(dat,annee%in%c(2016:2018)),.(name),nb_ue=min(nb_ue),summarise)
dat_min$name=factor(dat_min$name,dat_min$name[order(dat_min$nb_ue,decreasing=T)])

#nb ue existantes
pts=diag.points(db=db,years=c(2016:2018))[5]
pts_complete=pts[[1]][pts[[1]]$fiche=="complet",c(1,2)]
ddply(pts_complete,.(cantonnement),nb_ue=length(unique(placette)),summarise)

ggplot(data=dat,aes(x=annee,y=nb_ue))+
  geom_point()+
  facet_wrap(~name, scale="free")

ggplot(dat_min)+
  geom_bar(mapping=aes(x=name,y=nb_ue), stat = "identity")+ 
  theme(axis.text.x=element_text(angle=90))

###height
height=function(secteur,graph,sapling,sp.group, sp.name){
#list of functionning points
if(T){
  #import data's
  #import data's
  dat_point=sqlQuery(channel = channel, query="SELECT secteurs.name, enex_point.placette,  Format(enex_fiche.date, 'yyyy') , enex_fiche.enc_detruit, enex_fiche.ex1_detruit, enex_fiche.ex2_detruit, enex_fiche.id, enex_fiche.cause_perturb, enex_dico_perturb.name
                     FROM enex_dico_perturb INNER JOIN (secteurs INNER JOIN (enex_point INNER JOIN enex_fiche ON enex_point.id = enex_fiche.id_point) ON secteurs.id = enex_point.id_secteur) ON enex_dico_perturb.id = enex_fiche.cause_perturb;")
  colnames(dat_point)[colnames(dat_point)=="Expr1002"]="annee"
  
  #points sans encodage détruit (aucune année)
  pl.dest=subset(dat_point,enc_detruit==1|ex1_detruit+ex2_detruit==2)$placette
  ue.sains=dat_point[-which(dat_point$placette%in%pl.dest),]
  
  #fiche ue*anne (mesure 0 - 1)
  pl_fct=ddply(ue.sains,.(annee,placette),nb=length(placette),summarise)#sum nb of fiche/year
  pl_fct=pl_fct%>%spread(annee,nb)#spread (one column by year)
  pl_fct=na.zero(pl_fct)#replace na's by 0
  colnames(pl_fct)[-1]=paste('y',colnames(pl_fct)[-1],sep="")#non numeric names of col years
  colnames(pl_fct)[dim(pl_fct)[2]]="nodata"#name of col nodata
  
  #nombre de fiches doublons
  supp=do.call(cbind,lapply(pl_fct[,3:5],function(x)ifelse(x>1,x-1,0)))
  supp=apply(supp,1,sum)+pl_fct$nodata+pl_fct$y2015
  pl_fct$supp=supp
  
  #nombre d'années manquantes
  miss=do.call(cbind,lapply(pl_fct[,3:5],function(x)ifelse(x==0,1,0)))
  miss=apply(miss,1,sum)
  pl_fct$miss=miss
  
  #doubles-manquants
  pl_fct$diff=pl_fct$miss-pl_fct$supp
  
  #ventilation des points complets/doublons/années manquantes
  pl_fct$fiche=ifelse(pl_fct$miss==0,"complet","?")
  pl_fct$fiche=ifelse(pl_fct$fiche=="?"&pl_fct$miss!=0&pl_fct$diff==0,"manquant = doublons",pl_fct$fiche)
  pl_fct$fiche=ifelse(pl_fct$fiche=="?"&pl_fct$miss!=0&pl_fct$diff>0,"manquant > doublons",pl_fct$fiche)
  pl_fct$fiche=ifelse(pl_fct$fiche=="?"&pl_fct$diff<0,"manquant < doublons",pl_fct$fiche)
  pl_fct$fiche=ifelse(pl_fct$diff<0,"manquant < doublons",pl_fct$fiche)
  pttokeep=subset(pl_fct,fiche=="complet")$placette
  
  #exclos1 ou exclos2 lacking on year?
    #subset non destroyed ues
    ex1_2=subset(ue.sains,placette%in%pttokeep,select=c(placette,annee,ex1_detruit,ex2_detruit))#points non detruit
    #count destroyed ex1 & ex2 by placette
    ex1=subset(ex1_2,select=c("placette","annee","ex1_detruit"))
    rownames(ex1)<-NULL
    ex1=ex1 %>% spread(annee,ex1_detruit)
    ex1$miss=apply(ex1[,c(2:dim(ex1)[2])],1,sum)
    ex2=subset(ex1_2,select=c("placette","annee","ex2_detruit"))
    rownames(ex2)<-NULL
    ex2=ex2 %>% spread(annee,ex2_detruit)
    ex2$miss=apply(ex2[,c(2:dim(ex2)[2])],1,sum)
    #list placettes with functionning ex1
    fct_ex1=subset(ex1,miss==0)$placette
    fct_ex2=subset(ex2,miss==0)$placette
  }


##hmoy obj (NO!!)
if(F){
#query data
h.moy.obj1=sqlQuery(channel = channel, query="SELECT enex_point.placette, Format(enex_fiche.date, 'yyyy'), Avg(enex_ht_ess_ob1.enclos) AS MoyenneDeenclos, Avg(enex_ht_ess_ob1.exclos1) AS MoyenneDeexclos1, Avg(enex_ht_ess_ob1.exclos2) AS MoyenneDeexclos2, enex_ht_ess_ob1.essence, secteurs.name_conseil
FROM secteurs INNER JOIN ((enex_point INNER JOIN enex_fiche ON enex_point.id = enex_fiche.id_point) INNER JOIN enex_ht_ess_ob1 ON enex_fiche.id = enex_ht_ess_ob1.id_fiche) ON secteurs.id = enex_point.id_secteur
GROUP BY enex_point.placette, enex_fiche.date, enex_ht_ess_ob1.essence, secteurs.name_conseil;")

str(h.moy.obj1)
colnames(h.moy.obj1)=c("placette","annee","en","ex1","ex2","sp","conseil")

h.moy.obj1=subset(h.moy.obj1,placette%in%pttokeep)
#gather en, ex1 & ex2 (1 column)
h.moy.obj1=gather(h.moy.obj1,key="sue",value="hmoy",3:5)

h.moy.obj1$annee=as.factor(h.moy.obj1$annee)

#hauteur chene
h.moy.obj1sp=subset(h.moy.obj1,sp==1)
  #boxplot hmoy obj1~annee+sue+
ggplot(data=h.moy.obj1,aes(x=annee,y=hmoy,fill=sue))+
  geom_boxplot()+
  facet_wrap(~conseil, scale="free")
}
  
###height
###obj&compagne

##hmoy dominant sapling
#query height of all sapling measured (obj1 & obj2 & comp)
if(T){
#obj1
obj1=sqlQuery(channel = channel, query="SELECT enex_point.placette, enex_point.id_secteur, Format(enex_fiche.date, 'yyyy') AS annee, enex_ht_ess_ob1.essence, enex_ht_ess_ob1.enclos, enex_ht_ess_ob1.exclos1, enex_ht_ess_ob1.exclos2
FROM (enex_point INNER JOIN enex_fiche ON enex_point.id = enex_fiche.id_point) INNER JOIN enex_ht_ess_ob1 ON enex_fiche.id = enex_ht_ess_ob1.id_fiche;")
obj1=gather(obj1,key="sue",value="ht",5:7)

#obj2
obj2=sqlQuery(channel = channel, query="SELECT enex_point.placette, enex_point.id_secteur, Format(enex_fiche.date, 'yyyy') AS annee, enex_ht_ess_ob2.essence, enex_ht_ess_ob2.enclos, enex_ht_ess_ob2.exclos1, enex_ht_ess_ob2.exclos2
FROM enex_point INNER JOIN (enex_fiche INNER JOIN enex_ht_ess_ob2 ON enex_fiche.id = enex_ht_ess_ob2.id_fiche) ON enex_point.id = enex_fiche.id_point;")

obj2=gather(obj2,key="sue",value="ht",5:7)

#sp compagnes
comp=sqlQuery(channel = channel, query="SELECT enex_point.placette, enex_point.id_secteur, Format(enex_fiche.date, 'yyyy') AS annee, enex_ht_ess_com.type, enex_ht_ess_com.essence, enex_ht_ess_com.ht
FROM (enex_point INNER JOIN enex_fiche ON enex_point.id = enex_fiche.id_point) INNER JOIN enex_ht_ess_com ON enex_fiche.id = enex_ht_ess_com.id_fiche;")
colnames(comp)[colnames(comp)=="type"]="sue"
comp=comp[,c((1:3),5,4,6)]

#table all species
ht_tbl=rbind(obj1,obj2,comp)
ht_tbl=subset(ht_tbl,placette%in%pttokeep)#keep only functionning points(enclos+one of exclos)

  #keep only ex1, ex2 if ex1 destroyed
  ht_tblex1=subset(ht_tbl,sue=="exclos1"&placette%in%fct_ex1)#ex1 functionning
  ht_tblex2=subset(ht_tbl,sue=="exclos2")[-which(ht_tbl$placette%in%fct_ex1),]#ex2 where ex1 not functionning
  ht_tbl=rbind(subset(ht_tbl,sue=="enclos"),ht_tblex1,ht_tblex2)
  ht_tbl$sue=ifelse(ht_tbl$sue=="exclos2","exclos1",ht_tbl$sue)#change name of sue exclos2
  ht_tbl$sue=ifelse(ht_tbl$sue=="exclos1","exclos",ht_tbl$sue)#all the same name = exclos  

#group by sp if argument present
if(!missing(sp.group)){
l<-pblapply(1:length(sp.group),function(x){
  d=subset(ht_tbl,essence%in%sp.group[[x]])
  d$essence=sp.name[[x]]
  d
  })
ht_tbl=do.call(rbind,l)}

  
#select the 3 heighest of sue each year. 
  #if less than 3 : 0 for others.
  #if sp not present in one ue : nothing
if(sapling=="highest"){
ht_tbl$sbpl=paste(ht_tbl$placette,ht_tbl$annee,ht_tbl$sue)
sbpl=unique(ht_tbl$sbpl)
dom<-pblapply(sbpl,function(x){
subtbl=na.zero(subset(ht_tbl,sbpl==x))#select one sue*year 
subtbl=subtbl[order(subtbl$ht,decreasing=T),]#order ht decreasing
if(sum(subtbl$ht)==0){return(NULL)}#if only 0 height saplings : return nothing
subtbl=subtbl[1:3,]#select 3 first ht decreasing
subtbl
})
dom=do.call(rbind,dom)
dom$id=paste(dom$placette,dom$sue,dom$essence)

#mean height by sp/sue/y. 
#if not present in point : nothing
#if present in one sue of the point : height of seedling, 0 if absent
hmoy_dom=ddply(dom,.(placette,id_secteur,annee,sue,essence),ht=mean(ht),nb=length(ht),summarise)


  }

#select the 3 heighest of each sp in each sue
  #if less than 3 : 0 for others.
  #if sp not present in one ue : nothing
  ##for sp usually isolated : better just the only highest should be better...
if(sapling=="eachsp"){
  
  #select 3 highest of each sp in each sue*year
  #if sp absent of one sue*year : nothing
  #
  ht_tbl$sbpl=paste(ht_tbl$placette,ht_tbl$annee,ht_tbl$sue,ht_tbl$essence)
  sbpl=unique(subset(ht_tbl,id_secteur%in%secteur)$sbpl)#id of groups (only in secteurs)
  dom<-pblapply(sbpl,function(x){
    subtbl=na.zero(subset(ht_tbl,sbpl==x))#select one sue*year 
    subtbl=subtbl[order(subtbl$ht,decreasing=T),]#order ht decreasing
    if(sum(subtbl$ht)==0){return(NULL)}#if only 0 height saplings : return nothing
    subtbl=subtbl[1:3,]#select 3 first ht decreasing
        subtbl
  })
  dom=do.call(rbind,dom)
  dom$id=paste(dom$placette,dom$sue,dom$essence)
  
  #mean height by sp/sue/y. 
  #if not present in point : nothing
  #if present in one sue of the point : height of seedling, 0 if absent
  hmoy_dom=ddply(dom,.(placette,id_secteur,annee,sue,essence),ht=mean(ht),summarise)
}

#boxplot heights (by sp)
if(graph=="yearly"){
  dat=hmoy_dom
  dat$essence=as.factor(dat$essence)
  dat$essence=droplevels(dat$essence)
  dat$grp=paste(dat$sue,dat$essence)
  dat=na.zero(dat)
  
  g=ggplot(dat,aes(x=essence,y=ht,group=grp,fill=sue))+
    geom_boxplot(varwidth=T)+
    facet_wrap(~annee,ncol=1)+
    labs(x="Espèces",y="Hauteur (cm)")
    }

#boxplot of growth (by sp)
if(graph=="growth"){
  
  #calculate growth #if a sp present in one sue in 2018 and not 2016 : NA for 2016 changed in 0
  end=subset(hmoy_dom,annee==2018)
  beg=subset(hmoy_dom,annee==2016)
  growth=merge(end,beg,by=c("placette","essence","sue"),all=T)
  growth=na.zero(growth)
  growth$growth=growth$ht.x-growth$ht.y
  growth=growth[,c(4,1,3,2,10)]
  colnames(growth)=c("id_secteur","placette","sue","essence","growth")
  
  #adapt data for the graph 
  dat=growth
  dat$essence=as.factor(dat$essence)
  dat$essence=droplevels(dat$essence)
  dat$grp=paste(dat$sue,dat$essence)
  
  #graph
  g=ggplot(dat,aes(x=essence,y=growth,group=grp,fill=sue))+
    geom_boxplot(varwidth=T)+
    labs(x="Espèces",y="Croissance en hauteur (cm)")
}
  return(g)
}
}

height(secteur=c(49:53),graph="yearly",sapling="eachsp")
height(secteur=c(30:35),graph="growth",sapling="eachsp",sp.group=list(1,3,c(11,18,22:25)),sp.name=list("chêne","hêtre","bouleaux et saules"))
height(secteur=c(30:35),graph="yearly",sapling="eachsp",sp.group=list(1,3,c(11,18,22:25)),sp.name=list("chêne","hêtre","bouleaux et saules"))

height(secteur=c(30:35),graph="growth",sapling="highest",sp.group=list(1,3,c(11,18,22:25)),sp.name=list("chêne","hêtre","bouleaux et saules"))
height(secteur=c(30:35),graph="yearly",sapling="highest",sp.group=list(1,3,c(11,18,22:25)),sp.name=list("chêne","hêtre","bouleaux et saules"))
height(secteur=35,graph="yearly",sapling="highest",sp.group=list(1,3,c(11,18,22:25)),sp.name=list("chêne","hêtre","bouleaux et saules"))
height(secteur=35,graph="yearly",sapling="highest")

height(secteur=35,graph="yearly",sapling="highest",sp.group=list(1,3,c(11,18,22:25)),sp.name=list("chêne","hêtre","bouleaux et saules"))
height(secteur=31,graph="yearly",sapling="highest",sp.group=list(1,3,c(11,18,22:25)),sp.name=list("chêne","hêtre","bouleaux et saules"))

#Elsenborn
height(secteur=31,graph="yearly",sapling="highest")
height(secteur=31,graph="yearly",sapling="highest",sp.group=list(1,3,c(11,18,22:25,27,28)),sp.name=list("chêne","hêtre","bouleaux et saules"))

#ciernion
height(secteur=64,graph="yearly",sapling="highest",sp.group=list(1,3,c(11,18,22:25),14),sp.name=list("chêne","hêtre","bouleaux et saules","charme"))
height(secteur=64,graph="growth",sapling="highest",sp.group=list(1,3,c(11,18,22:25),14),sp.name=list("chêne","hêtre","bouleaux et saules","charme"))
height(secteur=64,graph="yearly",sapling="eachsp")

#divers
height(secteur=20,graph="yearly",sapling="eachsp")
height(secteur=13,graph="yearly",sapling="eachsp")
height(secteur=52,graph="yearly",sapling="eachsp")
height(secteur=52,graph="growth",sapling="eachsp")
height(secteur=71,graph="yearly",sapling="eachsp")
height(secteur=72,graph="yearly",sapling="eachsp")
height(secteur=74,graph="yearly",sapling="eachsp")
height(secteur=75,graph="yearly",sapling="eachsp")

#spa
height(secteur=c(56,57,58,59),graph="yearly",sapling="eachsp")
height(secteur=c(56,57,58,59),graph="yearly",sapling="highest",sp.group=list(1,3,c(11,18,22:25),14),sp.name=list("chêne","hêtre","bouleaux et saules","charme"))
height(secteur=c(56),graph="yearly",sapling="highest",sp.group=list(1,3,c(11,18,22:25),14),sp.name=list("chêne","hêtre","bouleaux et saules","charme"))
height(secteur=c(57),graph="yearly",sapling="highest",sp.group=list(1,3,c(11,18,22:25),14),sp.name=list("chêne","hêtre","bouleaux et saules","charme"))
height(secteur=c(58),graph="yearly",sapling="highest",sp.group=list(1,3,c(11,18,22:25),14),sp.name=list("chêne","hêtre","bouleaux et saules","charme"))
height(secteur=c(59),graph="yearly",sapling="highest",sp.group=list(1,3,c(11,18,22:25),14),sp.name=list("chêne","hêtre","bouleaux et saules","charme"))

###recouvrement
##par strate
#évolution rec strate 
channel<-odbcConnectAccess2007(db)#define acces to db
d.rec=sqlQuery(channel = channel, query="SELECT enex_point.placette, enex_point.id_secteur, Format(enex_fiche.Date,'yyyy') AS annee, enex_re_strate.strate, enex_dico_rec_midcl.milieu_classe AS enclos, enex_dico_rec_midcl_1.milieu_classe AS exclos1, enex_dico_rec_midcl_2.milieu_classe AS exclos2
FROM (enex_point INNER JOIN enex_fiche ON enex_point.id = enex_fiche.id_point) INNER JOIN (((enex_re_strate INNER JOIN enex_dico_rec_midcl ON enex_re_strate.enclos = enex_dico_rec_midcl.code) INNER JOIN enex_dico_rec_midcl AS enex_dico_rec_midcl_1 ON enex_re_strate.exclos1 = enex_dico_rec_midcl_1.code) INNER JOIN enex_dico_rec_midcl AS enex_dico_rec_midcl_2 ON enex_re_strate.exclos2 = enex_dico_rec_midcl_2.code) ON enex_fiche.id = enex_re_strate.id_fiche;
               ")

d.rec=gather(d.rec,key="sue",value="rec",5:7)#regroup columns rec enclos, rec exclos
  ggplot(data=subset(d.rec,strate=="Strate ligneuse"),aes(x=as.factor(annee),y=rec,fill=sue))+
    geom_boxplot()+
    facet_wrap('id_secteur')
  
  ggplot(data=subset(d.rec,strate=="Strate herbacée (hors fougère aigle et mousses)"),aes(x=as.factor(annee),y=rec,fill=sue))+
    geom_boxplot()+
    facet_wrap('id_secteur')

#ligneux et herbacé 1! panneau
  channel<-odbcConnectAccess2007(db)#define acces to db
  d.rec=sqlQuery(channel = channel, query="SELECT enex_point.placette, enex_point.id_secteur, Format(enex_fiche.Date,'yyyy') AS annee, enex_re_strate.strate, enex_dico_rec_midcl.milieu_classe AS enclos, enex_dico_rec_midcl_1.milieu_classe AS exclos1, enex_dico_rec_midcl_2.milieu_classe AS exclos2
                 FROM (enex_point INNER JOIN enex_fiche ON enex_point.id = enex_fiche.id_point) INNER JOIN (((enex_re_strate INNER JOIN enex_dico_rec_midcl ON enex_re_strate.enclos = enex_dico_rec_midcl.code) INNER JOIN enex_dico_rec_midcl AS enex_dico_rec_midcl_1 ON enex_re_strate.exclos1 = enex_dico_rec_midcl_1.code) INNER JOIN enex_dico_rec_midcl AS enex_dico_rec_midcl_2 ON enex_re_strate.exclos2 = enex_dico_rec_midcl_2.code) ON enex_fiche.id = enex_re_strate.id_fiche;
                 ")
  d.rec=gather(d.rec,key="sue",value="rec",5:7)#regroup columns rec enclos, rec exclos
  ggplot(data=subset(d.rec,id_secteur%in%c(secteur)),aes(x=as.factor(sue),y=rec,fill=strate))+
    geom_boxplot()+
    facet_wrap('annee',ncol=1)
  

##par essence ligneuse
channel<-odbcConnectAccess2007(db)#define acces to db
rec.sp=sqlQuery(channel = channel, query="SELECT enex_point.placette, enex_point.id_secteur, Format([enex_fiche].[Date],'yyyy') AS annee, enex_re_esp.espece, enex_dico_rec_midcl.milieu_classe AS enclos, enex_dico_rec_midcl_1.milieu_classe AS exclos1, enex_dico_rec_midcl_2.milieu_classe AS exclos2
FROM enex_point INNER JOIN (enex_fiche INNER JOIN (((enex_re_esp INNER JOIN enex_dico_rec_midcl ON enex_re_esp.enclos = enex_dico_rec_midcl.code) INNER JOIN enex_dico_rec_midcl AS enex_dico_rec_midcl_1 ON enex_re_esp.exclos1 = enex_dico_rec_midcl_1.code) INNER JOIN enex_dico_rec_midcl AS enex_dico_rec_midcl_2 ON enex_re_esp.exclos2 = enex_dico_rec_midcl_2.code) ON enex_fiche.id = enex_re_esp.id_fiche) ON enex_point.id = enex_fiche.id_point;
                ")
rec.sp
rec.sp=gather(rec.sp,key="sue",value="rec",5:7)#regroup columns rec enclos, rec exclos
ggplot(data=subset(rec.sp,id_secteur%in%c(secteur)),aes(x=as.factor(sue),y=rec,fill=espece))+
  geom_boxplot()+
  facet_wrap('annee',ncol=1)



###nb semis
semis=sqlQuery(channel = channel, query="SELECT enex_point.placette, enex_point.id_secteur, Format([enex_fiche].[Date],'yyyy') AS annee, enex_nb_ess.essence, enex_dico_semis.Max As enclos, enex_dico_semis_1.Max As exclos1, enex_dico_semis_2.Max As exclos2
FROM (enex_point INNER JOIN enex_fiche ON enex_point.id = enex_fiche.id_point) INNER JOIN (((enex_nb_ess INNER JOIN enex_dico_semis ON enex_nb_ess.enclos = enex_dico_semis.code) INNER JOIN enex_dico_semis AS enex_dico_semis_1 ON enex_nb_ess.exclos1 = enex_dico_semis_1.code) INNER JOIN enex_dico_semis AS enex_dico_semis_2 ON enex_nb_ess.exclos2 = enex_dico_semis_2.code) ON enex_fiche.id = enex_nb_ess.id_fiche;")
semis=gather(semis,key="sue",value="rec",5:7)#regroup columns rec enclos, rec exclos

ggplot(data=subset(semis,id_secteur%in%c(secteur)),aes(x=as.factor(essence),y=rec,fill=sue))+
  geom_boxplot()+
  facet_wrap('annee',ncol=1)

###hauteur semi-ligneux
cadran=sqlQuery(channel = channel, query="SELECT secteurs.code, enex_point.placette, Format([enex_fiche].[Date],'yyyy') AS annee, enex_ht_cadran.cadran, enex_ht_cadran.enclos, enex_ht_cadran.exclos1, enex_ht_cadran.exclos2, enex_fiche.esp_cadran_enc, enex_fiche.esp_cadran_ex1, enex_fiche.esp_cadran_ex2
FROM secteurs INNER JOIN (enex_point INNER JOIN (enex_fiche INNER JOIN enex_ht_cadran ON enex_fiche.id = enex_ht_cadran.id_fiche) ON enex_point.id = enex_fiche.id_point) ON secteurs.id = enex_point.id_secteur;
                ")
do.call(cbind,lapply(unique(cadran$placette),function(x){d=subset(cadran, placette==x)
d1=subset(d,)))


cadran=gather(cadran,key="sue",value="hauteur",5:7)#regroup columns rec enclos, rec exclos



#diversité semis dominants
#diversité alpha
#diversité gamma
                 
