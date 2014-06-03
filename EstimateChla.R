
#########Load required packages
libs<-c('robustbase','MASS') #list of packages to load

installLoad<-function(pck)#user defined function
{
  if(!pck%in%installed.packages()){install.packages(pck,repos="http://rweb.quant.ku.edu/cran/")}
  require(pck, character.only = TRUE)
}
lapply(libs,function(x) installLoad(x))  #Load/Install require packages
#########

#get the raw data
raw<- read.csv('http://www.plosone.org/article/fetchSingleRepresentation.action?uri=info:doi/10.1371/journal.pone.0081457.s001')
str(raw)  #View structure of the data

#get the NLA Chla data and add to df MRB1
  load('Chla.rda')
  MRB1<-merge(raw,Chla,by='WB_ID',all.x=T)

##############Add estimated Lake Concentrations of N&P  based on input/output model H6 to data.frame MRB1
    NLA<-MRB1[!is.na(MRB1$NLA_ID),]  #subset NLA data
  #nonlinear model for N
    nln<-nlrob(log10(TN) ~ log10(Nin/(1+(c1*hrt^c2*Zmean^c3*Nin^c4))),
           start=list(c1 = 3.0, c2 = .25, c3=.58,c4=.53),
           data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
    MRB1$TNvv<-10**predict(nln, newdata = MRB1) #get predicted values
  #nonlinear model for P
    nlp<-nlrob(log10(TP) ~ log10(Pin/(1+(c1*hrt^c2*Zmean^c3*Pin^c4))),
           start=list(c1 = 3.0, c2 = .25, c3=.58,c4=.53),
           data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
    MRB1$TPvv<-10**predict(nlp, newdata = MRB1) #get predicted values

####add field for NPR ratio 
    MRB1$NPR<-MRB1$TNvv/MRB1$TPvv

#find linear regression model for ChlaA based on predicted N and P outflow concentrations
  NLA<-MRB1[!is.na(MRB1$NLA_ID),]  #subset NLA data (again)
    #Try different combinations of TNvv, TPvv, and NPR
      summary(lm(log10(Chla) ~ log10(TNvv) + log10(TPvv)+NPR,data=NLA)) #R2=0.4982
      summary(lm(log10(Chla) ~ log10(TNvv) + log10(TPvv),data=NLA)) #R2=0.5  #TPvv not significant
      summary(lm(log10(Chla) ~ log10(TNvv),data=NLA)) #R2=0.4959  #best model 
      summary(lm(log10(Chla) ~ log10(TPvv),data=NLA)) #R2=0.4695
  lmChla<-lm(log10(Chla) ~ log10(TNvv),data=NLA) #R2=0.4959  #best model 

#predict Chla concentrations
  MRB1$predChla<-10**predict(lmChla, newdata = MRB1) #get predicted values


str(MRB1)

#Data Definitions for MRB1 (n=17,792)
# WB_ID:   unique lake identification number
# FlowM3_yr: (m3/yr) flow into and out of lake
# Ninput (kg/yr): Sum of nitrogen from SPARROW for all upstream flowlines plus the incremental load.
# Noutput: (kg/yr) Sparrow estimate of Nitrogen Load
# Pinput: (kg/yr) Sum of phosphorus from SPARROW for all upstream flowlines plus incremental load.
# Poutput: (kg/yr) Sparrow estimate of Phosphorus Load
# Volume: (m3) lake volume estimated from Zmax
# Zmax:  estimated Maximum depth of the lake
# Area: (m2) [AlbersAreaM] Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
# AlbersX: (m) X coordinate of lake Albers projection
# AlbersY: (m) Y coordinate of lake Albers projection
# NLA_ID: National Lake Assessment (NLA) Lake Identification Number
# SITE_TYPE: NLA Site Type; PROB_Lake=Lake Chosen using Probablistic Design; REF_Lake=Lake chosen for comparisons
# WGT_NLA: Sample Weight for NLA Lakes Chosen using Probablistic Design (SITE_TYPE=PROB_Lake)
# TN: (mg/l) Total Nitrogen from NLA
# TP: (mg/l) Total Phosphorus from NLA
# Nin:(mg/l) Nitrogen inflow load concentration from sparrow (Ninput/FlowM3_yr)
# Nout:(mg/l) Nitrogen outflow load concentration from sparrow (Noutput/FlowM3_yr)
# Pin:(mg/l) Phosphorus inflow load concentration from sparrow (Pinput/FlowM3_yr)
# Pout:(mg/l) Phosphorus outflow load concentration from sparrow (Poutput/FlowM3_yr)
# hrt:(yr) Hydraulic retention time for GIS estimated max depth and volume (Volume/FlowM3_yr)
# Zmean:(m) Mean Depth for GIS estimated max depth and volume (Volume/Area)
# TNvv: (mg/l) Predicted Total Nitrogen based on the nonlinear Eutromod model (H6) for NLA~SPARROW (nln)
# TPvv: (mg/l) Predicted Total Phosphorus based on the nonlinear Eutromod model (H6) for NLA~SPARROW (nlp)
# NPR: (unitless) TNvv/TPvv; Nitrogen Phosphorus Ratio (predicted)
# predChla: (mg/l) Chla predicted from TNvv

###################end of file