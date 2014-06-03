

#########Load required packages
libs<-c('robustbase') #list of packages to load

installLoad<-function(pck)#user defined function
{
  if(!pck%in%installed.packages()){install.packages(pck,repos="http://rweb.quant.ku.edu/cran/")}
  require(pck, character.only = TRUE)
}
lapply(libs,function(x) installLoad(x))  #Load/Install require packages
#########

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/Rwork.mdb")
Chla<- sqlQuery(con, "
SELECT tblJoinNLAID_WBID.WB_ID, tblNLA_WaterQualityData.Chla
FROM tblNLA_WaterQualityData INNER JOIN tblJoinNLAID_WBID ON tblNLA_WaterQualityData.SITE_ID = tblJoinNLAID_WBID.NLA_ID
WHERE (((tblNLA_WaterQualityData.VISIT_NO)=1) AND ((tblJoinNLAID_WBID.Rank)=1));
")
close(con)
str(Chla)

#convert Chla from ug/l to mg/l
  Chla$Chla<-Chla$Chla/1000

#Method detection limit Update
  #all values for Chla$ChlA greater than the MDL of 0.1ug/l

###Filters:
  #Visit_No=1 for NLA data
  #Rank=1 for NLA_ID (to avoid lakes with multiple NLA_ID's

#Data Definitions "Chla
  #'data.frame':  156 obs. of  2 variables:
    # WB_ID: unique lake identification number
    # Chla: (ug/l):  Chorophyll A concentration in waterbody from NLA

save(MRB1,Chla,file='Chla.rda')
