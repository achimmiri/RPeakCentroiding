cenPeaks <- function(mzV,intenV, MZ_TOLERANCE=DEFAULT_MZ_TOLERANCE )
{
  
################################
mzV1<-c(0,mzV)
###########################
###########################
av<-abs(mzV[2:length(mzV)] - mzV[1:(length(mzV)-1)]) <= MZ_TOLERANCE
###########################
###########################
Ntes2=mzV[which(!av)]
Ntes3=intenV[which(!av)]
###########################
CVL=split(which(av), cumsum(c(1, diff(which(av)) != 1)))
###########################
###########################
NMZV1=c()
INDV1=c()
############################
TV=c()
for(i in 1:length(CVL))
{
  LV=CVL[i]
  LV1=purrr::flatten_dbl(LV)
  LV2=tail(LV1,n=1)
  LV3=LV2+1
  NLV=unname(unlist(c(LV,LV3), recursive = FALSE))
  TV=append(TV,NLV)
  #############################
  #############################
  weightsV=intenV[NLV]
  dataV=mzV[NLV]
  weight_sum = sum(weightsV)
  #######################################
  #######################################

  NDV=sum(dataV)/length(dataV)
  NIV=sum(weightsV)/length(weightsV)

  NMZV1=append(NMZV1,NDV)
  INDV1=append(INDV1,NIV)
  #######NIV=sum(weightsV)/length(weightsV)


}## end of for loop

NCVLIN=seq(1,length(mzV))
DIFFIN=setdiff(NCVLIN,TV)

for(i in 1:length(DIFFIN))
{
  Val=DIFFIN[i]
  weightsV=intenV[Val]
  dataV=mzV[Val]

  NMZV1=append(NMZV1,dataV)
  INDV1=append(INDV1,weightsV)

}


N1tes2=NMZV1
N1tes3=INDV1


N2tes2=N1tes2[order(N1tes2)]
N2tes3=N1tes3[order(N1tes2)]

N2tes4=N2tes2[!is.na(N2tes2)]
N2tes5=N2tes3[!is.na(N2tes2)]

return(list(N2tes4,as.numeric(N2tes5)))

}

