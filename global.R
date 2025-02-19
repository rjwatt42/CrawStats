switches<-list(do_explore=TRUE,do_files=FALSE)

maincolours<-list(windowC="#002D40",panelC="#005E86",graphC="#BFECFF")
subpanelcolours<-list(hypothesisC="#FFD6DB",designC="#F6DFBD",simulateC="#CFF8CF",exploreC="#DDBBDD",filesC="#EEBB88",likelihoodC="#DDDDBB")
panelcolours<-list(hypothesisC="#F3B6BB",designC="#E6CFAD",simulateC="#ADE6AD",exploreC="#BB99BB",filesC="#CC9988",likelihoodC="#BBBB99")

plotcolours<-list(sampleC="#FFCC00",descriptionC="#FF8833",
                  descriptionC1="#FF5533",descriptionC2="#CCBB33",
                  infer_sigC="#22FF00",infer_nsigC="#FF2222",
                  infer_err="#333333",infer_nerr="#00CCFF")

# maincolours<-list(windowC="#002D40",panelC="#005E86",graphC="#FFFFFF")
# plotcolours<-list(sampleC="#FFFFFF",descriptionC="#FFFFFF",
#                   descriptionC1="#DDDDDD",descriptionC2="#333333",
#                   infer_sigC="#22FF00",infer_nsigC="#FF2222",
#                   infer_err="#333333",infer_nerr="#00CCFF")

controlLabel_fontSize=8
controlInput_fontSize=8
helpText_fontSize=7
report_fontSize=3.5

localStyle=paste("font-size:", format(controlLabel_fontSize), "pt;font-weight:bold;text-align: right;",sep="")
helpStyle=paste("font-size:", format(helpText_fontSize), "pt;line-height:75%;margin:0px;margin-top:-6px;padding:0px;", "color:", maincolours$panelC, ";",sep="")

IV<-list(name="IV",type="Interval",mu=0,sd=1,ncats=2,cases="C1,C2",proportions="1,1")
IV2<-list(name="none",type="Interval",mu=0,sd=1,ncats=2,cases="D1,D2",proportions="1,1")
DV<-list(name="DV",type="Interval",mu=0,sd=1,ncats=2,cases="E1,E2",proportions="1,1")

effect<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0)

design<-list(sN=42, sMethod="Random" ,sIV1Use="Between",sIV2Use="Between", 
             sRangeOn=FALSE, sIVRange=c(-3,3), sDVRange=c(-3,3), 
             sHeteroscedasticity=0,
             sDependence=0, sOutliers=0, sClustering=0
             )    

evidence<-list(rInteractionOn=TRUE,showType="direct")

fullRange<-3
allScatter<-"all"
inferIdentify<-"effectsize"
inferValue<-0
oldInferIdentity<-inferIdentify

result<-list()
