plotDescriptionCols = c()


drawPoints<-function(g,IV,DV,result,colindex=1,off=0){
  showRawData<-(allScatter=="all")
  
  if (colindex==1)
          {  col<- plotcolours$descriptionC
          xoff=0
          barwidth=1}
  else
          { 
          col <-plotDescriptionCols[[colindex-1]]
          xoff=-0.25+off*0.5
          barwidth=0.5}

  col2="yellow"
  x<-result$ivplot
  y<-result$dvplot
  
  hypothesisType=paste(IV$type,DV$type,sep=" ")
  
  dotSize<-7
  if (length(x)>100) {
    dotSize<-3.5*sqrt(100/length(x))
  }
  switch (hypothesisType,
          "Interval Interval"={
            pts<-data.frame(IV=x,DV=y)
            if (allScatter=="corr") {showRawData<-TRUE}
          },
          "Categorical Interval"={
            pts<-data.frame(IV=x+xoff,DV=y)
          },
          "Interval Categorical"={
            bin_breaks<-c(-Inf,seq(-1,1,length.out=10)*fullRange*IV$sd+IV$mu,Inf)
            dens1<-hist(result$iv[result$dv==DV$cases[2]],breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
            dens2<-hist(result$iv,breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
            densities<-dens1$counts/dens2$counts
            bins<-dens1$mids
            barwidth<-bins[3]-bins[2]
            pts<-data.frame(x=bins[2:(length(bins)-1)]+xoff/2,y=densities[2:(length(bins)-1)])
            # g<-g+geom_line(data=pts,aes(x=x,y=y),colour="black",fill="black")
            g<-g+geom_bar(data=pts,aes(x=x-barwidth/8,y=1-y),stat="identity",width=barwidth/4,colour="black",fill=col2)
            g<-g+geom_bar(data=pts,aes(x=x+barwidth/8,y=y),stat="identity",width=barwidth/4,colour="black",fill=col)
            xv<-c()
            yv<-c()
            for (i in 1:(length(dens1$counts)-1)){
              y<-dens1$counts[i]
              if (y>0){
              xv<-c(xv,rep(bins[i],y)+runif(y,min=-1,max=1)/2*barwidth/4)
              yv<-c(yv,runif(y,min=0,max=1)*densities[i]*0.95+0.05)
              }
            }
            pts<-data.frame(IV=xv+xoff/2+barwidth/8,DV=yv)
          },
          "Categorical Categorical"={
            b<-drawCatPositions(IV$ncats)
            xv<-as.numeric(result$iv)-1
            yv<-as.numeric(result$dv)-1
            yz<-c()
            for (i in 1:IV$ncats){ 
              yz<-c(yz,mean(yv[xv==(i-1)]))
            }
            x<-b[xv+1]+runif(length(xv),min=-0.2,max=0.2)
            y<-runif(length(xv),min=0,max=1)*yz[xv+1]*0.9+0.05
            pts<-data.frame(x=x+xoff,y=y)
        }
  )
  if (showRawData) {
    if (colindex>=2)
      g<-g+geom_point(data=pts,aes(x=IV,y=DV,fill=names(plotDescriptionCols)[colindex-1]),shape=21, colour = "black", alpha=0.25, size =dotSize)
    else
      g<-g+geom_point(data=pts,aes(x=IV,y=DV),shape=21, colour = "black", fill=col, alpha=0.25, size =dotSize)
  }
  g  
}

drawDescription<-function(IV,IV2,DV,effect,design,result){

  g<-ggplot()

  if (is.null(IV2)){
    g<-drawPoints(g,IV,DV,result,1)
    g<-drawPrediction(result$IVs,IV2,result$DVs,result,design,1,g)
  } else{
    
    switch (IV2$type,
            "Interval"={
              col<-c( plotcolours$descriptionC1, plotcolours$descriptionC2)
              names(col)<-c(paste(IV2$name,"<median",sep=""), paste(IV2$name,">median",sep=""))
              plotDescriptionCols <<- col
              
              Ivals<-IV$vals
              Dvals<-DV$vals
              rho<-result$rIV+seq(-1,1,length.out=2)*result$rIVIV2DV

              for (i in 1:2){
                switch (i,
                        use<-result$iv2<median(result$iv2),
                        use<-result$iv2>=median(result$iv2)
                        )
                result1<-result
                result1$iv<-result$iv[use]
                result1$dv<-result$dv[use]
                result1$ivplot<-result$ivplot[use]
                result1$dvplot<-result$dvplot[use]
                result1$rIV<-rho[i]
                
                result1$IVs$vals<-Ivals[use]
                result1$DVs$vals<-Dvals[use]
                g<-drawPrediction(result1$IVs,NULL,result1$DVs,result1,design,i+1,g)
                g<-drawPoints(g,result1$IVs,result1$DVs,result1,i+1,(i-1)/(2-1))
              }

              g<-g+scale_fill_manual(name=IV2$name,values=plotDescriptionCols)
            },
            "Categorical"={
              plotDescriptionCols <<- c()
              cols<-c()
              for (i in 1:IV2$ncats){
                off<-(i-1)/(IV2$ncats-1)
                col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
                cols<- c(cols,rgb(col[1]/255,col[2]/255,col[3]/255))
              }
              names(cols)<-IV2$cases
              plotDescriptionCols <<- cols
              
              Ivals<-IV$vals
              Dvals<-DV$vals
              rho<-result$rIV+seq(-1,1,length.out=IV2$ncats)*result$rIVIV2DV

              for (i in 1:IV2$ncats){
                use<-result$iv2==IV2$cases[i]
                
                result1<-result
                result1$iv<-result$iv[use]
                result1$dv<-result$dv[use]
                result1$ivplot<-result$ivplot[use]
                result1$dvplot<-result$dvplot[use]
                result1$rIV<-rho[i]
                
                result1$IVs$vals<-Ivals[use]
                result1$DVs$vals<-Dvals[use]
                g<-drawPrediction(result1$IVs,NULL,result1$DVs,result1,design,2+(i-1)/(IV2$ncats-1),g)
                g<-drawPoints(g,IV,DV,result1,i+1,(i-1)/(IV2$ncats-1))
              }
              g<-g+scale_fill_manual(name=IV2$name,values=plotDescriptionCols)
            }
    )
  }
  
  g
  
}
