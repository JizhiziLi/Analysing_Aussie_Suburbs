# age structure in 2012
library(XLConnect)
library(ecodist)
library(labdsv)
library(ape)
library(ade4)
library(smacof)
data= list()
fileNames=c('Ascot-Vale-Suburb - XLSX.xlsx',
            'Braybrook-Suburb - XLSX.xlsx', 'Craigieburn-Suburb - XLSX.xlsx',
            'Croydon-Suburb - XLSX.xlsx', 'Fawkner-Suburb - XLSX.xlsx',
            'Footscray-Suburb - XLSX.xlsx','Glenroy-Suburb - XLSX.xlsx', 
            'Malvern-East-Suburb - XLSX.xlsx', 'Malvern-Suburb - XLSX.xlsx',
            'Melbourne-Airport-Suburb - XLSX.xlsx', 'Mentone-Suburb - XLSX.xlsx',
            'Moorabbin-Suburb - XLSX.xlsx', 'Mordialloc-Suburb - XLSX.xlsx', 
            'Murrumbeena-Suburb - XLSX.xlsx', 'Noble-Park-Suburb - XLSX.xlsx',
            'North-Melbourne-Suburb - XLSX.xlsx', 'Northcote-Suburb - XLSX.xlsx',
            'Parkville-Suburb - XLSX.xlsx', 'Pascoe-Vale-South-Suburb - XLSX.xlsx',
            'Port-Melbourne-Suburb - XLSX.xlsx','Prahran-Suburb - XLSX.xlsx',
            'Somerville-Suburb - XLSX.xlsx','Sorrento-Suburb - XLSX.xlsx', 
            'South-Melbourne-Suburb - XLSX.xlsx','South-Yarra-Suburb - XLSX.xlsx',
            'Springvale-Suburb - XLSX.xlsx','St-Andrews-Beach-Suburb - XLSX.xlsx',
            'St-Kilda-East-Suburb - XLSX.xlsx','St-Kilda-Suburb - XLSX.xlsx',
            'St-Kilda-West-Suburb - XLSX.xlsx','Toorak-Suburb - XLSX.xlsx',
            'Tyabb-Suburb - XLSX.xlsx','Waterways-Suburb - XLSX.xlsx',
            'Windsor-Suburb - XLSX.xlsx')
suburbs=gsub("-Suburb - XLSX.xlsx","",fileNames)
path="/Users/yunwang/myFile/unimelb/machinelearning/assignment2/project2_data/"
for(i in 1:34){
  data[[i]]=readWorksheet(loadWorkbook(paste(path,fileNames[i],sep=""))
                          ,header=F,sheet=2)
}

#MDS and plot
matrixData=rbind(as.numeric(gsub(",", "", data[[1]]$Col3[seq(29,51,2)])),
                 as.numeric(gsub(",", "", data[[2]]$Col3[seq(29,51,2)])))
for(i in 3:34){
  matrixData=rbind(matrixData,as.numeric(gsub(",", "",data[[i]]$Col3[seq(29,51,2)])))
}

#scale() not used 
distance=dist(matrixData)
mds1 = cmdscale(distance, k = 2)
# plot
plot(mds1[,1], mds1[,2], type = "n", xlab = "", ylab = "", axes = FALSE,
     main = "Age structure in 2012")
text(mds1[,1], mds1[,2], labels=suburbs, cex = 0.8, xpd = TRUE,col=c(1:34))


#geographic distances
x=vector()
y=vector()
for(i in 1:34){
  s=strsplit(data[[i]]$Col3[5]," ")
  s=s[[1]]
  d=as.numeric(gsub("km", "", s[1]))
  if(s[2]=="NW"){ 
    x[i]=-d*sqrt(2)/2
    y[i]=-x[i]
  }
  if(s[2]=="NE"){
    x[i]=d*sqrt(2)/2
    y[i]=x[i]
  }
  if(s[2]=="SE"){
    x[i]=d*sqrt(2)/2
    y[i]=-x[i]
  }
  if(s[2]=="SW"){
    x[i]=-d*sqrt(2)/2
    y[i]=x[i]
  }
  if(s[2]=="E"){
    x[i]=d
    y[i]=0
  }
  if(s[2]=="W"){
    x[i]=-d
    y[i]=0
  }
  if(s[2]=="N"){
    x[i]=0
    y[i]=d
  }
  if(s[2]=="S"){
    x[i]=0
    y[i]=-d
  }
  if(s[2]=="WNW"){
    x[i]=-d*cos(pi/8)
    y[i]=d*sin(pi/8)
  }
  if(s[2]=="NNW"){
    x[i]=-d*sin(pi/8)
    y[i]=d*cos(pi/8)
  }
  if(s[2]=="NNE"){
    x[i]=d*sin(pi/8)
    y[i]=d*cos(pi/8)
  }
  if(s[2]=="ENE"){
    x[i]=d*cos(pi/8)
    y[i]=d*sin(pi/8)
  }
  if(s[2]=="ESE"){
    x[i]=d*cos(pi/8)
    y[i]=-d*sin(pi/8)
  }
  if(s[2]=="SSE"){
    x[i]=d*sin(pi/8)
    y[i]=-d*cos(pi/8)
  }
  if(s[2]=="SSW"){
    x[i]=-d*sin(pi/8)
    y[i]=-d*cos(pi/8)
  }
  if(s[2]=="WSW"){
    x[i]=-d*cos(pi/8)
    y[i]=-d*sin(pi/8)
  }
  
}




#plot(x,y,xlab="km",ylab="km",main="Map")
#text(x, y, labels=suburbs, cex = 0.8, xpd = TRUE,col=c(1:34))





locationMatrix=cbind(x,y)
gDistanceVector=as.vector(as.matrix(dist(locationMatrix)))
distanceVector=as.vector(as.matrix(distance))
gdd=cbind(gDistanceVector,distanceVector)
colnames(gdd)=c("geoDistance","similarityScore")
gdd=gdd[order(gdd[,1]),]
plot(gdd[,1],gdd[,2],type="p",pch=16,xlab="geographic distance",ylab="similarity measure score",main="Age structure similarity vs geographic distance")

#average
#gdd[,1]=round(gdd[,1],digits=0)
#gdd=as.data.frame(gdd)
#gdd=aggregate(.~geoDistance, FUN=mean, data=gdd)
#plot(gdd$geoDistance,gdd$similarityScore,type="l",pch=16,xlab="geographic distance",ylab="similarity measure score",main="Average age structure similarity vs geographic distance")

gdd=as.data.frame(gdd)
fit <- lm(similarityScore ~ geoDistance, data=gdd)
lines(gdd$geoDistance,fitted(fit),col="green",lwd="4")



