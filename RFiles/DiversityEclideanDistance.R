# the top five countries of birth in diversity

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

countries=c(data[[1]]$Col3[seq(181,193,3)])
for(i in 2:34){
  countries=c(countries,data[[i]]$Col3[seq(181,193,3)])
}
countries=unique(countries)
countries=gsub(" ","",countries)
countries=gsub("\\(","",countries)
countries=gsub("\\)","",countries)
cList=list()
for(i in 1:length(countries)){
  cList=append(cList,0)
}
names(cList)=countries

for(c in countries){
  for(i in 1:34){
    cList[[c]][i]=0
  }
}

for(i in 1:34){
  for(j in seq(181,193,3)){
    country=data[[i]]$Col3[j]
    country=gsub(" ","",gsub("\\(","",gsub("\\)","",country)))
    cList[[country]][i]=data[[i]]$Col3[j+2]
  }
}

matrixData=cList[[countries[1]]]
for(c in countries){
  if(c!=countries[1]){
    matrixData=cbind(matrixData,cList[[c]])
  }
  
}
colnames(matrixData)=countries


class(matrixData)="numeric"
distance=dist(matrixData)

mds1 = cmdscale(distance, k = 2)

# plot
plot(mds1[,1], mds1[,2], type = "n", xlab = "", ylab = "", axes = FALSE,
     main = "Top five countries of birth(diversity) MDS")
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


locationMatrix=cbind(x,y)
gDistanceVector=as.vector(as.matrix(dist(locationMatrix)))
distanceVector=as.vector(as.matrix(distance))
gdd=cbind(gDistanceVector,distanceVector)
colnames(gdd)=c("geoDistance","similarityScore")
gdd=gdd[order(gdd[,1]),]
plot(gdd[,1],gdd[,2],type="p",pch=16,xlab="geographic distance",ylab="similarity measure score",main="Diversity similarity vs geographic distance")


gdd=as.data.frame(gdd)
fit <- lm(similarityScore ~ geoDistance, data=gdd)
lines(gdd$geoDistance,fitted(fit),col="green",lwd="4")
#average
#gdd[,1]=round(gdd[,1],digits=0)
#gdd=as.data.frame(gdd)
#gdd=aggregate(.~geoDistance, FUN=mean, data=gdd)
#plot(gdd$geoDistance,gdd$similarityScore,type="l",pch=16,xlab="geographic distance",ylab="similarity measure score",main="Average diversity similarity vs geographic distance")


