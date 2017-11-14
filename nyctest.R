nyc=read.csv("F:/test/NYPD_Complaint_Data_Historic.csv",head=T)
hist(as.numeric(nyc$BORO_NM))
hist(as.numeric(nyc$PREM_TYP_DESC))
require(ggplot2)
plot(nyc$Longitude[nyc$BORO_NM=="QUEENS"],nyc$Latitude[nyc$BORO_NM=="QUEENS"],col="red",xlim=c(-74.3,-73.6),ylim=c(40.4,41))
points(nyc$Longitude[nyc$BORO_NM=="MANHATTAN"],nyc$Latitude[nyc$BORO_NM=="MANHATTAN"],col="blue")
points(nyc$Longitude[nyc$BORO_NM=="BRONX"],nyc$Latitude[nyc$BORO_NM=="BRONX"],col="green")
points(nyc$Longitude[nyc$BORO_NM=="BROOKLYN"],nyc$Latitude[nyc$BORO_NM=="BROOKLYN"],col="pink")
points(nyc$Longitude[nyc$BORO_NM=="STATEN ISLAND"],nyc$Latitude[nyc$BORO_NM=="STATEN ISLAND"],col="yellow")
legend("topright",c("QUEENS","MANHATTAN","BRONX","BROOKLYN","STATEN ISLAND"),pch=20,col=c("red","blue","green","pink","yellow"))


sum(is.na(nyc$CMPLNT_FR_DT))
[1] 0
date=as.Date(nyc$CMPLNT_FR_DT,format="%m/%d/%Y")
byyear <- cut(date, breaks = "year") 
select=which(byyear=="2015-01-01")
nycnew=nyc[select,]

nrow(nycnew)
[1] 468576
nrow(nyc)
[1] 1048575
plot(nycnew$Longitude[nycnew$BORO_NM=="QUEENS"],nycnew$Latitude[nycnew$BORO_NM=="QUEENS"],col="red",xlim=c(-74.3,-73.6),ylim=c(40.4,41))
points(nycnew$Longitude[nycnew$BORO_NM=="MANHATTAN"],nycnew$Latitude[nycnew$BORO_NM=="MANHATTAN"],col="blue")
points(nycnew$Longitude[nycnew$BORO_NM=="BRONX"],nycnew$Latitude[nycnew$BORO_NM=="BRONX"],col="green")
points(nycnew$Longitude[nycnew$BORO_NM=="BROOKLYN"],nycnew$Latitude[nycnew$BORO_NM=="BROOKLYN"],col="pink")
points(nycnew$Longitude[nycnew$BORO_NM=="STATEN ISLAND"],nycnew$Latitude[nycnew$BORO_NM=="STATEN ISLAND"],col="yellow")
legend("topright",c("QUEENS","MANHATTAN","BRONX","BROOKLYN","STATEN ISLAND"),pch=20,col=c("red","blue","green","pink","yellow"))
sum(is.na(nyc$CMPLNT_FR_DT))



ggplot(data= nycnew, aes(x=Longitude,y=Latitude)) + geom_point(color = "darkred",stat = "sum")
ggplot(data= nycnew, aes(x=Longitude,y=Latitude)) + geom_point(aes(colour =BORO_NM),alpha=0.01)
ggplot(data= nycnew, aes(x=Longitude,y=Latitude)) + geom_point(colour ="red",alpha=0.01)


nycQS=subset(nycnew,BORO_NM=="QUEENS")
nycMH=subset(nycnew,BORO_NM=="MANHATTAN")
nycBR=subset(nycnew,BORO_NM=="BRONX")
nycBL=subset(nycnew,BORO_NM=="BROOKLYN")
nycSI=subset(nycnew,BORO_NM=="STATEN ISLAND")



tt <- strptime(paste("2001-01-01", nyc$CMPLNT_FR_TM), format="%Y-%m-%d %H:%M")
nyc$Timenew=format(round(tt, units="hours"), format="%H:%M")
hist(as.numeric(nyc$Timenew))

hist(as.numeric(factor(nyc$Timenew,levels=c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00"))))

nyc$timelevel=factor(nyc$Timenew,levels=c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00"))
p1=ggplot(nyc, aes(x=timelevel)) +geom_histogram(stat="count")
p2=ggplot(nycQS, aes(x=timelevel)) +geom_histogram(stat="count")
p3=ggplot(nycMH, aes(x=timelevel)) +geom_histogram(stat="count")
p4=ggplot(nycBR, aes(x=timelevel)) +geom_histogram(stat="count")
p5=ggplot(nycBL, aes(x=timelevel)) +geom_histogram(stat="count")
p6=ggplot(nycSI, aes(x=timelevel)) +geom_histogram(stat="count")



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1,p2,p3,p4,p5,p6,cols=3)



p1=ggplot(nyc, aes(x=LAW_CAT_CD)) +geom_histogram(stat="count")
p2=ggplot(nycQS, aes(x=LAW_CAT_CD)) +geom_histogram(stat="count")
p3=ggplot(nycMH, aes(x=LAW_CAT_CD)) +geom_histogram(stat="count")
p4=ggplot(nycBR, aes(x=LAW_CAT_CD)) +geom_histogram(stat="count")
p5=ggplot(nycBL, aes(x=LAW_CAT_CD)) +geom_histogram(stat="count")
p6=ggplot(nycSI, aes(x=LAW_CAT_CD)) +geom_histogram(stat="count")
multiplot(p1,p2,p3,p4,p5,p6,cols=3)
