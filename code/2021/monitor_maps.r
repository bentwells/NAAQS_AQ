###############################################
## Generate monitor maps for NAAQS AQ documents
###############################################

## Set up working environment
cat("Creating .png images of monitor maps...")
colors <- c("gray50","blue3","yellow3","red3")
load(paste("NAAQS_AQ/data/",curr.year,"/monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))
load(paste("NAAQS_AQ/data/",curr.year,"/nonreg_monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))
if (!dir.exists(paste("NAAQS_AQ/monitor_maps",curr.year,sep="/"))) { 
  dir.create(paste("NAAQS_AQ/monitor_maps",curr.year,sep="/")) 
}

## CO monitor map
co.monitors$site <- substr(co.monitors$id,1,9)
co.monitors$class <- mapply(function(type,network) ifelse(network == "NEAR ROAD","NEAR ROAD",
  ifelse(network == "NCORE","NCORE",ifelse(type == "SLAMS","SLAMS","SPM/OTHER"))),
  co.monitors$monitor_type,co.monitors$network)
t <- ddply(co.monitors,"site",summarize,latitude=latitude[1],
  longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  NE=colors[3],NC=colors[2],SL=colors[1],colors[4]))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)[c(3,1,2,4)]
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/COmonitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="grey95")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=21,cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,
  pch=21,cex=1.5,pt.cex=2,bty='n',ncol=4)
dev.off()

## NO2 monitor map
no2.monitors$site <- substr(no2.monitors$id,1,9)
no2.monitors$class <- mapply(function(type,network) ifelse(network == "NEAR ROAD","NEAR ROAD",
  ifelse(network %in% c("NCORE","PAMS","UNOFFICIAL PAMS"),"NCORE/PAMS",
  ifelse(type == "SLAMS","SLAMS","SPM/OTHER"))),no2.monitors$monitor_type,no2.monitors$network)
t <- ddply(no2.monitors,"site",summarize,latitude=latitude[1],
  longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  NE=colors[3],NC=colors[2],SL=colors[1],colors[4]))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)[c(3,1,2,4)]
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/NO2monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="grey95")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=21,cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,
  pch=21,cex=1.5,pt.cex=2,bty='n',ncol=4)
dev.off()

## O3 monitor map
o3.monitors$site <- substr(o3.monitors$id,1,9)
o3.monitors$class <- mapply(function(type,network) ifelse(network == "CASTNET","CASTNET",
  ifelse(network %in% c("NCORE","PAMS","UNOFFICIAL PAMS"),"NCORE/PAMS",
  ifelse(type == "SLAMS","SLAMS","SPM/OTHER"))),o3.monitors$monitor_type,o3.monitors$network)
t <- ddply(o3.monitors,"site",summarize,latitude=latitude[1],
  longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  CA=colors[3],NC=colors[2],SL=colors[1],colors[4]))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)[c(3,2,1,4)]
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/O3monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="grey95")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=21,cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,
  pch=21,cex=1.5,pt.cex=2,bty='n',ncol=4)
dev.off()

## SO2 monitor map
so2.monitors$site <- substr(so2.monitors$id,1,9)
so2.monitors$class <- mapply(function(type,network) ifelse(type == "INDUSTRIAL","INDUSTRIAL",
  ifelse(network == "NCORE","NCORE",ifelse(type == "SLAMS","SLAMS","SPM/OTHER"))),
  so2.monitors$monitor_type,so2.monitors$network)
t <- ddply(so2.monitors,"site",summarize,latitude=latitude[1],
  longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  IN=colors[3],NC=colors[2],SL=colors[1],colors[4]))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)[c(3,2,1,4)]
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/SO2monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="grey95")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=21,cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,
  pch=21,cex=1.5,pt.cex=2,bty='n',ncol=4)
dev.off()

## PM10 monitor map
pm10.monitors$site <- substr(pm10.monitors$id,1,9)
pm10.monitors$class <- mapply(function(type,network) ifelse(type == "INDUSTRIAL","INDUSTRIAL",
  ifelse(network == "NCORE","NCORE",ifelse(type == "SLAMS","SLAMS","SPM/OTHER"))),
  pm10.monitors$monitor_type,pm10.monitors$network)
t <- ddply(pm10.monitors,"site",summarize,latitude=latitude[1],
  longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  IN=colors[3],NC=colors[2],SL=colors[1],colors[4]))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)[c(3,2,1,4)]
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/PM10monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="grey95")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=21,cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,
  pch=21,cex=1.5,pt.cex=2,bty='n',ncol=4)
dev.off()

## PM2.5 monitor map
pm25.monitors$site <- substr(pm25.monitors$id,1,9)
pm25.monitors$class <- mapply(function(type,network) ifelse(network == "NEAR ROAD","NEAR ROAD",
  ifelse(network == "NCORE","NCORE",ifelse(type == "SLAMS","SLAMS","SPM/OTHER"))),
  pm25.monitors$monitor_type,pm25.monitors$network)
t <- ddply(pm25.monitors,"site",summarize,latitude=latitude[1],
  longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  NE=colors[3],NC=colors[2],SL=colors[1],colors[4]))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)[c(3,1,2,4)]
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/PM25monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="grey95")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=21,cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,
  pch=21,cex=1.5,pt.cex=2,bty='n',ncol=4)
dev.off()

## Regulatory Pb monitor map
pb.monitors$site <- substr(pb.monitors$id,1,9)
t <- ddply(pb.monitors,"site",summarize,latitude=latitude[1],
  longitude=longitude[1],parameter=max(parameter))
t$color <- sapply(t$parameter,function(x) ifelse(x == "14129",colors[2],
  ifelse(x == "12128",colors[3],colors[4])))
N <- table(t$parameter)[c(2,1,3)]
legend.lab <- paste(c("Lead (TSP) LC","Lead (TSP) STP","Lead (PM10) LC")," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/Pbmonitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.11))
draw.map("state",proj.args=pa,hires=TRUE,col="grey95")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=21,cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors[2:4],pch=21,cex=1.5,pt.cex=2,
  title="AQS Parameter Description (# Sites)",bty='n',ncol=3)
dev.off()

## Non-regulatory Lead (Pb-PM10 and Pb-PM2.5) monitor map
pb.nonreg.monitors$site <- substr(pb.nonreg.monitors$id,1,9)
pb.nonreg.monitors$class <- sapply(pb.nonreg.monitors$network,function(x) ifelse(grepl("CSN",x),"CSN",
  ifelse(grepl("IMPROVE",x),"IMPROVE",ifelse(grepl("NCORE",x) | grepl("NATTS",x),"NCORE/NATTS","OTHER"))))
t <- ddply(pb.nonreg.monitors,c("parameter","site"),summarize,
  latitude=latitude[1],longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,1),function(x) switch(x,
  C=colors[1],I=colors[2],N=colors[3],colors[4]))
t$pch <- sapply(t$parameter,function(x) ifelse(x == "88128",21,22))
t <- t[order(t$parameter,t$class,t$site,decreasing=TRUE),]
N1 <- table(t$pch); N2 <- table(t$class);
legend.lab1 <- paste(c("Pb-PM2.5","Pb-PM10")," (",N1,")",sep="")
legend.lab2 <- paste(names(N2)," (",N2,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/Pb_nonreg_monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.15))
draw.map("state",proj.args=pa,hires=TRUE,col="grey95")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=t$pch[i],cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("top",legend=legend.lab1,pch=c(21,22),pt.bg="white",
  cex=1.5,pt.cex=2,bty='n',ncol=2)
legend("bottom",legend=legend.lab2,pt.bg=colors,
  pch=21,cex=1.5,pt.cex=2,bty='n',ncol=4)
dev.off()

## Coarse Particulates (PM10-2.5) monitor map
pm10_25.monitors$site <- substr(pm10_25.monitors$id,1,9)
pm10_25.monitors$class <- mapply(function(type,network) ifelse(network == "IMPROVE","IMPROVE",
  ifelse(grepl("NCORE",network),"NCORE",ifelse(type == "SLAMS","SLAMS","SPM/OTHER"))),
  pm10_25.monitors$monitor_type,pm10_25.monitors$network)
t <- ddply(pm10_25.monitors,"site",summarize,latitude=latitude[1],
  longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,2),function(x) switch(x,
  SL=colors[3],IM=colors[1],NC=colors[2],colors[4]))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/PM10-25monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="grey95")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=21,cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,
  pch=21,cex=1.5,pt.cex=2,bty='n',ncol=4)
dev.off()

## PM2.5 speciation monitor map
pm25.spec.monitors <- subset(pm25.spec.monitors,parameter == "88403")
pm25.spec.monitors$site <- substr(pm25.spec.monitors$id,1,9)
pm25.spec.monitors$class <- sapply(pm25.spec.monitors$network,function(x) ifelse(grepl("CSN",x),"CSN",
  ifelse(grepl("IMPROVE",x),"IMPROVE",ifelse(grepl("NCORE",x),"NCORE","OTHER"))))
t <- ddply(pm25.spec.monitors,"site",summarize,latitude=latitude[1],
  longitude=longitude[1],class=min(class))
t$color <- sapply(substr(t$class,1,1),function(x) switch(x,
  C=colors[1],I=colors[2],N=colors[3],colors[4]))
t <- t[order(t$class,t$site,decreasing=TRUE),]
N <- table(t$class)
legend.lab <- paste(names(N)," (",N,")",sep="")

file.name <- paste("NAAQS_AQ/monitor_maps/",curr.year,"/PM25spec_monitors",curr.year,".png",sep="")
png(file=file.name,width=1200,height=800)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="grey95")
for (i in 1:nrow(t)) {
  add.layer(type="points",x=t$longitude[i],y=t$latitude[i],
    proj.args=pa,bg=t$color[i],pch=21,cex=2)
}
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=colors,
  pch=21,cex=1.5,pt.cex=2,bty='n',ncol=4)
dev.off()

cat("Done.",as.character(Sys.time()),"\n")