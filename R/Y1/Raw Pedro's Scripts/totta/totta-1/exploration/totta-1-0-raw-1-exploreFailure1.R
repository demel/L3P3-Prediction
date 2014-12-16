baseDataPath <- "C:/Users/capelastegui/workspace/OFP/Totta/Totta-1/1-Data"
testFolder <- file.path(baseDataPath,"0-test")

dataPath <-testFolder
fileIn <- file(file.path(dataPath, "totta-1-tmp-cpu.csv"))
cpu<-read.csv(fileIn)
names(cpu)<-c("date","cpu")
cpu$date<-strptime (cpu$date,"%m/%d/%Y %H:%M")
cpu$cpu<-as.numeric(cpu$cpu)

library(xts)
cpuXts<-xts(cpu$cpu,order.by=cpu$date)
cpuTs<-ts(cpu$cpu)

plot(cpu$date[55:75],cpu$cpu[55:75],type="l",xaxt="n",xlab=NA)
points(cpu$date[55:75],cpu$cpu[55:75])

axis.POSIXct(side=1, las=2,at=as.character(cpu$date))


plot(cpuXts[55:75],xaxt="n",main="")
points(cpu$date[55:75],cpu$cpu[55:75])
axis.POSIXct(side=1, las=2,at=as.character(cpu$date))

fileIn <- file(file.path(dataPath, "totta-1-tmp-mem.csv"))
mem<-read.csv(fileIn)
names(mem)<-c("date","mem")

mem$date<-strptime (mem$date,"%m/%d/%Y %H:%M")
mem$mem<-as.numeric(mem$mem)

memXts<-xts(mem$mem,order.by=mem$date)

plot(memXts[55:75],xaxt="n",main="")
points(mem$date[55:75],mem$mem[55:75])
axis.POSIXct(side=1, las=2,at=as.character(mem$date))
