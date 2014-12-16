library(shiny)
library(datasets)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  output$nombre <- renderText({
    input$machine    
  })
  
  
  recalculateCPU <-reactive({
  if (input$machine=="bvtolpcbdp01") 
    {fileCPU <-paste(input$machine,"_CPU_151113.txt",sep="")}
  else if(input$machine=="bvtowpocwp01")
    {fileCPU <-paste(input$machine,".central.rinterna.local_CPU_151113.txt",sep="")}
  else {fileCPU <-paste(input$machine,".totta.corp_CPU_151113.txt",sep="")}
  CPU <- CPUParser(fileCPU)
  })
  
  try(
  recalculateHD <-reactive({
    if (input$machine=="bvtolpcbdp01") 
    {fileHD <-paste(input$machine,"_HD_151113.txt",sep="")}
    else if(input$machine=="bvtowpocwp01")
    {fileHD <-paste(input$machine,".central.rinterna.local_HD_151113.txt",sep="")}
    else {fileHD <-paste(input$machine,".totta.corp_HD_151113.txt",sep="")}
  HD  <- HDParser(fileHD) 
  }))
  
  recalculateMEM <-reactive({
    if (input$machine=="bvtolpcbdp01") 
    {fileMEM <-paste(input$machine,"_MEM_151113.txt",sep="")}
    else if(input$machine=="bvtowpocwp01")
    {fileMEM <-paste(input$machine,".central.rinterna.local_MEM_151113.txt",sep="")}
    else {fileMEM <-paste(input$machine,".totta.corp_MEM_151113.txt",sep="")}
  MEM <- MEMParser(fileMEM)
  })

  

  output$Plot <- renderPlot({
    CPU<-recalculateCPU()
    MEM<-recalculateMEM()
    try(rm(HD),silent=TRUE)
    try(HD <-recalculateHD())
    
    plot(1, type="n", axes=TRUE, xlab="Time", ylab="Percentage of use",xlim=c((input$range[1]),(input$range[2])),ylim=c(input$yrange[1],input$yrange[2]))
    title(input$machine)
    par(new=TRUE)
    
    if (input$CPU == TRUE) 
    {
      plot(CPU$eventTime[(1+12*input$range[1]):(12+12*input$range[2])],CPU$CPUUsage[(1+12*input$range[1]):(12+12*input$range[2])],type="l",col="red",xlab="",ylab="",ylim=c(input$yrange[1],input$yrange[2]),axes=FALSE)
      legend("topleft",legend="CPU",fill="red")
      par(new=TRUE)
    }
    
    if (input$MEM == TRUE)
    {
      plot(CPU$eventTime[(1+12*input$range[1]):(12+12*input$range[2])],MEM$MEMUsage[(1+12*input$range[1]):(12+12*input$range[2])],type="l",col="green",xlab="",ylab="",ylim=c(input$yrange[1],input$yrange[2]),axes=FALSE)
      legend("topright",legend="Memory",fill="green")
      par(new=TRUE)
    }
    try(
     if (input$HD == TRUE)
     {
      HDcolors<-rainbow(length(colnames(HD))-1) 
      for (i in 1:length(colnames(HD))-1)
        {
        plot(CPU$eventTime[(1+12*input$range[1]):(12+12*input$range[2])],HD[((1+12*input$range[1]):(12+12*input$range[2])),i+1],type="l",col=HDcolors[i],xlab="",ylab="",ylim=c(input$yrange[1],input$yrange[2]),axes=FALSE)
        par(new=TRUE)
        }
      if(input$Hide == FALSE)
      {
      legend("bottomright",legend=colnames(HD)[2:length(colnames(HD))],fill=HDcolors)
      }
     })
    
    })
  
  
  output$Text <- renderText({input$range})

  })
