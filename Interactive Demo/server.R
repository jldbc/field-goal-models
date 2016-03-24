
library(shiny)
library(ggplot2)
library(reshape2)
library(caret)
library(deepnet)
library(plyr)

shinyServer(function(input, output) {
  
  #load trained network  -- nn2 is the net
  load("model.rda")
  #load scaled training data  -- called scaled
  load("scaleddata.rda")
  load("maxs.rda")
  load("mins.rda")
  
  output$distPlot <- renderPlot({
    
    #make sure you generate the fit function in other file before launching server 
    
    if(input$turfa == "1"){
      turfval = 1
    }
    else{
      turfval = 0
    }
    
    windVal = input$wspd
    
    if(input$alta == TRUE){
      altval = 1
    }
    else{
      altval = 0
    }
    if(input$precipa == TRUE){
      precipval = 1
    }
    else{
      precipval = 0
    }
    if(input$iceda == TRUE){
      icedval = 1
    }
    else{
      icedval = 0
    }
    tempVal = input$temp
    
    
    game1 <- data.frame(temp=tempVal, wspd=windVal, iced=icedval, turf=turfval,precip=precipval,mileHigh=altval, 
                        k_AD0800=0, k_AE0700=0, k_AH2600=0, k_AP1000=0, k_AV0400=0, k_BC2300=0, 
                        k_BC2600=0, k_BC3000=0, k_BD0100=0, k_BG1300=0, k_BW0350=0,k_CB0700=0, 
                        k_CB1700=0, k_CH2900=0, k_CO0400=0, k_CS4000=0, k_CS4250=0, k_DA0300=0, 
                        k_DB0200=0, k_DB3500=0, k_DB3900=0,k_DB5500=0, k_DC0500=0, k_DR0600=0, 
                        k_EM2000=0, k_GA0300=0, k_GG0100=0, k_GH0600=0, k_GZ2000=0, k_HE0100=0,
                        k_JA2100=0, k_JB7100=0, k_JC0900=0, k_JC2100=0, k_JC5000=0, k_JE0200=0, 
                        k_JF0800=0, k_JF0900=0, k_JH0400=0, k_JH0500=0, k_JH0900=0, k_JH3800=0, 
                        k_JH5000=0, k_JJ1400=0, k_JK0200=0, k_JM4000=0, k_JN0600=0, k_JP3850=0,
                        k_JR1100=0, k_JS1100=0, k_JT1000=0, k_JT3950=0, k_JT4400=0, k_JW3300=0, 
                        k_KB2300=0, k_KF0250=0, k_KH1500=0, k_LT1100=0, k_MA0700=0, k_MB4600=0, 
                        k_MC3000=0, k_MG1200=0, k_MH3100=0, k_MH3900=0, k_MK1100=0, k_MK1200=0, 
                        k_MN0800=0, k_MP2100=0, k_MS5200=0, k_MV0100=0, k_NF0300=0, k_NK0200=0, 
                        k_NN0200=0, k_NR0100=0, k_OK0100=0, k_OM0100=0, k_OP0200=0, k_PD0200=0, 
                        k_PE0100=0, k_PS0900=0, k_RB2200=0, k_RB4650=0, k_RC2900=0, k_RG1500=0, 
                        k_RL0900=0, k_RL1300=0, k_RS0600=0, k_RS3400=0, k_SA1100=0, k_SB0500=0, 
                        k_SC0700=0, k_SG0800=0, k_SG1100=0, k_SH0400=0, k_SJ0300=0, k_SL0400=0, 
                        k_SL0600=0, k_SM0600=0, k_SS3100=0, k_TD2400=0, k_TF1200=0, k_TM2400=0, 
                        k_TP1200=0, k_TS0500=0, k_TS1100=0, k_WR0500=0)
    
    
    
   
    
    
    if(input$kicker == "0"){
      game1$k_AV0400 = 1
    }
    if(input$kicker == "1"){
      game1$k_BW0350 = 1
    }
    if(input$kicker == "2"){
      game1$k_JH0900 = 1
    }
    if(input$kicker == "3"){
      game1$k_JS1100 = 1
    }
    if(input$kicker == "4"){
      game1$k_JT3950 = 1
    }
    if(input$kicker == "5"){
      game1$k_KB2300 = 1
    }
    if(input$kicker == "6"){
      game1$k_LT1100 = 1
    }
    if(input$kicker == "7"){
      game1$k_MV0100 = 1
    }
    if(input$kicker == "8"){
      game1$k_PE0100 = 1
    }
    if(input$kicker == "9"){
      game1$k_RB2200 = 1
    }
    if(input$kicker == "10"){
      game1$k_SG0800 = 1
    }
    if(input$kicker == "11"){
      game1$k_SH0400 = 1
    }
    if(input$kicker == "12"){
      game1$k_SJ0300 = 1
    }
    if(input$kicker == "13"){
      game1$k_BC3000 = 1
    }
    if(input$kicker == "14"){
      game1$k_JC5000 = 1
    }
    if(input$kicker == "15"){
      game1$k_SM0600 = 1
    }
    
    
    game1 <- game1[rep(seq_len(nrow(game1)), each=65),]
    game1$dist<-seq.int(nrow(game1))
    game1$dist<- input$dist
    
    
    #data frame complete. now scale for neural network.
    
    #moving dot
    scaled = as.data.frame(scale(game1, center = mins, scale = maxs - mins))
    preds = nn.predict(nn2, scaled)
    
    game1$pred <- preds
    
    predsaa <- data.frame(game1$dist)
    predsaa$preda <- preds
    
    game2 <- game1
    game2 <- game2[1,]
    game2 <- game2[rep(seq_len(nrow(game2)), each=65),]
    
    if(input$toggleTrendline == TRUE){
      game2$dist<-seq.int(nrow(game2))
    }
    else{
      game2$dist<-0
    }
    game2$pred <- NULL
    
    scaled2 = as.data.frame(scale(game2, center = mins, scale = maxs - mins))
    predsb = nn.predict(nn2, scaled2)
    
    predsbb <- data.frame(game2$dist)
    predsbb$preda <- predsb
    predsbb <- rename(predsbb, c("game2.dist"="dist"))
    
    #predsbb$predb <- predsbb$predb
    predsaa <- rename(predsaa, c("game1.dist"="dist"))
    predsaa <- rbind(predsaa, predsbb)
    
    predsaa <- melt(predsaa, id="dist")
    
    
    ggplot(data=predsaa,
           aes(x=dist, y=value, colour=variable)) + geom_line(color="red") +
      geom_point(size=5, x=input$dist, y=predsaa$value[predsaa$dist == input$dist][1]) + scale_x_continuous(limits = c(15, 65),breaks = round(seq(min(15), max(75), by = 5),1)) + 
      scale_y_continuous(limits = c(0, 1),breaks=round(seq(min(0), max(1), by=.05), 1)) + 
      scale_colour_manual(
        values = c("preda" = "blue"), 
        breaks=c("Predicted Probability of Success")) +
      theme(axis.text.x = element_text(face="bold",
                                       size=16),  axis.text.y = element_text(face="bold", size=14), 
            axis.title.x = element_text(size=16,face="bold"),
            axis.title.y = element_text(size=16,face="bold"))  
    
  })
})

