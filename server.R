library(shiny)
library(foreign)
library(ggplot2)
library(corrgram)
library("rnaturalearth")
library("rnaturalearthdata")
library(spData)
library(tmap)
library(sf)
library(raster)
library(dplyr)


load("data.RData")

#Main part of server
shinyServer(function(session, input, output) {


    #TAB - Correlogram
    corr_data <- subset(ASD_data, select = -c(result))
    output$corr <- renderPlot(corrgram(corr_data, order=NULL,
                                       upper.panel=panel.cor, main="Correlogram for participants answers"))
    
    #TAB - DATA
    output$data_answers <- renderDataTable(ASD_answers, options = list(searching = TRUE,pageLength = 10, scrollX = T))
    output$data <- renderDataTable(ASD_data, options = list(searching = TRUE,pageLength = 10, scrollX = T))

    #TAB - TEST
    ans1 <- reactive({input$ans1})
    ans2 <- reactive({input$ans2})
    ans3 <- reactive({input$ans3})
    ans4 <- reactive({input$ans4})
    ans5 <- reactive({input$ans5})
    ans6 <- reactive({input$ans6})
    ans7 <- reactive({input$ans7})
    ans8 <- reactive({input$ans8})
    ans9 <- reactive({input$ans9})
    ans10 <- reactive({input$ans10})
    
    diagnose = "We suggest taking a proper Autism Disorder test."
    healthy = "You don't have anything to worry about!"
    
    print_diganose <- eventReactive(input$button, {})
    
     output$result <- renderPrint({ 
       print_diganose()
                         result <- as.numeric(ans1()) + as.numeric(ans2()) + as.numeric(ans3()) + as.numeric(ans4()) + as.numeric(ans5()) + as.numeric(ans6()) + as.numeric(ans7()) + as.numeric(ans8()) + as.numeric(ans9()) + as.numeric(ans10())
                         if (identical(result, 5) || identical(result, 6) || identical(result, 7) || identical(result, 8) || identical(result, 9) || identical(result, 10)){
                           cat(diagnose)
                         } else {
                           cat(healthy)
                         }
                           
                       })
    
    
    #TAB - map
    factor_to_char <- function(df){
      for(i in which(sapply(df, class) == "factor")) df[[i]] = as.character(df[[i]])
      return(df)
    }
    #Plot for all participants
    map_all_df <- as.data.frame(table(ASD_data$contry_of_res))
    colnames(map_all_df) <- c("name_long", "counter")
    map_all_df <- subset(map_all_df, map_all_df$counter !=0)
    map_all_df <- factor_to_char(map_all_df)
    world_all <- merge(world, map_all_df, by="name_long", all.x = TRUE)
    output$map_all <- renderPlot(tm_shape(world_all) + tm_fill(col = "counter", title = "All participants", n = 10, palette = "Spectral"))
    
    ethnicity_df_all <- as.data.frame(table(ASD_data$ethnicity))
    colnames(ethnicity_df_all) <- c("ethnicity", "counter")
    slices_map_all <- ethnicity_df_all$counter
    labels_map_all <- ethnicity_df_all$ethnicity
    output$pie_map_all <- renderPlot(pie(slices_map_all, labels = labels_map_all, main="Pie Chart for all respondents"))
    
    #plot for diagnosed
    map_diagnosed_df <- as.data.frame(table(ASD_diagnosed$contry_of_res))
    colnames(map_diagnosed_df) <- c("name_long", "counter")
    map_diagnosed_df <- subset(map_diagnosed_df, map_diagnosed_df$counter != 0)
    map_diagnosed_df <- factor_to_char(map_diagnosed_df)
    world_diagnosed <- merge(world, map_diagnosed_df, by="name_long", all.x = TRUE)
    output$map_diagnosed <- renderPlot(tm_shape(world_diagnosed) + tm_fill(col = "counter", title = "Diagnosed participants", n = 10, palette = "Spectral"))
    
    ethnicity_df_diagnosed <- as.data.frame(table(ASD_diagnosed$ethnicity))
    colnames(ethnicity_df_diagnosed) <- c("ethnicity", "counter")
    slices_map_diagnosed <- ethnicity_df_diagnosed$counter
    labels_map_diagnosed <- ethnicity_df_diagnosed$ethnicity
    output$pie_map_diagnosed<- renderPlot(pie(slices_map_diagnosed, labels = labels_map_diagnosed, main="Pie Chart for diagnosed respondents"))
    
    #TAB - answers analysis
    #Question 1
    slices_Q1_all <- c(nrow(ASD_data)-sum(ASD_data$A1_Score), sum(ASD_data$A1_Score))
    agree_perc <- ceiling(sum(ASD_data$A1_Score)/nrow(ASD_data)*100)
    dis_perc <- 100-agree_perc
    labels_Q1_all <- c(paste(toString(dis_perc),"% Disagree",sep =""), paste(toString(agree_perc),"% Agree",sep =""))
    output$plot_Q1_all <- renderPlot(pie(slices_Q1_all, labels = labels_Q1_all, main="Pie Chart for all respondents"))
    slices_Q1 <- c(nrow(ASD_diagnosed)-sum(ASD_diagnosed$A1_Score), sum(ASD_diagnosed$A1_Score))
    agree_perc <- ceiling(sum(ASD_diagnosed$A1_Score)/nrow(ASD_diagnosed)*100)
    dis_perc <- 100-agree_perc
    labels_Q1 <- c(paste(toString(dis_perc),"% Disagree",sep =""), paste(toString(agree_perc),"% Agree",sep =""))
    output$plot_Q1 <- renderPlot(pie(slices_Q1, labels = labels_Q1, main="Pie Chart for diagnosed respondents"))
    
    #Question 2
    slices_Q2_all <- c(nrow(ASD_data)-sum(ASD_data$A2_Score), sum(ASD_data$A2_Score))
    dis_perc <- ceiling(sum(ASD_data$A2_Score)/nrow(ASD_data)*100)
    agree_perc <- 100-dis_perc
    labels_Q2_all <- c(paste(toString(agree_perc),"% Agree",sep =""), paste(toString(dis_perc),"% Disagree",sep =""))
    output$plot_Q2_all <- renderPlot(pie(slices_Q2_all, labels = labels_Q2_all, main="Pie Chart for all respondents"))
    slices_Q2 <- c(nrow(ASD_diagnosed)-sum(ASD_diagnosed$A2_Score), sum(ASD_diagnosed$A2_Score))
    dis_perc <- ceiling(sum(ASD_diagnosed$A2_Score)/nrow(ASD_diagnosed)*100)
    agree_perc <- 100-dis_perc
    labels_Q2 <- c(paste(toString(agree_perc),"% Agree",sep =""), paste(toString(dis_perc),"% Disagree",sep =""))
    output$plot_Q2 <- renderPlot(pie(slices_Q2, labels = labels_Q2, main="Pie Chart for diagnosed respondents"))

    #Question 3
    slices_Q3_all <- c(nrow(ASD_data)-sum(ASD_data$A3_Score), sum(ASD_data$A3_Score))
    dis_perc <- ceiling(sum(ASD_data$A3_Score)/nrow(ASD_data)*100)
    agree_perc <- 100-dis_perc
    labels_Q3_all <- c(paste(toString(agree_perc),"% Agree",sep =""), paste(toString(dis_perc),"% Disagree",sep =""))
    output$plot_Q3_all <- renderPlot(pie(slices_Q3_all, labels = labels_Q3_all, main="Pie Chart for all respondents"))
    slices_Q3 <- c(nrow(ASD_diagnosed)-sum(ASD_diagnosed$A3_Score), sum(ASD_diagnosed$A3_Score))
    dis_perc <- ceiling(sum(ASD_diagnosed$A3_Score)/nrow(ASD_diagnosed)*100)
    agree_perc <- 100-dis_perc
    labels_Q3 <- c(paste(toString(agree_perc),"% Agree",sep =""), paste(toString(dis_perc),"% Disagree",sep =""))
    output$plot_Q3 <- renderPlot(pie(slices_Q3, labels = labels_Q3, main="Pie Chart for diagnosed respondents"))
    
    #Question 4
    slices_Q4_all <- c(nrow(ASD_data)-sum(ASD_data$A4_Score), sum(ASD_data$A4_Score))
    dis_perc <- ceiling(sum(ASD_data$A4_Score)/nrow(ASD_data)*100)
    agree_perc <- 100-dis_perc
    labels_Q4_all <- c(paste(toString(agree_perc),"% Agree",sep =""), paste(toString(dis_perc),"% Disagree",sep =""))
    output$plot_Q4_all <- renderPlot(pie(slices_Q4_all, labels = labels_Q4_all, main="Pie Chart for all respondents"))
    slices_Q4 <- c(nrow(ASD_diagnosed)-sum(ASD_diagnosed$A4_Score), sum(ASD_diagnosed$A4_Score))
    dis_perc <- ceiling(sum(ASD_diagnosed$A4_Score)/nrow(ASD_diagnosed)*100)
    agree_perc <- 100-dis_perc
    labels_Q4 <- c(paste(toString(agree_perc),"% Agree",sep =""), paste(toString(dis_perc),"% Disagree",sep =""))
    output$plot_Q4 <- renderPlot(pie(slices_Q4, labels = labels_Q4, main="Pie Chart for diagnosed respondents"))
    
    #Question 5
    slices_Q5_all <- c(nrow(ASD_data)-sum(ASD_data$A5_Score), sum(ASD_data$A5_Score))
    agree_perc <- ceiling(sum(ASD_data$A5_Score)/nrow(ASD_data)*100)
    dis_perc <- 100-agree_perc
    labels_Q5_all <- c(paste(toString(dis_perc),"% Disagree",sep =""), paste(toString(agree_perc),"% Agree",sep =""))
    output$plot_Q5_all <- renderPlot(pie(slices_Q5_all, labels = labels_Q5_all, main="Pie Chart for all respondents"))
    slices_Q5 <- c(nrow(ASD_diagnosed)-sum(ASD_diagnosed$A5_Score), sum(ASD_diagnosed$A5_Score))
    agree_perc <- ceiling(sum(ASD_diagnosed$A5_Score)/nrow(ASD_diagnosed)*100)
    dis_perc <- 100-agree_perc
    labels_Q5 <- c(paste(toString(dis_perc),"% Disagree",sep =""), paste(toString(agree_perc),"% Agree",sep =""))
    output$plot_Q5 <- renderPlot(pie(slices_Q5, labels = labels_Q5, main="Pie Chart for diagnosed respondents"))
    
    #Question 6
    slices_Q6_all <- c(nrow(ASD_data)-sum(ASD_data$A6_Score), sum(ASD_data$A6_Score))
    dis_perc <- ceiling(sum(ASD_data$A6_Score)/nrow(ASD_data)*100)
    agree_perc <- 100-dis_perc
    labels_Q6_all <- c(paste(toString(agree_perc),"% Agree",sep =""), paste(toString(dis_perc),"% Disagree",sep =""))
    output$plot_Q6_all <- renderPlot(pie(slices_Q6_all, labels = labels_Q6_all, main="Pie Chart for all respondents"))
    slices_Q6 <- c(nrow(ASD_diagnosed)-sum(ASD_diagnosed$A6_Score), sum(ASD_diagnosed$A6_Score))
    dis_perc <- ceiling(sum(ASD_diagnosed$A6_Score)/nrow(ASD_diagnosed)*100)
    agree_perc <- 100-dis_perc
    labels_Q6 <- c(paste(toString(agree_perc),"% Agree",sep =""), paste(toString(dis_perc),"% Disagree",sep =""))
    output$plot_Q6 <- renderPlot(pie(slices_Q6, labels = labels_Q6, main="Pie Chart for diagnosed respondents"))
    
    #Question 7
    slices_Q7_all <- c(nrow(ASD_data)-sum(ASD_data$A7_Score), sum(ASD_data$A7_Score))
    agree_perc <- ceiling(sum(ASD_data$A7_Score)/nrow(ASD_data)*100)
    dis_perc <- 100-agree_perc
    labels_Q7_all <- c(paste(toString(dis_perc),"% Disagree",sep =""), paste(toString(agree_perc),"% Agree",sep =""))
    output$plot_Q7_all <- renderPlot(pie(slices_Q7_all, labels = labels_Q7_all, main="Pie Chart for all respondents"))
    slices_Q7 <- c(nrow(ASD_diagnosed)-sum(ASD_diagnosed$A7_Score), sum(ASD_diagnosed$A7_Score))
    agree_perc <- ceiling(sum(ASD_diagnosed$A7_Score)/nrow(ASD_diagnosed)*100)
    dis_perc <- 100-agree_perc
    labels_Q7 <- c(paste(toString(dis_perc),"% Disagree",sep =""), paste(toString(agree_perc),"% Agree",sep =""))
    output$plot_Q7 <- renderPlot(pie(slices_Q7, labels = labels_Q7, main="Pie Chart for diagnosed respondents"))
    
    #Question 8
    slices_Q8_all <- c(nrow(ASD_data)-sum(ASD_data$A8_Score), sum(ASD_data$A8_Score))
    dis_perc <- ceiling(sum(ASD_data$A8_Score)/nrow(ASD_data)*100)
    agree_perc <- 100-dis_perc
    labels_Q8_all <- c(paste(toString(agree_perc),"% Agree",sep =""), paste(toString(dis_perc),"% Disagree",sep =""))
    output$plot_Q8_all <- renderPlot(pie(slices_Q8_all, labels = labels_Q8_all, main="Pie Chart for all respondents"))
    slices_Q8 <- c(nrow(ASD_diagnosed)-sum(ASD_diagnosed$A8_Score), sum(ASD_diagnosed$A8_Score))
    dis_perc <- ceiling(sum(ASD_diagnosed$A8_Score)/nrow(ASD_diagnosed)*100)
    agree_perc <- 100-dis_perc
    labels_Q8 <- c(paste(toString(agree_perc),"% Agree",sep =""), paste(toString(dis_perc),"% Disagree",sep =""))
    output$plot_Q8 <- renderPlot(pie(slices_Q8, labels = labels_Q8, main="Pie Chart for diagnosed respondents"))
    
    #Question 9
    slices_Q9_all <- c(nrow(ASD_data)-sum(ASD_data$A9_Score), sum(ASD_data$A9_Score))
    dis_perc <- ceiling(sum(ASD_data$A9_Score)/nrow(ASD_data)*100)
    agree_perc <- 100-dis_perc
    labels_Q9_all <- c(paste(toString(agree_perc),"% Agree",sep =""), paste(toString(dis_perc),"% Disagree",sep =""))
    output$plot_Q9_all <- renderPlot(pie(slices_Q9_all, labels = labels_Q9_all, main="Pie Chart for all respondents"))
    slices_Q9 <- c(nrow(ASD_diagnosed)-sum(ASD_diagnosed$A9_Score), sum(ASD_diagnosed$A9_Score))
    dis_perc <- ceiling(sum(ASD_diagnosed$A9_Score)/nrow(ASD_diagnosed)*100)
    agree_perc <- 100-dis_perc
    labels_Q9 <- c(paste(toString(agree_perc),"% Agree",sep =""), paste(toString(dis_perc),"% Disagree",sep =""))
    output$plot_Q9 <- renderPlot(pie(slices_Q9, labels = labels_Q9, main="Pie Chart for diagnosed respondents"))
    
    #Question 10
    slices_Q10_all <- c(nrow(ASD_data)-sum(ASD_data$A10_Score), sum(ASD_data$A10_Score))
    agree_perc <- ceiling(sum(ASD_data$A10_Score)/nrow(ASD_data)*100)
    dis_perc <- 100-agree_perc
    labels_Q10_all <- c(paste(toString(dis_perc),"% Disagree",sep =""), paste(toString(agree_perc),"% Agree",sep =""))
    output$plot_Q10_all <- renderPlot(pie(slices_Q10_all, labels = labels_Q10_all, main="Pie Chart for all respondents"))
    slices_Q10 <- c(nrow(ASD_diagnosed)-sum(ASD_diagnosed$A10_Score), sum(ASD_diagnosed$A10_Score))
    agree_perc <- ceiling(sum(ASD_diagnosed$A10_Score)/nrow(ASD_diagnosed)*100)
    dis_perc <- 100-agree_perc
    labels_Q10 <- c(paste(toString(dis_perc),"% Disagree",sep =""), paste(toString(agree_perc),"% Agree",sep =""))
    output$plot_Q10 <- renderPlot(pie(slices_Q10, labels = labels_Q10, main="Pie Chart for diagnosed respondents"))
    
    #Question jundice
    slices_jundice_all <- c(nrow(ASD_data)-sum(ASD_data$jundice), sum(ASD_data$jundice))
    agree_perc <- ceiling(sum(ASD_data$jundice)/nrow(ASD_data)*100)
    dis_perc <- 100-agree_perc
    labels_jundice_all <- c(paste(toString(dis_perc),"% No",sep =""), paste(toString(agree_perc),"% Yes",sep =""))
    output$plot_jundice_all <- renderPlot(pie(slices_jundice_all, labels = labels_jundice_all, main="Pie Chart for all respondents"))
    slices_jundice<- c(nrow(ASD_diagnosed)-sum(ASD_diagnosed$jundice), sum(ASD_diagnosed$jundice))
    agree_perc <- ceiling(sum(ASD_diagnosed$jundice)/nrow(ASD_diagnosed)*100)
    dis_perc <- 100-agree_perc
    labels_jundice <- c(paste(toString(dis_perc),"% No",sep =""), paste(toString(agree_perc),"% Yes",sep =""))
    output$plot_jundice <- renderPlot(pie(slices_jundice, labels = labels_jundice, main="Pie Chart for diagnosed respondents"))
    
    #Question family
    slices_family_all <- c(nrow(ASD_data)-sum(ASD_data$austim), sum(ASD_data$austim))
    agree_perc <- ceiling(sum(ASD_data$austim)/nrow(ASD_data)*100)
    dis_perc <- 100-agree_perc
    labels_family_all <- c(paste(toString(dis_perc),"% No",sep =""), paste(toString(agree_perc),"% Yes",sep =""))
    output$plot_family_all <- renderPlot(pie(slices_family_all, labels = labels_family_all, main="Pie Chart for all respondents"))
    slices_family<- c(nrow(ASD_diagnosed)-sum(ASD_diagnosed$austim), sum(ASD_diagnosed$austim))
    agree_perc <- ceiling(sum(ASD_diagnosed$austim)/nrow(ASD_diagnosed)*100)
    dis_perc <- 100-agree_perc
    labels_family <- c(paste(toString(dis_perc),"% No",sep =""), paste(toString(agree_perc),"% Yes",sep =""))
    output$plot_family <- renderPlot(pie(slices_family, labels = labels_family, main="Pie Chart for diagnosed respondents"))
    
    #Question Age
    output$hist_age_all <- renderPlot(hist(ASD_data$age, xlab = "Childs age", main = "Histogram of examined children", col = "cadetblue1"))
    output$hist_age <- renderPlot(hist(ASD_diagnosed$age, xlab = "Childs age", main = "Histogram of diagnosed children", col = "cadetblue1"))

   
})

    