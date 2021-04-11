#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    navbarPage("ASD Test",
      tabPanel("Main page",
               br(), br(),
               h2("Autistic Spectrum Disorder Application", align = "center"),
               h3("Welcome to app presenting analysis of data collected from ASDQuiz app.", align = "center"),
               h3( "Quiz consists of 10 test-questions and it required participants to provide personal information.", align = "center"),
               h3(" Presented shiny appication allows users to take the mentioned test.", align = "center")),
      tabPanel("Take the test", 
               h3 ("Answer questions to know whether you likely have ASD"),
               br(),
               radioButtons(inputId = "ans1", 
                            "Do you often notice small sounds when others do not?",
                            choices = c("Yes"=1,"No"=0), selected = "Yes", inline = TRUE),
                radioButtons(inputId = "ans2", 
                            "Do you usually concentrate more on the whole picture, rather the small details?",
                            choices = c("Yes"=0,"No"=1), selected = "Yes", inline = TRUE),
                radioButtons(inputId = "ans3", 
                            "In a social group, can you easily keep track of several different peoples conversations?",
                            choices = c("Yes"=0,"No"=1), selected = "Yes", inline = TRUE),
                radioButtons(inputId = "ans4", 
                            "Do you find it easy to go back and forth between different activities?",
                            choices = c("Yes"=0,"No"=1), selected = "Yes", inline = TRUE),
                radioButtons(inputId = "ans5", 
                            "Do you know how to keep a conversation going with your peers?",
                            choices = c("Yes"=0,"No"=1), selected = "Yes", inline = TRUE),
                radioButtons(inputId = "ans6", 
                            "Are you good at social chit-chat?",
                            choices = c("Yes"=0,"No"=1), selected = "Yes", inline = TRUE),
                radioButtons(inputId = "ans7", 
                            "When you are reading a story, do you find it difficult to work out the characters intensions or feelings?",
                            choices = c("Yes"=1,"No"=0), selected = "Yes", inline = TRUE),
                radioButtons(inputId = "ans8", 
                            "When you were in (pre)school, did you use to enjoy playing games involving pretending with other children?",
                            choices = c("Yes"=0,"No"=1), selected = "Yes", inline = TRUE),
                radioButtons(inputId = "ans9", 
                            "Do you find it easy to work out what someone is thinking or feeling just by looking at their face?",
                            choices = c("Yes"=0,"No"=1),selected = "Yes", inline = TRUE),
                radioButtons(inputId = "ans10", 
                            "Do you find it hard to make new friends?",
                            choices = c("Yes"=1,"No"=0), selected = "Yes", inline = TRUE),
                br(),
                splitLayout(actionButton("button", "Check diagnosis"),
                h3(textOutput("result")))
                
               ),
      tabPanel("Map",
               tabsetPanel(
                 tabPanel("All participants",
                          h2 ("Map of all participants", align = "center"),
                          plotOutput(outputId = "map_all"),
                          plotOutput(outputId = "pie_map_all")
                 ),
                 tabPanel("Diagnosed participants",
                          h2 ("Map of diagnosed participants", align = "center"),
                          plotOutput(outputId = "map_diagnosed"),
                          plotOutput(outputId = "pie_map_diagnosed")
                 )
               )
      ),
    
      tabPanel("Answers analysis",
               navlistPanel(
                 "Answers analysis",
                 
                 tabPanel("Age",h2("How old is s/he?"),
                          splitLayout(
                            plotOutput(outputId = "hist_age_all"),
                            plotOutput(outputId = "hist_age"))),
                 
                 tabPanel("Question 1", h2("S/he often notices small sounds when others do not."),
                          h3("Likely answer for ASD child - Agree"),
                          splitLayout(
                            plotOutput(outputId = "plot_Q1_all"),
                            plotOutput(outputId = "plot_Q1"))),
                 
                 tabPanel("Question 2", h2("S/he usually concentrates more on the whole picture, rather the small details."),
                          h3("Likely answer for ASD child - Disagree"),
                          splitLayout(
                            plotOutput(outputId = "plot_Q2_all"),
                            plotOutput(outputId = "plot_Q2"))),
                 tabPanel("Question 3", h2("In a social group, s/he can easily keep track of several different people\'s conversations."),
                          h3("Likely answer for ASD child - Disagree"),
                          splitLayout(
                            plotOutput(outputId = "plot_Q3_all"),
                            plotOutput(outputId = "plot_Q3"))),
                 
                 tabPanel("Question 4",h2("S/he finds it easy to go back and forth between different activities "),
                          h3("Likely answer for ASD child - Disagree"),
                          splitLayout(
                            plotOutput(outputId = "plot_Q4_all"),
                            plotOutput(outputId = "plot_Q4"))),
                 
                 tabPanel("Question 5",h2("S/he doesnt know how to keep a conversation going with his/her peers. "),
                          h3("Likely answer for ASD child - Agree"),
                          splitLayout(
                            plotOutput(outputId = "plot_Q5_all"),
                            plotOutput(outputId = "plot_Q5"))),
                 
                 tabPanel("Question 6",h2("S/he is good at social chit-chat."),
                          h3("Likely answer for ASD child - Disagree"),
                          splitLayout(
                            plotOutput(outputId = "plot_Q6_all"),
                            plotOutput(outputId = "plot_Q6"))),
                 
                 tabPanel("Question 7",h2("When s/he is read a story, s/he finds it difficult to work out the character\'s intensions or feelings."),
                          h3("Likely answer for ASD child - Agree"),
                          splitLayout(
                            plotOutput(outputId = "plot_Q7_all"),
                            plotOutput(outputId = "plot_Q7"))),
                 
                 tabPanel("Question 8",h2("When s/he was in preschool, s/he used to enjoy playing games involving pretending with other children."),
                          h3("Likely answer for ASD child - Disagree"),
                          splitLayout(
                            plotOutput(outputId = "plot_Q8_all"),
                            plotOutput(outputId = "plot_Q8"))),
                 
                 tabPanel("Question 9",h2("S/he finds it easy to work out what someone is thinking or feeling just by looking at their face."),
                          h3("Likely answer for ASD child - Disagree"),
                          splitLayout(
                            plotOutput(outputId = "plot_Q9_all"),
                            plotOutput(outputId = "plot_Q9"))),
                 
                 tabPanel("Question 10",h2("S/he finds it hard to make new friends."),
                          h3("Likely answer for ASD child - Agree"),
                          splitLayout(
                            plotOutput(outputId = "plot_Q10_all"),
                            plotOutput(outputId = "plot_Q10"))),
                 
                 tabPanel("Jundice",h2("Was s/he born with jundice?"),
                          splitLayout(
                            plotOutput(outputId = "plot_jundice_all"),
                            plotOutput(outputId = "plot_jundice"))),
                 
                 tabPanel("ASD in family",h2("Is there any diagnosed with ASD family member in her/his family?"),
                          splitLayout(
                            plotOutput(outputId = "plot_family_all"),
                            plotOutput(outputId = "plot_family"))),
                 
                 tabPanel("Answers - data",dataTableOutput("data_answers"))
               )
                
      ),
      
      tabPanel("Correlogram",
               br(),
               plotOutput(outputId = "corr"), 
               br(),
               h4("On following correlogram, we can see that such questions like those about family members
                   or jaundice dont affect the diagnose. It turned out that the most influencial question was
                   the fourth one - S/he finds it easy to go back and forth between different activities ", align = "center")
              ),
      
      tabPanel("Data", dataTableOutput("data")))
    
))
