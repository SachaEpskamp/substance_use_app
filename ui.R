shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
                    
                    h4 {
                    line-height: 1.8;
                    }
                    
                    "),
               type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }")
  ),
  theme = shinytheme("spacelab"),
  tags$style(type = "text/css", "
             .irs-bar {width: 100%; height: 25px; background: #708090; border-top: 1px solid black; border-bottom: 1px solid black;}
             .irs-bar-edge {background: black; border: 1px solid black; height: 25px; border-radius: 0px; width: 20px;}
             .irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
             .irs-grid-text {font-family: 'arial'; color: white; bottom: 17px; z-index: 1;}
             .irs-grid-pol {display: none;}
             .irs-max {font-family: 'arial'; color: black;}
             .irs-min {font-family: 'arial'; color: black;}
             .irs-single {color:black; background:#6666ff;}
             .irs-slider {width: 30px; height: 30px; top: 22px;}
             
             .irs-bar1 {width: 50%; height: 25px; background: red; border-top: 1px solid black; border-bottom: 1px solid black;}
             .irs-bar-edge1 {background: black; border: 1px solid red; height: 25px; border-radius: 0px; width: 20px;}
             .irs-line1 {border: 1px solid red; height: 25px; border-radius: 0px;}
             .irs-grid-text1 {font-family: 'arial'; color: #708090; bottom: 17px; z-index: 1;}
             .irs-grid-pol1 {display: none;}
             .irs-max1 {font-family: 'arial'; color: red;}
             .irs-min1 {font-family: 'arial'; color: red;}
             .irs-single1 {color:black; background:#708090;}
             .irs-slider1 {width: 30px; height: 30px; top: 22px;}
             
             "),
  
  navbarPage("Drug use and loneliness questionnaire",
             id = "PREMISE",
             
             tabPanel("Introduction",
                      # icon = icon("fas fa-home"),
                      value = "panel0",
                      titlePanel("Introduction"),

                      tags$br(),
                      htmlOutput("hometext0"),
                      br(),
                      #tags$hr(),
                      actionButton(inputId = "nextpanel0", label = "Start questionaire"),
                      tags$hr(),
                      htmlOutput("hometext5.1"),
                      htmlOutput("hometext5.2"),
                      htmlOutput("hometext5.3")
             ),
             
             tabPanel("Participant ID",
                      value = "panel1",
                      h4("If this is the first time you use this app, please generate a participant ID here. If you do not have a participant ID, you can generate a random ID below, or think of your own ID you can remember (for example, your date of birth coupled with your lucky number). If this is the second time you are using this app, please do not generate a new one but use the same participant ID as the last time"),
                      actionButton("createID", "Generate a participant ID"),
                      htmlOutput("participantID"),
                      tags$br(),
                      textInput("id","Please enter your participant ID here:"),
                      uiOutput("buttonToQ1")
                      ),
             
             # Alcohol question:
             tabPanel("Q1",
                      value = "panel2",
                      h4("To your best recall, indicate how many drinks you had per week in the past 10 years."),
                      p("You can draw a line in the drawing area below by clicking once and dragging the mouse across the window from LEFT to RIGHT. Once you have finished, click again. You can reset the drawing as many times as you want to. Only your final version will be considered."),
                      # sliderInput("mywidth", "width of the pencil", min=1, max=30, step=1, value=10),
                      plotOutput("plot_alc", width = "800px", height = "500px",
                                 hover=hoverOpts(id = "hover_alc", delay = 100, delayType = "throttle", clip = TRUE, nullOutside = TRUE),
                                 click="click_alc"),
                      actionButton("reset_alc", "RESET DRAWING"),
                      br(), br(),
                      uiOutput("buttonToQ2")
                      ),
             
             
             # Marijuana question:
             tabPanel("Q2",
                      value = "panel3",
                      h4("To your best recall, indicate how often you used cannabis per week in the past 10 years."),
                      p("You can draw a line in the drawing area below by clicking once and dragging the mouse across the window from LEFT to RIGHT. Once you have finished, click again. You can reset the drawing as many times as you want to. Only your final version will be considered. One unit is defined as (for example) one joint."),
                      # sliderInput("mywidth", "width of the pencil", min=1, max=30, step=1, value=10),
                      plotOutput("plot_mar", width = "800px", height = "500px",
                                 hover=hoverOpts(id = "hover_mar", delay = 100, delayType = "throttle", clip = TRUE, nullOutside = TRUE),
                                 click="click_mar"),
                      actionButton("reset_mar", "RESET DRAWING"),
                      br(), br(),
                      uiOutput("buttonToQ3")
                      ),
             
             
             # Loneliness question:
             tabPanel("Q3",
                      value = "panel4",
                      h4("Loneliness is defined as the 'feeling of lacking needed social connections' (Hawkley & Cacioppo, 2010). To your best recall, indicate the levels of social connectedness/ lack of social connections in the past 10 years."),
                      p("You can draw a line in the drawing area below by clicking once and dragging the mouse across the window from LEFT to RIGHT. Once you have finished, click again. You can reset the drawing as many times as you want to. Only your final version will be considered."),
                      # sliderInput("mywidth", "width of the pencil", min=1, max=30, step=1, value=10),
                      plotOutput("plot_lon", width = "800px", height = "500px",
                                 hover=hoverOpts(id = "hover_lon", delay = 100, delayType = "throttle", clip = TRUE, nullOutside = TRUE),
                                 click="click_lon"),
                      actionButton("reset_lon", "RESET DRAWING"),
                      br(), br(),
                      uiOutput("buttonToQ4")
             ),
             
             
             tabPanel("Q4",
                      value = "panel5",
                      h4("A negative life event can be defined as a sudden, acute, undesirable experience that is limited in duration (Adkins et al. 2009)."),
                      
                      checkboxGroupInput("liveevents","To your best recall, did you experience a negative life event in the year...",choices = 2010:2020),
                      br(), br(),
                      actionButton(inputId = "buttonToEnd", label = "To last page")
             ),
             
             
             
             tabPanel("Final",
                      value = "panel6",
                      h4("Thank you for participating in this research! Please press the button below to submit your responses."),
                      p("By clicking the submit button, you confirm your agreement to the terms as stated in the informed consent form (via Google Forms)."),
                      
                      
                      htmlOutput("participantID_reminder"),
                      
                      actionButton(inputId = "submitbutton", label = "SUBMIT RESPONSES"),
                      
                      htmlOutput("submitted")
                      
             )
             
             
             )
        )
             
  )
  
  
 