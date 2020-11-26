shinyServer(function(input, output, session) {
  
  # Page 1 info:
  output$hometext0 <- renderText({
    paste("<h4>", input$foo,
          "Dear Participant,<br><br>Thank you for showing interest in our research!<br><br>In the following, you will be asked to recall patterns of drug use and loneliness of the past 10 years. Please be as honest and truthful as possible.<br><br>For negative life events, please click one answer option per year. Please remember that you do not have to have experience with drug use and/ or loneliness and/or negative life events in order to be able to participate. You can always indicate that.<br><br>In case you do not want to continue, you can always stop participating or ask to withdraw your data without giving reasons for doing so.",
          "</h4>")
  })
 
  
  output$hometext5.1 <- renderText({
    paste("<h3><font color=\"#4682B4\">", "Contact")
  })
  
  output$hometext5.2 <- renderText({
    paste("<h4>", "Maarten van den Ende, University of Amsterdam")
  })
  output$hometext5.3 <- renderText({
    mail <- a("m.w.j.vandenende@uva.nl",href="mailto:m.w.j.vandenende@uva.nl")
    lab <- a("Psychonetrics", href = "http://www.psychonetrics.org", target="_blank")
    paste("<h4>", "E-mail:" ,mail, br(), "Lab:", lab)
  })
  
  
  # Go to panel 1:
  observeEvent(input$nextpanel0, {
    updateTabsetPanel(session, "PREMISE",
                      selected = "panel1")
  })
  
  ## Generate participant ID:
  
  output$participantID <- renderText({
    if (input$createID == 0){
      return("")
    } else {
      use <- c(LETTERS,0:9)
      use <- use[!use %in% c(0,1,"O","I")]
      return(
        paste0("<h4>Your participant ID is: </h4><h2>",paste0(sample(use,6,TRUE),collapse=""),"</h2><h4><br><b>PLEASE WRITE THIS DOWN</b>, you need the participant ID for the next measurement, and due to the annonymous nature of our research we won't know your participant number ourselves.</h4>")    
      )
    }
    
  })
  
  output$participantID_reminder <- renderText({
    if (input$createID == 0){
      return("")
    } else {
      use <- c(LETTERS,0:9)
      use <- use[!use %in% c(0,1,"O","I")]
      return(
        paste0("<h4>Reminder: you entered the following participant ID is: </h4><h2>",input$id,"</h2><h4><br><b>PLEASE WRITE THIS DOWN</b>, you need the participant ID for the next measurement, and due to the annonymous nature of our research we won't know your participant number ourselves.</h4>")    
      )
    }
    
  })
  
  # Button to go to Q1:
  output$buttonToQ1 <- renderUI({
    if (input$id != ""){
      return(
        actionButton(inputId = "nextpanel1", label = "Go to question 1")
      )
    }
  })
  
  # Action go to Q1:
  observeEvent(input$nextpanel1, {
    updateTabsetPanel(session, "PREMISE",
                      selected = "panel2")
  })
  
  
  # Button to Q2:
  
  # Action go to Q2:
  observeEvent(input$buttonToQ2, {
    updateTabsetPanel(session, "PREMISE",
                      selected = "panel3")
  })
  
  
  
  # Action go to Q3:
  observeEvent(input$buttonToQ3, {
    updateTabsetPanel(session, "PREMISE",
                      selected = "panel4")
  })
  
  observeEvent(input$buttonToQ4, {
    updateTabsetPanel(session, "PREMISE",
                      selected = "panel5")
  })
  
  
  observeEvent(input$buttonToEnd, {
    updateTabsetPanel(session, "PREMISE",
                      selected = "panel6")
  })
  
  
  
  ###
  
  
  
  ### ALCOHOL QUESTION ###
  vals_alc_hover <- reactiveValues(x=NA, y=NA)
  vals_alc <- reactiveValues(x=NULL, y=NULL)
  draw_alc <- reactiveVal(FALSE)

  observeEvent(input$click_alc, handlerExpr = {

    temp_alc <- draw_alc(); draw_alc(!temp_alc)
    # if(!draw_alc()) {
    #   browser()
    #   vals_alc$x <- c(vals_alc$x, NA)
    #   vals_alc$y <- c(vals_alc$y, NA)
    # }
    
    })
  
  
  observeEvent(input$reset_alc, handlerExpr = {
    vals_alc$x <- NULL; vals_alc$y <- NULL
  })
  
  observeEvent(input$hover_alc, {
    
    vals_alc_hover$x <- pmin(pmax(round(input$hover_alc$x),2010),2020)
    vals_alc_hover$y <- pmax(0,round(input$hover_alc$y))
    if (vals_alc_hover$y > 50){
      vals_alc_hover$y <- 55
    }
    
    # Hover values:
   
   
    
    if (draw_alc()) {
      vals_alc$x <- c(vals_alc$x, vals_alc_hover$x)
      vals_alc$y <- c(vals_alc$y, vals_alc_hover$y)
      
      if (length(vals_alc$x) > 1){
        if (vals_alc$x[length(vals_alc$x)] < max(vals_alc$x)){
          vals_alc$y <- vals_alc$y[vals_alc$x<=vals_alc$x[length(vals_alc$x)]]
          vals_alc$x <- vals_alc$x[vals_alc$x<=vals_alc$x[length(vals_alc$x)]]
        }
        
        vals_alc$y <- rev(rev(vals_alc$y)[!duplicated(rev(vals_alc$x))])
        vals_alc$x <- rev(rev(vals_alc$x)[!duplicated(rev(vals_alc$x))])
      }
    }})
  
  output$plot_alc <- renderPlot({
    par(cex = 1.5, mar = c(5,6,2,2))
    plot(x=vals_alc$x, y=vals_alc$y, xlim=c(2010, 2020), ylim=c(0, 55), ylab="Amount of drinks per week", xlab="", type="l", lwd=2, xaxt="n", yaxt = "n")
    axis(1, at = seq(2010,2020,by=1), las=2)
    axis(2, at = c(seq(0,50,by=5),55),labels = c(seq(0,50,by=5),"> 50"), las=2)

    if (!is.na(vals_alc_hover$x) && !is.na(vals_alc_hover$y) &&
        vals_alc_hover$x > par("usr")[1] && vals_alc_hover$x < par("usr")[2] &&
        vals_alc_hover$y > par("usr")[3] && vals_alc_hover$y < par("usr")[4]){

      usage <- vals_alc_hover$y
      if (usage == 55){
        usage <- "> 50"
      }
      text(par("usr")[1] + 0.01 * (par("usr")[2]-par("usr")[1]),par("usr")[4] - 0.01 * (par("usr")[4]-par("usr")[3]),paste0("Year: ",vals_alc_hover$x,"; amount: ",usage),adj = c(0,1))  
    }

  })
  
  
  
  
  ### MARIJUNA QUESTION ###
  vals_mar_hover <- reactiveValues(x=NA, y=NA)
  vals_mar <- reactiveValues(x=NULL, y=NULL)
  draw_mar <- reactiveVal(FALSE)
  
  observeEvent(input$click_mar, handlerExpr = {
    
    temp_mar <- draw_mar(); draw_mar(!temp_mar)
    # if(!draw_mar()) {
    #   browser()
    #   vals_mar$x <- c(vals_mar$x, NA)
    #   vals_mar$y <- c(vals_mar$y, NA)
    # }
    
  })
  
  
  observeEvent(input$reset_mar, handlerExpr = {
    vals_mar$x <- NULL; vals_mar$y <- NULL
  })
  
  observeEvent(input$hover_mar, {
    
    vals_mar_hover$x <- pmin(pmax(round(input$hover_mar$x),2010),2020)
    vals_mar_hover$y <- pmax(0,round(input$hover_mar$y))
    if (vals_mar_hover$y > 50){
      vals_mar_hover$y <- 55
    }
    
    # Hover values:
    
    
    
    if (draw_mar()) {
      vals_mar$x <- c(vals_mar$x, vals_mar_hover$x)
      vals_mar$y <- c(vals_mar$y, vals_mar_hover$y)
      
      if (length(vals_mar$x) > 1){
        if (vals_mar$x[length(vals_mar$x)] < max(vals_mar$x)){
          vals_mar$y <- vals_mar$y[vals_mar$x<=vals_mar$x[length(vals_mar$x)]]
          vals_mar$x <- vals_mar$x[vals_mar$x<=vals_mar$x[length(vals_mar$x)]]
        }
        
        vals_mar$y <- rev(rev(vals_mar$y)[!duplicated(rev(vals_mar$x))])
        vals_mar$x <- rev(rev(vals_mar$x)[!duplicated(rev(vals_mar$x))])
      }
    }})
  
  output$plot_mar <- renderPlot({
    par(cex = 1.5, mar = c(5,6,2,2))
    plot(x=vals_mar$x, y=vals_mar$y, xlim=c(2010, 2020), ylim=c(0, 55), ylab="Amount of units per week", xlab="", type="l", lwd=2, xaxt="n", yaxt = "n")
    axis(1, at = seq(2010,2020,by=1), las=2)
    axis(2, at = c(seq(0,50,by=5),55),labels = c(seq(0,50,by=5),"> 50"), las=2)
    
    if (!is.na(vals_mar_hover$x) && !is.na(vals_mar_hover$y) &&
        vals_mar_hover$x > par("usr")[1] && vals_mar_hover$x < par("usr")[2] &&
        vals_mar_hover$y > par("usr")[3] && vals_mar_hover$y < par("usr")[4]){
      
      usage <- vals_mar_hover$y
      if (usage == 55){
        usage <- "> 50"
      }
      text(par("usr")[1] + 0.01 * (par("usr")[2]-par("usr")[1]),par("usr")[4] - 0.01 * (par("usr")[4]-par("usr")[3]),paste0("Year: ",vals_mar_hover$x,"; amount: ",usage),adj = c(0,1))  
    }
    
  })
  
  
  
  ### Loneliness QUESTION ###
  vals_lon_hover <- reactiveValues(x=NA, y=NA)
  vals_lon <- reactiveValues(x=NULL, y=NULL)
  draw_lon <- reactiveVal(FALSE)
  
  observeEvent(input$click_lon, handlerExpr = {
    
    temp_lon <- draw_lon(); draw_lon(!temp_lon)
    # if(!draw_lon()) {
    #   browser()
    #   vals_lon$x <- c(vals_lon$x, NA)
    #   vals_lon$y <- c(vals_lon$y, NA)
    # }
    
  })
  
  
  observeEvent(input$reset_lon, handlerExpr = {
    vals_lon$x <- NULL; vals_lon$y <- NULL
  })
  
  observeEvent(input$hover_lon, {
    
    vals_lon_hover$x <- pmin(pmax(round(input$hover_lon$x),2010),2020)
    vals_lon_hover$y <- pmin(1,pmax(-1,input$hover_lon$y))
   
    # Hover values:
    
    
    
    if (draw_lon()) {
      vals_lon$x <- c(vals_lon$x, vals_lon_hover$x)
      vals_lon$y <- c(vals_lon$y, vals_lon_hover$y)
      
      if (length(vals_lon$x) > 1){
        if (vals_lon$x[length(vals_lon$x)] < max(vals_lon$x)){
          vals_lon$y <- vals_lon$y[vals_lon$x<=vals_lon$x[length(vals_lon$x)]]
          vals_lon$x <- vals_lon$x[vals_lon$x<=vals_lon$x[length(vals_lon$x)]]
        }
        
        vals_lon$y <- rev(rev(vals_lon$y)[!duplicated(rev(vals_lon$x))])
        vals_lon$x <- rev(rev(vals_lon$x)[!duplicated(rev(vals_lon$x))])
      }
    }})
  
  output$plot_lon <- renderPlot({
    par(cex = 1.5, mar = c(5,6,2,2))
    plot(x=vals_lon$x, y=vals_lon$y, xlim=c(2010, 2020), ylim=c(-1, 1), ylab="", xlab="", type="l", lwd=2, xaxt="n", yaxt = "n")
    axis(1, at = seq(2010,2020,by=1), las=2)
    axis(2, at = seq(-1,1,length=5),labels = 
           c("very\nlonely",
             "somewhat\nlonely",
             "neutral",
             "somewhat\nconnected",
             "very\nconnected"), las=2)
    
    if (!is.na(vals_lon_hover$x) && !is.na(vals_lon_hover$y) &&
        vals_lon_hover$x > par("usr")[1] && vals_lon_hover$x < par("usr")[2] &&
        vals_lon_hover$y > par("usr")[3] && vals_lon_hover$y < par("usr")[4]){
      
      text(par("usr")[1] + 0.01 * (par("usr")[2]-par("usr")[1]),par("usr")[4] - 0.01 * (par("usr")[4]-par("usr")[3]),paste0("Year: ",vals_lon_hover$x),adj = c(0,1))  
    }
    
  })
  
  
  
  ### Download data ###
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("data-",input$userid, format(Sys.time(), format = "%Y-%m-%d--%H-%M-%S"), ".csv", sep="")
    },
    content = function(file) {
      # Make data frame:
      df <- expand.grid(
        id = input$userid,
        time = Sys.time(),
        x = 0:100,
        y = NA
      )
      y <- vals$y[match(df$x,vals$x)]
      df$y <- round(approx(df$x[!is.na(y)],y[!is.na(y)],xout=df$x)$y,2)
      write.csv(df, file)
    }
  )
  
  
})