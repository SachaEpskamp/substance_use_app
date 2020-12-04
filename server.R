shinyServer(function(input, output, session) {
  
  # Page 1 info:
  output$hometext0 <- renderText({
    paste("<h4>", input$foo,
          "Dear Participant,<br><br>Thank you for showing interest in our research!<br><br>In the following, you will be asked to recall patterns of drug use and loneliness of the past 10 years. Please be as honest and truthful as possible. Data will be completely anonymous. We cannot link your email address to the responses you give in the following.<br><br>Indicate whether you have had a negative life event in the given years. Please remember that you do not have to have experience with drug use and/ or loneliness and/or negative life events in order to be able to participate. You can always indicate that.<br><br>In case you do not want to continue, you can always stop participating or ask to withdraw your data without giving reasons for doing so.",
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
        paste0("<h4>Generated participant ID: </h4><h2>",paste0(sample(use,6,TRUE),collapse=""))    
      )
    }
    
  })
  
  output$participantID_reminder <- renderText({
  
      return(
        paste0("<h4>Reminder: you entered the following participant ID: </h4><h2>",input$id,"</h2><h4><br><b>PLEASE WRITE THIS DOWN</b>, you need the participant ID for the next measurement, and due to the annonymous nature of our research we won't know your participant number ourselves.</h4>")    
      )

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
  
  output$buttonToQ2 <- renderUI({
    if (sum(!is.na(vals_alc$y))>1){
      return(
        actionButton(inputId = "nextpanel2", label = "Go to question 2")
      )
    }
  })
  
  # Action go to Q2:
  observeEvent(input$nextpanel2, {
    updateTabsetPanel(session, "PREMISE",
                      selected = "panel3")
  })
  
  
  
  # Action go to Q3:
  output$buttonToQ3 <- renderUI({
    if (sum(!is.na(vals_mar$y))>1){
      return(
        actionButton(inputId = "nextpanel3", label = "Go to question 3")
      )
    }
  })
  
  
  observeEvent(input$nextpanel3, {
    updateTabsetPanel(session, "PREMISE",
                      selected = "panel4")
  })
  
  
  
  # Go to Q4:
  
  output$buttonToQ4 <- renderUI({
    if (sum(!is.na(vals_lon$y))>1){
      return(
        actionButton(inputId = "nextpanel4", label = "Go to question 4")
      )
    }
  })
  
  observeEvent(input$nextpanel4, {
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
    if (vals_mar_hover$y > 10){
      vals_mar_hover$y <- 11
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
    plot(x=vals_mar$x, y=vals_mar$y, xlim=c(2010, 2020), ylim=c(0, 11), ylab="Amount of units per week", xlab="", type="l", lwd=2, xaxt="n", yaxt = "n")
    axis(1, at = seq(2010,2020,by=1), las=2)
    axis(2, at = c(seq(0,10,by=1),11),labels = c(seq(0,10,by=1),"> 10"), las=2)
    
    if (!is.na(vals_mar_hover$x) && !is.na(vals_mar_hover$y) &&
        vals_mar_hover$x > par("usr")[1] && vals_mar_hover$x < par("usr")[2] &&
        vals_mar_hover$y > par("usr")[3] && vals_mar_hover$y < par("usr")[4]){
      
      usage <- vals_mar_hover$y
      if (usage == 11){
        usage <- "> 10"
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
    vals_lon_hover$y <- pmin(1,pmax(-1,round(input$hover_lon$y,2)))
   
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
  observeEvent(input$submitbutton,{
    
    # Collect the user ID:
    ID <- input$id
    
    # collect the life events:
    years <- 2010:2020
    life_event <- years %in% as.numeric(input$liveevents)
    
    # Collect the alcohol responses:
    df_alc <- data.frame(
      ID = ID,
      question = "alcohol",
      time = Sys.time(),
      x = years,
      y = NA,
      life_event = life_event
    )
    y <- vals_alc$y[match(df_alc$x,vals_alc$x)]
    df_alc$y <- round(approx(df_alc$x[!is.na(y)],y[!is.na(y)],xout=df_alc$x)$y,2)
    df_alc$y <- ifelse(df_alc$y > 50, "> 50", df_alc$y)
    
    # Collect marijuana responses:
    df_mar <- data.frame(
      ID = ID,
      question = "marijuana",
      time = Sys.time(),
      x = years,
      y = NA,
      life_event = life_event
    )
    y <- vals_mar$y[match(df_mar$x,vals_mar$x)]
    df_mar$y <- round(approx(df_mar$x[!is.na(y)],y[!is.na(y)],xout=df_mar$x)$y,2)
    df_mar$y <- ifelse(df_mar$y > 10, "> 10", df_mar$y)
    
    # Collect loneliness responses:
    df_lon <- data.frame(
      ID = ID,
      question = "loneliness",
      time = Sys.time(),
      x = years,
      y = NA,
      life_event = life_event
    )
    y <- vals_lon$y[match(df_lon$x,vals_lon$x)]
    df_lon$y <- approx(df_lon$x[!is.na(y)],y[!is.na(y)],xout=df_lon$x)$y
    
    # Combine them all:
    df <- rbind(
      df_alc,
      df_mar,
      df_lon
    )


    # Write file:
    write.csv(df, row.names=FALSE, file = paste0("/data/bachelor_projects/data/",format(Sys.time(), format = "%Y_%m_%d_%H_%M_%S"),"_",ID,".csv"))
  })
  
  # Submitted response:
  output$submitted <- renderText({
    if (input$submitbutton > 0){
      '<p style = "color:red">Your responses have been submitted! You can now close this window.</p>'
    } else {
      return("")
    }
  })
  
})