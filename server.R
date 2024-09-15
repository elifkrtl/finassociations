# Finassociations: Investment Recommendation Application
# Author: Elif KARTAL, M. Erdal BALABAN, & Zeki OZEN
# Date: September 2024
# File: server.R

server <- function(input, output, session) {
  {
    shinyjs::js$disableTab("s3")
    shinyjs::js$disableTab("m3")
    shinyjs::js$enableTab("m4")
    my_startDate <- reactive({
      if (input$date_intrvl == 1) {
        startDate <-
          seq(as.Date(my_endDate),
              length = 2,
              by = "-1 weeks")[2] - (my_lag + 3)
      } else if (input$date_intrvl == 2) {
        startDate <-
          seq(as.Date(my_endDate),
              length = 2,
              by = "-1 month")[2] - (my_lag + 3)
      } else if (input$date_intrvl == 3) {
        startDate <-
          seq(as.Date(my_endDate),
              length = 2,
              by = "-3 months")[2] - (my_lag + 3)
      } else if (input$date_intrvl == 4) {
        startDate <-
          seq(as.Date(my_endDate),
              length = 2,
              by = "-6 months")[2] - (my_lag + 3)
      } else if (input$date_intrvl == 5) {
        startDate <-
          seq(as.Date(my_endDate),
              length = 2,
              by = "-1 year")[2] - (my_lag + 3)
      } else{
        startDate <-
          seq(as.Date(my_endDate),
              length = 2,
              by = "-2 years")[2] - (my_lag + 3)
      }
      as.character(startDate)
    })
    
    observeEvent(input$next1, {
      shinyjs::js$enableTab("s3")
      shinyjs::js$enableTab("m3")
      updateTabsetPanel(session, "myTabs",
                        selected = "s3")
      updateTabsetPanel(session, "myTabs2",
                        selected = "m3")
      shinyjs::js$disableTab("s2")
      shinyjs::js$disableTab("m2")
      
    })
    

    
 
  
    observeEvent(input$prev, {
      shinyjs::js$enableTab("s2")
      shinyjs::js$enableTab("m2")
      updateTabsetPanel(session, "myTabs",
                        selected = "s2")
      updateTabsetPanel(session, "myTabs2",
                        selected = "m2")
      shinyjs::js$disableTab("s3")
      shinyjs::js$disableTab("m3")

      output$dataSelection3 <-  DT::renderDataTable(NULL)
      output$dataSelection4 <-  DT::renderDataTable(NULL)
      output$my_assocRules <-  DT::renderDataTable(NULL)
      output$mostFreqBar <- renderPlot(NULL)
      output$ruleGraph <- renderVisNetwork(NULL)
      output$RoC <- renderUI(NULL)
      output$RoC_stat <- renderUI(NULL)
      output$fassocs <- renderUI(NULL)
      output$ress2 <- NULL
    })
  }
  
  
  
  # GRAPHS - TAB I
  {
    # CORRELATIONS AND SCATTER PLOTS
    output$cors1 <- renderPlot({
      ggpairs(data_ARM()[1:(nrow(data_ARM()) - (my_lag)),-1])
    })

    
    # LINE PLOT
    output$lp <- renderPlotly({
      group <- rep("",  times = (nrow(data_ARM()) - (my_lag)))
      
      data_Plot <-
        data_ARM()[1:(nrow(data_ARM())), c("date", input$tSGraph)]
      
      for(i in 1:(nrow(data_Plot)-1)){
        if(data_Plot[i,2] > data_Plot[i+1,2]){
          group[i] <- "INC"
        }else if(data_Plot[i,2] < data_Plot[i+1,2]){
          group[i] <- "DEC"
        }else{
          group[i] <- "NOCH"
        }
        
      }
      
      data_Plot <- data_ARM()[1:(nrow(data_ARM()) - (my_lag)), c("date", input$tSGraph)]
      
      cn <- colnames(data_Plot)[2]
      maxPoint <- max(data_Plot[, 2])
      minPoint <- min(data_Plot[, 2])
      meanPoint <- mean(data_Plot[, 2])
      Thedate <- as.Date(data_Plot[, 1], "%Y-%m-%d")
      Thevalue <- data_Plot[, 2]
      
      p <- data_Plot %>%
        ggplot(aes(x = Thedate, y = Thevalue)) +
        geom_line(color = "#69b3a2") +
        geom_point(aes(color = group)) +
        xlab("Date") +
        ylab(cn) +
        ylim(minPoint, maxPoint) +
        geom_line(color = "#69b3a2") +
        geom_hline(yintercept = meanPoint,
                   color = "orange",
                   size = .5)
      p <- ggplotly(p)
      p
      
    })

  }
  
  
  # PREPARING THE DATASET
  {
    # MP 1 - CURRENCIES
    
    df <- eventReactive(table_connections1(), {
      if (length(input$cur_id) < 1) {
        a <- seq(as.Date(my_startDate()), as.Date(my_endDate), "days")
        a <- a[order(a, decreasing = TRUE)]
        my_defDF <- as.data.frame(matrix(ncol = 1, nrow = length(a)))
        my_defDF$date <- as.character(a)
        colnames(my_defDF)<-c("bos","date")
        return(my_defDF)
      } else{
        return(
          get_my_data(
            data_list = input$cur_id,
            stDate = my_startDate(),
            enDate = my_endDate
          )
        )
      }
      
    }, ignoreNULL = FALSE)
    
    table_connections1 <- reactive({
      list(input$cur_id, input$date_intrvl)
    })
    
    # MP 1 - STOCK MARKETS
    df2 <- eventReactive(table_connections2(), {
      if (length(input$stock_id) < 1) {
        a <- seq(as.Date(my_startDate()), as.Date(my_endDate), "days")
        a <- a[order(a, decreasing = TRUE)]
        my_defDF <- as.data.frame(matrix(ncol = 1, nrow = length(a)))
        my_defDF$date <- as.character(a)
        colnames(my_defDF)<-c("bos","date")
        return(my_defDF)
      } else{
        return(
          get_my_data(
            data_list = input$stock_id,
            stDate = my_startDate(),
            enDate = my_endDate
          )
        )
      }
    }, ignoreNULL = FALSE)
    
    table_connections2 <- reactive({
      list(input$date_intrvl, input$stock_id)
    })
    
    # MP 1 - CRYPTO CUREENCIES

    df3 <- eventReactive(table_connections3(), {
      if (length(input$crypto_id) < 1) {
        a <- seq(as.Date(my_startDate()), as.Date(my_endDate), "days")
        a <- a[order(a, decreasing = TRUE)]
        my_defDF <- as.data.frame(matrix(ncol = 1, nrow = length(a)))
        my_defDF$date <- as.character(a)
        colnames(my_defDF)<-c("bos","date")
        return(my_defDF)
      } else{
        return(
          get_my_data(
            data_list = input$crypto_id,
            stDate = my_startDate(),
            enDate = my_endDate
          )
        )
      }
    }, ignoreNULL = FALSE)
    
    table_connections3 <- reactive({
      list(input$date_intrvl, input$crypto_id)
    })
    
  }
  
  output$my_alert <- reactive({
    if ((sum(
      length(input$cur_id),
      length(input$stock_id),
      length(input$crypto_id)
    )) < 2) {
      return(warning("Please select a minimum of 2 investment tools!"))
    } else{
      return(NULL)
    }
  })
  
  output$dataSelection <- DT::renderDataTable({
    DT::datatable(round(data_ARM()[,-1],3), options = list(pageLength = 10))
  })
  
  observe({
    updateSelectInput(
      session,
      "tSGraph",
      choices = c(input$cur_id, input$crypto_id, input$stock_id)
    )
  })
  
  
  # ARM DATA
  data_ARM <- reactive({
    cur <- df()
    stck <- df2()
    crCur <- df3()

    a <- seq(as.Date(my_startDate()), as.Date(my_endDate), "days")
    a <- a[order(a, decreasing = TRUE)]
    my_defDF <- as.data.frame(matrix(ncol = 1, nrow = length(a)))
    my_defDF$date <- as.character(a)
    colnames(my_defDF)<-c("bos","date")
    
    my_datalist <- list(cur, stck, crCur, my_defDF)

    data <- Reduce(function(x, y)  merge(x, y, all = TRUE), my_datalist)
    empty_columns <- colSums(is.na(data) | data == "") == nrow(data)
    data <- data[,!empty_columns]
    data <- data[-1,]
    row.names(data) <- data$date
    
    data2 <- as.data.frame(lapply(data, zoo::na.locf0, maxgap = 5))
    row.names(data2) <- row.names(data)
    colnames(data2) <- colnames(data)
    
    data2 <-
      data2[order(as.Date(row.names(data2)), decreasing = T),]
    data2 <- data2[-c((nrow(data2) - 3):nrow(data2)), ]
    
  })
  
  # MP 2 - ASSOCIATION RULES
  
  {
    observeEvent(input$next2, {
      dataARM <- data_ARM()
      day_c <- nrow(dataARM) - (my_lag)
      my_data_1 <- as.data.frame(matrix(ncol = 0, nrow = day_c))
      my_data_1 <-
        -1 * data.frame(diff(
          as.matrix(subset(dataARM, select = -c(date))),
          lag = my_lag,
          differences = 1
        ))
      row.names(my_data_1) <- dataARM$date[1:day_c]
      roc_data <- (my_data_1 / dataARM[-(1:my_lag), -1]) * 100
      row.names(roc_data) <- dataARM$date[1:day_c]
 
      
      
      # RATE OF CHANGE TABLE
      output$dataSelection3 <- DT::renderDataTable({
        DT::datatable(round(roc_data, 3), options = list(pageLength = 5))
      })
      output$RoC <- renderUI({tags$h3("The Rate of Change (%)")})
      
      my_data_01 <-
        as.data.frame(ifelse(
          roc_data > as.numeric(input$changeRate_up),
          "INC",
          ifelse(
            roc_data < -as.numeric(input$changeRate_down),
            "DEC",
            "NOCH"
          )
        ))
      
      my_data_02 <-
        as.data.frame(sapply(colnames(my_data_01), function(name) {
          paste(name, my_data_01[, name], sep = "_")
        }))
      row.names(my_data_02) <- row.names(my_data_01)
      
      output$dataSelection4 <- DT::renderDataTable({
        DT::datatable(my_data_02, options = list(pageLength = 5))
      })
      
      output$RoC_stat <- renderUI({tags$h3("The Rate of Change Status")})
      
      # ARM ANALYSES
      my_data_3 <-
        as.data.frame(matrix(ncol = 1, nrow = nrow(my_data_02)))
      rownames(my_data_3) <- rownames(my_data_02)
      colnames(my_data_3) <- "my_items"
      
      for (i in 1:nrow(my_data_02)) {
        y <- unlist(my_data_02[i,])
        y <- y[complete.cases(y)]
        my_data_3[i,] <- paste(y, collapse = ",")
      }
      
      trans <- as(strsplit(my_data_3$my_items, ","), "transactions")
      inspect(head(trans, 5))
      
      # CREATING RULES
      rules <-
        apriori(
          trans,
          parameter = list(
            supp = input$minSup,
            conf = input$minConf,
            minlen = input$minRL,
            target = "rules"
          )
        )
      
      rules <- rules[!is.redundant(rules)]
      inspect(rules)
      summary(trans)
      
      # WARNING
      output$ress2 <- reactive({
        if (length(rules) == 0) {
          return(
            "No sufficient financial assocations! Please try again with different parameters."
          )
        } else{
          return(NULL)
        }
      })
      
      
      # TABLE OF ASSOCIATION RULES
      output$my_assocRules <- DT::renderDataTable(server = FALSE, {
        df <- data.frame(lhs = labels(lhs(rules)),
                         rhs = labels(rhs(rules)),
                         round(rules@quality, 2))
        
        df <- df[order(-df$lift, -df$confidence,-df$support), ]
        df <- df[df$lift > 1, ]
        
        DT::datatable(df,
                      extensions = 'Buttons',
                      options = list(
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      ))
      })
      output$fassocs <- renderUI({tags$h3("Financial Associations")})
      
      
      # GRAPHS - TAB II
      {
      # GRAPH OF ASSOCIATION RULES
      output$ruleGraph <- renderVisNetwork({
        plot(head(rules, n = 10,  by = input$rules_by),
             method = "graph",
             engine = "htmlwidget")
        
      })
      
      # GRAPH OF THE 10 MOST FREQUENT ITEMS
      output$mostFreqBar <- renderPlot({
        arules::itemFrequencyPlot(
          trans,
          topN = 10,
          col = brewer.pal(10, 'Paired'),
          main = 'Relative Item Frequency Plot',
          type = "absolute",
          ylab = "Item Frequency"
        )
        
      })
      
      
      }
      
      
      
    })
    
    output$helpFile <- renderUI({
      rawText <- readLines('my_help.txt')
      splitText <- stringi::stri_split(str = rawText, regex = '\\n')
      replacedText <- lapply(splitText, p)
      replacedText
    })
  }
}
