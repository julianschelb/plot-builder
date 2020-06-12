#
# Diese Applikation soll dabei helfen schnell einen Überblick über Datensätze zu gewinnen.
#

# Server Logik
shinyServer(function(input, output, session) {
    
    # Definieren reaktiver Values
    values <- reactiveValues()
    values$df <- df

    # Bestimmen der Datentypen der ausgewählten Variablen
    varDataTypes <- reactive({
      
      # bestimmen, ob die ausgewählten Variablen numerisch sind
      var1Num <- input$inputSelect_Var1 %in% names(df %>% select_if(is.numeric))
      if (input$inputSelect_Var2 != "keine") {
        var2Num <- input$inputSelect_Var2 %in% names(df %>% select_if(is.numeric))
      } else {
        var2Num = NA
      }
      
      # speichern der Datentypen als Liste
      varDataTypes <- list(var_1_isNumeric = var1Num, var_2_isNumeric = var2Num)
    })
    
    # Aktualisieren der verfügbaren Diagrammarten auf Grundlage der Datentypen
    observe({
      req(varDataTypes())
      
      # passende Plots finden
      varDataTypes <- varDataTypes()
      plotSuggestions <- listPlotTypes %>% 
        filter(var_1_isNumeric == varDataTypes["var_1_isNumeric"] &
               (var_2_isNumeric == varDataTypes["var_2_isNumeric"] | 
                  (is.na(var_2_isNumeric) & is.na(varDataTypes["var_2_isNumeric"]))
                )
              )
      
      # Liste der passenden Plots
      plotSuggestions <- plotSuggestions %>% distinct(plot) %>% pull(1) 
      
      # Werte des Auswahlfeldes aktualisieren
      updateSelectInput(session, "inputSelect_plotType", 
                        choices = plotSuggestions)
       
    })


    # Plots -------------------------------------------------------------------

    
    #### Histogramm ####
    output$plotUI_hist <- renderPlotly({
      
      req(input$inputSelect_plotType == "Histogramm", 
          input$inputSelect_plotType, input$inputSelect_Var1,
          input$inputText_hist_binsize, input$inputSelect_hist_legendPosition,
          input$inputSelect_hist_fill, input$inputSelect_hist_palette)
          
      # Zwischenspeichern der reaktiven Vriable
      df  <- values$df

      # Deklarieren von Variablen
      var         <- sym(input$inputSelect_Var1)
      
      # Fullfarbe hinzufügen, wenn gewünscht
      if(input$inputSelect_hist_fill == "keine") {
        p <- df %>%
          plot_ly()
      } else{
        p <- df %>%
          plot_ly(color = ~replace(get(input$inputSelect_hist_fill), is.na(get(input$inputSelect_hist_fill)), "NA"),
                  colors = input$inputSelect_hist_palette )
      }
      
      p <- p %>%
        add_histogram(x = ~get(input$inputSelect_Var1), 
                      xbins = list(size = input$inputText_hist_binsize))
      
      # Plot durch Beschriftungen usw. ergänzen
      p <- p %>%
        layout(title = input$inputText_hist_Title,
               xaxis = list(title = ifelse(input$inputText_hist_xDesc == "", 
                                           input$inputSelect_Var1, 
                                           input$inputText_hist_xDesc)),
               yaxis = list(title = ifelse(input$inputText_hist_yDesc == "", 
                                           "Anzahl", 
                                           input$inputText_hist_yDesc)),
               legend = list(title = list(text = ifelse(input$inputText_hist_Legend == "", 
                                           input$inputSelect_hist_fill, 
                                           input$inputText_hist_Legend))),
               barmode='stack'
               )
      
      # Position der Legende anpassen, wenn gewünscht
      if(input$inputSelect_hist_legendPosition == "h") {
        p <- p %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                        xanchor = "center",  # use center of legend as anchor
                                        x = 0.6, y = -0.2)) 
      }
      
      # Ausgabe des Plots
      p
    })
    
    #### Streudiagramm ####
    output$plotUI_scatter <- renderPlotly({
      
      req(input$inputSelect_plotType == "Streudiagramm", 
          input$inputSelect_Var1, input$inputSelect_Var2)
      
      # Zwischenspeichern der reaktiven Vriable
      df  <- values$df
      
      # Fullfarbe hinzufügen, wenn gewünscht
      if(input$inputSelect_scatter_fill == "keine") {
        p <- df %>%
          plot_ly()
      } else{
        p <- df %>%
          plot_ly(color = ~replace(get(input$inputSelect_scatter_fill), is.na(get(input$inputSelect_scatter_fill)), "NA"),
                  colors = input$inputSelect_scatter_palette )
      }
      
      p <- p %>% 
        add_markers(x = ~get(input$inputSelect_Var1), 
                    y = ~get(input$inputSelect_Var2),
                    size = 2)
      
      # Plot durch Beschriftungen usw. ergänzen
      p <- p %>%
        layout(title = input$inputText_scatter_Title,
               xaxis = list(title = ifelse(input$inputText_scatter_xDesc == "", 
                                           input$inputSelect_Var1, 
                                           input$inputText_scatter_xDesc)),
               yaxis = list(title = ifelse(input$inputText_scatter_yDesc == "", 
                                           input$inputSelect_Var2,
                                           input$inputText_scatter_yDesc)),
               legend = list(title = list(text = ifelse(input$inputText_scatter_Legend == "", 
                                                        input$inputSelect_scatter_fill, 
                                                        input$inputText_scatter_Legend)))
        )
      
      # Position der Legende anpassen, wenn gewünscht
      if(input$inputSelect_scatter_legendPosition == "h") {
        p <- p %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                        xanchor = "center",  # use center of legend as anchor
                                        x = 0.6, y = -0.2)) 
      }
      
      # Ausgabe des Plots
      p
  
    })
    
    #### Balkendiagramm ####
    output$plotUI_bar <- renderPlotly({

      req(input$inputSelect_plotType == "Balkendiagramm", 
          input$inputSelect_Var1)

      # Zwischenspeichern der reaktiven Vriable
      df  <- values$df
      
      # Deklarieren der Variablen
      var         <- sym(input$inputSelect_Var1)
      varFill     <- sym(input$inputSelect_bar_fill)

      # Sortierung vornehmen
      order <- df %>%
        count(!!var) %>%
        arrange(desc(n)) %>%
        pull(1)
      
      df[[input$inputSelect_Var1]] <- factor(df[[input$inputSelect_Var1]], levels = c(order))
      
      # Fullfarbe hinzufügen, wenn gewünscht
      if(input$inputSelect_bar_fill == "keine") {
        
        # Freq berechnen
        dfGrouped <- df %>%
          group_by(!!var) %>%
          summarise(n = n())
        
        p <-dfGrouped %>%
          plot_ly()
      } else{
        # Freq berechnen
        dfGrouped <- df %>%
          group_by(!!var, !!varFill) %>%
          summarise(n = n())
        
        p <- dfGrouped %>%
          plot_ly(color = ~replace(get(input$inputSelect_bar_fill), is.na(get(input$inputSelect_bar_fill)), "NA"),
                  colors = input$inputSelect_bar_palette )
      }
      
      p <- p %>% 
        add_bars(x = ~replace(get(input$inputSelect_Var1), is.na(get(input$inputSelect_Var1)), "NA"), y = ~n)
    
      # Plot durch Beschriftungen usw. ergänzen
      p <- p %>%
        layout(title = input$inputText_bar_Title,
               xaxis = list(title = ifelse(input$inputText_bar_xDesc == "", 
                                           input$inputSelect_Var1, 
                                           input$inputText_bar_xDesc)),
               yaxis = list(title = ifelse(input$inputText_bar_yDesc == "", 
                                           "Anzahl",
                                           input$inputText_bar_yDesc)),
               legend = list(title = list(text = ifelse(input$inputText_bar_Legend == "", 
                                                        input$inputSelect_bar_fill, 
                                                        input$inputText_bar_Legend))),
               barmode = input$inputSelect_bar_barmode
        )
      # Position der Legende anpassen, wenn gewünscht
      if(input$inputSelect_bar_legendPosition == "h") {
        p <- p %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                        xanchor = "center",  # use center of legend as anchor
                                        x = 0.6, y = -0.2)) 
      }
      
      # Ausgabe des Plots
      p
    })
    
    #### Kreisdiagramm ####
    output$plotUI_pie <- renderPlotly({
      
      req(input$inputSelect_plotType == "Kreisdiagramm", 
          input$inputSelect_Var1)
      
      # Zwischenspeichern der reaktiven Vriable
      df  <- values$df
      
      # Deklarieren der Variablen
      var         <- sym(input$inputSelect_Var1)
      
      # Freq berechnen
      dfGrouped <- df %>%
        group_by(!!var) %>%
        summarise(n = n())
      
      p <- dfGrouped %>%
        plot_ly() %>%
        add_pie(labels = ~get(input$inputSelect_Var1), values = ~n)
      
      # Plot durch Beschriftungen usw. ergänzen
      p <- p %>%
        layout(title = input$inputText_pie_Title,
               legend = list(title = list(text = ifelse(input$inputText_pie_Legend == "", 
                                                        input$inputSelect_Var1, 
                                                        input$inputText_pie_Legend)))
        )
      
      # Position der Legende anpassen, wenn gewünscht
      if(input$inputSelect_pie_legendPosition == "h") {
        p <- p %>% layout(legend = list(orientation = "h",   # show entries horizontally
                                        xanchor = "center",  # use center of legend as anchor
                                        x = 0.6, y = -0.2)) 
      }
      
      # Ausgabe des Plots
      p
      
    })
    
    #### Heatmap ####
    output$plotUI_heat <- renderPlotly({
      
      req(input$inputSelect_plotType == "Heatmap", 
          input$inputSelect_Var1, input$inputSelect_Var2)
      
      # Zwischenspeichern der reaktiven Vriable
      df  <- values$df
      
      # Deklarieren der Variablen
      var         <- sym(input$inputSelect_Var1)
      var2        <- sym(input$inputSelect_Var2)
      
      # Freq berechnen
      dfGrouped <- df %>%
        group_by(!!var, !!var2) %>%
        summarise(n = n())
      
      # Sortierung vornehmen
      order <- df %>%
        count(!!var) %>%
        arrange(desc(n)) %>%
        pull(1)
      
      dfGrouped[[input$inputSelect_Var1]] <- factor(dfGrouped[[input$inputSelect_Var1]], levels = c(order))
      
      order2 <- df %>%
        count(!!var2) %>%
        arrange(desc(n)) %>%
        pull(1)
      
      dfGrouped[[input$inputSelect_Var2]] <- factor(dfGrouped[[input$inputSelect_Var2]], levels = c(order2))
      
      
      p <- dfGrouped %>%
        #ungroup() %>%
        plot_ly(colors = 'YlOrRd') %>%
        add_heatmap(x = ~replace(get(input$inputSelect_Var1), is.na(get(input$inputSelect_Var1)), "NA"), 
                    y = ~replace(get(input$inputSelect_Var2), is.na(get(input$inputSelect_Var2)), "NA"),
                    z = ~n)
      
      # Plot durch Beschriftungen usw. ergänzen
      p <- p %>%
        layout(title = input$inputText_heat_Title,
               xaxis = list(title = ifelse(input$inputText_heat_xDesc == "", 
                                           input$inputSelect_Var1, 
                                           input$inputText_heat_xDesc)),
               yaxis = list(title = ifelse(input$inputText_heat_yDesc == "", 
                                           input$inputSelect_Var2, 
                                           input$inputText_heat_yDesc))
        )
      
      # Ausgabe des Plots
      p
      
    })
    
    #### Boxplot ####
    output$plotUI_box <- renderPlotly({
      
      req(input$inputSelect_plotType == "Boxplot", 
          input$inputSelect_Var1, input$inputSelect_Var2)
      
      # Zwischenspeichern der reaktiven Vriable
      df  <- values$df
      
      p <- df %>%
        plot_ly() %>%
        add_boxplot(x = ~get(input$inputSelect_Var1), 
                    y = ~get(input$inputSelect_Var2))
      
      # Plot durch Beschriftungen usw. ergänzen
      p <- p %>%
        layout(title = input$inputText_box_Title,
               xaxis = list(title = ifelse(input$inputText_box_xDesc == "", 
                                           input$inputSelect_Var1, 
                                           input$inputText_box_xDesc)),
               yaxis = list(title = ifelse(input$inputText_box_yDesc == "", 
                                           input$inputSelect_Var2, 
                                           input$inputText_box_yDesc))
        )
      
      # Ausgabe des Plots
      p
    })
    
    #### Senkey ####
    output$plotUI_sankey <- renderSankeyNetwork({
      
      req(input$inputSelect_plotType == "Sankey Diagramm", 
          input$inputSelect_Var1, input$inputSelect_Var2)
      
      # Zwischenspeichern der reaktiven Vriable
      df  <- values$df
      
      # Deklarieren der Variablen
      var         <- sym(input$inputSelect_Var1)
      var2        <- sym(input$inputSelect_Var2)
      
      data <- df %>% select(!!var, !!var2)
      
      # NA Werte ersetzen
      colList <- colnames(data)
      data <- data %>% 
        select(all_of(colList)) %>% 
        mutate_at(colList, function(x) {ifelse(is.na(x), "kein Wert", x)})
      
      data_long <- data %>% count(!!var, !!var2)
      
      colnames(data_long) <- c("source", "target", "value")
      data_long$target <- paste(data_long$target, " ", sep="")
      
      # From these flows we need to create a node data frame: it lists every entities involved in the flow
      nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())
      
      # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
      data_long$IDsource=match(data_long$source, nodes$name)-1 
      data_long$IDtarget=match(data_long$target, nodes$name)-1
      
      # prepare colour scale
      ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
      
      # Make the Network
      sankeyNetwork(Links = data_long, Nodes = nodes,
                    Source = "IDsource", Target = "IDtarget",
                    Value = "value", NodeID = "name", 
                    sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)
    })
  
    
    # Tabellen ----------------------------------------------------------------
    
    #### Freq Tabellen erzeugen ####
    output$tableGenerator <- function() {
        req(input$inputSelect_Var1, input$inputSelect_Var2)
        
        # zwischenspeichern der reaktiven Vriable
        df  <- values$df 
        
        # Deklarieren der Variablen 
        var         <- sym(input$inputSelect_Var1)
        var2        <- sym(input$inputSelect_Var2)
        
        
        if (input$inputSelect_Var2 != "keine") {
            df <- table(df %>% select(!!var) %>% pull(1), df %>% select(!!var2) %>% pull(1))
        } else{
            df <- df %>% group_by(!!var) %>% summarise(n = n())
        }
        
        df %>%
            knitr::kable("html") %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
            scroll_box(height = "450px")
    }
    
    #### Download Button ####
    output$buttonDownloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
          
          req(input$inputSelect_Var1, input$inputSelect_Var2)
          
          # zwischenspeichern der reaktiven Vriable
          df  <- values$df 
          
          # Deklarieren der Variablen 
          var         <- sym(input$inputSelect_Var1)
          var2        <- sym(input$inputSelect_Var2)
          
          
          if (input$inputSelect_Var2 != "keine") {
              df <- table(df %>% select(!!var) %>% pull(1), df %>% select(!!var2) %>% pull(1))
          } else{
              df <- df %>% group_by(!!var) %>% summarise(n = n())
          }  
          
        write.csv(df, con)
      })
    
})
