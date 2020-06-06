#
# Diese Applikation soll dabei helfen schnell einen Überblick über Datensätze zu gewinnen.
#

library(shiny)

# Server Logik
shinyServer(function(input, output, session) {
    
    # Definieren reaktiver Values
    values <- reactiveValues()
    values$df <- df

    # dynamischer Plot, abhängig vom Datentyp der ausgewählten Variable
    output$plotGenerator <- renderPlot({
        req(input$inputSelect_Var1, input$inputSelect_Var2, 
            input$inputSelect_fill, input$inputSelect_facet,
            input$inputSelect_xLabelRotation, input$inputSelect_yLabelRotation,
            #input$inputSelect_shape, input$inputSelect_size,
            input$inputSelect_legendPosition, #input$inputCheck_FlipCoord,
            input$inputSelect_palette, input$inputSelect_theme)
        
        # zwischenspeichern der reaktiven Vriable
        df  <- values$df 
        
        # Deklarieren der Variablen 
        var         <- sym(input$inputSelect_Var1)
        var2        <- sym(input$inputSelect_Var2)
        varFill     <- sym(input$inputSelect_fill)
        #varShape    <- sym(input$inputSelect_shape)
        #varSize     <- sym(input$inputSelect_size)
        varFacet    <- sym(input$inputSelect_facet)
        theme       <- input$inputSelect_theme
        palette     <- input$inputSelect_palette
        
        # Datentypen der Variablen bestimmen
        var1Num <- input$inputSelect_Var1 %in% names(df %>% select_if(is.numeric))
        if (input$inputSelect_Var2 != "keine") {
          var2Num <- input$inputSelect_Var2 %in% names(df %>% select_if(is.numeric))
        }
        
        #Anzahl der Ausprägungen ermitteln
        var1count <- df %>% distinct(!!var) %>% pull(1) %>% length()

        
        ######################### Single Variable - numerisch ################################
        
        if(input$inputSelect_Var2 == "keine" & var1Num) {
            
          # Density Plot oder Histogramm
          if(input$inputCheck_Density) {
            p <- generateBaseDensityPlot(df, var, varFill, input$inputSelect_fill, palette)
          } else{
            p <- generateBaseHistPlot(df, var, varFill, input$inputSelect_fill, palette)
          }
        } 
        
        ######################### Single Variable - kategorial ################################
        
        else if (input$inputSelect_Var2 == "keine" & !var1Num){
          
          if(input$inputCheck_Pie | var1count < 4) {
            
            p <- generateBasePieChart(df, var, palette, input$inputCheck_rmNA)
            
          } else {
            
            p <- generateBaseBarPlot(df, var, varFill, input$inputSelect_fill,
                                     palette, input$inputCheck_rmNA)
          } 
          
        }
        
        ######################### double Variable - kategorial ################################
        
        else if(input$inputSelect_Var2 != "keine" & !var2Num & !var1Num) {
            
          p <- generateBaseHeatmap(df, var, var2, varFill, input$inputSelect_fill, input$inputCheck_rmNA)
            
        }
        
        ######################### double Variable - kategorial/numerisch ################################
        
        else if(input$inputSelect_Var2 != "keine" & (var2Num != var1Num)) {
          
          p <- generateBaseBoxPlot(df, var, var2, input$inputCheck_rmNA)
            
        }
        
        ######################### double Variable - numerisch ################################
        
        else {

          p <- generateBaseScatterplot(df, var, var2, varFill, input$inputSelect_fill, palette)
            
        } 
        
        
        # Achsen wechseln, wenn geüwünscht
        if(input$inputCheck_FlipCoord) {
            p <- p + 
                coord_flip()
        }    
        
        # Plot ergänzen
        p <- p + labs(
            title = input$inputText_Title,
            subtitle = input$inputText_Subtitle,
            caption = input$inputText_Caption,
            tag = input$inputText_Tag
            )
        
        # Achsenbeschriftung ergänzen, wenn ausgefüllt
        if(input$inputText_xDesc != "") p <- p + labs(x = input$inputText_xDesc)
        if(input$inputText_yDesc != "") p <- p + labs(y = input$inputText_yDesc)
        
        # Facet, wenn definiert
        if(input$inputSelect_facet != "keine") {
            p <- p + facet_wrap(as.formula(paste("~", varFacet))) 
        }    
        
        if(input$inputSelect_Var2 == "keine" & 
           (!var1Num & input$inputCheck_Pie | var1count < 4)) {
          
          # Position der Legende und Rotation der Achsenbeschriftung anpassen
          p <- p + theme_minimal() +
            theme(
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.border = element_blank(),
              axis.text.x=element_blank(),
              panel.grid=element_blank(),
              axis.ticks = element_blank(),
              plot.title=element_text(size=14, face="bold")
            )
        } else{
          
          # Je nach Auswahl wird das entsprechende Theme angewendet
          if (theme == "classic")      p <- p + theme_classic()
          if (theme == "schwarz-weiß") p <- p + theme_bw()
          if (theme == "light")        p <- p + theme_light()
          if (theme == "minimal")      p <- p + theme_minimal()
          
          # Position der Legende und Rotation der Achsenbeschriftung anpassen
          p <- p + theme(legend.position=input$inputSelect_legendPosition,
                         axis.text.x = element_text(angle=as.numeric(input$inputSelect_xLabelRotation), hjust = 1),
                         axis.text.y = element_text(angle=as.numeric(input$inputSelect_yLabelRotation))
          )
        }

        
        # Ausgabe des Plots
        p
    }, height = 500)
    
    
    output$sankeyPlotGnerator <- renderSankeyNetwork({
      generateSankeyPlot()
    })
    
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
