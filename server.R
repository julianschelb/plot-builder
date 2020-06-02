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
        
        # Kennwerte berechnen
        mean <- df %>% select(!!var) %>% pull(1) %>% mean()
        classVar1 <- df[input$inputSelect_Var1]  %>% pull(1) %>% typeof()
        if (input$inputSelect_Var2 != "keine") {
            classVar2 <- df[input$inputSelect_Var2] %>% pull(1) %>% typeof()
        }

        
        ######################### Single Variable - numerisch ################################
        
        if(input$inputSelect_Var2 == "keine" & classVar1 == "double") {
            
            # Plot erstellen
            p <- df %>%
                ggplot(mapping = aes(x = !!var)) 
            
            # Fill, wenn definiert
            if(input$inputSelect_fill == "keine") {
                
                # Density Plot oder Histogramm
                if(input$inputCheck_Density) {
                    p <- p + geom_density(fill = "#1F78B4", color = "#1F78B4", alpha = 0.6)
                } else{
                    p <- p + geom_histogram(binwidth = 1, fill = "#1F78B4") +
                        geom_vline(xintercept = mean)
                }

            } else{
                
                # geignete Farbpalette ertstellen
                colourCount = df %>% distinct(!!varFill) %>% pull(1) %>% length()
                getPalette = colorRampPalette(brewer.pal(colourCount, palette))
                
                # Density Plot oder Histogramm
                if(input$inputCheck_Density) {
                    p <- p + geom_density(aes(fill = !!varFill, color = !!varFill), alpha = 0.6) + 
                        scale_fill_manual(values = getPalette(colourCount),
                                          na.translate = TRUE, na.value = "grey") +
                        scale_color_manual(values = getPalette(colourCount),
                                          na.translate = TRUE, na.value = "grey")
                } else{
                    p <- p + 
                        geom_histogram(binwidth = 1, aes(fill = !!varFill)) +
                        geom_vline(xintercept = mean) +
                        scale_fill_manual(values = getPalette(colourCount),
                                          na.translate = TRUE, na.value = "grey")
                }
            }   
        
        } 
        
        
        
        ######################### Single Variable - kategorial ################################
        
        else if (input$inputSelect_Var2 == "keine" & classVar1 == "character"){
            
            # Gruppieren nach einer oder zwei Variablen
            if(input$inputSelect_fill == "keine") {
                dfGrouped <- df %>% 
                    group_by(!!var) %>% 
                    summarise(n = n()) 
            } else {
                dfGrouped <- df %>% 
                    group_by(!!var, !!varFill) %>% 
                    summarise(n = n())  
            }
            
            # NA Werte entfernen, wenn gewünscht
            if (input$inputCheck_rmNA) {
                dfGrouped <- dfGrouped %>% drop_na()
            }
            
            # Plot erstellen
            p <- dfGrouped %>%
                ggplot(aes(x = reorder(!!var, -n), y = n))
            
            # Fill, wenn definiert
            if(input$inputSelect_fill == "keine") {
                p <- p + geom_col(fill = "#1F78B4")
            } else{
                
                # geignete Farbpalette ertstellen
                colourCount = df %>% distinct(!!varFill) %>% pull(1) %>% length()
                getPalette = colorRampPalette(brewer.pal(colourCount, palette))
                
                p <- p + geom_col(aes(fill = !!varFill)) +
                    scale_fill_manual(values = getPalette(colourCount), 
                                      na.translate = TRUE, na.value = "grey")
            }         
        }
        
        ######################### double Variable - kategorial ################################
        
        else if(input$inputSelect_Var2 != "keine" & classVar2 == "character" & classVar1 == "character") {
            
            dfGrouped <- df %>% 
                group_by(!!var, !!var2) %>%
                summarise(n = n())
            
            # NA Werte entfernen, wenn gewünscht
            if (input$inputCheck_rmNA) {
                dfGrouped <- dfGrouped %>% drop_na()
            }
            
            # Plot erstellen
            p <- dfGrouped %>%
                ggplot(aes(x = reorder(!!var, -n), y = reorder(!!var2, -n)))
            
            p <- p + geom_tile(aes(fill = n)) +
                scale_fill_gradient(low="white", high="red")
            
        }
        
        ######################### double Variable - kategorial/numerisch ################################
        
        else if(input$inputSelect_Var2 != "keine" & 
                (classVar2 == "character" & classVar1 == "double") | 
                 classVar2 == "double" & classVar1 == "character") {
            
            # Plot erstellen
            p <- df %>%
                ggplot(aes(x = !!var, y = !!var2))
            
            p <- p + geom_boxplot()
            
        }
        
        ######################### double Variable - numerisch ################################
        
        else {

            # Plot erstellen
            p <- df %>%
                ggplot(mapping = aes(x = !!var, y = !!var2)) 
            
            # Fill, wenn definiert
            if(input$inputSelect_fill == "keine") {
                p <- p + 
                    geom_point(colour = "#1F78B4", shape = 20, size = 4)
            } else{
                
                # geignete Farbpalette ertstellen
                colourCount = df %>% distinct(!!varFill) %>% pull(1) %>% length()
                getPalette = colorRampPalette(brewer.pal(colourCount, palette))
                
                p <- p + 
                    geom_point(aes(colour = !!varFill), shape = 20, size = 4) +
                    scale_colour_manual(values = getPalette(colourCount), 
                                        na.translate = TRUE, na.value = "grey")
            }  
            
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
        
        # Ausgabe des Plots
        p
    }, height = 500)
    
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
