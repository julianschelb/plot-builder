#
# Diese Applikation soll dabei helfen schnell einen Überblick über Datensätze zu gewinnen.
#

# GUI Definition

ui <- fluidPage(
    
    titlePanel("Explorative Datenanalyse"),
    

    # Sidebar -----------------------------------------------------------------
    
    sidebarLayout(
        sidebarPanel(
            selectInput("inputSelect_Var1", "Variable 1 (x-Achse):",
                        selected = "Alter",
                        choices = listVars),
            selectInput("inputSelect_Var2", "Variable 2 (y-Achse):",
                        selected = "keine",
                        choices = c("keine", listVars)),
            
            p("Zum Anzeigen einer Verteilung Variable 1 auswählen. Zum  
            Vergleichen zweier Variablen, z.B. in Form einer Heatmal 
            oder einem Scatter Plot, kann eine zweite Variable ausgewählt
            werden."),
            
            selectInput("inputSelect_plotType", "Diagrammart:",
                        choices = listPlotTypes %>% distinct(plot) %>% pull(1))
        ),
        

        # Main Panel  -------------------------------------------------------------

        mainPanel(
            tabsetPanel(type = "pill",
                        tabPanel("Plot",
                            #br(),
                            

                            ### Histogramm ####
                            
                            conditionalPanel(
                                condition = "input.inputSelect_plotType == 'Histogramm'",
                                plotlyOutput("plotUI_hist", height = 480) %>% withSpinner(),
                                dropdownButton(
                                    circle = FALSE, status = "primary", up = TRUE, label = "weitere Optionen",
                                    icon = icon("gear"), width = "300px", #size = "sm",
                                    tooltip = tooltipOptions(title = "Klicke hier für weitere Optionen!"),
                                    
                                    textInput("inputText_hist_Title", "Titel"),
                                    fluidRow(
                                        column(6,
                                               textInput("inputText_hist_Legend", "Titel der Legende"),
                                        ),
                                        column(6,
                                               selectInput("inputSelect_hist_legendPosition", "Position:",
                                                           selected = "horizontal",
                                                           choices = c("vertikal" = "v", "horizontal" = "h"))
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               textInput("inputText_hist_xDesc", "X-Achsen Beschriftung"),
                                        ),
                                        column(6,
                                               textInput("inputText_hist_yDesc", "Y-Achsen Beschriftung"),
                                        )
                                    ),
                                    hr(),
                                    selectInput("inputSelect_hist_fill", "Variable für Füllfarbe:",
                                                selected = "keine",
                                                choices = c("keine", listVarsChar)),
                                    selectInput("inputSelect_hist_palette", "Farbpalette:",
                                                selected = "Paired",
                                                choices = listColorPalettesQual),
                                    numericInput("inputText_hist_binsize", "Größe der Bins (0 = auto) ",
                                                 min = 0, value = 0)
                                )
                            ),
        

                            ### Streudiagramm ####

                            conditionalPanel(
                                condition = "input.inputSelect_plotType == 'Streudiagramm'",
                                plotlyOutput("plotUI_scatter", height = 480) %>% withSpinner(),
                                dropdownButton(
                                    circle = FALSE, status = "primary", up = TRUE, label = "weitere Optionen",
                                    icon = icon("gear"), width = "300px", #size = "sm",
                                    tooltip = tooltipOptions(title = "Klicke hier für weitere Optionen!"),
                                    
                                    textInput("inputText_scatter_Title", "Titel"),
                                    fluidRow(
                                        column(6,
                                               textInput("inputText_scatter_Legend", "Titel der Legende"),
                                        ),
                                        column(6,
                                               selectInput("inputSelect_scatter_legendPosition", "Position:",
                                                           selected = "horizontal",
                                                           choices = c("vertikal" = "v", "horizontal" = "h"))
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               textInput("inputText_scatter_xDesc", "X-Achsen Beschriftung"),
                                        ),
                                        column(6,
                                               textInput("inputText_scatter_yDesc", "Y-Achsen Beschriftung"),
                                        )
                                    ),
                                    hr(),
                                    selectInput("inputSelect_scatter_fill", "Variable für Füllfarbe:",
                                                selected = "keine",
                                                choices = c("keine", listVarsChar)),
                                    selectInput("inputSelect_scatter_palette", "Farbpalette:",
                                                selected = "Paired",
                                                choices = listColorPalettesQual)
                                ),
                            ),
                            
                            ### Balkendiagramm ####
                            
                            conditionalPanel(
                                condition = "input.inputSelect_plotType == 'Balkendiagramm'",
                                plotlyOutput("plotUI_bar", height = 480) %>% withSpinner(),
                                dropdownButton(
                                    circle = FALSE, status = "primary", up = TRUE, label = "weitere Optionen",
                                    icon = icon("gear"), width = "300px", #size = "sm",
                                    tooltip = tooltipOptions(title = "Klicke hier für weitere Optionen!"),
                                    
                                    textInput("inputText_bar_Title", "Titel"),
                                    fluidRow(
                                        column(6,
                                               textInput("inputText_bar_Legend", "Titel der Legende"),
                                        ),
                                        column(6,
                                               selectInput("inputSelect_bar_legendPosition", "Position:",
                                                           selected = "horizontal",
                                                           choices = c("vertikal" = "v", "horizontal" = "h"))
                                        )
                                    ),
                                    fluidRow(
                                        column(6,
                                               textInput("inputText_bar_xDesc", "X-Achsen Beschriftung"),
                                        ),
                                        column(6,
                                               textInput("inputText_bar_yDesc", "Y-Achsen Beschriftung"),
                                        )
                                    ),
                                    hr(),
                                    selectInput("inputSelect_bar_fill", "Variable für Füllfarbe:",
                                                selected = "keine",
                                                choices = c("keine", listVarsChar)),
                                    selectInput("inputSelect_bar_palette", "Farbpalette:",
                                                selected = "Paired",
                                                choices = listColorPalettesQual),
                                    selectInput("inputSelect_bar_barmode", "Modus:",
                                                selected = "stack",
                                                choices = c("stack", "group"))
                                )
                            ),
                            
                            ### Kreisdiagramm ####
                            
                            conditionalPanel(
                                condition = "input.inputSelect_plotType == 'Kreisdiagramm'",
                                plotlyOutput("plotUI_pie", height = 480) %>% withSpinner(),
                                dropdownButton(
                                    circle = FALSE, status = "primary", up = TRUE, label = "weitere Optionen",
                                    icon = icon("gear"), width = "300px", #size = "sm",
                                    tooltip = tooltipOptions(title = "Klicke hier für weitere Optionen!"),
                                    
                                    textInput("inputText_pie_Title", "Titel"),
                                    fluidRow(
                                        column(6,
                                               textInput("inputText_pie_Legend", "Titel der Legende"),
                                        ),
                                        column(6,
                                               selectInput("inputSelect_pie_legendPosition", "Position:",
                                                           selected = "horizontal",
                                                           choices = c("vertikal" = "v", "horizontal" = "h"))
                                        )
                                    )
                                )
                            ),
                            
                            ### Heatmap ####
                            
                            conditionalPanel(
                                condition = "input.inputSelect_plotType == 'Heatmap'",
                                plotlyOutput("plotUI_heat", height = 480) %>% withSpinner(),
                                dropdownButton(
                                    circle = FALSE, status = "primary", up = TRUE, label = "weitere Optionen",
                                    icon = icon("gear"), width = "300px", #size = "sm",
                                    tooltip = tooltipOptions(title = "Klicke hier für weitere Optionen!"),
                                    
                                    textInput("inputText_heat_Title", "Titel"),
                                    fluidRow(
                                        column(6,
                                               textInput("inputText_heat_xDesc", "X-Achsen Beschriftung"),
                                        ),
                                        column(6,
                                               textInput("inputText_heat_yDesc", "Y-Achsen Beschriftung"),
                                        )
                                    )
                                )
                            ),
                            
                            ### Boxplot ####
                            
                            conditionalPanel(
                                condition = "input.inputSelect_plotType == 'Boxplot'",
                                plotlyOutput("plotUI_box", height = 480) %>% withSpinner(),
                                dropdownButton(
                                    circle = FALSE, status = "primary", up = TRUE, label = "weitere Optionen",
                                    icon = icon("gear"), width = "300px", #size = "sm",
                                    tooltip = tooltipOptions(title = "Klicke hier für weitere Optionen!"),
                                    
                                    textInput("inputText_box_Title", "Titel"),
                                    fluidRow(
                                        column(6,
                                               textInput("inputText_box_xDesc", "X-Achsen Beschriftung"),
                                        ),
                                        column(6,
                                               textInput("inputText_box_yDesc", "Y-Achsen Beschriftung"),
                                        )
                                    )
                                )
                            ),
                            
                            ### Sankey ####
                            
                            conditionalPanel(
                                condition = "input.inputSelect_plotType == 'Sankey Diagramm'",
                                sankeyNetworkOutput("plotUI_sankey", height = 520) %>% withSpinner()
                            )
                        ),
                        
        # Tabelle  -------------------------------------------------------------
                        
                        tabPanel("Tabelle",
                                 br(),
                                 tableOutput("tableGenerator") %>% withSpinner(),
                                 br(),
                                 downloadButton('buttonDownloadData', 'Download') 
                        )
            )
        )
    )
)