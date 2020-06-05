library(janitor)

### Sammlung nützlicher Funktionen #### 
convertFactorToMat <- function(data, bool = TRUE) {
  
  # NA Werte ersetzen
  colList <- colnames(data)
  data <- data %>% 
    select(all_of(colList)) %>% 
    mutate_at(colList, function(x) {ifelse(is.na(x), "kein Wert", x)})
  
  # Jeden Wert zu einem Factor machen
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
  
  # Faktorlevel über alle Spalten hinweg zusammenführen
  allLevels <- unique(unlist(lapply(data , levels)))
  
  # Sicherstellen, dass alle Spalten über die gleichen Faktorlevel verfügen
  for (i in 1:length(colnames(data))) {
    
    data[[i]] <- factor(data[[i]], levels = c(allLevels))
    
    # Matrizen zusammenführen, wenn bereits vorhanden
    if (i > 1) {
      m <- m + model.matrix(~.+0, data[i])
    } else {
      m <- model.matrix(~.+0, data[i])
    }
  }

  # Umwandeln in boolsche Werte, wenn gewünscht
  #if(bool) m <- ifelse(m > 0, TRUE, FALSE)
  if(bool) m <- ifelse(m > 0, "Ja", "Nein")
  
  df <- m %>% as.data.frame()
  colnames(df) <- allLevels
  df <- df %>% clean_names()
  
  return(df)
}


# Density Plot ------------------------------------------------------------

generateBaseDensityPlot <- function(df, var, varFill, inputSelect_fill, palette) {
  p <- df %>%
    ggplot(mapping = aes(x = !!var))
  
  # Fill, wenn definiert
  if(inputSelect_fill == "keine") {
    
      p <- p + geom_density(fill = "#1F78B4", color = "#1F78B4", alpha = 0.6)
    
  } else{
    
    # geignete Farbpalette ertstellen
    colourCount = df %>% distinct(!!varFill) %>% pull(1) %>% length()
    getPalette = colorRampPalette(brewer.pal(colourCount, palette))
    
    p <- p + geom_density(aes(fill = !!varFill, color = !!varFill), alpha = 0.6) + 
      scale_fill_manual(values = getPalette(colourCount), na.translate = TRUE, na.value = "grey") +
      scale_color_manual(values = getPalette(colourCount), na.translate = TRUE, na.value = "grey")
  }
  return(p)
}


# Histogramm --------------------------------------------------------------

generateBaseHistPlot <- function(df, var, varFill, inputSelect_fill, palette) {
  
  mean <- df %>% select(!!var) %>% pull(1) %>% mean()
  
  p <- df %>%
    ggplot(mapping = aes(x = !!var))
  
  # Fill, wenn definiert
  if(inputSelect_fill == "keine") {

    p <- p + geom_histogram(binwidth = 1, fill = "#1F78B4") +
      geom_vline(xintercept = mean)
    
  } else{
    
    # geignete Farbpalette ertstellen
    colourCount = df %>% distinct(!!varFill) %>% pull(1) %>% length()
    getPalette = colorRampPalette(brewer.pal(colourCount, palette))

    p <- p + 
      geom_histogram(binwidth = 1, aes(fill = !!varFill)) +
      geom_vline(xintercept = mean) +
      scale_fill_manual(values = getPalette(colourCount), na.translate = TRUE, na.value = "grey")
  }
  return(p)
}


# Balkendiagramm ----------------------------------------------------------

generateBaseBarPlot <- function(df, var, varFill, inputSelect_fill, palette, inputCheck_rmNA) {
  
  # Gruppieren nach einer oder zwei Variablen
  if(inputSelect_fill == "keine") {
    dfGrouped <- df %>% 
      group_by(!!var) %>% 
      summarise(n = n()) 
  } else {
    dfGrouped <- df %>% 
      group_by(!!var, !!varFill) %>% 
      summarise(n = n())  
  }
  
  # NA Werte entfernen, wenn gewünscht
  if (inputCheck_rmNA) {
    dfGrouped <- dfGrouped %>% drop_na()
  }
  
  # Plot erstellen
  p <- dfGrouped %>%
    ggplot(aes(x = reorder(!!var, -n), y = n))
  
  # Fill, wenn definiert
  if(inputSelect_fill == "keine") {
    p <- p + geom_col(fill = "#1F78B4")
  } else{
    
    # geignete Farbpalette ertstellen
    colourCount = df %>% distinct(!!varFill) %>% pull(1) %>% length()
    getPalette = colorRampPalette(brewer.pal(colourCount, palette))
    
    p <- p + geom_col(aes(fill = !!varFill)) +
      scale_fill_manual(values = getPalette(colourCount), 
                        na.translate = TRUE, na.value = "grey")
  }
  return(p)
}


# Heatmap -----------------------------------------------------------------

generateBaseHeatmap <- function(df, var, var2, varFill, inputSelect_fill, inputCheck_rmNA) {
  
  dfGrouped <- df %>% 
    group_by(!!var, !!var2) %>%
    summarise(n = n())
  
  # NA Werte entfernen, wenn gewünscht
  if (inputCheck_rmNA) {
    dfGrouped <- dfGrouped %>% drop_na()
  }
  
  # Plot erstellen
  p <- dfGrouped %>%
    ggplot(aes(x = reorder(!!var, -n), y = reorder(!!var2, -n)))
  
  p <- p + geom_tile(aes(fill = n)) +
    scale_fill_gradient(low="white", high="red")
  
  return(p)
}


# Boxplot -----------------------------------------------------------------

generateBaseBoxPlot <- function(df, var, var2, inputCheck_rmNA) {
  
  # NA Werte entfernen, wenn gewünscht
  if (inputCheck_rmNA) {
    df <- df %>% select(!!var, !!var2) %>% drop_na()
  }
  
  # Plot erstellen
  p <- df %>%
    ggplot(aes(x = !!var, y = !!var2))
  
  p <- p + geom_boxplot()
  
  return(p)
}


# Scatterplot -------------------------------------------------------------

generateBaseScatterplot <- function(df, var, var2, varFill, inputSelect_fill, palette) {
  
  # Plot erstellen
  p <- df %>%
    ggplot(mapping = aes(x = !!var, y = !!var2)) 
  
  # Fill, wenn definiert
  if(inputSelect_fill == "keine") {
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

  return(p)
}
