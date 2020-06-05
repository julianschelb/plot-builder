library(janitor)

### Sammlung nützlicher Funktionen #### 
convertFactorToMat <- function(data, bool = TRUE) {
  
  # NA Werte ersetzen
  colList <- colnames(data)
  data <- data %>% 
    select(colList) %>% 
    mutate_at(colList, function(x) {ifelse(is.na(x), "kein Wert", x)})
  
  # Jeden Wert zu einem Factor machen
  data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
  
  # Faktorlevel über alle Spalten hinweg zusammenführen
  allLevels <- unique(unlist(lapply(data , levels)))
  
  # Sicherstellen, dass alle Spalten über die gleichen Faktorlevel verfügen
  for (i in 1:length(colnames(data))) {
    
    print(i)
    data[[i]] <- factor(data[[i]], levels = c(allLevels))
    
    # Matrizen zusammenführen, wenn bereits vorhanden
    if (i > 1) {
      m <- m + model.matrix(~.+0, data[i])
    } else {
      m <- model.matrix(~.+0, data[i])
    }
  }

  # Umwandeln in boolsche Werte, wenn gewünscht
  if(bool) m <- ifelse(m > 0, TRUE, FALSE)
  
  df <- m %>% as.data.frame()
  colnames(df) <- allLevels
  df <- df %>% clean_names()
  
  return(df)
}
