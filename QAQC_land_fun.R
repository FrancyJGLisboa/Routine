
QAQC_land <- function(df,  x, op = NULL) { 
  
  
  if(missing(df)) stop(" Please provide  wide format df")
  if(missing(x)) stop(" Please provide df column with equations")
  if(is.null(op)) stop(" please provide regular expression with match symbol
                       between brackets")
  
  
  x <- as.character(x)
  
  for(i in seq(x)) e[i] <- list(x[i])
  
  x1 <- str_replace_all(e, "[+|-]", "")
  
  x2 <- strsplit(x1, "  ")
  
  int <- lapply(x2, function(z) intersect(z, names(df)))
  
  s1 <- lapply(int, function(z) paste("df$" , z,  sep = ""))
  
  s2 <- lapply(s1, function(z)  toString(z))
  
  l <- lapply(s2, function(z) gsub(", ", op , z))
  
  
  df1 <-  lapply(l, function(x) {
    
    df1 <- 
      df %>% 
      mutate_(~eval(parse(text = x))) 
    
    df1 <- df1[, ncol(df1)]
    
    df1})
  
  
  
  df2 <- data.frame(Results = matrix(unlist(df1)))
  
  return(df2)
  
}
