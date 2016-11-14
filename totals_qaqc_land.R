#Author: Francy Lisboa

#Description:

# This function check if the estimation of a total area for a given land use categorie
# corresponds to the sum of a set of land use sucategories 



# TOY DATA FRAMES FOR TESTING THE FUNCTION
# #This is an example of the wide df from ratioswide function
# df <- data.frame(AreaName = c(rep("Jaburu", 5), rep("Soledad", 5)),
#                  Year = trunc(seq(1991, 2000, 1)),
#                  I_663_E_000 = rnorm(10, 40, 2.5),
#                  I_664_E_000 = rnorm(10, 30, 21),
#                  I_665_E_000 = rnorm(10, 1000, 4),
#                  I_666_E_000 = rnorm(10, 2000, 5),
#                  I_667_E_000 = rnorm(10, 45, 2.1),
#                  I_668_E_000 = rnorm(10, 110, 24))
# 
# #These are examples of equations used for the QAQC
# Eq1 <- c("I_663_E_000 + I_664_E_000 + I_665_E_000 + I_666_E_000")
# Eq2 <- c("I_663_E_000 + I_666_E_000")
# Eq3 <- c("I_665_E_000 + I_666_E_000")
# # 
# # # toy data frame with equations
# df_eq <- data.frame(Eq_names = c("T_6600", "T_6601", "T_6661"),
#                     Equations = c("I_663_E_000 + I_664_E_000 + I_665_E_000 + I_666_E_000",
#                                   "I_663_E_000 + I_666_E_000",
#                                   "I_665_E_000 + I_666_E_000"))


              
                     
totals_qaqc_land <- function(df,  df_eq , op = NULL) { 

pack_list <- c("dplyr", "tidyr", "stringr")
lapply(pack_list, require, ch = T)
  
  
#to be used at the end
eq <- df_eq[, 2] 
df_country_year <- df[, c(1,2)]
  

#Equations reshape  
e <- as.character(df_eq[, 2])
  
for(i in seq(x)) e[i] <- list(x[i]) 
  
e <- e 
  
x1 <- str_replace_all(e, "[+|-]", "") 
  
x2 <- strsplit(x1, "  ") 
  
int <- lapply(x2, function(z) intersect(z, names(df))) 
  
s1 <- lapply(int, function(z) paste("df$" , z,  sep = ""))
  
s2 <- lapply(s1, function(z)  toString(z))
  
l <- lapply(s2, function(z) gsub(", ", " - " , z))
  

# calculations  
df1 <-  lapply(l, function(x) {
    
  df1 <- 
      df %>% 
      mutate_(~eval(parse(text = x))) 
    
    df1 <- df1[, ncol(df1)]
    
    df1
})
  
  
#Gatherinng results into one dataframe
df2 <- do.call("cbind", df1)
colnames(df2) <- df_eq[ , 1]
  
  
#Adding AreaName and Year
df3 <- cbind(df_country_year, df2)
  
#long format
df4 <- df3 %>% gather(Item, Value, starts_with("T_"))
  
#Returning dataframe with discrepancies 

thres_1 <- 1
thres_2 <- -1
  
df5 <- df4 %>%  filter(Value > thres_1 | Value < thres_2)

# returning two dataframes: 
# 1) with all  equations results 
# 2) with results out of threshold

return(list(df4, df5)) 
  
}


df_res_qaqc <- totals_qaqc_land(df = df, df_eq = df_eq, op = " - ")
