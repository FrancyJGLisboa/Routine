
#This function applies the interpolation process used for filling gaps in 
# outsourcing data tables. 
# the naive_inter function expands the variable year from the minimum year with obseved value
# to the year passed on the Year_end in the function. 
# From that point, operations as the last observed observation carried forward and backward
# are used to replace the NAs for each complet time-series in the country-itemName combination.

# Beyond the dataframe the user can pass  arguments regarding ItemName, ItemCode,
#ElementCode, ElementName, Unit, and Flag. The final outcome is a typical dataframe with
# AreaCode, Areaname, ItemCode, Item Name, ElementCode, ElementName, Year, Value, Unit, Flag

naive_inter <- function(df, Year_ini = NULL, Year_end = NULL){ 
                          
#suppressing warning due to merge operation
options(warn=-1)  

  
if(Year_ini == "initial"){
  df_ts_comp <-   
    df %>% 
    group_by(AreaName, ItemName) %>% 
    complete(Year = min(Year):Year_end) %>% 
    mutate_(na_count = ~ ifelse(all(is.na(Value)), 0, 1),
            YR = ~ ifelse(!is.na(Value), Year, NA),
            YR = ~ifelse(Year == min(Year) & is.na(YR), Year, YR),
            Value = ~ ifelse(na_count == 0, na_count, Value)) %>% 
    ungroup() %>% 
    mutate_(INI_VAL = ~ na.locf(Value) ,
            END_VAL = ~ na.locf(Value, fromLast = TRUE),
            YR_INI =  ~ na.locf(YR),
            YR_INI = ~ ifelse(YR_INI == Year_end, NA, YR_INI),
            YR_INI =  ~ na.locf(YR_INI),
            YR_END = ~ na.locf(YR, fromLast = TRUE),
            Value = ~ ifelse(YR_INI == YR_END,
                             Value,
                             INI_VAL + (END_VAL - INI_VAL)/(YR_END - YR_INI) * (Year - YR_INI))) %>% 
    select_( ~-END_VAL, ~-INI_VAL, ~-YR_END, ~-YR_INI, ~-YR)
  
 
} else {

  df_ts_comp <-   
    df %>% 
    group_by(AreaName, ItemName) %>% 
    complete(Year = Year_ini:Year_end) %>% 
    mutate_(na_count = ~ ifelse(all(is.na(Value)), 0, 1),
            YR = ~ ifelse(!is.na(Value), Year, NA),
            YR = ~ifelse(Year == min(Year) & is.na(YR), Year, YR),
            Value = ~ ifelse(na_count == 0, na_count, Value)) %>% 
    ungroup() %>% 
    mutate_(INI_VAL = ~ na.locf(Value) ,
            END_VAL = ~ na.locf(Value, fromLast = TRUE),
            YR_INI =  ~ na.locf(YR),
            YR_INI = ~ ifelse(YR_INI == Year_end, NA, YR_INI),
            YR_INI =  ~ na.locf(YR_INI),
            YR_END = ~ na.locf(YR, fromLast = TRUE),
            Value = ~ ifelse(YR_INI == YR_END,
                             Value,
                             INI_VAL + (END_VAL - INI_VAL)/(YR_END - YR_INI) * (Year - YR_INI))) %>% 
    select_( ~-END_VAL, ~-INI_VAL, ~-YR_END, ~-YR_INI, ~-YR)
  
}
  
  
#returning dataframe
df_ts_comp

} 

