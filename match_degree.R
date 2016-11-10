#Date: 11/01/2016
#
#Author: Francy Lisboa
#
#Description:
# This function deal with the comparison between dataframes
# For each new computation  is necessary to compare old and new values 
# to check the cquality of the estimation
# The nommacth function  makes this task by comparing the level of dissimilarity between 
# values. Rows where the difference between values is above the treshold (here 1)
# are returned as dataframe of nonmatched data.

#This functions needs two dataframes to be passed as arguments. 
#Both dataframes MUST HAVE the same columm name sequence: 
# AreaCode, ElementCode, ItemCode, Year, Value

nonmatch_fs <- function(df_new, df_old){
  
  options(warn=-1)
  #Changing columm names
  colnames(df_new) <- c("AreaCode", "ElementCode", "ItemCode", "Year", "Value")
  colnames(df_old) <- c("AreaCode", "ElementCode", "ItemCode", "Year", "Value")
  
  #forcing df_old to be similar to df_new
  df_old1 <- 
    df_old %>% 
    dplyr::filter_(
      ~!(ItemCode %in% dplyr::setdiff(df_old$ItemCode, df_new$ItemCode)),
      ~!(ElementCode %in% dplyr::setdiff(df_old$ElementCode, df_new$ElementCode)),
      ~!(AreaCode %in% dplyr::setdiff(df_old$AreaCode, df_new$AreaCode)),
      ~!(Year %in% dplyr::setdiff(df_old$Year, df_new$Year))) %>% 
    dplyr::arrange_(~AreaCode, ~ElementCode, ~ItemCode, ~Year)
  
  df_new1 <- 
    df_new %>% 
    dplyr::filter_(~!(ItemCode %in% dplyr::setdiff(df_new$ItemCode, df_old$ItemCode)),
                   ~!(ElementCode %in% dplyr::setdiff(df_new$ElementCode, df_old$ElementCode)),
                   ~!(AreaCode %in% dplyr::setdiff(df_new$AreaCode, df_old$AreaCode)),
                   ~!(Year %in% dplyr::setdiff(df_new$Year, df_old$Year))) %>% 
    dplyr::arrange_(~AreaCode, ~ElementCode, ~ItemCode, ~Year)
  
  #joining dataframes
  if(dim(df_old1)[1] >= dim(df_new1)[1]){
    
    joint <- inner_join(df_old1, df_new1, by = c("AreaCode", "ElementCode", "ItemCode", "Year")) %>% 
      dplyr::rename_(Val_old = ~Value.x, Val_new = ~Value.y) %>% 
      dplyr::filter_(~!is.na(Val_old), ~!is.na(Val_new))
    
  } 
  
  if(dim(df_old1)[1] <= dim(df_new1)[1]){
    
    joint <- inner_join(df_new1, df_old1, by = c("AreaCode", "ElementCode", "ItemCode", "Year")) %>% 
      dplyr::rename_(Val_new = ~Value.x, Val_old = ~Value.y) %>% 
      dplyr::filter_(~!is.na(Val_old), ~!is.na(Val_new))
    
  }
  
  tresh <- 1
  
  
  df_diss <- 
    joint %>% 
    dplyr::mutate_(ItemCode = ~paste0("I", ItemCode),
                   ElementCode = ~ paste0("E", ElementCode)) %>% 
    tidyr::unite(Item_Element, ItemCode, ElementCode, sep = "_") %>% 
    dplyr::mutate_(diss = ~abs(Val_new - Val_old),
                   anchor = ~ifelse(diss <= tresh, 1, 2))
  
  
  df_informer <-
    df_diss %>% 
    dplyr::filter_(~anchor == 2)
  
#Retuning the dataframe with discrepant value
  df_informer
  
}







