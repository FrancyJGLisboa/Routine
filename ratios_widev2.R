
#function to generate the wide format datatable to calculate the ratios in EL domain
ratioswide <- function(df, items_list = NULL){
  
require(dplyr)
require(tidyr)
  
  options(warn = -1)
  
  colnames(df) <- c("ItemCode", "ElementCode", "AreaName", "Year", "Value")
  
  if(items_list %in% c("All")) {
    items_list <- c(6601, 6600, 6610, 6671, 6621, 6713, 
                    6650, 6655, 6700, 6701, 6690, 6661,
                    6714, 6716, 6717, 6729, 6730)
  }
  
  if(items_list %in% c("Agriculture")) {
    items_list <- c(6610, 6671, 6621, 6713,
                    6650, 6655, 6690, 6729)
  }
  
  if(items_list %in% c("Forest")){
    items_list <- c(6661, 6714, 6716, 6717)
  }
  
  df_1 <- 
    df  %>% 
    dplyr::filter_(~ItemCode %in% items_list) %>% 
    dplyr::select_(~ItemCode, ~ElementCode, ~AreaName, ~Year, ~Value) %>%
    dplyr::arrange_(~ItemCode, ~ElementCode, ~AreaName, ~Year) %>% 
    dplyr::mutate_(ItemCode = ~paste0("I_", ItemCode),
                   ElementCode = ~paste0("E_", ElementCode)) %>% 
    tidyr::unite(ITEM_ELEMENT, ItemCode, ElementCode, sep = "_") %>% 
    tidyr::spread(ITEM_ELEMENT, Value) %>% 
    dplyr::arrange_(~AreaName, ~Year)
  
  df_1
  
}

