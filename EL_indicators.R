#Creating a function to get shares of total land area
EL_indicators <- function(df, indicator = NULL){

#Packages the fucntion need
require("dplyr")
require("tidyr")
require("xlsx")

# loading AREAS reference table
AREAS <- read.xlsx("T:/Team_working_folder/Env/Reserved/2016/ANALYSIS/AEIN/EL/data/AREAS.xls", sheetIndex = 1) %>% tbl_df()
  
# loading ITEM reference table
ITEMS <- read.xlsx("T:/Team_working_folder/Env/Reserved/2016/ANALYSIS/AEIN/EL/data/ITEMS.xls", sheetIndex = 1) %>% tbl_df()

  
# Parameters for annual change 
if(indicator == 7207){
dots_mutate_1 <- list(~((I_6661_E_5110 - dplyr::lag(I_6661_E_5110))/dplyr::lag(I_6661_E_5110)) * 100, 
                        ~((I_6714_E_5110 - dplyr::lag(I_6714_E_5110))/dplyr::lag(I_6714_E_5110)) * 100,
                        ~((I_6716_E_5110 - dplyr::lag(I_6716_E_5110))/dplyr::lag(I_6716_E_5110)) * 100,
                        ~((I_6717_E_5110 - dplyr::lag(I_6717_E_5110))/dplyr::lag(I_6717_E_5110)) * 100,
                        ~((I_6610_E_5110 - dplyr::lag(I_6610_E_5110))/dplyr::lag(I_6610_E_5110)) * 100,
                        ~((I_6671_E_5110 - dplyr::lag(I_6671_E_5110))/dplyr::lag(I_6671_E_5110)) * 100,
                        ~((I_6729_E_5110 - dplyr::lag(I_6729_E_5110))/dplyr::lag(I_6729_E_5110)) * 100,
                        ~((I_6730_E_5110 - dplyr::lag(I_6730_E_5110))/dplyr::lag(I_6730_E_5110)) * 100,
                        ~((I_6700_E_5110 - dplyr::lag(I_6700_E_5110))/dplyr::lag(I_6700_E_5110)) * 100,
                        ~((I_6701_E_5110 - dplyr::lag(I_6701_E_5110))/dplyr::lag(I_6701_E_5110)) * 100, 
                        ~((I_6713_E_5110 - dplyr::lag(I_6713_E_5110))/dplyr::lag(I_6713_E_5110)) * 100, 
                        ~((I_6621_E_5110 - dplyr::lag(I_6621_E_5110))/dplyr::lag(I_6621_E_5110)) * 100, 
                        ~((I_6650_E_5110 - dplyr::lag(I_6650_E_5110))/dplyr::lag(I_6650_E_5110)) * 100, 
                        ~((I_6655_E_5110 - dplyr::lag(I_6655_E_5110))/dplyr::lag(I_6655_E_5110)) * 100, 
                        ~((I_6690_E_5110 - dplyr::lag(I_6690_E_5110))/dplyr::lag(I_6690_E_5110)) * 100)
  
  names_mutate_1 <- c("I6661_E7207", "I6714_E7207", "I6716_E7207", "I6717_E7207",
                      "I6610_E7207", "I6671_E7207", "I6729_E7207", "I6730_E7207",
                      "I6700_E7207", "I6701_E7207", "I6713_E7207", "I6621_E7207",
                      "I6650_E7207", "I6655_E7207", "I6690_E7207")
  
  
  dots_mutate_2 <- list(~as.integer(substring(ItemCode, 2)),
                        ~as.integer(substring(ElementCode, 2)),
                        ~"EL",
                        ~"Land",
                        ~"Fc",
                        ~ "Calculated data",
                        ~ "% of annual change",
                        ~ as.integer(ItemCode),
                        ~ "Percentage")
  
  names_mutate_2 <- c("ItemCode", "ElementCode", "DomainCode",
                      "DomainName", "Flag", "FlagD",
                      "ElementName", "ItemCode", "Unit")
  
  
  dots_rename <- list(~AREA_CODE, ~ITEM_NAME)
  
  dots_select_1 <- list(~DomainCode, ~DomainName, 
                        ~AreaCode, ~AreaName, 
                        ~ItemCode, ~ItemName,
                        ~ElementCode, ~ElementName,
                        ~Year, ~Value,
                        ~Unit, ~Flag, ~FlagD)
  
EL_df <-
  df %>% 
    dplyr::mutate_(.dots = setNames(dots_mutate_1, names_mutate_1)) %>% 
    dplyr::select(-matches("I_")) %>% 
    tidyr::gather(ITEM_ELEMENT_CODE, Value, I6661_E7207:I6690_E7207) %>% 
    tidyr::separate(ITEM_ELEMENT_CODE, c("ItemCode", "ElementCode")) %>% 
    dplyr::mutate_(.dots = setNames(dots_mutate_2, names_mutate_2)) %>% 
    dplyr::filter_(~!is.na(Value)) %>%  
    dplyr::left_join(AREAS, by =  c(AreaName = "AREA_NAME")) %>% 
    dplyr::left_join(ITEMS, by = c("ItemCode"= "ITEM_CODE")) %>%
    dplyr::rename_(.dots = setNames(dots_rename, c("AreaCode", "ItemName"))) %>% 
    select_(.dots = dots_select) %>% 
    dplyr::group_by_(~AreaName) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange_(~AreaCode, ~AreaName, ~ItemName, ~ElementName, ~Year) 
  
}

# Parameters for % of agriculture area
if(indicator == 7208){
  
dots_mutate_1 <- list(~(I_6671_E_5110/I_6610_E_5110) * 100,  
                        ~(I_6729_E_5110/I_6610_E_5110) * 100, 
                        ~(I_6713_E_5110/I_6610_E_5110) * 100, 
                        ~(I_6621_E_5110/I_6610_E_5110) * 100, 
                        ~(I_6650_E_5110/I_6610_E_5110) * 100, 
                        ~(I_6655_E_5110/I_6610_E_5110) * 100, 
                        ~(I_6690_E_5110/I_6610_E_5110) * 100) 
  
names_mutate_1 <- c("I6671_E7208", "I6729_E7208", "I6713_E7208", 
                        "I6621_E7208","I6650_E7208", "I6655_E7208", "I6690_E7208")
    
    
dots_mutate_2 <- list(~as.integer(substring(ItemCode, 2)),
                          ~as.integer(substring(ElementCode, 2)),
                          ~"EL",
                          ~"Land",
                          ~"Fc",
                          ~ "Calculated data",
                          ~ "% of Agriculture area",
                          ~ as.integer(ItemCode),
                          ~ "Percentage")
    
names_mutate_2 <- c("ItemCode", "ElementCode", "DomainCode",
                        "DomainName", "Flag", "FlagD",
                        "ElementName", "ItemCode", "Unit")
    
    
dots_rename <- list(~AREA_CODE, ~ITEM_NAME)
    
dots_select_1 <- list(~DomainCode, ~DomainName, 
                          ~AreaCode, ~AreaName, 
                          ~ItemCode, ~ItemName,
                          ~ElementCode, ~ElementName,
                          ~Year, ~Value,
                          ~Unit, ~Flag, ~FlagD)
    
EL_df <-
      df %>% 
      dplyr::mutate_(.dots = setNames(dots_mutate_1, names_mutate_1)) %>% 
      dplyr::select(-matches("I_")) %>% 
      tidyr::gather(ITEM_ELEMENT_CODE, Value, I6671_E7208:I6690_E7208) %>% 
      tidyr::separate(ITEM_ELEMENT_CODE, c("ItemCode", "ElementCode")) %>% 
      dplyr::mutate_(.dots = setNames(dots_mutate_2, names_mutate_2)) %>% 
      dplyr::filter_(~!is.na(Value)) %>%  
      dplyr::left_join(AREAS, by =  c(AreaName = "AREA_NAME")) %>% 
      dplyr::left_join(ITEMS, by = c("ItemCode"= "ITEM_CODE")) %>%
      dplyr::rename_(.dots = setNames(dots_rename, c("AreaCode", "ItemName"))) %>% 
      select_(.dots = dots_select) %>% 
      dplyr::group_by_(~AreaName) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange_(~AreaCode, ~AreaName, ~ItemName, ~ElementName, ~Year) 

}
  
  
# Parameters for % of land area 
if(indicator == 7209){
  
dots_mutate_1 <- list(~(I_6661_E_5110/I_6601_E_5110) * 100,
                      ~(I_6714_E_5110/I_6601_E_5110) * 100, 
                      ~(I_6716_E_5110/I_6601_E_5110) * 100, 
                      ~(I_6717_E_5110/I_6601_E_5110) * 100, 
                      ~(I_6610_E_5110/I_6601_E_5110) * 100, 
                      ~(I_6671_E_5110/I_6601_E_5110) * 100,  
                      ~(I_6729_E_5110/I_6601_E_5110) * 100, 
                      ~(I_6730_E_5110/I_6601_E_5110) * 100, 
                      ~(I_6700_E_5110/I_6601_E_5110) * 100,  
                      ~(I_6701_E_5110/I_6600_E_5110) * 100,  
                      ~(I_6713_E_5110/I_6601_E_5110) * 100, 
                      ~(I_6621_E_5110/I_6601_E_5110) * 100, 
                      ~(I_6650_E_5110/I_6601_E_5110) * 100, 
                      ~(I_6655_E_5110/I_6601_E_5110) * 100, 
                      ~(I_6690_E_5110/I_6601_E_5110) * 100) 
  
names_mutate_1 <- c("I6661_E7209", "I6714_E7209", "I6716_E7209", "I6717_E7209",
                        "I6610_E7209", "I6671_E7209", "I6729_E7209", "I6730_E7209",
                        "I6700_E7209", "I6701_E7209", "I6713_E7209", "I6621_E7209",
                        "I6650_E7209", "I6655_E7209", "I6690_E7209")


dots_mutate_2 <- list(~as.integer(substring(ItemCode, 2)),
                      ~as.integer(substring(ElementCode, 2)),
                      ~"EL",
                      ~"Land",
                      ~"Fc",
                      ~ "Calculated data",
                      ~ "% of Land area",
                      ~ as.integer(ItemCode),
                      ~ "Percentage")

names_mutate_2 <- c("ItemCode", "ElementCode", "DomainCode",
                    "DomainName", "Flag", "FlagD",
                    "ElementName", "ItemCode", "Unit")



dots_rename_1 <- list(~AREA_CODE, ~ITEM_NAME)

dots_select_1 <- list(~DomainCode, ~DomainName, 
                    ~AreaCode, ~AreaName, 
                    ~ItemCode, ~ItemName,
                    ~ElementCode, ~ElementName,
                    ~Year, ~Value,
                    ~Unit, ~Flag, ~FlagD) 
 

EL_df <-
  df %>% 
  dplyr::mutate_(.dots = setNames(dots_mutate_1, names_mutate_1)) %>% 
  dplyr::select(-matches("I_")) %>% 
  tidyr::gather(ITEM_ELEMENT_CODE, Value, I6661_E7209:I6690_E7209) %>% 
  tidyr::separate(ITEM_ELEMENT_CODE, c("ItemCode", "ElementCode")) %>% 
  dplyr::mutate_(.dots = setNames(dots_mutate_2, names_mutate_2)) %>% 
  dplyr::filter_(~!is.na(Value)) %>%  
  dplyr::left_join(AREAS, by =  c(AreaName = "AREA_NAME")) %>% 
  dplyr::left_join(ITEMS, by = c("ItemCode"= "ITEM_CODE")) %>%
  dplyr::rename_(.dots = setNames(dots_rename, c("AreaCode", "ItemName"))) %>% 
  select_(.dots = dots_select) %>% 
  dplyr::group_by_(~AreaName) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange_(~AreaCode, ~AreaName, ~ItemName, ~ElementName, ~Year) 

}
  
# Parameters for % of Forest area 
if(indicator == "EXXX1"){
    
dots_mutate_1 <- list(~(I_6661_E_5110/I_6661_E_5110) * 100,
                          ~(I_6714_E_5110/I_6661_E_5110) * 100, 
                          ~(I_6716_E_5110/I_6661_E_5110) * 100, 
                          ~(I_6717_E_5110/I_6661_E_5110) * 100) 
    
names_mutate_1 <- c("I6661_EXXX1", "I6714_EXXX1", "I6716_EXXX1", "I6717_EXXX1")
    
    
dots_mutate_2 <- list(~as.integer(substring(ItemCode, 2)),
                          ~as.integer(substring(ElementCode, 2)),
                          ~"EL",
                          ~"Land",
                          ~"Fc",
                          ~ "Calculated data",
                          ~ "% of Forest area",
                          ~ as.integer(ItemCode),
                          ~ "Percentage")
    
names_mutate_2 <- c("ItemCode", "ElementCode", "DomainCode",
                        "DomainName", "Flag", "FlagD",
                        "ElementName", "ItemCode", "Unit")
    
    
    
dots_rename_1 <- list(~AREA_CODE, ~ITEM_NAME)
    
dots_select_1 <- list(~DomainCode, ~DomainName, 
                          ~AreaCode, ~AreaName, 
                          ~ItemCode, ~ItemName,
                          ~ElementCode, ~ElementName,
                          ~Year, ~Value,
                          ~Unit, ~Flag, ~FlagD) 
    
    
EL_df <-
      df %>% 
      dplyr::mutate_(.dots = setNames(dots_mutate_1, names_mutate_1)) %>% 
      dplyr::select(-matches("I_")) %>% 
      tidyr::gather(ITEM_ELEMENT_CODE, Value, I6661_EXXX1:I6717_EXXX1) %>% 
      tidyr::separate(ITEM_ELEMENT_CODE, c("ItemCode", "ElementCode")) %>% 
      dplyr::mutate_(.dots = setNames(dots_mutate_2, names_mutate_2)) %>% 
      dplyr::filter_(~!is.na(Value)) %>%  
      dplyr::left_join(AREAS, by =  c(AreaName = "AREA_NAME")) %>% 
      dplyr::left_join(ITEMS, by = c("ItemCode"= "ITEM_CODE")) %>%
      dplyr::rename_(.dots = setNames(dots_rename, c("AreaCode", "ItemName"))) %>% 
      select_(.dots = dots_select) %>% 
      dplyr::group_by_(~AreaName) %>% 
      dplyr::ungroup() %>% 
      dplyr::arrange_(~AreaCode, ~AreaName, ~ItemName, ~ElementName, ~Year) 
    
}

# returning df

EL_df

}




EL_7209 <- EL_indicators(df = wide_bind, indicator = 7209)
  
  

formals(EL_indicators)



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
