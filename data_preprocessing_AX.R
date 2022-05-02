

# load require packages
library(data.table)
library(dplyr)
library(tidyr)
library(forcats)
library(gt)


### read input data, change path here
inputData <- read.csv("/Users/xieaoji/Downloads/mock_data_4table.csv")

## there are 3 functions,  task1Func() is for adding Difference (95% CI) and LSMEAN (SE/SD), take the inputData as input
##                         task2Func() is for adding * to p-values, take the output of task1Func() as input
##                         plotTableFunc() is for plotting the table using gt package, take the output of task2Func() as input

# How to run those functions: (see at line 153:156)
## task1_result <- task1Func(inputData)
## task2_result <- task2Func(task1_result)
## plot <- plotTableFunc(task2_result)

###############  Task1  ##############
task1Func <- function(dt) {
    
    data <- dt %>% setDT() 
    data_95 <- data[resulttype %in% c("95% CI High", "95% CI Low", "Difference")] %>%.[,result := sprintf('%#.2f', round(result,2))]
    data_se <- data[resulttype %in% c( "LS Mean", "Std Err")] %>%.[,result :=sprintf('%#.2f', round(result,2))] 
    data_other <- data[!resulttype %in% c("95% CI High", "95% CI Low", "Difference", "LS Mean", "Std Err")] %>% .[,result :=sprintf('%#.2f', round(result,2))]
  
    data_wide1 <- data_95 %>% 
      pivot_wider(names_from = resulttype, values_from = result) %>% setDT %>% 
      .[,`Difference (95% CI)` := paste0(Difference, "(",`95% CI Low`, "," , `95% CI High`, ")" )]
    
    data_wide2 <- data_se %>%
      pivot_wider(names_from = resulttype, values_from = result) %>% setDT %>% 
      .[, `LSMEAN (SE/SD)` := paste(`LS Mean`, "(",`Std Err`, ")" , sep = "")]
    
    data_long1 <- data_wide1 %>%
                pivot_longer( -c("timepointn", "timepoint" ,
                                 "timepoint2n", "timepoint2",  "trtn", "trt",
                                 "reftrtn", "reftrt", "analysistype",   "analysistype2",
                                 "resulttype2", "resulttype3", "round"), values_to = "result", names_to = "resulttype")
    data_long2 <- data_wide2 %>%
      pivot_longer( -c("timepointn", "timepoint" ,
                       "timepoint2n", "timepoint2",  "trtn", "trt",
                       "reftrtn", "reftrt", "analysistype",   "analysistype2",
                       "resulttype2", "resulttype3", "round"), values_to = "result", names_to = "resulttype")
    
    result <- rbind(data_long1, data_long2, data_other) %>%  setDT %>% .[order( timepointn )]
    
    result
   
}


##############TASK 2: add star to p-value##########
task2Func <- function(dt){

    data2 <- dt
    data2_in <- data2[resulttype ==  "p-value"] 
    data2_other <- data2[resulttype !="p-value"] 
    
    data2_wide <- data2_in %>% pivot_wider(names_from = resulttype, values_from = result) %>% setDT %>%
                  .[,`p-value*` := ifelse(`p-value`%>% as.numeric() < 0.001, paste0(`p-value`, "***"), 
                                          ifelse(`p-value` %>% as.numeric()  < 0.01, paste0(`p-value`,"**"),
                                                                         ifelse(`p-value`%>% as.numeric() < 0.05, paste0(`p-value`,"*"), `p-value`)))]
    data2_long <- data2_wide %>% 
                .[,`p-value` := format(`p-value`,drop0Trailing = F) %>% as.character()] %>%
                pivot_longer( -c("timepointn", "timepoint" ,
                       "timepoint2n", "timepoint2",  "trtn", "trt",
                       "reftrtn", "reftrt", "analysistype",   "analysistype2",
                       "resulttype2", "resulttype3", "round"), values_to = "result", names_to = "resulttype")
    result2 <- rbind(data2_long, data2_other) %>%  setDT %>% .[order( timepointn )]
    
    result2
}

################Task3: Plot The Table#############
plotTableFunc <- function(dt){

      data3 <- copy(dt) 
      data3 <- data3 %>% .[,key := paste0(timepoint, trtn, trt)]
      ## step 1, concat the p value for non PBO groups##
      data3_in <- data3[resulttype == "p-value*"]
      data3_other <- data3[resulttype != "p-value*"]
      
      data3_wide <- data3_in %>% pivot_wider(names_from = resulttype, values_from = result) %>% setDT()
      
      group_p <- data3_wide[, list(`p-value*` = paste(rev(`p-value*`), collapse = " ")), by = key]
      
            data3_merge <- merge(data3_wide, group_p, by.x = "key", by.y = "key")
      setnames(data3_merge, "p-value*.y", "p-value*")
      data3_merge$`p-value*.x` = NULL
      data3_merge$key = NULL
      
      data3_long <- data3_merge %>% pivot_longer( -c("timepointn", "timepoint" ,
                                                     "timepoint2n", "timepoint2",  "trtn", "trt",
                                                     "reftrtn", "reftrt", "analysistype",   "analysistype2",
                                                     "resulttype2", "resulttype3", "round"), values_to = "result", names_to = "resulttype")
      data3_other$key <- NULL
      data3$key <- NULL
      result3 <- rbind(data3_long, data3_other) %>%  setDT %>% .[order( timepointn )]
      
      ## step2 construct input for table plot##
      result3 <- result3[ analysistype != "Change from baseline"] %>% 
              .[,list(ANALYSISTYPE = analysistype, RESULTTYPE = resulttype, trt, result, timepoint, reftrt)] 
      
      dt1 <- result3[RESULTTYPE %in% c( "Difference (95% CI)", "N")]
      dt2 <- result3[RESULTTYPE %in% c("p-value*","LSMEAN (SE/SD)") & !reftrt %in% c("PBO")]
      input_table <- rbind(dt1, dt2) 
      
      exibble <- input_table%>%
        pivot_wider(names_from = trt, values_from = result) %>% 
        setDT() %>% .[,reftrt :=NULL] 
      
      exibble[is.na(exibble)] <- "-"
      exibble[exibble == ""] <- "-"
      
      #### format table ####
      exibble %>%
        mutate(RESULTTYPE= fct_relevel(RESULTTYPE, c("N","LSMEAN (SE/SD)", "p-value*","Difference (95% CI)"))) %>%
        arrange(RESULTTYPE) %>%
        gt(rowname_col = "ANALYSISTYPE", groupname_col = "timepoint") %>%
        tab_stubhead(label = "ANALYSISTYPE") %>%
        tab_header(
          title = md("title"),
          subtitle = md("subtitle")
        ) %>% 
        opt_align_table_header(align = "left") %>%
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_column_labels()
        ) %>% 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_row_groups()
        ) %>% 
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_stubhead()
        ) %>% 
        tab_options(table.font.size = px(18)) %>%
        opt_table_font(
          font = list(
            google_font(name = "Merriweather"),
            "Cochin", "Serif"
          )
        )
}


task1_result <- task1Func(inputData)
task2_result <- task2Func(task1_result)
plot <- plotTableFunc(task2_result)

