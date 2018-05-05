##
count_unique <-
  function(data, group, count_what) {
    col_name <- paste0(c("count_unique_of", count_what, "by", group),
                       collapse = "_")
    
    print(paste("add", col_name, "..."))
    
    data[, paste0(col_name) := uniqueN(get(count_what)),
         by = group]
    
  }


##
count_all <-
  function(data, group) {
    col_name <- paste0(c("count_by", group), collapse = "_")
    
    print(paste("add", col_name, "..."))
    
    data[, paste0(col_name) := .N,
         by = group]
  }


##
cum_count <-
  function(data, group) {
    col_name <- paste0(c("cum_count_by", group), collapse = "_")
    
    print(paste("add", col_name, "..."))
    
    data[, paste0(col_name) := 1:.N,
         by = group]
  }


##
cal_delta_both <-
  function(data, group, time_var, add_prev=T) {
    
    col_name <- paste0(c("delta_nextclick_by", group), collapse = "_")
    
    print(paste("add", col_name, "..."))
          
    data[, paste0(col_name) :=
           data.table::shift(get(time_var),
                             type = "lead") - get(time_var),
         by = group]
    
    if (add_prev) {
      
      col_name_fw <- paste0(c("delta_prevlick_by", group), collapse = "_")
      print(paste("add", col_name_fw, "..."))
      
      data[, paste0(col_name_fw) :=
             data.table::shift(get(col_name), type = "lag")]
    }
  }


cal_delta_only_prev <-
  function(data, group, time_var) {
    
    col_name <- paste0(c("delta_prevclick_by", group), collapse = "_")
    
    print(paste("add", col_name, "..."))
    
    data[, paste0(col_name) :=
           get(time_var) - data.table::shift(get(time_var), type = "lag"),
         by = group]
  }






##
cal_var <-
  function(data, group, what) {
    
    col_name <- paste0(c("var_of", what, "by", group), collapse = "_")
    
    print(paste("add", col_name, "..."))
    
    data[,  paste0(col_name) := var(get(what)),
         by = group]
    
    ## fill NA
    data[is.na(col_name), (col_name) := 0]
    
  }

