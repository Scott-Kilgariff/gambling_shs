

format_read_plot <- function(desc, text){
  
  # read the df created by 1_gambling_data.R
  df_data <- read_csv("df_sheet.csv")
  
  # in this report we are only looking at questions
  # where the answer is Yes/No.  The report will only
  # report the percentage of people answering "Yes"
  df_data <- df_data %>%
    filter(response_map == "Yes")
  
  # the category values, i.e. age groups, SIMD quintiles
  # need to be mapped to more readable values for display
  # on the charts
  map_category <- read.csv("map_category.csv")
  
  df_data <- df_data %>%
    left_join(map_category, by = c("category" = "category"))
  
  # remove unnecessary text from the questions
  df_data$measure_map <- gsub('Spent any money on:', '', df_data$measure_map)
  df_data$measure_map <- gsub('Spent money on:', '', df_data$measure_map)
  
  # The ordinal categories need to be assigned an order
  df_data$category_map <- ordered(df_data$category_map, 
                                  levels = c("All populations", '16 to 24', '25 to 34', '35 to 44', '45 to 54', '55 to 65', '65 to 74', '75 plus', 
                                             'Manager & prof.', 'Intermediate', 'Small emps & own account', 'Lower supervisory & tech.', 'Semi routine and routine', 
                                             'Top quintile earners', 'Top quintile SIMD', 'x2', 'x3', 'x4', '5th quintile earners', '5th quintil SIMD'))
  
  # specify which of the group descriptions are to be included
  # e.g. age_grp or simd_quintiles
  df_data <- df_data %>% filter(description %in% desc)
  
  # for each question...  
  for (m in seq_along(unique(df_data$measure_map))){
    
    # filter the dataframe using each question
    x <- df_data %>%
      filter(measure_map == unique(df_data$measure_map)[m] &
               response_map != "Never")
    
    # the same questions may be asked using the
    # DSMI or PGS protocols
    answer_code_list <- unique(x$answer_code_map)
    
    # if the same question has been asked under different protocols 
    # then print each resulting plot individually
    for (a in seq_along(answer_code_list)){
      y <- x %>% filter(answer_code_map == answer_code_list[a])
      
      # this if statement prevents printing plots with only one datapoint
      if (nrow(y) > length(unique(y$category))*length(unique(y$response_map))*length(unique(y$sex))){
        
        p <- ggplot(y,aes(year, value)) + 
          geom_line(na.rm=TRUE) +    #removing the NA values
          geom_point(na.rm = TRUE) +
          ggtitle(paste(unique(df_data$measure_map)[m],"\n","Response grouping:", text)) +
          theme(plot.title = element_text(lineheight=.8, face="bold",size = 20)) +
          theme(text = element_text(size=10)) +
          xlab("Year") + ylab("Percentage") +
          theme_bw() +
          facet_grid(category_map ~ sex)
        print(p)
      }
    }
  }
}