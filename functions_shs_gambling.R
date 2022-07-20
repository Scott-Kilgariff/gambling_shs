

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

format_read_plot_likert <- function(desc, text){
  
  # read the df created by 1_gambling_data.R
  df_data <- read_csv("df_sheet.csv")
  
  # the list of likert style questions
  measure_list <- c("Have you gone back another day to try to win back the money you had lost?",
                    "Have you bet more than you could really afford to lose?",
                    "Have you felt that you might have a problem with gambling?",
                    "Have people criticised your betting, or told you that you have a gambling problem, whether or not you thought it is true?",
                    "Have you felt guilty about the way you gamble or what happens when you gamble?",
                    "Have you borrowed money or sold anything to get money to gamble?",
                    "Have you felt that gambling has caused you any health problems, including stress or anxiety?",
                    "Have you felt your gambling has caused financial problems for you or your household?",
                    "How often have you found yourself thinking about gambling?",
                    "Have you felt restless or irritable when trying to cut down gambling?",
                    "Have you lied to family, or others, to hide the extent of your gambling?",
                    "Have you committed a crime in order to finance gambling or to pay gambling debts?",
                    "Have you asked others to provide money to help with a financial crisis caused by gambling?",
                    "Have you gambled to escape from problems or when you are feeling depressed, anxious or bad about yourself?",
                    "Have you made unsuccessful attempts to control, cut back or stop gambling?",
                    "Have you risked or lost an important relationship, job, educational or work opportunity because of gambling?",
                    "Have you needed to gamble with larger amounts of money to get the same excitement?")
  
  # in this report we are only looking at questions
  # where the response follow the likert methodology
  df_data <- df_data %>%
    filter(measure_map %in% measure_list)
  
  # The ordinal categories need to be assigned an order
  df_data$response_map <- ordered(df_data$response_map, levels = c("Everytime", "Very often", "Most times", "Fairly often", "Sometime or Occasionally"))
  
  # the category values, i.e. age groups, SIMD quintiles
  # need to be mapped to more readable values for display
  # on the charts
  map_category <- read.csv("map_category.csv")
  
  df_data <- df_data %>%
    left_join(map_category, by = c("category" = "category"))
  
  # The ordinal categories need to be assigned an order
  df_data$category_map <- ordered(df_data$category_map, 
                                  levels = c('All populations', '16 to 24', '25 to 34', '35 to 44', '45 to 54', '55 to 65', '65 to 74', '75 plus', 
                                             'Manager & prof.', 'Intermediate', 'Small emps & own account', 'Lower supervisory & tech.', 'Semi routine and routine', 
                                             'Top quintile earners', 'Top quintile SIMD', 'x2', 'x3', 'x4', '5th quintile earners', '5th quintil SIMD')
  )
  
  # specify which of the group descriptions are to be included
  # e.g. age_grp or simd_quintiles
  df_data <- df_data %>% filter(description %in% desc)
  
  # for each question
  for (m in seq_along(measure_list)){
    
    # filter the dataframe using each question
    x <- df_data %>%
      filter(measure_map == measure_list[m] &
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
        
        p <- ggplot(y,aes(year, value, colour = sex)) + 
          geom_line(na.rm=TRUE) +    #removing the NA values
          geom_point(na.rm = TRUE) +
          ggtitle(paste(measure_list[m],"\n",answer_code_list[a],"\n response grouping:", text)) +
          theme(plot.title = element_text(lineheight=.8, face="bold",size = 20)) +
          theme(text = element_text(size=10)) +
          xlab("Year") + ylab("Percentage") +
          theme_bw() +
          facet_grid(category_map ~ response_map)+
          scale_color_manual(values=c("blue", "green","red"))
        print(p)
      }
    }
  }
}