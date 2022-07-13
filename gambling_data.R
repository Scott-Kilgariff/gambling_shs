library(dplyr)
library(tidyverse)
library(readxl)
library(httr)
library(janitor)


# the list of urls for each of the SHS gambling excel files
urls <- c("https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2018/10/scottish-health-survey-2017-supplementary-tables/documents/part-15---gambling/part-15---gambling/govscot%3Adocument/Part%2B15%2B-%2BGambling.xls",
          "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2017/10/scottish-health-survey-2016-supplementary-tables/documents/part-15---gambling/part-15---gambling/govscot%3Adocument/Part%2B15%2B-%2BGambling.xls",
          "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2016/09/scottish-health-survey-2015-supplementary-tables/documents/part-15---gambling/part-15---gambling/govscot%3Adocument/Part%2B15%2B-%2BGambling.xls",
          "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2015/09/scottish-health-survey-2014-supplementarty-tables/documents/15--gambling/15--gambling/govscot%3Adocument/Gambling.xls",
          "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2014/12/scottish-health-survey-2013-supplementary-tables/documents/part-10---gambling/part-10---gambling/govscot%3Adocument/Part%2B10%2B-%2BGambling.xls",
          "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2013/09/scottish-health-survey-2012-supplementary-tables/documents/part-11---gambling/part-11---gambling/govscot%3Adocument/Part%2B11%2B-%2BGambling.xls"
          )

# the years corresponding to each of the urls
years <- c(2017,
           2016,
           2015,
           2014,
           2013,
           2012)

n <- 1
# loop through a counter in the range 1:length(urls)
for (u in seq_along(urls)){
  
  reported_by <- c("age_grp", "ns_sec", "equivalised_income", "simd_quintiles")

  for (r in seq_along(reported_by)){
    
    reported_by_text <- reported_by[r]
  
    #get the url
    url <- urls[u]
    
    #get the year
    year <- years[u]
    
    #open the excel file
    GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
    
    # read the "Index" tab and convert to df
    tables <- read_excel(tf, sheet = "Index")
    df_tables <- data.frame(tables)
    
    # get the list of tab names from the "Index" sheet
    table_names <- df_tables[,1]
    
    # pre-2015, responses by age occur on every 4th sheet
    if (year < 2015){
      skip <- 4
    # post 2014, responses by age occur every 5th sheet   
    } else {
      skip <- 5
    }
    
    # loop through every nth sheet based on "skip" value
    for (t in seq(r,length(table_names), by=skip)){
    #for (t in seq(1,36, by=5)){
      
      # get the sheet name
      t_name <- table_names[t]
      
      if (t_name != "W784"){
        
        # read the sheet
        df <- read_excel(tf, sheet = t_name)
        
        answer_code <- df[[3,1]]
        
        # get the question text
        measure <- colnames(df)[1]
        
        # find the breaks between tables on the sheet
        blanks <- df %>%
          add_rownames() %>%
          filter(if_all(c(everything(), -rowname), ~ is.na(.))) %>%
          `[[`("rowname") %>%
          as.numeric()
        
        # the 2013 spreadsheet has a different layout
        if (year == 2013){
          blanks <- c(blanks)
        } else {
          blanks <- c(blanks,nrow(df)+1)
        }
        
        # the row to start at
        start <- 2
        
        # the column headers
        col_headers <- df[2,][!is.na(df[2,])]
        
        # a loop to move down each sheet and extract the tables in those sheets
        for (i in seq_along(blanks)){
          
          #get the last row of each table
          end <- blanks[i]
          end <- end - 1
          
          # slice table "i" and clean the column names
          df_slice <- df[start:end, ]
          colnames(df_slice) <- df_slice[1,]  
          df_slice <- df_slice[2:nrow(df_slice),]
          df_slice <- clean_names(df_slice)
          
          # normalise the table
          df_slice_ages <- df_slice %>% 
            filter(!is.na(na_2)) %>%
            select(na_2:make_clean_names(col_headers[length(col_headers)])) %>%
            pivot_longer( cols = make_clean_names(col_headers), names_to = "category") %>%
            mutate(description = reported_by_text)
          
          # creat a lookup for the weighted and unweighted category bases
          bases_lookup <- data.frame(t(df_slice)) %>% 
            add_rownames() %>% 
            select(c(1,5,6))
          
          # rename columns in lookup
          names(bases_lookup)[2] <- "Unweighted_bases"
          names(bases_lookup)[3] <- "Weighted_bases"
          
          # pre 2014 the percentage figures were given as values between 0 and 1
          if (year<2015){
            mult <- 100
          } else {
            mult <- 1
          }
          
          # left join the weighted and unweighted category bases
          # and format the values
          df_slice_ages <- df_slice_ages %>%
            left_join(bases_lookup, c("category" = "rowname")) %>%
            mutate(value = ifelse(value=="-", 0, value)) %>%
            mutate(value = round(as.numeric(value) * mult, 2))
          
          # create features called "measure" and "year" and assigne them with values
          df_slice_ages$measure <- measure
          df_slice_ages$year <- year
          
          # rename the "na_2" column as "response"
          names(df_slice_ages)[names(df_slice_ages) == "na_2"] <- "response"
          
          #add a textual description based on sex code
          if (i == 1){
            df_slice_ages$sex <- "Male"
          } else if (i == 2){
            df_slice_ages$sex <- "Female"
          } else {
            df_slice_ages$sex <- "All"
          }
          
          # add an answer code description based on answer_code
          df_slice_ages$answer_code <- answer_code
          
          # n==1 for the first iteration of the loop only
          if (n == 1){
            df_sheet <- df_slice_ages
          } else {
            df_sheet <- rbind(df_sheet, df_slice_ages)
          }
          
          # set the "start" value for the next iteration round the loop
          start <- end + 3
          
          #increment n by 1
          n <- n + 1
      
        } # end of "i" loop
      } # end of "W784" if statement
    } # end of "t" loop
  } # end of "r" loop
} # end of "u" loop

df_data <- df_sheet

#remove any characters after ", by " in the measure field
df_data$measure <- sub(", by .*", "", as.character(df_data$measure))

# remove any rows where the response == "All".  These always add to 100% and are not needed
df_data <- df_data %>% filter(response != "All")

# create a vector of strings to be replaced and 
# a vector of their corresponding replacements
cat_updates_1 <- c('quintile', 'semi_routine_occupations', 'x5th_least_deprived', 
                   'x4th', 'x3rd', 'x2nd', 'x1st_most_deprived')
cat_updates_2 <- c('quintile', 'semi_routine_and_routine_occupations', 'least_deprived', 
                   'x4', 'x3', 'x2', 'most_deprived')

# loop through each vector and its corresponding replacement
for (i in seq_along(cat_updates_1)) {

  df_data$category <- sub(paste0(cat_updates_1[i],".*"), 
                          cat_updates_2[i], 
                          as.character(df_data$category))
}

# the measures being reported are not consistent from one year
# to the next and therefore need to be mapped to a consistent set
map_measure <- read.csv("map_measure.csv")

# the answer codes being reported are not consistent from one year
# to the next and therefore need to be mapped to a consistent set
map_answer_code <- read.csv("map_answer_code.csv")

df_data <- df_data %>%
  left_join(map_measure, c("measure" = "measure_orig"))

df_data <- df_data %>%
  left_join(map_answer_code, c("answer_code" = "answer_code"))

# export the data as a csv
write.csv(df_data, "df_sheet.csv")

# END


