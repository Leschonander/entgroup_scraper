library(tidyverse)
library(rvest)
library(lubridate)
library(DBI)

url <- "http://english.entgroup.cn/boxoffice/cn/daily/"

get_daily_data <- function(){
  url %>%
    read_html() %>%
    html_nodes("#listbox") %>%
    html_nodes(css = ".person") %>%
    html_table(fill = TRUE, header = T) %>%
    .[[1]] %>%
    as_tibble(.name_repair = "unique") -> table_ex
  
  table_ex %>%
    subset(Rank %in% c("1")) %>%
    mutate(
      `Gross(M)` =  `Per Screen Avg.`,
      `Cume Gross(M)` =  `Per Ticket Avg.` ,
      `Per Screen Avg.` = Showings,
      `Per Ticket Avg.` = Admissions,
      Showings = Days,
      Admissions = ...10,
      Days = ...11
    ) %>%
    select(-...10, -...11) -> first_row
  
  table_ex %>%
    subset(Rank %in% c("2", "3", "4", "5", "6", "7", "8", "9", "10")) %>%
    mutate(
      `Gross(M)` =  `Cume Gross(M)`,
      `Cume Gross(M)` = `Per Screen Avg.`,
      `Per Screen Avg.` = `Per Ticket Avg.`,
      `Per Ticket Avg.` = Showings,
      Showings = Admissions,
      Admissions = Days,
      Days = ...10
    ) %>%
    select(-...10, -...11) -> other_rows
  
  first_row %>%
    rbind(other_rows) %>%
    mutate(
      `Gross(M)` = parse_number(`Gross(M)`),
      `Cume Gross(M)` = parse_number(`Cume Gross(M)`),
      `Per Screen Avg.` = parse_number(`Per Screen Avg.`),
      `Per Ticket Avg.` = parse_number(`Per Ticket Avg.`),
      Showings = as.integer(Showings),
      Date = now()
    )

    
}

data <- get_daily_data()

con <- dbConnect(drv     = RMySQL::MySQL(),
                username = "<NO>",
                password = "<NO>",
                host     = "<NO>",
                port     = "<NO>",
                dbname = "<NO>") 

dbWriteTable(conn = con,
            name = "box_office_data",
            value = data,
            append = TRUE)

dbDisconnect(con)
