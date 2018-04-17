library(RSelenium)
library(rvest)
library(tidyverse)
library(stringr)

###############################################################################
# creating the web scraper for craigslist
###############################################################################

test_url <- "https://www.ksl.com/auto/search/index?make%5B%5D=Ford&model%5B%5D=F-150&yearFrom=1998&mileageTo=150000&priceTo=18000&zip=84044&miles=100&newUsed%5B%5D=All&cx_navSource=hp_search&page=0"

pJS <- phantom()
remDr <- remoteDriver(browserName = "phantomjs")
remDr$open(silent = FALSE)
remDr$navigate(test_url)
page0.html <- read_html(remDr$getPageSource()[[1]])

page.num <- page0.html %>% 
  html_node(css = ".link:nth-child(7)") %>% 
  html_text() %>% 
  as.numeric() - 1

links.vector <- vector("character", 0)

for (i in 0:page.num) {
  site <- glue::glue("https://www.ksl.com/auto/search/index?make%5B%5D=Ford&model%5B%5D=F-150&yearFrom=1998&mileageTo=150000&priceTo=18000&zip=84044&miles=100&newUsed%5B%5D=All&cx_navSource=hp_search&page={i}")
  print(site)
  remDr$navigate(site)
  page.html <- read_html(remDr$getPageSource()[[1]])
  links.vector <- page.html %>%
    html_nodes(css = '.title .link , .featured-listing-title .link')%>% 
    html_attr("href") %>% 
    append(links.vector, .)
}

cars.vector <- vector("character", 0)

for(i in seq(1, length(links.vector))) {
  site <- glue::glue("https://www.ksl.com{links.vector[i]}")
  remDr$navigate(site)
  print(site)
  car.html <-  read_html(remDr$getPageSource()[[1]])
  
  # We need car name
  car.name <- car.html %>% 
    html_nodes(".title .cXenseParse") %>% 
    html_text() %>% 
    str_replace_all("\n| ","") %>% 
    paste0("Name:",.)
  
  # We also need car price
  car.price <- car.html %>% 
    html_nodes(xpath = '//*[(@id = "titleMain")]//*[contains(concat( " ", @class, " " ), concat( " ", "price", " " ))]') %>% 
    html_text() %>% 
    parse_number() %>% 
    paste0("Price:",.)
  
  # We also need the city where the car is located
  car.city <- car.html %>% 
    html_nodes(
      xpath = '//*[(@id = "titleMain")] | //*[contains(concat( " ", @class, " " ), concat( " ", "location", " " ))]') %>% 
    html_text() %>% 
    str_replace_all("\n| ","") %>% 
    str_extract("[A-Za-z]+,UT") %>% 
    paste0("City:",.)
  
  # We also need the date this car was posted
  car.date <- car.html %>% 
    html_nodes(
      xpath = '//*[(@id = "titleMain")] | //*[contains(concat( " ", @class, " " ), concat( " ", "location", " " ))]') %>% 
    html_text() %>% 
    str_replace_all("\n| ","") %>% 
    str_extract("[A-Za-z]+[0-9]+,[0-9]+") %>% 
    paste0("Date Posted:",.)
  
  # Here is where we collect the table data
  cars.vector <- car.html %>%
    html_nodes(xpath = '//*[@id="specificationsTable"]')%>%
    html_text() %>% 
    str_replace_all("\n| ","") %>% 
    str_split("\\s") %>% 
    unlist() %>% 
    append(., c(car.name, car.price, car.city, car.date)) %>% 
    append(cars.vector, .)
}

remDr$close()
pJS$stop()

dat <- cars.vector %>% 
  tibble(vec = .) %>% 
  separate(vec, into = c("var", "value"), sep = ":") %>% 
  na.omit() %>%
  group_by(var) %>% 
  do(tibble::rowid_to_column(.)) %>% 
  spread(var, value) %>% 
  filter(!Mileage %in% "notspecified",
         !ExteriorColor %in% c("NotSpecified", NA),
         !TitleType %in% "NotSpecified") %>% 
  mutate_at(c("Mileage", "Price", "Year"), parse_number) %>% 
  mutate_at(c("ExteriorColor","InteriorColor","TitleType","Trim"), factor) %>% 
  mutate(Trim = str_replace(Trim, "TOURING", "Touring")) %>% 
  select(Price, Mileage, everything(), 
         -c(DealerLicense, rowid, StockNumber, Cylinders))

write_csv(dat, "./Semester_Project/ksl_data.csv")

###############################################################################
# pulling the zip codes in california that have a cars per household number
# greater than 1.25
###############################################################################
census <- read_csv("https://github.com/cestastanford/historical-us-city-populations/raw/master/data/1790-2010_MASTER.csv")

pop <- census %>% 
  select()


