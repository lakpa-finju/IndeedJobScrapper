#scrapping indeed websites
library("crul")
library("rvest")
library("glue")
WORKINGDIR<- "~/git/webScrapper/"
setwd(WORKINGDIR)

HEADERS<- list(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/96.0.4664.110 Safari/537.36",
  "Accept-Encoding"= "gzip, deflate",
  "Accept"= "application/json, text/xml, application/xml, */*"
)

parse_search <- function(response){
  # build rvest tree
  tree <- read_html(response$parse())
  # find total jobs available
  total_jobs <- tree %>% html_element("#searchCountPages") %>% html_text2()
  total_jobs <- strtoi(stringr::str_match(total_jobs, "(\\d+) jobs")[,2])
  # find displayed job container boxes:
  jobs <- tree %>% html_elements('#mosaic-zone-jobcards .result')
  # extract job listing from each job container box
  parsed <- list()
  dataTable<- data.frame(matrix(nrow = length(jobs), ncol = 6))
  colnames(dataTable)<- c("Title","Company","Location","Date","URL","Company_URL")
  i<-1
  for (job in  jobs){
    dataTable$Title[i]<- job %>% html_element('h2') %>% html_text2()
    dataTable$Company[i]<- job %>% html_element('.companyOverviewLink') %>% html_text2()
    dataTable$Location[i]<- job %>% html_element('.companyLocation') %>% html_text2()
    dataTable$Date[i]<- job %>% html_element(xpath=".//span[contains(@class,'date')]/text()") %>% html_text2()
    dataTable$URL[i] <-  url_build(response$url, job %>% html_attr('href'))
    dataTable$Company_URL[i] <- url_build(response$url, job %>% html_element(xpath='.//a[contains(@class,"companyOverviewLink")]') %>% html_attr('href'))
    i<-i+1
    }
  print(paste("Total Jobs:",total_jobs))
  # return parsed jobs and total job count in the query
  print(glue("found total {length(jobs)} jobs from total of {total_jobs}"))
  # list(jobs=parsed, total=total_jobs)
  list(jobs=dataTable, total=total_jobs)
}

#Code to scrap the rest of the pages
scrape_search_page <- function(query, location, offset=0, limit=10){
  # first we need to create all urls we'll be scraping based on offset and limit arguments
  # 0:10 will create first page scrape url, and 10:80 will create 1-8 pages since there are 10 results per page
  print(glue("scraping {query} at {location} in range {offset}:{limit}"))
  urls <- list()
  for (i in seq(offset+10, limit, by=10)){
    urls <- append(urls, glue("https://www.indeed.com/jobs?q={query}&l={location}&start={i}"))
  }
  
  # then we want to retrieve these urls in parallel:
  print(glue("scraping search page urls: {urls}"))
  responses <- Async$new(
    urls = urls,
    headers = HEADERS,
  )$get()
  
  # finally we want to unpack results of each individual page into final dataset
  found_jobs <- nullfile()
  total_jobs <- NULL
  for (response in responses){
    page_results <- parse_search(response)
    #check if the csv file is null or not
    #if null add the csv file from page_result 
    #otherwise append it at the and of the current non empty csv file.
    if (is.null(found_jobs)){
      found_jobs<- page_results$jobs
    }else{
      found_jobs<- rbind(found_jobs,page_results$jobs)
    }
    total_jobs <- page_results$total
  }
  # we return jobs we parsed and total jobs presented in the search page:
  list(jobs=found_jobs, total=total_jobs)
}

scrape_search <- function(query, location){
  # this is our main function that scrapes all pages of the query explicitly
  # first, we scrape the first page
  first_page_results <- scrape_search_page(query, location)
  found_jobs <- first_page_results$jobs
  total_jobs <- first_page_results$total
  
  # then we scrape remaining pages: 
  print(glue("scraped first page, found {length(found_jobs)} jobs; continuing with remaining: {total_jobs}"))
  remaining_page_results <- scrape_search_page(query, location, offset = 10, limit = total_jobs)
  # finally, we return combined dataset
  found_jobs<- rbind(found_jobs, remaining_page_results$jobs)
}


# url <- "https://uk.indeed.com/jobs?q=r&l=scotland"
# response <- HttpClient$new(url, headers= HEADERS)$get()
# #print(response$parse())
# print(parse_search(response))

start = Sys.time()
#if there is space in query and/or location use %20 instead of space. 20 is a space 
#hexadecimal and its rule of URL 
scrappedData <- scrape_search("software%20engineer%20intern", "United%20States")
print(Sys.time() - start)
print(scrappedData$Title)
write.csv(scrappedData, row.names = FALSE ,file = paste(WORKINGDIR,"scrapppedJobs.csv", sep = "/"))




