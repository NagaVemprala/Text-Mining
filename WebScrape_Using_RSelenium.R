library(tidyverse)
library(RSelenium)
library(netstat)
library(binman)

#' -------------------------------------------------------------------------- '#
#'  RSelenium is used to extract data from dynamic pages. 
#'  Required packages - RSelenium. install.packages("RSelenium")
#'  Requires Java. Install the JDK executable. Runtime is automatically installed
#'  Other packages - netstat (freeport function to check a free port)
#'  tidyverse to tidy the data if required. 
#' -------------------------------------------------------------------------- '#



#' -------------------------------------------------------------------------- '#
#'  Steps involved.  
#'  1. Start the server. Using rsDriver function.
#'  2. Create a client object. (Client is our web page that we are seeing on PC)
#'  3. Open the search page. (Here, we are trying to scrape data from indeed)
#'  4. Fetch the job posting URLs from the main page (Currently, 15 job urls)
#'  5. Navigate to rest of the pages in loop and fetch all the URLs into a list 
#'  6. Create a data.frame to store the main four columns from the web page 
#'  7. Loop through all the collected URLs and fetch the required fields 
#'  8. Finally, close the browser once all the required data is scraped. 
#' -------------------------------------------------------------------------- '#


#' -------------------------------------------------------------------------- '#
#'  1. Start the server. Using rsDriver function.
#' -------------------------------------------------------------------------- '#
rs_driver_object <- rsDriver(browser = 'chrome', chromever = '101.0.4951.41',
                             verbose = F, port = free_port())
#' -------------------------------------------------------------------------- '#
#'  Check for other details about the rsDriver. But I am keeping it simple. 
#'  You can rely on a lot of defaults unless you are going for advanced scraping
#'  Help page is available at: ?rsDriver()
#'  If JDK is installed, you can install the binman package if it is not 
#'  available through R base for some reason. 
#'  You can check the latest version of chrome using 
#'  binman::list_versions("chromedriver") infact, you can check any application.
#'  binman::list_versions() can be used to list the version names of any 
#'  application on the operating system you are using. 
#' -------------------------------------------------------------------------- '#


#' -------------------------------------------------------------------------- '#
#'  2. Create a client object. (Client is our web page that we are seeing on PC)
#'  rs_driver_object has two objects. Client and Server.
#'  We are connecting to the remote server using our client browser, which is 
#'  automatically controlled using the rs_driver_object. Let it open browser!!!  
#'  (Simply put:) *** We are just using the web browser side of things *** 
#'  clientDriver is of type remoteDriver class of RSelenium package. 
#'  Access help page ?remoteDriver to learn more about different functions that 
#'  are possible (e.g., fetching cookies, adjusting window position etc). 
#' -------------------------------------------------------------------------- '#

clientDriver <- rs_driver_object$client
# clientDriver$open()

#' -------------------------------------------------------------------------- '#
#'  3. Open the search page. (Here, we are trying to scrape data from indeed)
#'  We are searching based on some strings. As they go into a URL, "+" is added
#'  Key functions of the client driver: 
#'  navigate - To go to the required page 
#'  findElement (findElements if multiple HTML tags are associated with a unique
#'  identifier. You could find a HTML element based on ID, CSS tag, html tag, 
#'  class name, and even a partial link text)
#' -------------------------------------------------------------------------- '#
# We are going to search for indeed jobs based on some search strings
searchStrings = "data+mining+bachelors"
clientDriver$navigate(paste0("https://www.indeed.com/jobs?q=",searchStrings))


#' -------------------------------------------------------------------------- '#
#'  4. Fetch the job posting URLs from the main page (Currently, 15 job urls)
#' -------------------------------------------------------------------------- '#
jobElements <- clientDriver$findElements(using = "xpath", "//a[@data-hiring-event='false']")
jobUrls <- sapply(jobElements, function(x) x$getElementAttribute("href") ) 

#' -------------------------------------------------------------------------- '#
#'  5. Navigate to rest of the pages in loop and fetch all the URLs into a list 
#'     *** Fetch the URLs from the second page through page 50 ***
#'     *** Navigate to the corresponding page using navigate function *** 
#'     *** Then get the 15 job posting URLs from each of these pages *** 
#' -------------------------------------------------------------------------- '#

for (i in 2:50) { 
    string_substitution <- gsub('\\.', as.character(i), "//a[@aria-label='.']")
    jobElement <- clientDriver$findElement(using = "xpath", string_substitution)
    pageURL <- jobElement$getElementAttribute("href")
    clientDriver$navigate(unlist(pageURL))
    
    #' ---------------------------------------------------------------------- '#
    #'  Repeating the same two steps that were done in the main page to fetch 
    #'  the 15 URLs  
    #' -----------------------------------------------------------------------'#
    jobElements <- clientDriver$findElements(using = "xpath", 
                                             "//a[@data-hiring-event='false']")
    jobUrls <- append(jobUrls, sapply(jobElements, 
                                      function(x) x$getElementAttribute("href")))
}

#' -------------------------------------------------------------------------- '#
#'  6. Create a data.frame to store the main four columns from the web page 
#'     - Job Title
#'     - Job Company 
#'     - Job Details
#'     - Job Qualifications
#' -------------------------------------------------------------------------- '#
jobsDF <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(jobsDF) <- c("Title", "Company", "Details", "Qualifications")

#' -------------------------------------------------------------------------- '#
#'  7. Loop through all the collected URLs and fetch the required fields 
#' -------------------------------------------------------------------------- '#

for (url in jobUrls) { 
    clientDriver$navigate(unlist(url))
    jobElement <- clientDriver$findElement(using = "class", value = "jobsearch-JobInfoHeader-title-container")
    jobTitle <- jobElement$getElementText()
    
    jobElement <- clientDriver$findElement(using = "class", value = "jobsearch-CompanyInfoContainer")
    jobCompany <- jobElement$getElementText()
    jobCompany <- unlist(jobCompany)
    
    jobElement <- clientDriver$findElement(using = "id", value = "jobDetailsSection")
    jobDetails <- jobElement$getElementText()
    
    t1 <- try(
        jobElement <- clientDriver$findElement(using = "id", value = "jobDetailsSection")
    )
    if("try-error" %in% class(t1)) {
        #print("Errored out ")
        jobElement <- clientDriver$findElement(using = "id", value = "jobDescriptionText")
    }
    jobDetails <- jobElement$getElementText()
    jobDetails <- unlist(jobDetails)
    
    
    t2 <- try(
        jobElement <- clientDriver$findElement(using = "id", value = "qualificationsSection")
    )
    if("try-error" %in% class(t2)) {
        #print("Errored out ")
        jobElement <- clientDriver$findElement(using = "id", value = "jobDescriptionText")
    }
    jobQualifications <- jobElement$getElementText()
    jobQualifications <- unlist(jobQualifications)
    
    jobsDF <- rbind(jobsDF, c(jobTitle, jobCompany, jobDetails, jobQualifications) )
}
colnames(jobsDF) <- c("Title", "Company", "Details", "Qualifications")

#' -------------------------------------------------------------------------- '#
#'  8. Finally, close the browser once all the required data is scraped. 
#' -------------------------------------------------------------------------- '#
clientDriver$close()
gc()
