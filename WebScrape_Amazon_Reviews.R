library(RSelenium)
library(binman)
library(netstat)
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
#'  3. Open the Amazon product page (We need to provide the product Id)
#'  4. Fetch the link to get the complete list of reviews 
#'  5. Navigate to rest of the pages in loop and fetch all the URLs into a list 
#'  6. Create a data.frame to store the main four columns from the web page 
#'  7. Loop through all the collected URLs and fetch the required fields 
#'  8. Finally, close the browser once all the required data is scraped. 
#' -------------------------------------------------------------------------- '#

binman::list_versions("chromedriver")

#' -------------------------------------------------------------------------- '#
#'  1. Start the server. Using rsDriver function.
#' -------------------------------------------------------------------------- '#
rs_driver_object <- rsDriver(browser = 'chrome', chromever = '101.0.4951.41',
                             verbose = F, port = free_port())
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
# We are going to open the product page of a Book whose id is: 1945051752
clientDriver$navigate("https://www.amazon.com/Book-Name/dp/1945051752/")

#' -------------------------------------------------------------------------- '#
#'  4. Fetch the link to read all the reviews 
#' -------------------------------------------------------------------------- '#
reviewElement <- clientDriver$findElement(using = "id", value = "acrCustomerReviewLink")
reviewsLink <- reviewElement$getElementAttribute("href")
reviewsLink <- reviewsLink[[1]]

#' -------------------------------------------------------------------------- '#
#'  5. Navigate to the reviews page  
#'  But, we want to see all the reviews. Click the link "See all reviews"
#' -------------------------------------------------------------------------- '#

clientDriver$navigate(reviewsLink)
reviewElement <- clientDriver$findElement(using = "xpath", 
                                          '//*[@id="cr-pagination-footer-0"]/a')
reviewsLink <- reviewElement$getElementAttribute("href")
reviewsLink <- reviewsLink[[1]]
#' -------------------------------------------------------------------------- '#
#'  6. Navigate to the "See all reviews" page  
#' -------------------------------------------------------------------------- '#
clientDriver$navigate(reviewsLink)

#' -------------------------------------------------------------------------- '#
#'  7. There are 10 reviews in each page. Fetch these 10 reviews and navigate 
#'  to the next page.   
#' -------------------------------------------------------------------------- '#

reviewsElement <- clientDriver$findElements(using = "xpath", 
                                            "//span[@data-hook='review-body']")
reviewsText <- sapply(reviewsElement, function(x) x$getElementText())

#' -------------------------------------------------------------------------- '#
#'  8. Get the next page URL. In total, there are N "Next-pages". Before 
#'  web scraping please set the value of N in advance
#' -------------------------------------------------------------------------- '#

for (page in 1:20) {
  reviewElement <- clientDriver$findElement(using = "xpath", 
                                  '//*[@id="cm_cr-pagination_bar"]/ul/li[2]/a')
  reviewsLink <- reviewElement$getElementAttribute("href")
  reviewsLink <- reviewsLink[[1]]
  clientDriver$navigate(reviewsLink)
  reviewsElement <- clientDriver$findElements(using = "xpath", 
                                            "//span[@data-hook='review-body']")
  reviewsText <- append(reviewsText, 
                        sapply(reviewsElement, function(x) x$getElementText()))
}

#' -------------------------------------------------------------------------- '#
#'  9. write the reviews to a CSV file  
#' -------------------------------------------------------------------------- '#
fileName <- paste0(getwd(), "/Output/AmazonReviews.csv")
reviewsDF <- as.data.frame(unlist(reviewsText), nrow=length(reviewsText))
colnames(reviewsDF) <- c("review_text")
write.csv(reviewsDF, fileName)

#' -------------------------------------------------------------------------- '#
#'  10. Finally, close the browser once all the required data is scraped. 
#' -------------------------------------------------------------------------- '#
clientDriver$close()
gc()

