library(searchConsoleR)
library(googleAnalyticsR)

## authentication with both GA and SC
options(googleAuthR.scopes.selected = 
          c("https://www.googleapis.com/auth/webmasters",
            "https://www.googleapis.com/auth/analytics",
            "https://www.googleapis.com/auth/analytics.readonly"))

googleAuthR::gar_auth()

## replace with your GA ID
ga_id <- 80379883

## date range to fetch
start <- as.character(Sys.Date() - 93)
end <- "2017-10-01"

## Using new GA v4 API
## GAv4 filters
google_seo <- 
  filter_clause_ga4(list(dim_filter("medium", 
                                    "EXACT", 
                                    "organic"),
                         dim_filter("source", 
                                    "EXACT", 
                                    "google")),
                    operator = "AND")

## Getting the GA data
gadata <-
  google_analytics_4(ga_id,
                     date_range = c(start,end),
                     metrics = c("sessions",
                                 "transactions",
                                 "transactionRevenue"),
                     dimensions = c("landingPagePath"),
                     dim_filters = google_seo,
                     order = order_type("transactions", 
                                        sort_order = "DESC", 
                                        orderType = "VALUE"),
                     max = 20000)

## Getting the Search Console data
## The development version >0.2.0.9000 lets you get more than 5000 rows
scdata <- search_analytics("https://www.build.com", 
                           startDate = start, endDate = end,
                           dimensions = c("page","query"),
                           rowLimit = 20000)