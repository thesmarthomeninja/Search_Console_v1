# Search_Console_v1

Inspired by Mark Edmundson - an R programming God.  See here: http://code.markedmondson.me/search-console-google-analytics-r-keyword-research/

Graphs Can Be Found Here:
Click Curve Diagram: http://rpubs.com/kalendaniel/ctrclickcurvediagram
Keyword CTR Revenue Oppurtunity: http://rpubs.com/kalendaniel/clickcurveSEOreport
Final Graph: http://rpubs.com/kalendaniel/seoreportkeywords

Here's the order of the full Script (or scripts):

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
                           
                           
                          
library(dplyr)

## get urls in same format

## this will differ from website to website, 
## but in most cases you will need to add the domain to the GA urls:
gadata$page <- paste0("www.build.com", gadata$landingPagePath)
## gadata has urls www.example.com/pagePath
## scdata has urls in http://www.example.com/pagePath
#scdata$page2 <- gsub("https://www.build.com","", scdata$page)
scdata$page2 <- gsub("https://","", scdata$page)

## get SERP
scdata$serp <- cut(scdata$position, 
                   breaks = seq(1, 100, 10), 
                   labels = as.character(1:9),
                   include.lowest = TRUE, 
                   ordered_result = TRUE)

## % of SEO traffic to each page per keyword
scdata <- scdata %>% 
  group_by(page2) %>% 
  mutate(clickP = clicks / sum(clicks)) %>%
  ungroup()
  
library(dplyr)

## join data on page
joined_data <- gadata %>%
  left_join(scdata, by = c(page = "page2")) 
  mutate(transactionsEst = clickP*transactions,
         revenueEst = clickP*transactionRevenue,
         sessionEst = clickP*sessions,
         accuracyEst = ((sessionEst / clicks) - 1),
         positionRound = round(position))

## we only want clicks over 0, and get rid of a few columns.
tidy_data <- joined_data %>% 
  filter(clicks > 0) %>% 
  select(-page, -sessions, -transactions, -transactionRevenue)  
  
library(dplyr)

click_curve <- tidy_data %>% 
  group_by(positionRound) %>% 
  summarise(CTRmean = mean(clicks)/mean(impressions),
            n = n(),
            click.sum = sum(clicks),
            impressions.sum = sum(impressions),
            sd = sd(ctr),
            E = poisson.test(click.sum)$conf.int[2] / poisson.test(impressions.sum)$conf.int[1],
            lower = CTRmean - E/2,
            upper = CTRmean + E/2) %>% ungroup()

## add % increase to position 1
## could also include other positions
click_curve <- click_curve 
  mutate(CTR1 = CTRmean[1] / CTRmean,
         CTR1.upper = upper[1] / upper,
         CTR1.lower = lower[1] / lower)
  
  
library(dplyr)
## combine with data

predict_click <- tidy_data 
  mutate(positionRound = round(position)) 
  left_join(click_curve, by=c(positionRound = "positionRound")) 
  mutate(revenueEst1 = revenueEst * CTR1,
         transEst1 = transactionsEst * CTR1,
         clickEst1 = clicks * CTR1,
         sessionsEst1 = sessionEst * CTR1,
         revenueEst1.lower = revenueEst * CTR1.lower,
         revenueEst1.upper = revenueEst * CTR1.upper,
         revenueEst1.change = revenueEst1 - revenueEst)

estimates <- predict_click  
  select(landingPagePath, query, clicks, impressions, 
         ctr, position, serp, revenueEst, revenueEst1, 
         revenueEst1.change, revenueEst1.lower, revenueEst1.upper, 
         accuracyEst) 
  arrange(desc(revenueEst1))  
  dplyr::filter(abs(accuracyEst) < 10, 
                revenueEst1.change > 0, 
                clicks > 10)


library(ggplot2)

## CTR per position
ctr_plot <- ggplot(tidy_data, aes(x = position, 
                                  y = ctr))
ctr_plot <- ctr_plot + theme_minimal()
ctr_plot <- ctr_plot + coord_cartesian(xlim = c(1,30), 
                                       ylim = c(0, 0.5))
ctr_plot <- ctr_plot + geom_point(aes(alpha = log(clicks),
                                      color = serp, 
                                      size = clicks))
ctr_plot <- ctr_plot + geom_smooth(aes(weight = clicks), 
                                   size = 0.2)
ctr_plot + scale_y_continuous(labels = scales::percent)
ctr_plot

hh <- ggplot(click_curve, aes(positionRound, CTRmean)) 
hh <- hh + theme_minimal()
hh <- hh + geom_line(linetype = 2) + coord_cartesian(xlim = c(1, 30), 
                                                     ylim = c(0,0.5))
hh <- hh + geom_ribbon(aes(positionRound, ymin = lower, ymax = upper), 
                       alpha = 0.2, 
                       fill = "orange")
hh <- hh + scale_y_continuous(labels = scales::percent)
hh <- hh + geom_point() 
hh <- hh + geom_label(aes(label = scales::percent(CTRmean)))
hh


est_plot <- ggplot(estimates[1:30,], 
                   aes(reorder(query, revenueEst1), 
                       revenueEst1, 
                       ymax = revenueEst1.upper, 
                       ymin =  revenueEst1.lower))
est_plot <- est_plot + theme_minimal() + coord_flip()

est_plot <- est_plot + geom_crossbar(aes(fill = cut(accuracyEst, 
                                                    3, 
                                                    labels = c("Good",
                                                               "Ok",
                                                               "Poor"))), 
                                     alpha = 0.7, 
                                     show.legend = FALSE)

est_plot <- est_plot + scale_x_discrete(name = "Query")
est_plot <- est_plot + scale_y_continuous(name = "Estimated SEO Revenue Increase for Google #1", 
                                          labels = scales::dollar_format(prefix = "£"))
est_plot <- est_plot + geom_label(aes(label = round(position)), 
                                  hjust = "center")
est_plot <- est_plot + ggtitle("SEO Potential Revenue (Current position)")
est_plot


#export data into a csv for records
#Create list of google analytics and search console objects, written as strings:
library(WriteXLS)
objlist <- c("scdata", "gadata", "joined_data", "tidy_data", "predict_click", "estimates", "click_curve")

#Write out Excel file with auto-width columns, a bolded header row and filters turned on
WriteXLS(objlist, "/Users/Kalen/Documents/Search_Console_v1/export_results.xlsx",
         AdjWidth = TRUE, BoldHeaderRow = TRUE, AutoFilter = TRUE)
