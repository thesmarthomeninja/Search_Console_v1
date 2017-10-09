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