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
