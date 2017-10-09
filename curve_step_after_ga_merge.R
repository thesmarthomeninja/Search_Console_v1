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
  
  
