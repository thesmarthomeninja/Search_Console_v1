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