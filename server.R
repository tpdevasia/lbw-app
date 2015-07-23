
# Load relevant packages
library(maps)
library(mapproj)
library(ggplot2)
library(plyr)
library(dplyr)
library(mgcv)
library(shiny)
library(nlme)
library(reshape)

# Read in rank data and round estimates
ranks <- read.csv("all_model_ranks.csv")
ranks$est_samp <- round(ranks$est_samp, 1)
ranks$lb_samp <- round(ranks$lb_samp, 1)
ranks$ub_samp <- round(ranks$ub_samp, 1)
ranks$rank_est <- round(ranks$rank_est, 0)
ranks$lb_est <- round(ranks$lb_est, 0)
ranks$ub_est <- round(ranks$ub_est, 0)


# Read in county fips data
data(county.fips)

# Create server
shinyServer(function(input, output) {
  
  # Subset ranks data set to get appropriate data for map, plots, and tables
  
      dataInput <- reactive({
          if(input$region=="All states") {
              data_ranks <- filter(ranks, model==gsub( ":.*$", "", input$model) & (state=="IL" | state=="IA" | state=="MI" | state=="MN" | state=="WI")) 
          }
          else if(input$region=="Illinois") {
              data_ranks <- filter(ranks, model==gsub( ":.*$", "", input$model) & state=="IL")
          }
          else if(input$region=="Iowa") {
              data_ranks <- filter(ranks, model==gsub( ":.*$", "", input$model) & state=="IA")
          }
          else if(input$region=="Michigan") {
              data_ranks <- filter(ranks, model==gsub( ":.*$", "", input$model) & state=="MI")
          }
          else if(input$region=="Minnesota") {
              data_ranks <- filter(ranks, model==gsub( ":.*$", "", input$model) & state=="MN")
          }
          else {
              data_ranks <- filter(ranks, model==gsub( ":.*$", "", input$model) & state=="WI")
          }
        
        })
   
  
  # Generate heat map of ranks  
  output$plot <- renderPlot({
      data_ranks <- dataInput()
  
      # Subset county.fips to only include values for IA, IL, MI, MN, WI
      fips <- county.fips$fips[(county.fips$fips>17000&county.fips$fips<17204) | (county.fips$fips>19000&county.fips$fips<19198) | (county.fips$fips>26000&county.fips$fips<27174) | (county.fips$fips>55000&county.fips$fips<55142)]
      
      # Define color palatte
      colors = c("#FDEDF8", "#F9CAEA", "#F6A8DD", "#F385CF", "#EF62C1","#EE51BB", "#D648A8","#A63882","#77285D", "#5F204A")
      
      # Divide ranks into color ranges
      data_ranks$colorBuckets <- as.numeric(cut(data_ranks$per_rank, 10))
      
      # Match fips
      colorsmatched <- data_ranks$colorBuckets[match(fips, data_ranks$stcnty_fips)]
      
      # Map counties filled in by rank and add state outlines
      map("county", regions=c("illinois","iowa","michigan","minnesota","wisconsin"), col=colors[colorsmatched], fill=TRUE, projection="square")
      map("state", regions=c("illinois","iowa","michigan","minnesota","wisconsin"), col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 1, projection="square")
      
      # Add title and legend and set plot height and width
      leg.txt <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")
      legend("right", leg.txt, horiz = FALSE, fill = colors, bty="o", cex=1, title=c(expression(atop("Decile of","Percentile Ranks"))))
    }, height = 700, width = 800)
  
  
  
  
  #################################################
  ## Generate a table of the top ranked counties ##
  #################################################
  output$table_top <- renderTable({
      data_ranks <- dataInput()
      
      # Subset data frame 
      rt <- data.frame(data_ranks$state, data_ranks$county, data_ranks$lbw_births, data_ranks$births, data_ranks$est_samp, data_ranks$lb_samp, data_ranks$ub_samp, data_ranks$per_rank, data_ranks$per_lb, data_ranks$per_ub)
      
      # Assign relevant column names
      colnames(rt) <- c("State", "County", "LBW births", "Total births", "Estimated LBW births/1000 births", "Lower Bound of Estimate", "Upper Bound of Estimate", "Percentile Rank", "Lower Bound of Percentile Rank", "Upper Bound of Percentile Rank")
      rt <- rt[order(rt$`Percentile Rank`), ]
      
      # Print out top ranked counties
      top <- head(rt, n = input$slider)
      top
  })

  
  
  ####################################################
  ## Generate a table of the bottom ranked counties ##
  ####################################################
  output$table_bottom <- renderTable({
      data_ranks <- dataInput()
      
      # Subset data frame 
      rt <- data.frame(data_ranks$state, data_ranks$county, data_ranks$lbw_births, data_ranks$births, data_ranks$est_samp, data_ranks$lb_samp, data_ranks$ub_samp, data_ranks$per_rank, data_ranks$per_lb, data_ranks$per_ub)
      
      # Assign relevant column names
      colnames(rt) <- c("State", "County", "LBW births", "Total births", "Estimated LBW births/1000 births", "Lower Bound of Estimate", "Upper Bound of Estimate", "Percentile Rank", "Lower Bound of Percentile Rank", "Upper Bound of Percentile Rank")
      rt <- rt[order(rt$`Percentile Rank`), ]
      
      # Print out bottom ranked counties in decreasing order
      bottom <- tail(rt, n = input$slider)
      bottom <- bottom[order(bottom$`Percentile Rank`, decreasing=TRUE), ]
      bottom
  })
  
  
  
  ##############################################################
  ## Generate confidence interval plot of top ranked counties ##
  ##############################################################
  output$ci_top <- renderPlot({
      data_ranks <- dataInput()
      
      # Concatenate state and county info
      data_ranks$county <- sub(data_ranks$county, pattern = " [[:alpha:]]*$", replacement = "")
      data_ranks$stcnty <- paste(data_ranks$state, data_ranks$county, sep=",")
      ranks_ci <- data_ranks
      ranks_ci <- ranks_ci[order(ranks_ci$per_rank), ]
      ranks_ci$stcnty <- factor(ranks_ci$stcnty, levels = ranks_ci$stcnty)
    
      # Create confidence interval plot
      p_top <- ggplot(ranks_ci[1:input$slider,], aes(stcnty, per_rank)) + geom_point()
      p_top <- p_top + theme(axis.text.x = element_text(angle = 90)) + geom_pointrange(aes(ymax = per_ub, ymin = per_lb), color="black") + scale_y_reverse(lim=c(max(ranks_ci$per_rank)+10,0))
      p_top 
  })
  
  
  
  
  #################################################################
  ## Generate confidence interval plot of bottom ranked counties ##
  #################################################################
  output$ci_bottom <- renderPlot({
      data_ranks <- dataInput()
      
      # Concatenate state and county info
      data_ranks$county <- sub(data_ranks$county, pattern = " [[:alpha:]]*$", replacement = "")
      data_ranks$stcnty <- paste(data_ranks$state, data_ranks$county, sep=",")
      ranks_ci <- data_ranks
      ranks_ci <- ranks_ci[order(ranks_ci$per_rank), ]
      ranks_ci$stcnty <- factor(ranks_ci$stcnty, levels = ranks_ci$stcnty)
    
      # Create confidence interval plot
      n <- length(ranks_ci$per_rank)
      p_bottom <- ggplot(ranks_ci[(n-input$slider+1):n,], aes(stcnty, per_rank)) + geom_point()
      p_bottom <- p_bottom + theme(axis.text.x = element_text(angle = 90)) + geom_pointrange(aes(ymax = per_ub, ymin = per_lb), color="black") + scale_y_reverse(lim=c(max(ranks_ci$per_rank)+10,0))
      p_bottom 
  })
  
})

