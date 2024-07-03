
logistic <- function(lp) {
  exp(lp) / (1 + exp(lp))
}


adj_logistic <- function(lp) {
  probs_not_normalized <- exp(cumsum(lp))
  probs_not_normalized / sum(probs_not_normalized)
}

item_ExpectedScore <- function(theta, a, c, D, b, model) {
  if (model == 'logit') {
    return(c + (1 - c) * logistic(a * D * (theta - b)))
  } else if (model == 'adjacent logit') {
    lp <- c(0, a * D * (theta - b))
    prob <- adj_logistic(lp)
    return(sum(prob * 0:(length(prob) - 1)))
  }
}


item_ExpectedProb <- function(theta, a, c, D, b, model) {
  if (model == 'logit') {
    return(c + (1 - c) * logistic(a * D * (theta - b)))
  } else if (model == 'adjacent logit') {
    lp <- c(0, a * D * (theta - b))
    prob <- adj_logistic(lp)
    return(prob)
  }
}


item_Information <- function(theta, a, c, D, b, model) {
  if (model == 'logit') {
    prob <- c + (1 - c) * logistic(a * D * (theta - b))
    return((D * a)^2 * (prob - c)^2 / (1 - c)^2 * (1 - prob) / prob)
  } else if (model == 'adjacent logit') {
    lp <- c(0, a * D * (theta - b))
    prob <- adj_logistic(lp)
    multiplier <- (D * a)^2
    term1 <- sum(prob[-1] * (1:(length(prob) - 1))^2)
    term2 <- sum(prob[-1] * (1:(length(prob) - 1)))^2
    return(multiplier * (term1 - term2))
  }
}




test_ExpectedScore <- function(theta, form, normalization = 1) {
  # Initialize variables for expected and maximum scores
  test_expscore <- 0
  test_maxscore <- 0
  
  # Convert ItemId column to a list/vector
  items <- form$ItemId
  
  # Loop through each item
  for (item in items) {
    
    model <- form$model[form$ItemId  == item][1]
    item_maxscore <- form$ScorePoints[form$ItemId  == item][1]
    

    if (model == 'logit') {
      matching_values <- form$P1[form$ItemId  == item]
      b <- if (length(matching_values) > 0) matching_values[1] else NA
    } else if (model == 'adjacent logit') {
      matching_values_P0 <- form$P0[form$ItemId  == item]
      matching_values_P1 <- form$P1[form$ItemId  == item]
      b <- if (length(matching_values_P0) > 0 && length(matching_values_P1) > 0) c(matching_values_P0[1], matching_values_P1[1]) else NA
    }
    
    
    # Note: You need to ensure 'a', 'c', 'D' variables are defined or passed to this function
    item_expscore <- item_ExpectedScore(theta, a, c, D, b, model)
    
    # Update cumulative scores
    test_maxscore <- test_maxscore + item_maxscore
    test_expscore <- test_expscore + item_expscore
  }
  
  # Normalize if required
  if (normalization == 1) {
    test_expscore <- test_expscore / test_maxscore
  }
  
  return(test_expscore)
}




test_Information <- function(theta, form, normalization = 0) {
  # Initialize total information variable
  test_info <- 0
  
  # Extract the item IDs into a vector
  items <- form$ItemId
  
  # Loop through each item by its ID
  for (item in items) {
    
    model <- form$model[form$ItemId == item][1]
    maxscore <- form$ScorePoints[form$ItemId == item][1]
    
    if (model == 'logit') {
      matching_values <- form$P1[form$ItemId == item]
      b <- if (length(matching_values) > 0) matching_values[1] else NA
    } else if (model == 'adjacent logit') {
      matching_values_P0 <- form$P0[form$ItemId == item]
      matching_values_P1 <- form$P1[form$ItemId == item]
      b <- if (length(matching_values_P0) > 0 && length(matching_values_P1) > 0) c(matching_values_P0[1], matching_values_P1[1]) else NA
    }
    
    
    # Note: Ensure 'a', 'c', 'D' are correctly defined or passed as arguments
    item_info <- item_Information(theta, a, c, D, b, model)
    
    # Normalize information if requested
    if (normalization == 1) {
      item_info <- item_info / maxscore
    }
    
    # Accumulate item information to total test information
    test_info <- test_info + item_info
  }
  
  return(test_info)
}














# a = 1
# c = 0
# D = 1

# item_ExpectedScore(0,a,c, D, -0.8951,'logit')
# item_ExpectedScore(0,a,c, D, c(-0.8959, 0.2741),'adjacent logit')

# item_ExpectedProb(0,a,c, D, -0.8951,'logit')
# item_ExpectedProb(0,a,c, D, c(-0.8959, 0.2741),'adjacent logit')

# item_Information(0,a,c, D, -0.8951,'logit')
# item_Information(0,a,c, D, c(-0.8959, 0.2741),'adjacent logit')

# data <-read.csv("C:/Users/jichen/Documents/GitHub/Form-Review-Tool/tmp.csv")
# names(data)[names(data) == "item_id"] <- "ItemId"
# names(data)[names(data) == "IAT.Max.Score"] <- "ScorePoints"

# formA <- data[(data$opp == 'Opp2')&(data$stage=='Stage2')&(data$difficulty=='Low'),]
# formB <- data[(data$opp == 'Opp2')&(data$stage=='Stage2')&(data$difficulty=='Medium'),]
# formC <- data[(data$opp == 'Opp2')&(data$stage=='Stage2')&(data$difficulty=='High'),]

# test_ExpectedScore(0, formA,1)
# test_Information(0, formA,0)

# thetas <- round(seq(-4, 4, by = 0.1), 2)
# lapply(thetas, function(theta) test_ExpectedScore(theta, formA, 0))
# lapply(thetas, function(theta) test_Information(theta, formA, 0))

# thetas <- round(seq(-4, 4, by = 0.1), 2)
# formA_tcc<- lapply(thetas, function(theta) test_ExpectedScore(theta, formA, 1))
# formB_tcc<- lapply(thetas, function(theta) test_ExpectedScore(theta, formB, 1))
# formC_tcc<- lapply(thetas, function(theta) test_ExpectedScore(theta, formC, 1))

# formA_tic<- lapply(thetas, function(theta) test_Information(theta, formA, 0))
# formB_tic<- lapply(thetas, function(theta) test_Information(theta, formB, 0))
# formC_tic<- lapply(thetas, function(theta) test_Information(theta, formC, 0))


# library(plotly)

#   p <- plot_ly() %>%
#     add_lines(x = thetas, y = formA_tcc, name = 'Low', line = list(color = 'orange')) %>%
#     add_lines(x = thetas, y = formB_tcc, name = 'Medium', line = list(color = 'blue')) %>%
#     add_lines(x = thetas, y = formC_tcc, name = 'High', line = list(color = 'red')) %>%
#     layout(title = 'Test Characteristic Curve by Theta',
#            xaxis = list(title = 'Theta'),
#            yaxis = list(title = 'Test Characteristic Curve'))
  
#   # Show plot
#   p
  

#   p <- plot_ly() %>%
#     add_lines(x = thetas, y = formA_tic, name = 'Low', line = list(color = 'green')) %>%
#     add_lines(x = thetas, y = formB_tic, name = 'Medium', line = list(color = 'blue')) %>%
#     add_lines(x = thetas, y = formC_tic, name = 'High', line = list(color = 'orange')) %>%
#     layout(title = 'Test Characteristic Curve by Theta',
#            xaxis = list(title = 'Theta'),
#            yaxis = list(title = 'Test Characteristic Curve'))
  
#   # Show plot
#   p

