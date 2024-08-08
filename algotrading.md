---
title: "ACTL1101 Assignment Part A"
author: "Hoang Phuong Anh Nguyen"
date: "2024 T2"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Algorithmic Trading Strategy

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

```{r trading}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
current_price <- amd_df$close[i]

if (i==1) {
  amd_df$trade_type[i] <- 'buy'
  amd_df$costs_proceeds[i] <- -current_price * share_size
  accumulated_shares <- share_size  
  # Update accumulated_shares
  amd_df$accumulated_shares[i] <- accumulated_shares

  } else if (amd_df$close[i] < amd_df$close[i-1]) {
  amd_df$trade_type[i] <- 'buy'
  amd_df$costs_proceeds[i] <- -current_price * share_size
  accumulated_shares <- accumulated_shares + share_size 
  amd_df$accumulated_shares[i] <- accumulated_shares

  } else if (amd_df$close[i] >= amd_df$close[i-1]) {
  amd_df$trade_type[i] <- ''
  amd_df$costs_proceeds[i] <- '0'
  amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1]
}
#For the last trading day
if (i==nrow(amd_df)) {
  amd_df$trade_type[i] <- 'sell'
  amd_df$costs_proceeds[i] <- current_price * accumulated_shares
  amd_df$accumulated_shares[i] <- accumulated_shares
}
} 
```


## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}
# Fill your code here
library(dplyr)
#Setting the date for the dataset 
amd_df$date <- as.Date(amd_df$date)
#Defining the trading period
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-17")
  trading_period_df <- amd_df %>%
  filter(date >= start_date & date <= end_date)
#Filter the chosen period
  trading_period_df <- amd_df %>%
  filter(date >= start_date & date <= end_date)
#Structure and summarize the chosen period
  str(trading_period_df)
summary(trading_period_df)

```

## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# Fill your code here
# Convert costs_proceeds to numeric values 
trading_period_df$costs_proceeds <- as.numeric(as.character(trading_period_df$costs_proceeds))
#Eliminate the NAs existed by conversions
sum(is.na(trading_period_df$costs_proceeds))

# Total Profit or Loss
total_profit_loss <- sum(trading_period_df$costs_proceeds, na.rm = TRUE)
# Total Capital Invested
total_capital_invested <- -1*sum(trading_period_df$costs_proceeds[trading_period_df$trade_type == "buy"], na.rm = TRUE)

# Return on Investment 
roi <- (total_profit_loss / total_capital_invested) * 100

#Print out the results 
cat("Total Profit or Loss: ", total_profit_loss, "\n")
cat("Total Capital Invested: ", total_capital_invested, "\n")
cat("ROI: ", roi, "%\n")
```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```{r option}
#Take a profit-taking strategy
# Initialize variables
total_shares_bought <- 0
total_amount_spent <- 0
average_purchase_price <- 0

for (i in 1:nrow(amd_df)) {
  current_price <- trading_period_df$close[i]
  
  # Handle buy transactions
  if (trading_period_df$trade_type[i] == 'buy') {
    shares_bought_now <- 100
    total_shares_bought <- total_shares_bought + shares_bought_now
    total_amount_spent <- total_amount_spent + (current_price * shares_bought_now)
    average_purchase_price <- total_amount_spent / total_shares_bought
    trading_period_df$accumulated_shares[i] <- total_shares_bought
  }
  
  # Check for profit-taking condition
  if (current_price >= 1.2 * average_purchase_price && total_shares_bought > 0) {
    shares_sold <- total_shares_bought / 2
    total_shares_bought <- total_shares_bought - shares_sold
    total_amount_spent <- total_amount_spent - (average_purchase_price * shares_sold)
    trading_period_df$trade_type[i] <- 'sell'
    trading_period_df$costs_proceeds[i] <- current_price * shares_sold
    trading_period_df$accumulated_shares[i] <- total_shares_bought
  }
  
  # Update previous_price for the next iteration
  previous_price <- current_price
}

# Handle the last day of trading explicitly if not already handled
if (trading_period_df$trade_type[nrow(trading_period_df)] != 'sell') {
  trading_period_df$trade_type[nrow(trading_period_df)] <- 'sell'
  trading_period_df$costs_proceeds[nrow(trading_period_df)] <- trading_period_df$close[nrow(amd_df)] * trading_period_df$accumulated_shares[nrow(trading_period_df)]
  trading_period_df$accumulated_shares[nrow(trading_period_df)] <- 0
}
```


## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
# Fill your code here and Disucss

# Convert costs_proceeds to numeric values 
amd_df$costs_proceeds <- as.numeric(as.character(amd_df$costs_proceeds))

# Check for conversion issues by summarizing which entries are NA
sum(is.na(amd_df$costs_proceeds))

# Calculate Total Profit or Loss
total_profit_loss <- sum(as.numeric(trading_period_df$costs_proceeds), na.rm = TRUE)

# Calculate Total Capital Invested
total_capital_invested <- abs(sum(as.numeric(amd_df$costs_proceeds[amd_df$trade_type == "buy"]), na.rm = TRUE))

# Calculate ROI
roi <- 0  # Initialize ROI
if (total_capital_invested > 0) {  # Prevent division by zero
  roi <- (total_profit_loss / total_capital_invested) * 100
}

# Print the results
cat("Total Profit or Loss: ", total_profit_loss, "\n")
cat("Total Capital Invested: ", total_capital_invested, "\n")
cat("ROI: ", roi, "%\n")



```
Overview: 
After running the profit_taking strategy, AMD has witnessed a surge in ROI to approximately 202% during the trading period, with the profit increase significantly. Therefore, this strategy has enhance the trading strategy of AMD so that the company could have increase their return. 

```{r}
selldata <- trading_period_df
selldata <- selldata%>%
  filter(trade_type=='sell')
```

Discussion: From 25 May to 12 June of 2023, AMD shares observed a downfall in share price, straight for five consecutive days. This may have occured for 2 reasons. Firstly, take a look at the market risks, during the period from May 25 to June 12 2023, the global economy was under significant pressure due to persistent inflationary pressures and tighter financial conditions (World Economic Forum). Also, this may have happen because after 12 June, which was 13 June, AMD was preparing and doing their showcase its next-generation data center and AI technology. This event made the share price of AMD increase immediately. 
