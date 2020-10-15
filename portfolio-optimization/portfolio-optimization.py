import pandas as pd
import os
from functools import reduce
import matplotlib.pyplot as plt
import numpy as np

def portfolio_opt(df_list, returns, num_portfolios):
    # mean daily returns and covariance matrix
    mean_daily_returns = returns.mean()
    cov_matrix = returns.cov()

    # set up array to hold results
    # We have increased the size of the array to hold the weight values for each stock
    results = np.zeros((4 + len(df_list) - 1, num_portfolios))

    for i in range(num_portfolios):
        # select random weights for portfolio holdings
        weights = np.array(np.random.random(len(df_list)))
        # rebalance weights to sum to 1
        weights /= np.sum(weights)

        # calculate portfolio return and volatility
        portfolio_return = np.sum(mean_daily_returns * weights) * 252  # nr business days
        # std comes from definition with dot products
        portfolio_std_dev = np.sqrt(np.dot(weights.T, np.dot(cov_matrix, weights))) * np.sqrt(252)

        # store results in results array
        results[0, i] = portfolio_return
        results[1, i] = portfolio_std_dev
        # store Sharpe Ratio (return / volatility) - risk free rate element excluded for simplicity
        results[2, i] = results[0, i] / results[1, i]
        # iterate through the weight vector and add data to results array
        for j in range(len(weights)):
            results[j + 3, i] = weights[j]

    # convert results array to Pandas DataFrame
    column_names = ["ret", "stdev", "sharpe"]
    for stock in list(data.columns):
        column_names.append(stock)
    results_frame = pd.DataFrame(results.T, columns=column_names)

    # locate position of portfolio with highest Sharpe Ratio
    max_sharpe_port = results_frame.iloc[results_frame['sharpe'].idxmax()]
    # locate positon of portfolio with minimum standard deviation
    min_vol_port = results_frame.iloc[results_frame['stdev'].idxmin()]

    # create scatter plot coloured by Sharpe Ratio
    plt.scatter(results_frame.stdev, results_frame.ret, c=results_frame.sharpe, cmap='RdYlBu')
    plt.xlabel('Volatility')
    plt.ylabel('Returns')
    plt.colorbar()
    # plot red star to highlight position of portfolio with highest Sharpe Ratio
    plt.scatter(max_sharpe_port[1], max_sharpe_port[0], marker=(5, 1, 0), color='r', s=1000)
    # plot green star to highlight position of minimum variance portfolio
    plt.scatter(min_vol_port[1], min_vol_port[0], marker=(5, 1, 0), color='g', s=1000)

    plt.show()

    return max_sharpe_port, min_vol_port


path = "aktiedata"

# CLOSE PRICES
df_list = []
for r,d,f, in os.walk(path):
    for file in f:
        stock = pd.read_csv(path+"/"+file)[["Date", "Close"]]
        stock.columns = ["Date", file.split("_")[0]]
        df_list.append(stock)

# MERGE ALL
data = reduce(lambda x, y: pd.merge(x,y,on = "Date"), df_list)

# FIX DATE
data.Date = pd.to_datetime(data.Date.astype(str), format = "%Y%m%d").dt.date

# SET DATE TO INDEX
data.index = data.Date
data.drop("Date", axis = 1, inplace = True)

# LOG RETURNS
returns = np.log(data).diff()
returns.dropna(inplace=True)

portfolios = portfolio_opt(df_list, returns, 50000)

max_sharpe_p = portfolios[0].iloc[3:]
min_var_p = portfolios[1].iloc[3:]

budget = 82700




