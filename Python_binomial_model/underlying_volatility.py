'''
This module contains routines to compute the volatility of a stock's returns.
'''

import csv
import math
import pickle
from statistics import stdev  # Need Python >= 3.4

import warnings

def load_close(stock: str):
    '''
    Load historical data from the 'stock_data' folder.
    '''
    with open(f"stock_data/{stock}.csv") as f:
        reader = csv.DictReader(f)

        for row in reader:
            ret = row['Close']

            if ret != 'null':  # Yahoo Finance substitutes 'null' for missing data
                yield float(ret)

def compute_daily_return(close: list):
    '''
    Returns are approximated using the natural log, as shown in [book p. 324-326].
    '''
    return [math.log(a / b) for a, b in zip(close[1:], close[:-1])]


class Retriever:
    '''
    Load historical data for stock. This data must be downloaded manually first (maybe from Yahoo Finance) because the latter does not permit automated downloads of historical data.
    '''

    def __init__(self, pkl_name: str='volatility.pkl'):
        try:
            with open(pkl_name, 'rb') as f:
                self.option_map = pickle.load(f)
        except FileNotFoundError:
            warnings.warn(f'{type(self).__module__}.{type(self).__name__}: Could not open cache file {pkl_name!r}.')
            self.option_map = {}

        self.pkl_name = pkl_name

    def save(self):
        with open(self.pkl_name, 'wb') as f:
            pickle.dump(self.option_map, f, -1)

    def get(self, symbol: str) -> float:
        symbol = symbol.upper()

        try:
            ret = self.option_map[symbol]
        except KeyError:
            warnings.warn(f'{type(self).__module__}.{type(self).__name__}: Symbol {symbol!r} not found in cache')
        else:
            if __debug__: print(f"Cache hit for {symbol}")
            return ret

        returns = compute_daily_return(list(load_close(symbol)))
        ret = stdev(returns)

        self.option_map[symbol] = ret
        self.save()

        return ret

## NumPy approach
#import numpy as np
#
#def load_close(stock: str):
#    ret = np.genfromtxt(f'stock_data/{stock}.csv', delimiter=',', skip_header=1, usecols=(5, ))
#    return ret[~ np.isnan(ret)]
#
#
#def get_daily_return(prices: np.array):
#    return np.log(prices[1:] / prices[:-1])
#
#close = load_close('AAPL')
#daily_ret = get_daily_return(close)
#
#print(daily_ret.std())