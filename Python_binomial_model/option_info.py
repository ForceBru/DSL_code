'''
This module is responsible for downloading and parsing data about options from Yahoo Finance. The results are cached, so successive queries to the API are not made.
'''

import requests
import pprint
import pickle
import datetime
from collections import namedtuple

import warnings

import binom
import risk_free_rate as rfr

class Retriever:
    Info = namedtuple('Info', 'options dividendDate regularMarketPrice')

    def __init__(self, pkl_name: str='options.pkl'):
        try:
            with open(pkl_name, 'rb') as f:
                self.option_map = pickle.load(f)
        except FileNotFoundError:
            warnings.warn(f'{type(self).__module__}.{type(self).__name__}: Could not open cache file {pkl_name!r}.')
            self.option_map = {}

        self.pkl_name = pkl_name
        self.base_url = "https://query1.finance.yahoo.com/v7/finance/options/{}"

    def save(self):
        with open(self.pkl_name, 'wb') as f:
            pickle.dump(self.option_map, f, -1)

    def get(self, underlyingSymbol: str):
        underlyingSymbol = underlyingSymbol.upper()

        try:
            ret = self.option_map[underlyingSymbol]
        except KeyError:
            warnings.warn(f'{type(self).__module__}.{type(self).__name__}: Information about symbol {underlyingSymbol!r} not found in cache')
        else:
            if __debug__: print(f"Cache hit for {underlyingSymbol!r}")

            return Retriever.Info(*ret)

        optionChain = requests.get(self.base_url.format(underlyingSymbol)).json()['optionChain']

        if optionChain['error'] is not None:
            raise ValueError(f"Could not get options for {underlyingSymbol}: {optionChain['error']!r}")

        data = optionChain['result'][0]

        quote = data['quote']

        try:
            options = data['options'][0]
        except IndexError:
            raise ValueError(f"No options found for symbol {underlyingSymbol!r}")

        try:
            div = quote['dividendDate']
        except KeyError:
            warnings.warn(f'{type(self).__module__}.{type(self).__name__}: No data about dividends for symbol {underlyingSymbol!r}')
            div = 0

        price = quote['regularMarketPrice']

        self.option_map[underlyingSymbol] = options, datetime.date.fromtimestamp(div), price
        self.save()

        return Retriever.Info(*self.option_map[underlyingSymbol])


OptionInfo = namedtuple('OptionInfo', 'contractSymbol expiration lastPrice strike')
def get_option_info(option_json: dict):
    return OptionInfo(
        option_json['contractSymbol'],
        datetime.date.fromtimestamp(option_json['expiration']),
        option_json['lastPrice'],
        option_json['strike']
    )
    
