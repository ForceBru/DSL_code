import requests
import pickle
import datetime

import warnings
from bs4 import BeautifulSoup

class Retriever:
    '''
    Retrieve the risk-free rate from http://data.treasury.gov and cache it.

    Usage:
    >>> r = Retriever()
    >>> r.get(12, 2018)
    {datetime.timedelta(days=30): 2.32, datetime.timedelta(days=60): 2.39, datetime.timedelta(days=90): 2.41, datetime.timedelta(days=180): 2.54, datetime.timedelta(days=365): 2.69, datetime.timedelta(days=730): 2.72, datetime.timedelta(days=1095): 2.73, datetime.timedelta(days=1825): 2.71, datetime.timedelta(days=2555): 2.77, datetime.timedelta(days=3650): 2.85, datetime.timedelta(days=7300): 3.0, datetime.timedelta(days=10950): 3.13}
    >>> r.get_closest_ge(12, 2018, 4)
    2.32
    '''

    def __init__(self, pkl_name: str='risk_free.pkl'):
        try:
            with open(pkl_name, 'rb') as f:
                self.risk_free_map = pickle.load(f)
        except FileNotFoundError:
            warnings.warn(f'{type(self).__module__}.{type(self).__name__}: Could not open cache file {pkl_name!r}.')
            self.risk_free_map = {}

        self.pkl_name = pkl_name
        self.base_url = "http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData?$filter=month(NEW_DATE) eq {} and year(NEW_DATE) eq {}"

    def save(self):
        with open(self.pkl_name, 'wb') as f:
            pickle.dump(self.risk_free_map, f, -1)

    def get(self, month: int, year: int):
        try:
            ret = self.risk_free_map[(month, year)]
        except KeyError:
            warnings.warn(f'{type(self).__module__}.{type(self).__name__}: Risk-free rate for month {month}, year {year} not found in cache')
        else:
            if __debug__: print(f"Cache hit for {month, year}")
                
            return ret

        res = requests.get(self.base_url.format(month, year))
        soup = BeautifulSoup(res.content, 'lxml')

        latest = soup.find_all('m:properties')[-1]

        risk_free = {
            datetime.timedelta((30 if period == 'month' else 365) * concrete): float(latest.find(f'd:bc_{concrete}{period}').text)

            for period in 'month year'.split()
            for concrete in ((1,2,3,6) if period == 'month' else (1,2,3,5,7,10,20,30))
        }

        self.risk_free_map[(month, year)] = risk_free

        self.save()

        return risk_free

    def get_closest_ge(self, month: int, year: int, time_period: float):
        '''
        Get risk-free rate for given time_period.

        `time_period` - in days
        '''

        risk_free_map_period = self.get(month, year)

        time_period = datetime.timedelta(days=time_period)

        for fixed_period, rate in risk_free_map_period.items():
            if time_period <= fixed_period:
                return rate

        return rate
