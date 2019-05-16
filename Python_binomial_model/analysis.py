'''
This module does the actual analysis of the options involving the given underlying stock.
'''

import datetime
import math

import binom
import risk_free_rate as rfr
import option_info as oi
import underlying_volatility as uv

__INTERACTIVE = False

if __name__ == '__main__':
    TRADING_DAYS = 252
    MODEL_PERIODS = 100
    SYMBOL = 'AAPL'
    OPT_TYPE = binom.OptionType.CALL

    if __INTERACTIVE:
        MODEL_PERIODS_ = input(f'Enter number of periods in the model (default {MODEL_PERIODS}): ')
        if MODEL_PERIODS_:
            MODEL_PERIODS = int(MODEL_PERIODS_)

        SYMBOL_ = input(f'Enter stock symbol (default {SYMBOL}): ').strip()
        if SYMBOL_:
            SYMBOL = SYMBOL_

        OPT_TYPE_ = input(f'Enter option type (CALL, PUT; default {OPT_TYPE}): ').strip()
        if OPT_TYPE_:
            OPT_TYPE = getattr(binom.OptionType, OPT_TYPE_.upper())

    OPT_TYPE |= binom.OptionType.AMERICAN

    risk_free_provider = rfr.Retriever()
    underlying_volatility_provider = uv.Retriever()
    option_info_provider = oi.Retriever()

    data = option_info_provider.get(SYMBOL)
    volatility = underlying_volatility_provider.get(SYMBOL)

    today = datetime.date.today()

    print(f"\nStock symbol: {SYMBOL}")
    print(f"Today: {today}")
    print(f"Option type: {OPT_TYPE}")
    print(f"Model periods: {MODEL_PERIODS}")
    print(f"Div date: {data.dividendDate}")
    print(f"Market price: {data.regularMarketPrice}")

    for option in data.options['puts' if OPT_TYPE & binom.OptionType.PUT else 'calls']:
        info = oi.get_option_info(option)

        option_duration = (info.expiration - today).days
        risk_free_rate = risk_free_provider.get_closest_ge(today.month, today.year, option_duration) / 100

        dt = option_duration / TRADING_DAYS / MODEL_PERIODS

        vol = volatility / math.sqrt(dt * MODEL_PERIODS)  # formula from book p. 325
        
        u, d = binom.get_ud_vol(vol, dt)
        p_up = binom.compute_prob_up(u, d, dt, risk_free_rate, 0)
        N = MODEL_PERIODS + 1

        lattice = binom.gen_lattice(data.regularMarketPrice, u, d, N)

        print(f"\nContract symbol: {info.contractSymbol}")
        print(f'\tRisk-free rate    : {risk_free_rate:.2%}')
        print(f"\tDuration          : {option_duration} days")
        print(f"\tVolatility        : {vol:.2%}")
        print(f"\tMultipliers:")
        print(f"\t\tUp  : {u:.4f}")
        print(f"\t\tDown: {d:.4f}")
        print(f"\tProbability to rise: {p_up:.4f}")

        try:
            VALUE = binom.compute_value(lattice, OPT_TYPE, info.strike, risk_free_rate, dt, p_up)
        except AssertionError as e:
            print(f"\tOOPS! Could not calculate value because {e}")
        else:
            VALUE = .01 if VALUE < .01 else VALUE
            print(f"\n\tPrice actual: {info.lastPrice}")
            print(f"\tPrice MODEL : {VALUE:.2f} (= {VALUE / info.lastPrice:.4} * actual)")