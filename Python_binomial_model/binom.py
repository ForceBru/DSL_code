'''
The implementation of the Binomial Tree option pricing algorithm.

Required information about the option:
    * `vol` (fraction of unit, float) - volatility of the underlying stock's returns 
    * `S0`  (float) - initial value of the underlying stock
    * `strike` (float) - strike price for the option
    * `r` (fraction of unit, float)
    * `dt`  (years, float) - time interval between nodes in the tree
    * `N`   (int) - number of periods the node represents, _including the current one_

Option type must be one of the following:
    * Call and American
    * Call and European
    * Put and American
    * Put and European

These are to be combined using `OptionType` and the bitwise OR (`|`) operator (see example below).

Usage:
    1. Given `vol` and `dt`, compute `u` and `d` - upwards and downwards multipliers.
        >>> u, d = get_ud_vol(0.23, 0.001)
    2. Generate the actual tree (lattice).
        >>> lattice = gen_lattice(12.34, u, d, 60 + 1)
    3. Compute the probability of upwards movenent, `p`.
        >>> p = compute_prob_up(u, d, 0.001, 0.12, 0.0)
    4. Compute the estimated price of the option:
        >>> result = compute_value(lattice, OptionType.CALL | OptionType.AMERICAN, 13.24, 0.12, 0.001, p)
'''

import enum
import math

class OptionType(enum.Flag):
    CALL = enum.auto()
    PUT = enum.auto()
    AMERICAN = enum.auto()
    EUROPEAN = enum.auto()

def get_ud_vol(vol: float, dt: float):
    '''
    Returns u and d given volatility and dt.
    '''

    u = math.exp(vol * math.sqrt(dt))
    d = 1 / u

    return u, d

def gen_lattice(S0: float, u: float, d: float, N: int) -> list:
    '''
    Generate binomial tree of depth (N + 1), starting with level 0.
    At level `i` there are (i + 1) nodes.

    @args:
    S0 - initial value of stock
    u - upwards change
    N - number of periods, including the current one

    @returns:
    list of lists, where each list represents one layer of the tree
    '''
    assert u > 1
    assert 0 < d < 1

    return [[u**(n - i) * d**i * S0 for i in range(n + 1)] for n in range(N)]

def compute_prob_up(u: float, d: float, dt: float, r: float, q: float):
    assert u > 1
    assert 0 < d < 1

    return (math.exp((r - q) * dt) - d) / (u - d)

def compute_value(
    lattice: list,
    opt_type: OptionType,
    strike: float,
    r: float,
    dt: float,
    p: float):
    '''
    strike - strike price
    r -  the risk free rate corresponding to the life of the option
    dt - time step
    p - probability of going up
    '''

    assert 0 <= p <= 1, f"Invalid probability: {p}"

    if opt_type & OptionType.CALL:
        payoff_from_exercise = lambda S, strike: S - strike
    elif opt_type & OptionType.PUT:
        payoff_from_exercise = lambda S, strike: strike - S
    else:
        raise ValueError("Please specify option type (CALL or PUT)")

    # Depending on the style of the option, evaluate the possibility of early exercise at each node: if the option can be exercised, and the exercise value exceeds the Binomial Value, then the value at the node is the exercise value. If no early exercise is possible, the value at each node is the Binomial Value.
    if opt_type & OptionType.EUROPEAN:
        process = lambda binom, exerc: binom
    elif opt_type & OptionType.AMERICAN:
        process = lambda binom, exerc: max(binom, exerc)
    else:
        raise ValueError("Please specify option type (AMERICAN or EUROPEAN)")

    i = -1

    lattice[i] = [
        max(0, payoff_from_exercise(S, strike))
        for S in lattice[i]
    ]

    expected = lattice[i]

    discount = math.exp(-r * dt)

    for layer in lattice[-2::-1]:  # iterate backwards; the last layer has already been processed
        # compute expected value using risk neutrality assumption
        expected = [
            process(
                discount * (p * lattice[i][j] + (1 - p) * lattice[i][j + 1]),
                payoff_from_exercise(layer[j], strike)
                )
            for j in range(len(layer))
        ]

        i -= 1
        lattice[i] = expected

    assert len(expected) == 1

    return expected[0]