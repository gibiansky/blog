#!/usr/bin/env python
from math import exp
from numpy import array
from numpy.fft import fft, ifft

# Input stock parameters
dt = input("Enter the timestep: ")
S = input("Enter the initial asset price: ")
r = input("Enter the risk-free discount rate: ")
K = input("Enter the option strike price: ")
p = input("Enter the asset growth probability p: ")
u = input("Enter the asset growth factor u: ")
N = input("Enter the number of timesteps until expiration: ")

# Input whether this is a call or a put option
call = raw_input("Is this a call or put option? (C/P) ").upper().startswith("C")

def price(k, us):
    """ Compute the stock price after 'us' growths and 'k - us' decays. """
    return S * (u ** (2 * us - k))

def leaves(k):
    """ Compute the leaves of the tree for a depth of k timesteps. """
    values = []
    for i in xrange(k + 1):
        if call: values.append(max(0, price(k, i) - K))
        else:    values.append(max(0, K - price(k, i)))
    return values

def bopm(k):
    """
    Compute the option price for an option expiring in 'k' timesteps.
    """
    # Obtain the leaf prices as a NumPy array and create the q vector
    leafValues = array(list(reversed(leaves(k))))
    q = array([p, 1-p] + [0] * (k - 1))

    # Compute the options price via the fast Fourier transform
    C = ifft(fft(leafValues) * ((k + 1) * exp(-r * dt) * ifft(q)) ** k)

    # Return the first component, which is the only important one
    return C[0]

print 'Computed option price: $%.2f' % bopm(N).real
