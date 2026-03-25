import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.arima_process import arma_generate_sample, arma_acf
from statsmodels.tsa.stattools import acf

# 1. Setup
np.random.seed(42)
phi1, phi2 = -0.75, -0.3
ar_params = np.array([1, phi1, phi2])
ma_params = np.array([1])
n_obs, n_series, lags = 200, 5, 30

# 2. GENERATE AND SAVE (Run this once)
all_realizations = [arma_generate_sample(ar_params, ma_params, n_obs) for _ in range(n_series)]

# 3. PLOT TIME SERIES
plt.figure(figsize=(12, 6))
for i, y in enumerate(all_realizations):
    plt.plot(y, label=f'Realization {i+1}')

plt.title(f'AR(2) simulated via statsmodels ($\phi_1={phi1}$, $\phi_2={phi2}$)')
plt.legend()
plt.xlabel('Time (t)')
plt.ylabel('$X_t$')
plt.grid(True, linestyle='--', alpha=0.7)
plt.show()

# 4. PLOT ACF with Confidence Intervals
plt.figure(figsize=(12, 6))

# Calculate the 95% confidence interval bound
conf_interval = 1.96 / np.sqrt(n_obs)

# Theoretical ACF (The "Goal")
theo_acf = arma_acf(ar_params, ma_params, lags=lags + 1)
plt.stem(range(lags + 1), theo_acf, linefmt='k-', markerfmt='ko', label='Theoretical $\\rho(k)$')

# Empirical ACFs (The "Reality")
for i, y in enumerate(all_realizations):
    emp_acf = acf(y, nlags=lags)
    plt.plot(range(lags + 1), emp_acf, alpha=0.6, label=f'Realization {i+1}')

# Add Confidence Interval Lines
plt.axhline(y=conf_interval, color='red', linestyle='--', alpha=0.5, label='95% Conf. Interval')
plt.axhline(y=-conf_interval, color='red', linestyle='--', alpha=0.5)

# Formatting
plt.title(f'Theoretical vs. Empirical ACF ($n={n_obs}$)')
plt.xlabel('Lag (k)')
plt.ylabel('Autocorrelation')
plt.legend()
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()
