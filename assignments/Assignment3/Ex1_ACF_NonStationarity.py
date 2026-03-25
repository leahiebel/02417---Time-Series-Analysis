import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.stattools import acf

# 1. Setup for Case 1.6 (example)
np.random.seed(42)
phi1, phi2 = -0.75, -0.3
n_obs, n_series, lags = 200, 5, 30

# 2. MANUAL GENERATION (Bypassing statsmodels' safety checks)
all_realizations = []
for _ in range(n_series):
    y = np.zeros(n_obs)
    epsilon = np.random.normal(0, 1, n_obs)
    for t in range(2, n_obs):
        # Xt = -phi1*Xt-1 - phi2*Xt-2 + e (Note the signs!)
        y[t] = -phi1 * y[t-1] - phi2 * y[t-2] + epsilon[t]
    all_realizations.append(y[:])

# 3. PLOT TIME SERIES (Same as before)
plt.figure(figsize=(12, 4))
for i, y in enumerate(all_realizations):
    plt.plot(y, label=f'Realization {i+1}')

plt.title(f'Non-Stationary AR(2) ($\phi_1={phi1}$, $\phi_2={phi2}$)')
plt.legend()
plt.xlabel('Time (t)')
plt.ylabel('$X_t$')
plt.grid(True, linestyle='--', alpha=0.7)
plt.show()

# 4. PLOT EMPIRICAL ACF
plt.figure(figsize=(12, 6))
conf_interval = 1.96 / np.sqrt(n_obs)

for i, y in enumerate(all_realizations):
    # Note: Theoretical ACF is omitted because it is undefined for non-stationary processes
    emp_acf = acf(y, nlags=lags)
    plt.plot(range(lags + 1), emp_acf, marker='o', alpha=0.6, label=f'Realization {i+1}')

plt.axhline(y=conf_interval, color='red', linestyle='--')
plt.axhline(y=-conf_interval, color='red', linestyle='--')
plt.title(f'Empirical ACF for Non-Stationary Process ($\phi_1={phi1}$)')
plt.xlabel('Lag (k)')
plt.ylabel('Autocorrelation')
plt.legend()
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.show()
