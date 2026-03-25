import numpy as np
import matplotlib.pyplot as plt

# Coefficient values derived from your polynomial
phi1 = -0.7
phi2 = -0.2

# Solve the characteristic equation: z^2 + phi1*z + phi2 = 0
discriminant = phi1**2 - 4*phi2
z1 = (-phi1 + np.sqrt(discriminant)) / 2
z2 = (-phi1 - np.sqrt(discriminant)) / 2

# Initial conditions
rho_0 = 1.0
rho_1 = -phi1 / (1 + phi2)

# Solve for constants A1 and A2 in rho(k) = A1*z1^k + A2*z2^k
A = np.array([[1, 1], [z1, z2]])
b = np.array([rho_0, rho_1])
A1, A2 = np.linalg.solve(A, b)

# Calculate rho(k) for lags 0 to 30
n_lag = 30
lags = np.arange(n_lag + 1)
rho_k = A1 * (z1**lags) + A2 * (z2**lags)

# Plotting
plt.figure(figsize=(10, 5))
plt.stem(lags, rho_k)
plt.axhline(0, color='black', linewidth=0.8)
#plt.title(r'Autocorrelation function $\rho(k)$ for AR(2)')
plt.xlabel('Lag $k$')
plt.ylabel(r'$\rho(k)$')
plt.grid(True, linestyle='--', alpha=0.6)
plt.savefig('autocorrelation_plot.png')
