import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
import statsmodels.api as sm


###################################
# Q.1. LOAD DATA
###################################

df = pd.read_csv(r'C:\Users\manon\OneDrive\Bureau\Danemark\DTU\Spring 2026\02417 Time Series Analysis\02417---Time-Series-Analysis\assignments\Assignment3\assignment3_2026\box_data_60min.csv')

print(df.columns)
print(df.head)


###################################
# Q.1. PLOT
###################################

fig, axes = plt.subplots(3, 1, figsize=(12, 10), sharex=True)

# Plot Heating Power (Ph)
axes[0].plot(df['Ph'], color='firebrick', linewidth=1.5)
axes[0].set_ylabel('$P_h$ - Heating Power (W)')
axes[0].set_title('Hourly Heating of Test Box')
axes[0].grid(True, alpha=0.3)

# Plot Temperature Difference (Tdelta)
axes[1].plot(df['Tdelta'], color='darkblue', linewidth=1.5)
axes[1].set_ylabel('$T_{delta}$ = $T_{internal}$ - $T_{external}$ (°C)')
axes[1].grid(True, alpha=0.3)

# Plot Solar Radiation (Gv)
axes[2].plot(df['Gv'], color='orange', linewidth=1.5)
axes[2].set_ylabel('$G_v$ - Solar Radiation (W/m²)')
axes[2].set_xlabel('Time (Hours)')
axes[2].grid(True, alpha=0.3)

plt.tight_layout()
plt.show()



############################################
# Q.2. SPLIT BETWEEN TRAINING AND TEST SETS
############################################

# Define the split point
train_size = 167

# Split the data
train_df = df.iloc[:train_size].copy()
test_df = df.iloc[train_size:].copy()

# Verify the split
print(f"Training set: {train_df.shape[0]} samples")
print(f"Test set:     {test_df.shape[0]} samples")

# Display the last timestamp of the training set to ensure it matches "2013-02-06 00:00"
final_timestamp = train_df['tdate'].iloc[-1]
print(f"Last training timestamp: {final_timestamp}")



####################################################
# Q.3. SCATTER, AUTO-CORRELATION, CROSS-CORRELATION
####################################################

# Scatter Plots with Regression Lines
fig, ax = plt.subplots(1, 2, figsize=(14, 5))

sns.regplot(data=train_df, x='Tdelta', y='Ph', ax=ax[0], scatter_kws={'alpha':0.5}, line_kws={'color':'red'})
ax[0].set_title('$P_h$ vs $T_{delta}$ (Thermal Gradient)')

sns.regplot(data=train_df, x='Gv', y='Ph', ax=ax[1], scatter_kws={'alpha':0.5}, line_kws={'color':'orange'})
ax[1].set_title('$P_h$ vs $G_v$ (Solar Radiation)')
plt.tight_layout()
plt.show()


# Auto-correlation (ACF) and Partial Auto-correlation (PACF) for $P_h$
fig, ax = plt.subplots(1, 2, figsize=(14, 4))
plot_acf(train_df['Ph'], lags=48, ax=ax[0], title='ACF of Heating ($P_h$)')
plot_pacf(train_df['Ph'], lags=48, ax=ax[1], title='PACF of Heating ($P_h$)')
ax[0].set_xlabel('Lag (Hours)')
ax[1].set_xlabel('Lag (Hours)')
plt.show()


# Cross-correlation (CCF)
# ccf(x, y) shows how x at time t correlates with y at time t+k
fig, ax = plt.subplots(1, 2, figsize=(14, 4))

# CCF: Tdelta and Ph
ccf_tdelta = [train_df['Ph'].corr(train_df['Tdelta'].shift(i)) for i in range(25)]
ax[0].stem(range(25), ccf_tdelta)
ax[0].set_title('CCF: $T_{delta}$ lagging behind $P_h$')
ax[0].set_xlabel('Lag (Hours)')

# CCF: Gv and Ph
ccf_gv = [train_df['Ph'].corr(train_df['Gv'].shift(i)) for i in range(25)]
ax[1].stem(range(25), ccf_gv)
ax[1].set_title('CCF: $G_v$ lagging behind $P_h$')
ax[1].set_xlabel('Lag (Hours)')
plt.show()



#################################
# Q.4. IMPULSE RESPONSE FUNCTION
#################################

def estimate_irf(target, exogenous, max_lag=10):
    # Create a DataFrame for the lagged exogenous variables
    lag_cols = pd.DataFrame(index=target.index)
    for i in range(max_lag + 1):
        lag_cols[f'lag_{i}'] = exogenous.shift(i)
    
    # Drop rows with NaN from shifting
    valid_data = pd.concat([target, lag_cols], axis=1).dropna()
    
    y = valid_data[target.name]
    X = valid_data.drop(columns=[target.name])
    X = sm.add_constant(X) # Include intercept
    
    model = sm.OLS(y, X).fit()
    # Return coefficients for lag_0 to lag_10 (skipping the constant)
    return model.params[1:], model.bse[1:]

# Calculate IRFs (Impulse Response Functions) using the training data
irf_tdelta, stderr_tdelta = estimate_irf(train_df['Ph'], train_df['Tdelta'], max_lag=10)
irf_gv, stderr_gv = estimate_irf(train_df['Ph'], train_df['Gv'], max_lag=10)


# Plotting
fig, axes = plt.subplots(1, 2, figsize=(15, 5))

# Plot for Tdelta
axes[0].stem(range(11), irf_tdelta, basefmt=" ", linefmt='blue')
axes[0].errorbar(range(11), irf_tdelta, yerr=1.96*stderr_tdelta, fmt='none', ecolor='gray', alpha=0.5)
axes[0].axhline(0, color='blue', linewidth=1.5, zorder=1)
axes[0].set_title('Impulse Response: $T_{delta} \\rightarrow P_h$')
axes[0].set_xlabel('Lag (Hours)')
axes[0].set_ylabel('Response Coefficient')
axes[0].grid(True, alpha=0.3)

# Plot for Gv
axes[1].stem(range(11), irf_gv, basefmt=" ", linefmt='orange')
axes[1].errorbar(range(11), irf_gv, yerr=1.96*stderr_gv, fmt='none', ecolor='gray', alpha=0.5)
axes[1].axhline(0, color='orange', linewidth=1.5, zorder=1)
axes[1].set_title('Impulse Response: $G_v \\rightarrow P_h$')
axes[1].set_xlabel('Lag (Hours)')
axes[1].set_ylabel('Response Coefficient')
axes[1].grid(True, alpha=0.3)

plt.tight_layout()
plt.show()



###############################
# Q.5. LINEAR REGRESSION MODEL
###############################

X = train_df[['Tdelta', 'Gv']]
X = sm.add_constant(X)
y = train_df['Ph']

model_static = sm.OLS(y, X).fit()
train_df['pred_static'] = model_static.predict(X)
train_df['residuals'] = model_static.resid

print(model_static.summary())


fig, axes = plt.subplots(2, 2, figsize=(15, 10))

# Time Series Plot: Actual vs Predicted
axes[0,0].plot(train_df.index, train_df['Ph'], label='Actual $P_h$', alpha=0.7)
axes[0,0].plot(train_df.index, train_df['pred_static'], label='Static Prediction', linestyle='--')
axes[0,0].set_title('One-step Prediction (Static Model)')
axes[0,0].legend()

# Residuals over time
axes[0,1].plot(train_df.index, train_df['residuals'], color='purple')
axes[0,1].axhline(0, color='black', lw=1)
axes[0,1].set_title('Residuals $\epsilon_t$ over Time')

# ACF of Residuals (Check for independence)
plot_acf(train_df['residuals'], lags=40, ax=axes[1,0], title='ACF of Residuals')

# CCF between Tdelta and Residuals (Check for unmodeled dynamics)
# Manual CCF calculation for residuals
res_ccf = [train_df['residuals'].corr(train_df['Tdelta'].shift(i)) for i in range(25)]
axes[1,1].stem(range(25), res_ccf)
axes[1,1].axhline(0, color='black', lw=1)
axes[1,1].set_title('CCF: $T_{delta}$ vs Residuals')

plt.tight_layout()
plt.show()
