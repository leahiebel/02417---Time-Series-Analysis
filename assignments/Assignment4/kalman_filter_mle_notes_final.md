# Kalman Filter Initialization and Maximum Likelihood Estimation

These notes summarize the main ideas from the slides on:

1. Kalman filter initialization  
2. Maximum likelihood estimation for state-space models  
3. Kalman filter one-step prediction  
4. The likelihood function using prediction errors / innovations  
5. Reconstruction / update step and missing observations  
6. ARMA\((p,q)\) models written as state-space models  
7. Parameter estimation for ARMA models using the Kalman filter  
8. Formulating state-space models from physical equations  
9. Converting continuous-time state-space models to discrete time  
10. Treating unknown inputs / parameters as part of the state  

---

## 1. Big picture

The Kalman filter is used for two related tasks:

- **State estimation / filtering**: estimate the hidden state \(X_t\) using observations up to time \(t\).
- **Parameter estimation**: estimate unknown model parameters \(\theta\) by maximizing the likelihood of the observed data.

In a linear Gaussian state-space model, the Kalman filter gives the conditional distributions needed to build the likelihood function.

---

## 2. Notation

Let:

- \(X_t\): hidden state at time \(t\)
- \(Y_t\): observation at time \(t\)
- \(\mathcal{Y}_t = \{Y_1, \dots, Y_t\}\): observations available up to time \(t\)
- \(\theta\): model parameters
- \(\widehat{X}_{t|s}\): estimate of \(X_t\) using observations up to time \(s\)
- \(\Sigma^{xx}_{t|s}\): covariance matrix of the uncertainty in \(X_t\) given observations up to time \(s\)
- \(\widehat{Y}_{t|s}\): predicted observation at time \(t\) using observations up to time \(s\)
- \(\Sigma^{yy}_{t|s}\): covariance matrix of the prediction uncertainty for \(Y_t\)

So:

\[
\widehat{X}_{t|t} = E[X_t \mid \mathcal{Y}_t]
\]

and

\[
\Sigma^{xx}_{t|t} = \operatorname{Var}(X_t \mid \mathcal{Y}_t)
\]

This means:

\[
X_t \mid \mathcal{Y}_t \sim \mathcal{N}
\left(
\widehat{X}_{t|t},
\Sigma^{xx}_{t|t}
\right)
\]

---

## 3. Kalman filter initialization

Before the Kalman filter can run, we need an initial estimate of the state and its uncertainty.

That means choosing:

\[
\widehat{X}_{1|1}
\]

and

\[
\Sigma^{xx}_{1|1}
\]

The key idea is:

> The covariance matrix \(\Sigma^{xx}_{1|1}\) should reflect how uncertain we are about the initial state estimate \(\widehat{X}_{1|1}\).

---

## 4. Initialization cases

### Case 1: You have no idea about the starting state

If we have no information about the initial state, we can choose:

\[
\widehat{X}_{1|1} = 0
\]

and

\[
\Sigma^{xx}_{1|1} = \alpha I
\]

where:

- \(I\) is the identity matrix
- \(\alpha\) is a large constant

This means:

> We start with a neutral initial guess, but we tell the model that this guess is very uncertain.

A large covariance makes the filter less confident in the initial state and allows observations to quickly correct the estimate.

---

### Case 2: You know the starting state exactly

If the initial state is known exactly, set:

\[
\widehat{X}_{1|1} = \text{known value}
\]

and

\[
\Sigma^{xx}_{1|1} = 0
\]

This means:

> There is no uncertainty about the starting state.

In this case, the first observation still has observation noise covariance:

\[
\Sigma^{yy}_{1|1} = \Sigma_2
\]

where \(\Sigma_2\) is the observation noise covariance matrix.

---

### Case 3: You have a good guess about the starting state

If you have a reasonable initial guess, set:

\[
\widehat{X}_{1|1} = \text{guess}
\]

and

\[
\Sigma^{xx}_{1|1} = \Sigma_{\text{guess}}
\]

where \(\Sigma_{\text{guess}}\) represents your uncertainty about the guess.

This is the most realistic case in many applications.

---

## 5. Why initialization matters

The Kalman filter is recursive. This means each estimate depends on the previous estimate.

So a bad initialization can affect early estimates, especially when:

- the time series is short
- the initial covariance is too small
- the filter is too confident in a wrong starting value

The most important rule is:

> The uncertainty of the initial estimate should be encoded in the initial covariance matrix.

If you are unsure, use a larger covariance.  
If you are confident, use a smaller covariance.

---

## 6. Maximum likelihood estimation

Let \(\mathcal{Y}_{N^*}\) contain the available observations.

Let \(\theta\) contain the parameters of the model.

The likelihood function is:

\[
L(\theta; \mathcal{Y}_{N^*})
=
f(\mathcal{Y}_{N^*} \mid \theta)
\]

Important point:

> The likelihood is the density of the observed data, evaluated at the actual observations, as a function of the parameters \(\theta\).

So we are not treating it as a probability density in the observations.  
We treat the observed data as fixed and vary \(\theta\).

The maximum likelihood estimate is the parameter value that makes the observed data most likely:

\[
\widehat{\theta}_{\text{MLE}}
=
\arg\max_\theta L(\theta; \mathcal{Y}_{N^*})
\]

Usually, we maximize the log-likelihood instead:

\[
\widehat{\theta}_{\text{MLE}}
=
\arg\max_\theta \log L(\theta; \mathcal{Y}_{N^*})
\]

because sums are easier and more numerically stable than products.

---

## 7. Factorizing the likelihood

The joint density of all observations can be decomposed using conditional densities:

\[
f(\mathcal{Y}_{N^*} \mid \theta)
=
f(Y_{N^*} \mid \mathcal{Y}_{N^*-1}, \theta)
f(\mathcal{Y}_{N^*-1} \mid \theta)
\]

Repeating this recursively gives:

\[
L(\theta; \mathcal{Y}_{N^*})
=
f(Y_{N^*} \mid \mathcal{Y}_{N^*-1}, \theta)
f(Y_{N^*-1} \mid \mathcal{Y}_{N^*-2}, \theta)
\cdots
f(Y_1 \mid \theta)
\]

or compactly:

\[
L(\theta; \mathcal{Y}_{N^*})
=
\prod_{t=1}^{N^*}
f(Y_t \mid \mathcal{Y}_{t-1}, \theta)
\]

This is the key connection between the Kalman filter and MLE.

The Kalman filter gives us the one-step-ahead predictive distribution:

\[
Y_t \mid \mathcal{Y}_{t-1}, \theta
\]

Therefore, it gives us exactly the terms needed to evaluate the likelihood.

---

## 8. Kalman filter prediction step

Assume that at time \(t\), after seeing observations up to time \(t\), we have:

\[
\widehat{X}_{t|t}
=
E[X_t \mid \mathcal{Y}_t]
\]

and

\[
\Sigma^{xx}_{t|t}
=
\operatorname{Var}(X_t \mid \mathcal{Y}_t)
\]

Now we want to predict the state and observation at time \(t+1\).

---

## 9. State prediction

Using the state equation, the predicted next state is:

\[
\widehat{X}_{t+1|t}
=
A \widehat{X}_{t|t}
\]

The predicted covariance of the next state is:

\[
\Sigma^{xx}_{t+1|t}
=
A \Sigma^{xx}_{t|t} A^T
+
G \Sigma_1 G^T
\]

where:

- \(A\) is the state transition matrix
- \(G\) maps process noise into the state equation
- \(\Sigma_1\) is the process noise covariance matrix

Interpretation:

- \(A \Sigma^{xx}_{t|t} A^T\) propagates current state uncertainty forward.
- \(G \Sigma_1 G^T\) adds new process noise uncertainty.

---

## 10. Observation prediction

Using the observation equation, the predicted observation is:

\[
\widehat{Y}_{t+1|t}
=
C \widehat{X}_{t+1|t}
\]

The predicted observation covariance is:

\[
\Sigma^{yy}_{t+1|t}
=
C \Sigma^{xx}_{t+1|t} C^T
+
\Sigma_2
\]

where:

- \(C\) maps the hidden state into the observation space
- \(\Sigma_2\) is the observation noise covariance matrix

Interpretation:

- \(C \Sigma^{xx}_{t+1|t} C^T\) is uncertainty coming from the hidden state.
- \(\Sigma_2\) is additional measurement noise.

---

## 11. Predictive density used in the likelihood

Because the model is linear Gaussian, the one-step-ahead predictive distribution is Gaussian:

\[
Y_{t+1} \mid \mathcal{Y}_t, \theta
\sim
\mathcal{N}
\left(
\widehat{Y}_{t+1|t},
\Sigma^{yy}_{t+1|t}
\right)
\]

This means that the likelihood contribution at time \(t+1\) is the Gaussian density evaluated at the actual observed value \(y_{t+1}\):

\[
f(Y_{t+1} = y_{t+1} \mid \mathcal{Y}_t, \theta)
\]

with mean:

\[
\widehat{Y}_{t+1|t}
\]

and covariance:

\[
\Sigma^{yy}_{t+1|t}
\]

---

## 12. Innovation / prediction error

The one-step prediction error is called the innovation:

\[
v_{t+1}
=
y_{t+1}
-
\widehat{Y}_{t+1|t}
\]

The innovation covariance is:

\[
S_{t+1}
=
\Sigma^{yy}_{t+1|t}
\]

So:

\[
v_{t+1}
\sim
\mathcal{N}(0, S_{t+1})
\]

The likelihood rewards parameter values that make the innovations small relative to their predicted uncertainty.

---

## 13. Log-likelihood contribution

For an observation vector of dimension \(m\), the log-likelihood contribution at time \(t+1\) is:

\[
\ell_{t+1}(\theta)
=
-\frac{1}{2}
\left[
m \log(2\pi)
+
\log |S_{t+1}|
+
v_{t+1}^T S_{t+1}^{-1} v_{t+1}
\right]
\]

The total log-likelihood is:

\[
\ell(\theta)
=
\sum_{t=1}^{N^*}
\ell_t(\theta)
\]

So the MLE problem becomes:

\[
\widehat{\theta}_{\text{MLE}}
=
\arg\max_\theta
\sum_{t=1}^{N^*}
\ell_t(\theta)
\]

---

## 14. Summary of the workflow

The Kalman filter likelihood calculation works like this:

1. Initialize the state:
   \[
   \widehat{X}_{1|1}, \qquad \Sigma^{xx}_{1|1}
   \]

2. Predict the next state:
   \[
   \widehat{X}_{t+1|t}
   =
   A\widehat{X}_{t|t}
   \]

3. Predict the next state covariance:
   \[
   \Sigma^{xx}_{t+1|t}
   =
   A\Sigma^{xx}_{t|t}A^T
   +
   G\Sigma_1G^T
   \]

4. Predict the next observation:
   \[
   \widehat{Y}_{t+1|t}
   =
   C\widehat{X}_{t+1|t}
   \]

5. Predict the observation covariance:
   \[
   \Sigma^{yy}_{t+1|t}
   =
   C\Sigma^{xx}_{t+1|t}C^T
   +
   \Sigma_2
   \]

6. Evaluate the Gaussian density of the actual observation:
   \[
   y_{t+1}
   \sim
   \mathcal{N}
   \left(
   \widehat{Y}_{t+1|t},
   \Sigma^{yy}_{t+1|t}
   \right)
   \]

7. Add this density contribution to the log-likelihood.

8. Continue recursively through all observations.

9. Choose parameters \(\theta\) that maximize the total log-likelihood.

---

## 15. Core intuition

The Kalman filter gives a sequence of one-step-ahead predictions:

\[
\widehat{Y}_{t|t-1}
\]

and their uncertainties:

\[
\Sigma^{yy}_{t|t-1}
\]

The MLE procedure asks:

> Which parameters \(\theta\) make the actually observed data look most plausible under these one-step-ahead Gaussian predictions?

So the Kalman filter is not only a filtering algorithm.  
It is also a practical way to compute the likelihood of a linear Gaussian state-space model.

---

## 16. Most important formulas to remember

### Initialization

\[
\widehat{X}_{1|1} = 0,
\qquad
\Sigma^{xx}_{1|1} = \alpha I
\]

when no starting information is available.

---

### State prediction

\[
\widehat{X}_{t+1|t}
=
A\widehat{X}_{t|t}
\]

\[
\Sigma^{xx}_{t+1|t}
=
A\Sigma^{xx}_{t|t}A^T
+
G\Sigma_1G^T
\]

---

### Observation prediction

\[
\widehat{Y}_{t+1|t}
=
C\widehat{X}_{t+1|t}
\]

\[
\Sigma^{yy}_{t+1|t}
=
C\Sigma^{xx}_{t+1|t}C^T
+
\Sigma_2
\]

---

### Likelihood factorization

\[
L(\theta; \mathcal{Y}_{N^*})
=
\prod_{t=1}^{N^*}
f(Y_t \mid \mathcal{Y}_{t-1}, \theta)
\]

---

### Gaussian predictive density

\[
Y_t \mid \mathcal{Y}_{t-1}, \theta
\sim
\mathcal{N}
\left(
\widehat{Y}_{t|t-1},
\Sigma^{yy}_{t|t-1}
\right)
\]

---

### Log-likelihood

\[
\ell(\theta)
=
-\frac{1}{2}
\sum_{t=1}^{N^*}
\left[
m\log(2\pi)
+
\log|S_t|
+
v_t^T S_t^{-1}v_t
\right]
\]

where:

\[
v_t = y_t - \widehat{Y}_{t|t-1}
\]

and

\[
S_t = \Sigma^{yy}_{t|t-1}
\]

---

## 17. One-sentence exam summary

The Kalman filter provides the Gaussian one-step-ahead predictive densities \(f(Y_t \mid \mathcal{Y}_{t-1}, \theta)\), and these densities can be multiplied together to form the likelihood used for maximum likelihood estimation of the state-space model parameters.


---

## 18. Likelihood function using prediction errors / innovations

The likelihood can be written using the **one-step-ahead prediction errors** produced by the Kalman filter.

The prediction error, also called the **innovation**, is:

\[
\widetilde{Y}_{t+1|t}
=
Y_{t+1}-\widehat{Y}_{t+1|t}
\]

The covariance of this prediction error is:

\[
\Sigma^{yy}_{t+1|t}
=
C\Sigma^{xx}_{t+1|t}C^T+\Sigma_2
\]

So, conditionally on the past observations,

\[
Y_{t+1}\mid \mathcal{Y}_t
\sim
\mathcal{N}
\left(
\widehat{Y}_{t+1|t},
\Sigma^{yy}_{t+1|t}
\right)
\]

Equivalently, the innovation is centered around zero:

\[
\widetilde{Y}_{t+1|t}
\sim
\mathcal{N}
\left(
0,
\Sigma^{yy}_{t+1|t}
\right)
\]

This is why the innovation and its variance-covariance matrix are enough to build the likelihood.

---

## 19. Explicit likelihood formula

For observation dimension \(m\), the likelihood is a product of Gaussian densities:

\[
L(\theta;\mathcal{Y}_{N^*})
=
\prod_{i=1}^{N^*}
\left[
(2\pi)^m
\det\left(\Sigma^{yy}_{i|i-1}\right)
\right]^{-1/2}
\exp
\left[
-
\frac{1}{2}
\widetilde{Y}_{i|i-1}^T
\left(\Sigma^{yy}_{i|i-1}\right)^{-1}
\widetilde{Y}_{i|i-1}
\right]
\]

The important structure is:

\[
\text{Likelihood}
=
\prod
\text{normal density of each one-step prediction error}
\]

A parameter value \(\theta\) is good if it produces:

- small prediction errors \(\widetilde{Y}_{i|i-1}\), and
- realistic uncertainty matrices \(\Sigma^{yy}_{i|i-1}\).

---

## 20. Explicit log-likelihood formula

Taking logs turns the product into a sum:

\[
\log L(\theta;\mathcal{Y}_{N^*})
=
-
\frac{1}{2}
\sum_{i=1}^{N^*}
\left(
\log \det\left(\Sigma^{yy}_{i|i-1}\right)
+
\widetilde{Y}_{i|i-1}^T
\left(\Sigma^{yy}_{i|i-1}\right)^{-1}
\widetilde{Y}_{i|i-1}
\right)
+
c
\]

where \(c\) contains constants such as:

\[
-\frac{1}{2}N^*m\log(2\pi)
\]

Since \(c\) does not depend on the parameters, it often does not affect the location of the maximum.

The two main terms are:

1. **Uncertainty penalty**

   \[
   \log \det\left(\Sigma^{yy}_{i|i-1}\right)
   \]

   This penalizes overly large predictive variances.

2. **Prediction-error penalty**

   \[
   \widetilde{Y}_{i|i-1}^T
   \left(\Sigma^{yy}_{i|i-1}\right)^{-1}
   \widetilde{Y}_{i|i-1}
   \]

   This penalizes prediction errors, scaled by their uncertainty.

So MLE tries to find parameters that balance:

> accurate predictions and realistic uncertainty.

---

## 21. Parameter uncertainty from the log-likelihood

After estimating the parameters, the uncertainty of the estimate \(\widehat{\theta}\) can be approximated using the curvature of the log-likelihood.

The slide states that the variance of the parameter estimates can be approximated by the **second-order derivatives** of the log-likelihood.

The intuition is:

- If the log-likelihood is sharply peaked around \(\widehat{\theta}\), then the estimate is precise.
- If the log-likelihood is flat around \(\widehat{\theta}\), then the estimate is uncertain.

This is connected to the Hessian matrix:

\[
H(\widehat{\theta})
=
\left.
\frac{\partial^2 \log L(\theta)}{\partial \theta \partial \theta^T}
\right|_{\theta=\widehat{\theta}}
\]

A common approximation is:

\[
\operatorname{Var}(\widehat{\theta})
\approx
\left[-H(\widehat{\theta})\right]^{-1}
\]

assuming regularity conditions and a well-behaved maximum.

---

## 22. Reconstruction / update step when the observation is available

At time \(t+1\), after making the prediction, there are two cases:

1. The observation \(Y_{t+1}\) is available.
2. The observation \(Y_{t+1}\) is missing.

When \(Y_{t+1}\) is available, we update the predicted state using the Kalman filter reconstruction step.

The Kalman gain is:

\[
K_{t+1}
=
\Sigma^{xx}_{t+1|t}C^T
\left(\Sigma^{yy}_{t+1|t}\right)^{-1}
\]

The updated state estimate is:

\[
\widehat{X}_{t+1|t+1}
=
\widehat{X}_{t+1|t}
+
K_{t+1}
\left(
Y_{t+1}-\widehat{Y}_{t+1|t}
\right)
\]

The updated state covariance is:

\[
\Sigma^{xx}_{t+1|t+1}
=
\Sigma^{xx}_{t+1|t}
-
K_{t+1}
\Sigma^{yy}_{t+1|t}
K_{t+1}^T
\]

Interpretation:

- \(\widehat{X}_{t+1|t}\) is the prior / prediction before seeing \(Y_{t+1}\).
- \(Y_{t+1}-\widehat{Y}_{t+1|t}\) is the prediction error.
- \(K_{t+1}\) decides how strongly to correct the predicted state.
- \(\Sigma^{xx}_{t+1|t+1}\) is usually smaller than \(\Sigma^{xx}_{t+1|t}\), because the new observation gives information.

---

## 23. Reconstruction when the observation is missing

If \(Y_{t+1}\) is missing, then there is no new information to update the state.

So we simply keep the prediction:

\[
\widehat{X}_{t+1|t+1}
=
\widehat{X}_{t+1|t}
\]

and

\[
\Sigma^{xx}_{t+1|t+1}
=
\Sigma^{xx}_{t+1|t}
\]

This is one of the major advantages of the Kalman filter:

> It can naturally handle missing observations.

If an observation is missing, we skip the correction step but still continue the prediction step.

This same idea is also used for **multi-step prediction**:

- predict forward,
- do not update with observations,
- repeat until the desired forecast horizon is reached.

---

## 24. ARMA\((p,q)\) model

An ARMA\((p,q)\) model can be written as:

\[
Y_t
+
\phi_1Y_{t-1}
+
\cdots
+
\phi_pY_{t-p}
=
\varepsilon_t
+
\theta_1\varepsilon_{t-1}
+
\cdots
+
\theta_q\varepsilon_{t-q}
\]

where:

- \(p\) is the autoregressive order,
- \(q\) is the moving-average order,
- \(\phi_1,\dots,\phi_p\) are AR parameters,
- \(\theta_1,
\dots,\theta_q\) are MA parameters,
- \(\varepsilon_t\) is white noise.

The key point from the slide is:

> An ARMA model can be rewritten as a state-space model.

Once it is in state-space form, we can use the Kalman filter for prediction, reconstruction, missing-data handling, and likelihood evaluation.

---

## 25. ARMA\((p,q)\) as a state-space model

The state-space form is:

\[
X_t
=
AX_{t-1}+G\varepsilon_t
\]

\[
Y_t
=
CX_t
\]

For the ARMA model, define:

\[
d
=
\max(p,q+1)
\]

Then one possible state-space representation is:

\[
X_t
=
\begin{bmatrix}
-\phi_1 & 1 & 0 & \cdots & 0 \\
-\phi_2 & 0 & 1 & \cdots & 0 \\
\vdots & \vdots & \vdots & \ddots & \vdots \\
-\phi_{d-1} & 0 & 0 & \cdots & 1 \\
-\phi_d & 0 & 0 & \cdots & 0
\end{bmatrix}
X_{t-1}
+
\begin{bmatrix}
1 \\
\theta_1 \\
\vdots \\
\theta_{d-1}
\end{bmatrix}
\varepsilon_t
\]

and

\[
Y_t
=
\begin{bmatrix}
1 & 0 & \cdots & 0
\end{bmatrix}
X_t
\]

Any extra AR or MA parameters beyond the model order are fixed to zero.

So if \(d>p\), then:

\[
\phi_{p+1}=\cdots=\phi_d=0
\]

and if \(d-1>q\), then:

\[
\theta_{q+1}=\cdots=\theta_{d-1}=0
\]

The matrix row \(i\) describes how \(Y_t\) influences future values such as \(Y_{t+i}\).

---

## 26. Why write ARMA models in state-space form?

The main advantage is:

> Once the ARMA model is written as a state-space model, it can be plugged directly into the Kalman filter framework.

That gives us:

- one-step-ahead predictions,
- prediction variances,
- likelihood evaluation,
- maximum likelihood parameter estimation,
- missing-observation handling,
- multi-step forecasting.

This is especially useful because standard ARMA likelihood calculations become harder when observations are missing.

The Kalman filter solves this by simply skipping the update step at missing observations.

---

## 27. Estimation in ARMA\((p,q)\) models using the Kalman filter

For ARMA models, the Kalman filter provides the mean and variance of the one-step predictions of the observations:

\[
\widehat{Y}_{t+1|t}
=
C\widehat{X}_{t+1|t}
\]

\[
\Sigma^{yy}_{t+1|t}
=
C\Sigma^{xx}_{t+1|t}C^T+\Sigma_2
\]

Then the likelihood can be built from:

\[
Y_{t+1}\mid \mathcal{Y}_t
\sim
\mathcal{N}
\left(
\widehat{Y}_{t+1|t},
\Sigma^{yy}_{t+1|t}
\right)
\]

Therefore:

> The Kalman filter gives a way to compute maximum likelihood estimates in ARMA\((p,q)\) models, even when some observations are missing.

---

## 28. Formulating state-space models

When formulating a state-space model, one option is to let all parameters be free and estimate them directly.

For example, for a two-dimensional state:

\[
A
=
\begin{bmatrix}
a_{11} & a_{12} \\
a_{21} & a_{22}
\end{bmatrix}
\]

If we estimate every entry of every matrix freely, the number of parameters can grow quickly.

The slide emphasizes that this scales roughly **quadratically** with system size.

For a state vector of dimension \(n\), a completely free \(A\) matrix has:

\[
n^2
\]

parameters.

This can become expensive, unstable, and hard to interpret.

---

## 29. Using physical equations to reduce the number of parameters

State-space models are often used to describe physical systems.

Instead of estimating every matrix entry freely, we can derive the structure of the model from known physical equations, such as ODEs.

This gives models that are usually:

- more interpretable,
- lower-dimensional,
- easier to estimate,
- better constrained by domain knowledge.

The slide gives the example of a resistor-capacitor temperature model.

---

## 30. Resistor-capacitor temperature model

For a temperature model, define two state variables:

- \(X(t)\): one temperature state,
- \(Z(t)\): another temperature state.

A continuous-time resistor-capacitor model can be written as:

\[
\dot{X}(t)
=
\frac{1}{C_x}
\left(
\frac{1}{R_{xz}}(Z(t)-X(t))
+
a u_1(t)
\right)
\]

\[
\dot{Z}(t)
=
\frac{1}{C_z}
\left(
\frac{1}{R_{xz}}(X(t)-Z(t))
+
\frac{1}{R_{za}}(u_2(t)-Z(t))
+
bu_3(t)
\right)
\]

The unknown physical parameters are:

\[
C_x,\ C_z,\ R_{za},\ R_{xz},\ a,\ b
\]

So the number of parameters is:

\[
6
\]

For an equivalent system with fully free parameters and:

\[
\dim(X_t)=2,
\qquad
\dim(U_t)=3
\]

there would be about:

\[
10
\]

free parameters.

So the physical model reduces the number of parameters and gives them physical meaning.

---

## 31. Continuous-time versus discrete-time state-space models

The resistor-capacitor model is written in continuous time using differential equations.

But the Kalman filter in these slides is written for discrete time:

\[
X_{t+1}=AX_t+G\varepsilon_t
\]

\[
Y_t=CX_t+e_t
\]

So before applying the Kalman filter and maximum likelihood estimation, we need to convert the continuous-time model into a discrete-time model.

The general idea is:

1. Start from a continuous-time ODE model.
2. Discretize it using the sampling interval of the data.
3. Obtain discrete-time matrices such as \(A\), \(G\), and possibly input matrices.
4. Run the Kalman filter.
5. Evaluate the likelihood.
6. Estimate the unknown physical parameters by maximum likelihood.

This is the bridge between physical modeling and statistical estimation.

---


## 32. Discretizing a continuous-time state-space model

The Kalman filter framework used here is a **discrete-time** framework.

So if we start from a continuous-time model, we need to convert it into a discrete-time model before applying the Kalman filter.

A simple example is:

\[
\dot{X}(t)=aX(t)
\]

The solution is:

\[
X(t)=x_0e^{at}
\]

If observations are separated by a time interval \(\Delta t\), then:

\[
X_{t+1}=e^{a\Delta t}X_t
\]

So the discrete-time transition coefficient is:

\[
A_d=e^{a\Delta t}
\]

For a multivariate system, the same idea becomes a **matrix exponential**:

\[
A_d=e^{A_c\Delta t}
\]

where:

- \(A_c\) is the continuous-time system matrix,
- \(A_d\) is the discrete-time system matrix,
- \(\Delta t\) is the sampling interval.

---

## 33. Discretizing the input matrix

If the continuous-time model also has inputs, we need to discretize both the system matrix and the input matrix.

For a continuous-time model of the form:

\[
\dot{X}(t)=A_cX(t)+B_cU(t)
\]

we can use the block matrix exponential:

\[
\exp
\left(
\begin{bmatrix}
A_c & B_c \\
0 & 0
\end{bmatrix}
\Delta t
\right)
=
\begin{bmatrix}
\Phi & B_d \\
0 & I
\end{bmatrix}
\]

Here:

\[
A_d=\Phi
\]

and \(B_d\) is the corresponding discrete-time input matrix.

This gives the discrete-time state-space model:

\[
X_{t+1}=A_dX_t+B_dU_t
\]

where:

\[
A_d=A_d(\theta),
\qquad
B_d=B_d(\theta)
\]

The important point is that the discrete-time matrices are functions of the continuous-time parameters \(\theta\).

So the workflow is:

1. write down the continuous-time physical model,
2. discretize it using the sampling interval \(\Delta t\),
3. get \(A_d\) and \(B_d\),
4. plug the resulting discrete-time model into the Kalman filter,
5. estimate the unknown parameters by maximum likelihood.

This is how continuous-time ODE models can be used inside the discrete-time Kalman filter framework.

---

## 34. Parameter estimation as state estimation

Sometimes an input or parameter is unknown, uncertain, or changing over time.

The original linear state-space model is:

\[
X_t=AX_{t-1}+Bu_{t-1}+e_{1,t}
\]

\[
Y_t=CX_t+e_{2,t}
\]

If \(u_t\) is unknown, we can estimate it as part of an **augmented state vector**.

Define the augmented state:

\[
Z_t=
\begin{bmatrix}
X_t \\
u_t
\end{bmatrix}
\]

Then the state equation can be written as:

\[
\begin{bmatrix}
X_t \\
u_t
\end{bmatrix}
=
\begin{bmatrix}
A & B \\
0 & I
\end{bmatrix}
\begin{bmatrix}
X_{t-1} \\
u_{t-1}
\end{bmatrix}
+
e_{1,t}
\]

The observation equation can be written as:

\[
Y_t=
\begin{bmatrix}
C & 0
\end{bmatrix}
\begin{bmatrix}
X_t \\
u_t
\end{bmatrix}
+
e_{2,t}
\]

If both the original observation and the unknown input are observed or partially observed, the observation equation can also be written as:

\[
Y_t=
\begin{bmatrix}
C & 0 \\
0 & I
\end{bmatrix}
\begin{bmatrix}
X_t \\
u_t
\end{bmatrix}
+
e_{2,t}
\]

Interpretation:

- The unknown input \(u_t\) is treated like another hidden state variable.
- The Kalman filter estimates \(X_t\) and \(u_t\) together.
- As new observations arrive, the estimate of \(u_t\) is updated.

This is useful when:

- inputs are unknown,
- inputs are uncertain,
- parameters may vary over time,
- we want the model to update parameter estimates as more information becomes available.

The key idea is:

> If something unknown affects the system dynamics, we can sometimes include it in the state vector and let the Kalman filter estimate it recursively.

---

## 35. Updated complete Kalman filter + MLE workflow

The full workflow from the slides is:

1. **Choose or formulate a state-space model**

   This can come from:

   - a general statistical model,
   - an ARMA\((p,q)\) model,
   - a physical ODE model.

   If the model is continuous-time, first convert it to discrete time:

   \[
   A_d=e^{A_c\Delta t}
   \]

   and, when inputs are present, compute the corresponding \(B_d\) using the block matrix exponential.

2. **Initialize the Kalman filter**

   Choose:

   \[
   \widehat{X}_{1|1},
   \qquad
   \Sigma^{xx}_{1|1}
   \]

3. **Predict the next state**

   \[
   \widehat{X}_{t+1|t}=A\widehat{X}_{t|t}
   \]

   \[
   \Sigma^{xx}_{t+1|t}
   =
   A\Sigma^{xx}_{t|t}A^T+G\Sigma_1G^T
   \]

4. **Predict the next observation**

   \[
   \widehat{Y}_{t+1|t}=C\widehat{X}_{t+1|t}
   \]

   \[
   \Sigma^{yy}_{t+1|t}
   =
   C\Sigma^{xx}_{t+1|t}C^T+\Sigma_2
   \]

5. **Compute the innovation**

   \[
   \widetilde{Y}_{t+1|t}
   =
   Y_{t+1}-\widehat{Y}_{t+1|t}
   \]

6. **Add the Gaussian log-likelihood contribution**

   \[
   -\frac{1}{2}
   \left(
   \log\det\left(\Sigma^{yy}_{t+1|t}\right)
   +
   \widetilde{Y}_{t+1|t}^T
   \left(\Sigma^{yy}_{t+1|t}\right)^{-1}
   \widetilde{Y}_{t+1|t}
   \right)
   \]

   plus constants.

7. **If the observation is available, update the state**

   \[
   \widehat{X}_{t+1|t+1}
   =
   \widehat{X}_{t+1|t}
   +
   K_{t+1}
   \left(
   Y_{t+1}-\widehat{Y}_{t+1|t}
   \right)
   \]

8. **If the observation is missing, skip the update**

   \[
   \widehat{X}_{t+1|t+1}
   =
   \widehat{X}_{t+1|t}
   \]

   \[
   \Sigma^{xx}_{t+1|t+1}
   =
   \Sigma^{xx}_{t+1|t}
   \]

9. **If an input or parameter is unknown, consider augmenting the state**

   \[
   Z_t=
   \begin{bmatrix}
   X_t \\
   u_t
   \end{bmatrix}
   \]

   and estimate it recursively using the Kalman filter.

10. **Repeat through the whole time series**

11. **Maximize the total log-likelihood over \(\theta\)**

---

## 36. Extended exam summary

The Kalman filter is useful for MLE because it produces the Gaussian one-step-ahead predictive densities needed for the likelihood. At each time step, the filter predicts \(\widehat{Y}_{t|t-1}\) and \(\Sigma^{yy}_{t|t-1}\), computes the innovation \(Y_t-\widehat{Y}_{t|t-1}\), and adds the corresponding Gaussian log-density to the log-likelihood. If an observation is available, the state is updated using the Kalman gain. If an observation is missing, the update step is skipped and the prediction becomes the current estimate. ARMA\((p,q)\) models and physical ODE-based models can both be written in state-space form, which allows the same Kalman filter and maximum likelihood machinery to be used for prediction, reconstruction, missing data, and parameter estimation. Continuous-time physical models can be converted to discrete time using matrix exponentials, and unknown or time-varying inputs can sometimes be included in an augmented state vector and estimated recursively.
