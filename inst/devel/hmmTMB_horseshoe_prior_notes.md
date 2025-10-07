# Overview of `src/hmmTMB.hpp`

This note summarises the structure of the Template Model Builder objective used by **hmmTMB** and highlights the locations that will need to change when replacing the current normal priors with a horseshoe hierarchy for MCMC fitting.

## File layout and existing responsibilities

1. **Header and includes (lines 1–11)**  
   The file guards and includes (`<TMB.hpp>`, `dist.hpp`, plus standard headers) set up the compilation environment for the TMB objective.

2. **`objective_function<Type>::operator()` (lines 13–304)**  
   The entire likelihood and penalty computation happens inside this method. The code is laid out in logical blocks that mirror the model components.

   * **DATA block (lines 16–39)**  
     All inputs passed from R are declared here. Of interest for prior work are the four matrices named `*_prior`, each of which currently contains a pair of Gaussian mean/standard deviation values for the priors.

   * **PARAMETER block (lines 41–49)**  
     Working-scale parameters for fixed effects, random effects, smoothing penalties, and the initial distribution are declared. These are the quantities the prior information targets.

   * **Parameter transforms (lines 55–110)**  
     The code builds design-matrix products for the observation and hidden processes, then reshapes them into matrices convenient for the distribution code. Transition probability matrices are exponentiated and normalised here as well.

   * **Initial-state handling (lines 112–155)**  
     Depending on whether the stationary distribution is enforced, the code either solves for it or exponentiates free parameters to create `delta0`.

   * **Observation likelihood (lines 157–213)**  
     For each observation distribution the code generates a `Dist` instance, extracts the relevant working parameters, transforms them using `invlink`, and multiplies state-specific densities into the forward probabilities.

   * **Prior block (lines 217–247)**  
     The normal priors are accumulated here. Each loop calls `dnorm` on the relevant working-scale parameter whenever a finite mean/sd pair is supplied. This is the only place where prior densities are currently applied.

   * **Hidden Markov likelihood (lines 249–279)**  
     The forward algorithm computes the log-likelihood contribution of the state process and adds it to `llk`.

   * **Smoothing penalties (lines 281–328)**  
     If requested, Gaussian Markov random field penalties are added for the spline components of both the observation and hidden processes.

   * **Return (line 330)**  
     The function returns the accumulated negative log-likelihood plus penalties.

## Where horseshoe logic must be introduced

Implementing a horseshoe prior requires changes in three spots:

1. **DATA block additions**  
   New `DATA_*` declarations should capture any fixed hyperparameters (e.g., slab scales) or flags describing which coefficients receive horseshoe shrinkage. These go alongside the existing `*_prior` matrices.

2. **PARAMETER block additions**  
   The horseshoe hierarchy introduces global and local scale parameters. Declare them here (most likely on the log scale) so that they are jointly estimated with the existing coefficient vectors.

3. **Prior block rewrite**  
   Replace the loops that call `dnorm` with code that evaluates:
   * the conditional normal prior for each coefficient using the global/local scales, and
   * the half-Cauchy (or alternative) priors for both scale levels.

   This block is also where you would branch between the legacy Gaussian prior (when no horseshoe inputs are provided) and the new hierarchy.

No other sections of the file need structural edits for a horseshoe prior, but the new parameters and data will flow through the rest of the function automatically once declared here.

