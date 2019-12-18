# Chapter 14

- Testing
    - `Hspec` and `QuickCheck`
    - Unit testing: Smallest atomic units of software independently from each
      other
    - Spec testing: tests when given declared input, result of the operation
      will be equal to the desired result. `Hspec`
    - Property testing: Tests formal properties of programs without requiring
      formal proofs by allowing you to express a truth-valued, universally
      quantified function which can be checked against randomly generated
      inputs. `QuickCheck`
        - If it passes, you can't be positive it will never fail because data is
          randomly generated (monte carlo assurance).
        - Not appropriate for all programs, e.g. no assertable, truth-valued
          properties of software.
