\begin{code}[hide]
module twin-primes where

open import Data.Nat renaming (_+_ to _∔_)
open import Data.Product using (Σ; _×_; _,_; proj₁; proj₂; ∃; Σ-syntax; ∃-syntax)
open import Data.Sum renaming (_⊎_ to _+_)
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; trans; sym; cong; cong-app; subst)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)

\end{code}

\begin{code}
is-prime : ℕ → Set
is-prime n =
  (n ≥ 2) ×
  ((x y : ℕ) → x * y ≡ n → (x ≡ 1) + (x ≡ n))

twin-prime-conjecture : Set
twin-prime-conjecture = (n : ℕ) → Σ[ p ∈ ℕ ] (p ≥ n)
  × is-prime p
  × is-prime (p ∔ 2)

variable
  A : Set
  D : Set
  stuff : Set
  -- definition-body : Set
  -- T : Set

definition-body = ℕ

T = ℕ
L = ℕ
E = ℕ

proof : T
proof = zero

postulate
  axiom : A -- Axiom

definition : stuff → Set
definition s = definition-body

theorem : T     -- Theorem Statement
theorem = proof -- Proof

lemma : L
lemma = proof

example : E
example = proof

\end{code}

