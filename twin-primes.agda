module twin-primes where


open import Data.Nat renaming (_+_ to _∔_)
open import Data.Product using (Σ; _×_; _,_; proj₁; proj₂; ∃; Σ-syntax; ∃-syntax)
open import Data.Sum renaming (_⊎_ to _+_)
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; trans; sym; cong; cong-app; subst)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
-- open import Data.Empty using (⊥; ⊥-elim)
-- open import Data.Empty

is-prime : ℕ → Set
is-prime n = (n ≥ 2) × ((x y : ℕ) → x * y ≡ n → (x ≡ 1) + (x ≡ n))

twin-prime-conjecture : Set
twin-prime-conjecture = (n : ℕ) → Σ[ p ∈ ℕ ] (p ≥ n)
  × is-prime p
  × is-prime (p ∔ 2)


-- -- -- proof

-- a : 3 ≡ 1 → ⊥
-- a ()

-- a' : 3 ≡ 2 → ⊥
-- a' ()

-- lemma1 : ∀ x → suc (suc (suc x)) ≡ 1 → ⊥
-- lemma1 zero = λ x → a x
-- lemma1 (suc x) = λ ()

-- lemma2 : ∀ x → suc (suc (suc x)) ≡ 2 → ⊥
-- lemma2 zero = λ x → a' x
-- lemma2 (suc x) = λ ()

-- lemma3 : {x y : Set} → (x → ⊥) → (y → ⊥) → (x + y) → ⊥
-- lemma3 notx noty (inj₁ x₁) = notx x₁
-- lemma3 notx noty (inj₂ y₁) = noty y₁

-- n*0=suc->⊥ : ∀ x → x * 0 ≡ 0
-- n*0=suc->⊥ zero = refl
-- n*0=suc->⊥ (suc x) = n*0=suc->⊥ x

-- n*0=suc->⊥' : ∀ x y → x * 0 ≡ (suc y) → ⊥
-- n*0=suc->⊥' zero y = λ ()
-- n*0=suc->⊥' (suc x) y = λ x₁ → n*0=suc->⊥' x y x₁

-- -- 2-is-prime : is-prime 2
-- -- 2-is-prime = (s≤s (s≤s z≤n)) , λ x y x*y=2 → helper x y x*y=2
-- --   where
-- --     helper : ∀ x y (x*y=2 : x * y ≡ 2) → x ≡ 1 + x ≡ 2
-- --     helper (suc zero) y x*y=2 = inj₁ refl
-- --     helper (suc (suc zero)) y x*y=2 = inj₂ refl
--   --     helper (suc (suc (suc x))) zero x*y=2 = ⊥-elim (n*0=suc->⊥' x 1 x*y=2 )
-- --     helper (suc (suc (suc x))) (suc y) x*y=2 = {!helper x y x*y=2!}

-- -- helper₁ : ∀ x y (x*y=2 : suc (suc (suc x)) * suc y ≡ 2) → ⊥
-- -- helper₁ x y p  = {!!}
-- -- nested-sucs : ∀ x y → suc ( x ∔ y ) ≥ x

-- -- lemma1 : ⊥
-- -- lemma1 = {!!}

-- -- hasmorethanonesuc



-- -- lemma2 : ∀ x → suc (suc (suc x) ≡ 2 → ⊥


-- -- proof-of-tpc : twin-prime-conjecture
-- -- proof-of-tpc zero = 2 , {!!}
-- -- proof-of-tpc (suc n) = {!!}

-- -- variable names and comments give semantic content to a computer program for humans, and in some sense they are the most important. but these are completely irrelevant for computers, at least modulo alpha equivalence

-- -- A twin prime is a prime number that is either 2 less or 2 more than another prime number

-- -- a twin prime is a prime that has a prime gap of two
-- -- A prime gap is the difference between two successive prime numbers.

-- -- prime vs prime number

-- -- -- could be undecideable
-- -- However, it is unknown whether there are infinitely many twin primes (the so-called twin prime conjecture) or if there is a largest pair

-- in some sense , this problem, this problem of mathematics as a human endeavor, lets say mathematics we want to communicate with aliens, vs mathematics whats happening in some individual, or group of peoples heads collectively are two entirely different problems

-- If a logic is a specification, then a programming language is an implementation of that specification.
-- It tells you what proofs are, but not how to annotate them with terms, and the terms are what are, in some sense, of concern (think of untyped languages).

-- A programming language is a

-- Mathematics as done by mathematicians vs as done by computer scientists (via ITPs).

-- Will they ever be unified?

-- Billy
--   Mathematicians are interested in discovering which problems are interesting.

-- There are infinitely many primes,
-- There are infinitely many groups of prime order

-- (n : ℕ) → Σ[ p ∈ ℕ ] (isofprimeorder (p ≥ n)

-- There are two main approaches, statistical (NLP).

-- Proof discovery vs proof translation.


-- Mathematics
-- Syntax vs semantics

-- Can you train a nn to discover a proof? -- We dont want to answer the question in this work, but we
-- Can you train a nn to convert a proof?

-- pml 84
-- We shall here mainly be interested in mathematical logic in the second sense.
-- What we shall do is also mathematical logic in the first sense, but certainly not
-- in the third.


-- (2) mathematical logic as foundations (or philosophy) of mathematics;

-- foundations and philosophy, as treated by PML, are seen as the same. That in some sense the foundations, philosophy of mathematics is apriori mathematics itself.

-- From this view, the foundations provide a means of doing mathematics.  But these foundations are just some mathematical object, some theory , in some higher order mathematical theory.

-- Meaning explanations give a linguistic intuition to, or justification of the foundations
  
-- While revolutionary, this is, in some sense, contradicted by history.  That is, the historical evolution of mathematics as a discipline was driven by empircal need and observation,
