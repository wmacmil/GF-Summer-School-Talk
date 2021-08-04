\begin{code}[hide]

module i where

open import Agda.Builtin.Nat renaming (Nat to ℕ) public
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; trans; sym; cong; cong-app; subst)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)

variable
  A B : Set
  a a' : A

\end{code}

\begin{code}[hide]
data ℕ' : Set where
  zero' : ℕ'
  suc'  : ℕ' → ℕ'

data _≡'_ {A : Set} (a : A) : A → Set where
  refl' : a ≡' a

two : ℕ'
two = suc' (suc' zero')

double : ℕ' → ℕ'
double zero' = zero'
double (suc' n) = suc' (suc' (double n))

four : ℕ'
four = double two

_+'_ : ℕ' → ℕ' → ℕ'
zero' +' y = y
suc' x +' y = suc' (x +' y)

2+2=4 : (two +' two) ≡ four
2+2=4 = refl

ap : (f : A → B) → a ≡ a' → f a ≡ f a'
ap f refl = refl

associativity-plus : (m n p : ℕ) → ( ( m + n ) + p ) ≡ ( m + ( n + p ) )
associativity-plus zero n p = refl
associativity-plus (suc m) n p = ap suc (associativity-plus m n p)

\end{code}
\begin{code}

natind : {C : ℕ -> Set} ->                -- predicate
         C zero ->                        -- base case
         ((n : ℕ) -> C n -> C (suc n)) -> --IH
         (n : ℕ) -> C n
natind base step zero     = base
natind base step (suc n) = step n (natind base step n)

\end{code}
\begin{code}[hide]

associativity-plus-ind :
  (m n p : ℕ) →
  ((m + n) + p) ≡ (m + (n + p))
associativity-plus-ind m n p =
  natind
    baseCase
    (λ n₁ ih → simpl n₁ (indCase n₁ ih))
    m
  where
    baseCase : (zero + n + p) ≡ (zero + (n + p))
    baseCase = refl
    indCase : (n' : ℕ) → (n' + n + p) ≡ (n' + (n + p)) →
                suc (n' + n + p) ≡ suc (n' + (n + p))
    indCase = (λ n' x → ap suc x )
    simpl : (n' : ℕ)
            → suc (n' + n + p) ≡ suc (n' + (n + p))
            → (suc n' + n + p) ≡ (suc n' + (n + p))
    simpl n' x = x
\end{code}
