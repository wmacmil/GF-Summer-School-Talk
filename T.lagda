
\begin{code}[hide]

module T where

\end{code}

\begin{code}

-- data Nat : Set where
--   zero : Nat
--   suc  : Nat → Nat

-- open import Data.Nat --using (ℕ ; zero ; suc )
open import Agda.Builtin.Nat renaming (Nat to ℕ) public


data Nat2 : Set where
  zer : Nat2
  succ : Nat2 -> Nat2

natrec : {X : Set} -> (Nat2 -> X -> X) -> X -> Nat2 -> X
natrec f x zer = x
natrec f x (succ n) = f n (natrec f x n)

double : Nat2 -> Nat2
double zer = zer
double (succ n) = succ (succ (double n))

doubleNR : Nat2 -> Nat2
doubleNR n = natrec (λ n₁ x → succ (succ x)) zer n

plus : Nat2 -> Nat2 -> Nat2
plus n1 n2 = natrec (λ x x₁ → succ x₁) n1 n2

two = succ (succ zer)

four1 = double two
four2 = doubleNR two

eight = doubleNR four2

twelve = plus four2 eight

evalNat : Nat2 → ℕ
evalNat zer = 0
evalNat (succ x) = suc (evalNat x)

-- +'''' = λ x y → x + (evalNat y)

evalℕ : ℕ → Nat2
evalℕ zero = zer
evalℕ (suc n) = succ (evalℕ n)


12? = evalNat twelve



data _≡_ {A : Set} (a : A) : A → Set where
  refl : a ≡ a

postulate
  extn : {A B : Set} (f g : A → B) (x : A) → (f x) ≡ (g x) → f ≡ g


ℕrec : {X : Set} -> (ℕ -> X -> X) -> X -> ℕ -> X
ℕrec f x zero = x
ℕrec f x (suc n) = f n (ℕrec f x n)

double' : ℕ → ℕ
double' n = ℕrec (λ x y → suc (suc y)) 0 n

aa : (x : ℕ) (x₁ : Nat2) → Nat2
aa = λ (x : ℕ) (y : Nat2) → plus (evalℕ x) y

_+'_ : ℕ → ℕ → ℕ
n1 +' n2 = ℕrec (λ _ x₁ → suc x₁) n1 n2

+''' = λ n1 n2 → ℕrec (λ _ x₁ → suc x₁) n1 n2

_*''_ : ℕ → ℕ → ℕ
zero *'' x₁ = zero
suc zero *'' x₁ = x₁
suc (suc x) *'' x₁ = x₁ + (suc x *'' x₁)

f = λ ( x : ℕ ) → x + 3

_*'_ : ℕ → ℕ → ℕ
_*'_ n1 n2 = ℕrec (λ n3 n4 → n1 +' n4) 0 n2

_^''_ : ℕ → ℕ → ℕ
x ^'' zero = 1
x ^'' suc y = x * (x ^'' y)

_^'_ : ℕ → ℕ → ℕ
x ^' y = ℕrec (λ x₁ x₂ → x *' x₂) 1 y

variable
  A B : Set
  a a' : A

ap : (f : A → B) → a ≡ a' → f a ≡ f a'
ap f refl = refl

zsz : (y : ℕ) → (zero + suc y) ≡ suc y
zsz zero = refl
zsz (suc y) = ap suc refl

natind : {C : ℕ -> Set} -> C zero -> ((n : ℕ) -> C n -> C (suc n)) -> (n : ℕ) -> C n
natind base step zero     = base
natind base step (suc n) = step n (natind base step n)

associativity-plus-ind : (m n p : ℕ) → ((m + n) + p) ≡ (m + (n + p))
associativity-plus-ind m n p = natind {λ n' → ((n' + n) + p) ≡ ((n' + (n + p)))} refl (λ n₁ x → ap suc x) m

-- associativity-plus-ind : (m n p : ℕ) → ((m + n) + p) ≡ (m + (n + p))
-- associativity-plus-ind zero n p = refl
-- associativity-plus-ind (suc m) n p = ap suc (associativity-plus-ind m n p)

-- proving associativity using the induction axiom

-- associativity-plus-ind : (m n p : Nat) → I Nat ((m + n) + p) (m + (n + p))
-- associativity-plus-ind m n p = natind {λ p → I Nat ((m + n) + p) (m + (n + p))} refl ((λ q r → cong succ r))  p


-- asdf : Set -- (A : Set) (B : Set)


-- zerosuc=zero : ∀ (y : ℕ) → (zero +' suc y) ≡ suc y
-- zerosuc=zero y = ℕrec (λ x x₁ → {!!}) {!!} {!!}

-- plus= : (x y : ℕ) → (x + y) ≡  (x +' y)
-- plus= zero zero = {!extn!}
-- plus= zero (suc y) = {!!}
-- plus= (suc x) zero = {!!}
-- plus= (suc x) (suc y) = {!!}

\end{code}
