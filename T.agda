module T where

open import Agda.Builtin.Nat renaming (Nat to ℕ) public
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; trans; sym; cong; cong-app; subst)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)


-- data _≡_ {A : Set} (a : A) : A → Set where
--   refl : a ≡ a


-- notice X in Godels T is not that interesting, it is inherently just some parenthesized form of (ℕ → ℕ → ... → ℕ)
ℕrec : {X : Set} -> (ℕ -> X -> X) -> X -> ℕ -> X
ℕrec f x zero = x
ℕrec f x (suc n) = f n (ℕrec f x n)

double : ℕ → ℕ
double zero = zero
double (suc n) = suc (suc (double n))


double' : ℕ → ℕ
double' n = ℕrec (λ x y → suc (suc y)) 0 n

_+'_ : ℕ → ℕ → ℕ
n1 +' n2 = ℕrec (λ _ x₁ → suc x₁) n1 n2

+'' = λ n1 n2 → ℕrec (λ _ x₁ → suc x₁) n1 n2

+''' = λ (n1 n2 : ℕ) → ℕrec (λ _ x₁ → suc x₁) n1 n2

_+''''_ : ℕ → ℕ → ℕ
zero +'''' x₁ = x₁
suc x +'''' x₁ = suc (x + x₁)

-- Multiplication a binary function, taking two nats, x and y, and returning a nat.
-- We proceed by cases on the first arguement, x.
-- If x is zero, return zero, i.e. zero times anything is zero.
--   -- If the first arguement is zero, return zero. -- anaphora
-- If x is non-zero, return the sum of y and the product of x and y.
--   -- If x is a successor,  -- alterantive to

_*''_ : ℕ → ℕ → ℕ
zero *'' x₁ = zero
suc x *'' x₁ = x₁ + (x *'' x₁)

_*'_ : ℕ → ℕ → ℕ
_*'_ n1 n2 = ℕrec (λ n3 n4 → n1 +' n4) 0 n2

_^_ : ℕ → ℕ → ℕ
x ^ y = ℕrec (λ x₁ x₂ → x *' x₂) 1 y

_^'_ : ℕ → ℕ → ℕ
x ^' zero = 1
x ^' suc y = x * (x ^' y)

factorial : ℕ → ℕ
factorial zero = 1
factorial (suc x) = (suc x) * (factorial x)

pred : ℕ → ℕ
pred zero = zero
pred (suc n) = n

pred' : ℕ → ℕ
pred' n = ℕrec (λ x x₁ → x) 0 n

factorial' : ℕ → ℕ
factorial' = ℕrec (λ x y → (suc x) * y) 1

id : {A : Set} → A → A
id a = a

-- _∘_ : {A B C : Set} → (B → C) → (A → B) → (A → C)
_∘_ : {A B C : Set} → (B → C) → ((A → B) → (A → C))
(f ∘ g) z = f (g z)
-- f ∘ g = λ z → f (g z)

-- Compose is a function which, given a function from B to C, and given a function from A to B, and given an element of A, returns a C.
-- Compose is a function which, given a function f from B to C, and given a function g from A to B, and given an element of A, returns a C.
-- Compose is has a type as follows : given a function f from B to C, and given a function g from A to B, returns a function from a to c.

-- We define the composition

iter : {X : Set} → (X → X) → X → ℕ → X
iter f x zero = x
iter f x (suc n) = f (iter f x n)

iterℕ : (ℕ → ℕ) → ℕ → ℕ → ℕ
iterℕ f n m = ℕrec (λ _ g → f ∘ g) id n m

-- wikipedia
-- A(0,n) = n + 1
-- A(m,0) = A(m - 1, 1)
-- A(m,n) = A(m - 1,A(m,n-1))

ackerman : ℕ → ℕ → ℕ
ackerman zero y = y + 1
ackerman (suc x) zero = ackerman x 1
ackerman (suc x) (suc y) = ackerman x (ackerman (suc x) y)

ackerm' : ℕ → ℕ → ℕ
ackerm' m =
  ℕrec
    (λ _ f → λ n →
      -- ℕrec (λ _ g → f ∘ g) id n (f 1)
      iterℕ f n (f 1)
      )
    suc
    m

-- Natural language :
-- The Ackerman is defined as a binary function which takes two nats and returns a nat.
-- In the case that the first arguement is zero, we return the successor of the second arguement.
-- Otherwise, if the second arguement is zero, we return the ackerman function applied to the predessecor of the second arguement and 1.
-- Finally, if both arguements are nonzero, we apply the ackerman to the successor of the first arguement and, as the second arguement, the ackerman function itself applied to the first arguement to and the predecessor of the second.


variable
  A B : Set
  a a' : A

ap : (f : A → B) → a ≡ a' → f a ≡ f a'
ap f refl = refl

-- ℕrec : {X : Set} -> (ℕ -> X -> X) -> X -> ℕ -> X, but same program
natind : {C : ℕ -> Set} -> C zero -> ((n : ℕ) -> C n -> C (suc n)) -> (n : ℕ) -> C n
natind base step zero     = base
natind base step (suc n) = step n (natind base step n)

zeroRight' : (p : ℕ) → (p + 0) ≡ p
zeroRight' zero = refl
zeroRight' (suc p) = ap (λ x → suc x) (zeroRight' p)

--note, this is only different from the above program in the motive
zeroRight : (p : ℕ) → (p + 0) ≡ p
zeroRight p = natind {λ x → (x + 0) ≡ x} refl (λ n x → ap suc x) p


-- Theorem: For any n, m and p,
--   n + (m + p) = (n + m) + p.
-- Proof: By induction on n.

--   First, suppose n = 0. We must show that

--     0 + (m + p) = (0 + m) + p.

--   This follows directly from the definition of +.

--   Next, suppose n = S n', where

--     n' + (m + p) = (n' + m) + p.

--   We must now show that

--     (S n') + (m + p) = ((S n') + m) + p.

--   By the definition of +, this follows from

--     S (n' + (m + p)) = S ((n' + m) + p),

--   which is immediate from the induction hypothesis. Qed.

-- via pattern matching
associativity-plus : (m n p : ℕ) → ( ( m + n ) + p ) ≡ ( m + ( n + p ) )
associativity-plus zero n p = refl
associativity-plus (suc m) n p = ap suc (associativity-plus m n p)


-- case m of _ <-> pattern matching
-- parens


associativity-plus-ind : (m n p : ℕ) → ((m + n) + p) ≡ (m + (n + p))
-- associativity-plus-ind m n p = natind {λ n' → ((n' + n) + p) ≡ ((n' + (n + p)))} baseCase indCase m
associativity-plus-ind m n p = natind {λ n' → (n' + n) + p ≡ n' + (n + p)} baseCase indCase m
  where
    baseCase = refl
    indCase = λ (n' : ℕ) (x : n' + n + p ≡ n' + (n + p)) → ap suc x
    -- indCase = (λ n' x → ap suc x )

--without the motive
associativity-plus-ind'' : (m n p : ℕ) → ((m + n) + p) ≡ (m + (n + p))
associativity-plus-ind'' m n p = natind baseCase indCase m
  where
    baseCase : (zero + n + p) ≡ (zero + (n + p))
    baseCase = refl
    indCase : ∀ (n' : ℕ)
              → (n' + n + p) ≡ (n' + (n + p))
              → suc (n' + n + p) ≡ suc (n' + (n + p))
    indCase = (λ n' x → ap suc x )

--   Next, suppose n = S n', where

--     n' + (m + p) = (n' + m) + p.

--   We must now show that

--     (S n') + (m + p) = ((S n') + m) + p.

--   By the definition of +, this follows from

--     S (n' + (m + p)) = S ((n' + m) + p),


-- to mirror the coq (and NL proof)
associativity-plus-ind' : (m n p : ℕ) → ((m + n) + p) ≡ (m + (n + p))
associativity-plus-ind' m n p = natind baseCase (λ n₁ ih → simpl n₁ (indCase n₁ ih)) m
  where
    baseCase : (zero + n + p) ≡ (zero + (n + p))
    baseCase = refl
    indCase : (n' : ℕ) → (n' + n + p) ≡ (n' + (n + p)) →
                suc (n' + n + p) ≡ suc (n' + (n + p))
    indCase = (λ n' x → ap suc x )
    -- definitional equality, Coq has simpl tactic, hence the name
    simpl : (n' : ℕ)
            → suc (n' + n + p) ≡ suc (n' + (n + p))
            → (suc n' + n + p) ≡ (suc n' + (n + p))
    simpl n' x = x

-- notice, coq infers the motive
-- coq proof

-- Theorem plus_assoc'' : ∀ n m p : nat,
--   n + (m + p) = (n + m) + p.
-- Proof.
--   intros n m p. induction n as [| n' IHn'].
--   - (* n = 0 *)
--     reflexivity.
--   - (* n = S n' *)
--     simpl. rewrite → IHn'. reflexivity. Qed.

postulate
  +-identity : ∀ (m : ℕ) → m + zero ≡ m -- identity cancels on the left
  +-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n) -- successor and addition commute

--idea : can we not have propositional equality theorems baked into - definitional equalities, some kind of sledgehammer tactic

+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero =
  begin
    m + zero
  ≡⟨ +-identity m ⟩
  -- ≡⟨ zeroRight' m ⟩ -- my defn
    m
  ≡⟨⟩
    zero + m
  ∎
+-comm m (suc n) = begin
    m + suc n   ≡⟨ +-suc m n ⟩
    suc (m + n) ≡⟨ cong suc (+-comm m n) ⟩
    suc (n + m) ≡⟨⟩ --defntl, can cancel
    suc n + m   ∎

doublePlus : ℕ → ℕ
doublePlus n = (λ x → x + 3) 3 + double (ℕrec _+_ n n)


-- (λ x → x + 3) 3

-- _∘_ : (A B C : Set) → (A → B) → (B → C) → (A → C)
-- _∘_ = ?
