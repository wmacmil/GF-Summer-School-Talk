
\begin{code}[hide]
{-# OPTIONS --omega-in-omega --type-in-type #-}

module contr where

open import Agda.Builtin.Sigma public

variable
  A B : Set

data _≡_ {A : Set} (a : A) : A → Set where
  r : a ≡ a

infix 20 _≡_

id : A → A
id = λ z → z


\end{code}

\begin{code}

isContr : (A : Set) → Set
isContr A =  Σ A λ a → (x : A) → (a ≡ x)


\end{code}
