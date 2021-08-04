
\begin{code}[hide]

data _≡_ {A : Set} (a : A) : A → Set where
  r : a ≡ a

infix 20 _≡_

\end{code}

\begin{code}
J : {A : Set}
    → (D : (x y : A) → (x ≡ y) →  Set)
    → ((a : A) → (D a a r ))
    → (x y : A)
    → (p : x ≡ y)
    ------------------------------------
    → D x y p
J D d x .x r = d x

_⁻¹ : {A : Set} {x y : A} → x ≡ y → y ≡ x
_⁻¹ {A} {x} {y} p = J D d x y p
  where
    D : (x y : A) → x ≡ y → Set
    D x y p = y ≡ x
    d : (a : A) → D a a r
    d a = r
\end{code}
