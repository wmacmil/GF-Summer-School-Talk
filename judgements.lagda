\begin{code}[hide]

data ℕ : Set where
 zero : ℕ

variable
  A : Set
  D : Set
  stuff : Set

definition-body = ℕ

T = ℕ
L = ℕ
E = ℕ

proof : T
proof = zero


\end{code}

\begin{code}

postulate   -- Axiom
  axiom : A

definition : stuff → Set
definition s = definition-body

theorem : T     -- Theorem Statement
theorem = proof -- Proof

lemma : L     -- Lemma Statement
lemma = proof

example : E     -- Example Statement
example = proof

\end{code}
