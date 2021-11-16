Chapter 1: Naturals

Experimenting with holes 

\begin{code}
module Naturals where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ  

{-# BUILTIN NATURAL ℕ #-}

_+_ : ℕ → ℕ → ℕ
zero + n = n
(suc m) + n = suc (m + n)

_*_ : ℕ → ℕ → ℕ
zero * n = zero
(suc m) * n = n + (m * n)

_∸_ : ℕ → ℕ → ℕ
m     ∸ zero   =  m
zero  ∸ suc n  =  zero
suc m ∸ suc n  =  m ∸ n


{-# BUILTIN NATPLUS _+_ #-}
{-# BUILTIN NATTIMES _*_ #-}
{-# BUILTIN NATMINUS _∸_ #-}

\end{code}

\begin{code}
_ : 2 + 3 ≡ 5
_ =
  begin
    2 + 3
  ≡⟨⟩    -- is shorthand for
    (suc (suc zero)) + (suc (suc (suc zero)))
  ≡⟨⟩    -- inductive case
    suc ((suc zero) + (suc (suc (suc zero))))
  ≡⟨⟩    -- inductive case
    suc (suc (zero + (suc (suc (suc zero)))))
  ≡⟨⟩    -- base case
    suc (suc (suc (suc (suc zero))))
  ≡⟨⟩    -- is longhand for
    5
  ∎
\end{code}

Note that pragmas are declared after symbols are defined.

The binary number representation exercise...

  () O = 0
  () I = 1         
  () I O = 2
v  () 1 1 = 3...

Pattern matching on the "end" (postfix operator) seems odd..? But it works:
- goal is to flip a 0 to 1 (obvious increment by 1)
- if we find a 1 instead, flip it to 0 and look for the next 1 to flip
- if we get to the end (nil), we haven't seen a O; so we've flipped all the 1s and need to return ⟨⟩ 1

\begin{code}
data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

inc : Bin → Bin
inc ⟨⟩ = ⟨⟩ I
inc (m I) = (inc m) O
inc (m O) = m I

_ : inc (⟨⟩ O) ≡ ⟨⟩ I
_ = refl

_ : inc (⟨⟩ I) ≡ ⟨⟩ I O
_ = refl

_ : inc (⟨⟩ I O) ≡ ⟨⟩ I I
_ = refl

_ : inc (⟨⟩ I I) ≡ ⟨⟩ I O O
_ = refl

_ : inc (⟨⟩ I O I I) ≡ ⟨⟩ I I O O
_ = refl

\end{code}

Numbers <> Bitstring representation 

Using double...
  ⟨⟩ O ≡ 0 ≡ base case
  ⟨⟩ I ≡ 1 ≡ base case ?   
  ⟨⟩ I O ≡ 2 ≡
  ⟨⟩ I I ≡ 3 ≡
  ⟨⟩ I O O ≡ 4 ≡
  ⟨⟩ I O I ≡ 5 ≡ 

\begin{code}
toBin : ℕ → Bin
toBin zero = ⟨⟩ O
toBin (suc m) = inc (toBin m)

_ : toBin 0 ≡ ⟨⟩ O
_ = refl

_ : toBin 1 ≡ ⟨⟩ I
_ = refl

_ : toBin 8 ≡ ⟨⟩ I O O O
_ = refl

dbl : ℕ → ℕ
dbl zero = zero
dbl (suc m) = suc (suc (dbl m))

fromBin : Bin → ℕ
fromBin ⟨⟩ = zero
fromBin (m O) = dbl (fromBin m)
fromBin (m I) = suc (dbl (fromBin m)) 

-- _ : toBin ≡ fromBin

\end{code}
