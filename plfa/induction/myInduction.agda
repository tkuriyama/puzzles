import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_)

+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc zero n p =
  begin
    (zero + n) + p
  ≡⟨⟩
    n + p
  ≡⟨⟩
    zero + (n + p)
    ∎ 
+-assoc (suc m) n p =
  begin
    (suc m + n) + p
  ≡⟨⟩
    suc (m + n) + p
  ≡⟨⟩
    suc ((m + n) + p)
  ≡⟨ cong suc (+-assoc m n p) ⟩
    suc (m + (n + p))
  ≡⟨⟩
    suc m + (n + p)
  ∎


+-identityʳ : ∀ (m : ℕ) → m + zero ≡ m
+-identityʳ zero =
  begin
    zero + zero
  ≡⟨⟩
    zero
  ∎
+-identityʳ (suc m) =
  begin
    suc m + zero
  ≡⟨⟩
    suc (m + zero)
  ≡⟨ cong suc (+-identityʳ m) ⟩
    suc m
  ∎

+-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc zero n =
  begin
    zero + suc n
  ≡⟨⟩
    suc n
  ≡⟨⟩
    suc (zero + n)
  ∎
+-suc (suc m) n =
  begin
    suc m + suc n
  ≡⟨⟩
    suc (m + suc n)
  ≡⟨ cong suc (+-suc m n) ⟩
    suc (suc (m + n))
  ≡⟨⟩
    suc (suc m + n)
  ∎

+-rearrange : ∀ (m n p q : ℕ) → (m + n) + (p + q) ≡ m + (n + p) + q
+-rearrange m n p q =
  begin
    (m + n) + (p + q)
  ≡⟨ +-assoc m n (p + q) ⟩
    m + (n + (p + q))
  ≡⟨ cong (m +_) (sym (+-assoc n p q)) ⟩
    m + ((n + p) + q)
  ≡⟨ sym (+-assoc m (n + p) q) ⟩
    (m + (n + p)) + q
  ∎

+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero =
  begin
    m + zero
  ≡⟨ +-identityʳ m ⟩
    m
  ≡⟨⟩
    zero + m
  ∎
+-comm m (suc n) =
  begin
    m + suc n
  ≡⟨ +-suc m n ⟩
    suc (m + n)
  ≡⟨ cong suc (+-comm m n) ⟩
    suc (n + m)
  ≡⟨⟩
    suc n + m
  ∎

-- Practice using interactivity
+-assoc' : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc' zero n p = refl
+-assoc' (suc m) n p rewrite +-assoc' m n p = refl



-- Exercise finite-+-assoc (stretch)
-- Write out what is known about associativity of addition on each of the first four days using a finite story of creation, as earlier.

{-
On the first day... 
(0 + 0) + 0 ≡ 0 + (0 + 0)

On the second day...
(0 + 0) + 0 ≡ 0 + (0 + 0)

(0 + 1) + 0 ≡ 0 + (1 + 0)
(1 + 0) + 0 ≡ 1 + (0 + 0)
(0 + 0) + 1 ≡ 0 + (0 + 1)

etc
-}


-- Exercise +-swap (recommended)
-- Show
-- m + (n + p) ≡ n + (m + p)
-- for all naturals m, n, and p. No induction is needed, just apply the previous results which show addition is associative and commutative.
*-swap : ∀ (m n p : ℕ) → m + (n + p) ≡ n + (m + p)
*-swap m n p =
  begin
    m + (n + p)
    ≡⟨ sym (+-assoc m n p) ⟩
    m + n + p
    ≡⟨ cong (_+ p) (+-comm m n) ⟩
    n + m + p
    ≡⟨ +-assoc n m p ⟩
    n + (m + p)
    ∎ 

-- Exercise *-distrib-+ (recommended
-- Show multiplication distributes over addition, that is,
-- (m + n) * p ≡ m * p + n * p
-- for all naturals m, n, and p.

-- +-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
-- ((suc m) + n) * p ≡ (suc m) * p + (suc n) * p
-- suc (m + n) * p ≡ suc m * p + suc n * p
-- (p + (m*p)) * (n*p) ≡ p + ((m*p) + (n*p))

*-distrib-+ : ∀ (m n p : ℕ) → (m + n) * p ≡ m * p + n * p
*-distrib-+ zero n p     = refl
*-distrib-+ (suc m) n p 
  rewrite *-distrib-+ m n p | +-assoc p (m * p) (n * p) = refl

-- Exercise *-assoc (recommended)
-- Show multiplication is associative, that is,
-- (m * n) * p ≡ m * (n * p)
-- for all naturals m, n, and p.

*-assoc : ∀ (m n p : ℕ) → (m * n) * p ≡ m * (n * p)
*-assoc zero n p = refl
*-assoc (suc m) n p rewrite *-distrib-+ n (m * n) p | *-assoc m n p = refl

-- Exercise *-comm (practice)
-- Show multiplication is commutative, that is,
-- m * n ≡ n * m
-- for all naturals m and n. As with commutativity of addition, you will need to formulate and prove suitable lemmas.

*-null : ∀ (m : ℕ) → m * 0 ≡ 0
*-null zero = refl
*-null (suc m) rewrite *-null m = refl

*-suc' : ∀ (m n : ℕ) → m * suc n ≡ m + m * n
*-suc' zero n rewrite *-null n = refl
*-suc' (suc m) n rewrite *-suc' m n | *-swap m n (m * n) = refl

*-comm : ∀ (m n : ℕ) →  m * n ≡ n * m
*-comm m zero = *-null m
*-comm m (suc n) rewrite *-suc' m n | *-comm m n = refl 

-- Exercise 0∸n≡0 (practice)
-- Show
-- zero ∸ n ≡ zero
-- for all naturals n. Did your proof require induction?

zero-sub : (n : ℕ) → zero ∸ n ≡ zero
zero-sub zero = refl
zero-sub (suc n) = refl

-- Exercise ∸-+-assoc (practice)
-- Show that monus associates with addition, that is,
-- m ∸ n ∸ p ≡ m ∸ (n + p)
-- for all naturals m, n, and p.

∸-assoc-+ : (m n p : ℕ) → m ∸ n ∸ p ≡ m ∸ (n + p)
∸-assoc-+ m zero p  = refl
∸-assoc-+ zero (suc n) p rewrite zero-sub p = refl
∸-assoc-+ (suc m) (suc n) p rewrite ∸-assoc-+ m n p = refl

-- Exercise +*^ (stretch)
-- Show the following three laws
--  m ^ (n + p) ≡ (m ^ n) * (m ^ p)  (^-distribˡ-+-*)
--  (m * n) ^ p ≡ (m ^ p) * (n ^ p)  (^-distribʳ-*)
--  (m ^ n) ^ p ≡ m ^ (n * p)        (^-*-assoc)
-- for all m, n, and p.


-- Exercise Bin-laws (stretch)
-- Recall that Exercise Bin defines a datatype Bin of bitstrings representing natural numbers, and asks you to define functions
-- inc   : Bin → Bin
-- to    : ℕ → Bin
-- from  : Bin → ℕ

data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

inc : Bin → Bin
inc ⟨⟩ = ⟨⟩ I
inc (m I) = (inc m) O
inc (m O) = m I

toBin : ℕ → Bin
toBin zero = ⟨⟩ O
toBin (suc m) = inc (toBin m)

dbl : ℕ → ℕ
dbl zero = zero
dbl (suc m) = suc (suc (dbl m))

fromBin : Bin → ℕ
fromBin ⟨⟩ = zero
fromBin (m O) = dbl (fromBin m)
fromBin (m I) = suc (dbl (fromBin m)) 

-- Consider the following laws, where n ranges over naturals and b over bitstrings:
-- from (inc b) ≡ suc (from b)
-- to (from b) ≡ b
-- from (to n) ≡ n
-- For each law: if it holds, prove; if not, give a counterexample.

inc-suc : (b : Bin) →  fromBin (inc b) ≡ suc (fromBin b)
inc-suc ⟨⟩ = refl
inc-suc (b O) = refl
inc-suc (b I) rewrite inc-suc b = refl

-- from-to : (b : Bin) → toBin (fromBin b) ≡ b
-- not true with leading zeros...

to-from : (n : ℕ) → fromBin (toBin n) ≡ n
to-from zero = refl
to-from (suc n) rewrite inc-suc (toBin n) | to-from n = refl
