-- https://plfa.github.io/Relations/

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Nat.Properties using (+-comm; +-identityʳ; *-comm)

-- Non-Strict Inequality 

data _≤_ : ℕ → ℕ → Set where
  z≤n : ∀ {n : ℕ}
      --------
    → zero ≤ n
  s≤s : ∀ {m n : ℕ}
    → m ≤ n
      -------------
    → suc m ≤ suc n

infix 4 _≤_

-- going from larger to smaller 
inv-s≤s : ∀ {m n : ℕ}
  → suc m ≤ suc n
    -------------
  → m ≤ n
inv-s≤s (s≤s m≤n) = m≤n

-- only one way for a number to be <= zero
inv-z≤n : ∀ {m : ℕ}
  → m ≤ zero
    --------
  → m ≡ zero
inv-z≤n z≤n = refl


-- Exercise Give an example of a preorder that is not a partial order.

-- Exericse Give an example of a partial order that is not a total order.


-- * Reflexive. For all n, the relation n ≤ n holds.
-- * Transitive. For all m, n, and p, if m ≤ n and n ≤ p hold, then m ≤ p holds.
-- * Anti-symmetric. For all m and n, if both m ≤ n and n ≤ m hold, then m ≡ n holds.
-- * Total. For all m and n, either m ≤ n or n ≤ m holds.

≤-refl : ∀ {n : ℕ}
    -----
  → n ≤ n
≤-refl {zero} = z≤n
≤-refl {suc n} = s≤s ≤-refl

≤-trans : ∀ {m n p : ℕ}
  → m ≤ n
  → n ≤ p
    -----
  → m ≤ p
≤-trans z≤n       _          =  z≤n
≤-trans (s≤s m≤n) (s≤s n≤p)  =  s≤s (≤-trans m≤n n≤p)

≤-trans′ : ∀ (m n p : ℕ)
  → m ≤ n
  → n ≤ p
    -----
  → m ≤ p
≤-trans′ zero    _       _       z≤n       _          =  z≤n
≤-trans′ (suc m) (suc n) (suc p) (s≤s m≤n) (s≤s n≤p)  =  s≤s (≤-trans′ m n p m≤n n≤p)

≤-antisym : ∀ {m n : ℕ}
  → m ≤ n
  → n ≤ m
    -----
  → m ≡ n
≤-antisym z≤n       z≤n        = refl
≤-antisym (s≤s m≤n) (s≤s n≤m)  =  cong suc (≤-antisym m≤n n≤m)

-- Exercise ≤-antisym-cases (practice)
-- The above proof omits cases where one argument is z≤n and one argument is s≤s. Why is it ok to omit them?

-- The case for the constructor s≤s is impossible
-- because unification ended with a conflicting equation
 --  suc n ≟ zero

data Total (m n : ℕ) : Set where
  forward :
      m ≤ n
      ---------
    → Total m n

  flipped :
      n ≤ m
      ---------
    → Total m n

data Total′ : ℕ → ℕ → Set where
  forward′ : ∀ {m n : ℕ}
    → m ≤ n
      ----------
    → Total′ m n

  flipped′ : ∀ {m n : ℕ}
    → n ≤ m
      ----------
    → Total′ m n

≤-total : ∀ (m n : ℕ) → Total m n
≤-total zero    n                         =  forward z≤n
≤-total (suc m) zero                      =  flipped z≤n
≤-total (suc m) (suc n) with ≤-total m n
...                        | forward m≤n  =  forward (s≤s m≤n)
...                        | flipped n≤m  =  flipped (s≤s n≤m)


≤-total′ : ∀ (m n : ℕ) → Total m n
≤-total′ zero    n        =  forward z≤n
≤-total′ (suc m) zero     =  flipped z≤n
≤-total′ (suc m) (suc n)  =  helper (≤-total′ m n)
  where
  helper : Total m n → Total (suc m) (suc n)
  helper (forward m≤n)  =  forward (s≤s m≤n)
  helper (flipped n≤m)  =  flipped (s≤s n≤m)

≤-total″ : ∀ (m n : ℕ) → Total m n
≤-total″ m       zero                      =  flipped z≤n
≤-total″ zero    (suc n)                   =  forward z≤n
≤-total″ (suc m) (suc n) with ≤-total″ m n
...                        | forward m≤n   =  forward (s≤s m≤n)
...                        | flipped n≤m   =  flipped (s≤s n≤m)


-- Monotonicity
+-monoʳ-≤ : ∀ (n p q : ℕ)
  → p ≤ q
    -------------
  → n + p ≤ n + q
+-monoʳ-≤ zero    p q p≤q  =  p≤q
+-monoʳ-≤ (suc n) p q p≤q  =  s≤s (+-monoʳ-≤ n p q p≤q) 

+-monoˡ-≤ : ∀ (m n p : ℕ)
  → m ≤ n
    -------------
  → m + p ≤ n + p
+-monoˡ-≤ m n p m≤n rewrite +-comm m p | +-comm n p  = +-monoʳ-≤ p m n m≤n

+-mono-≤ : ∀ (m n p q : ℕ)
  → m ≤ n
  → p ≤ q
    -------------
  → m + p ≤ n + q
+-mono-≤ m n p q m≤n p≤q  =  ≤-trans (+-monoˡ-≤ m n p m≤n) (+-monoʳ-≤ n p q p≤q)

-- Exercise *-mono-≤ (stretch)
-- Show that multiplication is monotonic with regard to inequality.

*-monoʳ-≤ : ∀ (n p q : ℕ)
  → p ≤ q
    -------------
  → n * p ≤ n * q
*-monoʳ-≤ zero    p q p≤q = z≤n
*-monoʳ-≤ (suc n) p q p≤q = +-mono-≤ p q (n * p) (n * q) p≤q (*-monoʳ-≤ n p q p≤q) 

*-monoˡ-≤ : ∀ (m n p : ℕ)
  → m ≤ n
    -------------
  → m * p ≤ n * p
*-monoˡ-≤ m n p m≤n  rewrite *-comm m p | *-comm n p  = *-monoʳ-≤ p m n m≤n

*-mono-≤ : ∀ (m n p q : ℕ)
  → m ≤ n
  → p ≤ q
    -------------
  → m * p ≤ n * q
*-mono-≤ m n p q m≤n p≤q  =  ≤-trans (*-monoˡ-≤ m n p m≤n) (*-monoʳ-≤ n p q p≤q)

-- Strict Inequality

infix 4 _<_

data _<_ : ℕ → ℕ → Set where

  z<s : ∀ {n : ℕ}
      ------------
    → zero < suc n

  s<s : ∀ {m n : ℕ}
    → m < n
      -------------
    → suc m < suc n

-- Exercise <-trans (recommended)
-- Show that strict inequality is transitive.

<-trans : ∀ {m n p : ℕ}
  → m < n
  → n < p
    -----
  → m < p
<-trans z<s       (s<s n<p)  =  z<s
<-trans (s<s m<n) (s<s n<p)  =  s<s (<-trans m<n n<p)

-- Exercise trichotomy (practice)

-- Show that strict inequality satisfies a weak version of trichotomy, in the sense that for any m and n that one of the following holds:

-- * m < n,
-- * m ≡ n, or
-- * m > n.

-- Define m > n to be the same as n < m. You will need a suitable data declaration, similar to that used for totality. (We will show that the three cases are exclusive after we introduce
-- negation.)


data Trichotomy (m n : ℕ) : Set where
  is-< : m < n → Trichotomy m n
  is-≡ : m ≡ n → Trichotomy m n
  is-> : n < m → Trichotomy m n

<-trichotomy : ∀ (m n : ℕ) → Trichotomy m n
<-trichotomy zero    zero    = is-≡ refl
<-trichotomy zero    (suc n) = is-< z<s 
<-trichotomy (suc m) zero    = is-> z<s
<-trichotomy (suc m) (suc n) with <-trichotomy m n
...                             | is-< m<n = is-< (s<s m<n)
...                             | is-≡ m≡n = is-≡ (cong suc m≡n)
...                             | is-> m>n = is-> (s<s m>n)
                             

-- Exercise +-mono-< (practice)

-- Show that addition is monotonic with respect to strict inequality. As with inequality, some additional definitions may be required.
+-monoʳ-< : ∀ (n p q : ℕ)
  → p < q
    -------------
  → n + p < n + q
+-monoʳ-< zero    p q p<q  = p<q
+-monoʳ-< (suc n) p q p<q  =  s<s (+-monoʳ-< n p q p<q) 

+-monoˡ-< : ∀ (m n p : ℕ)
  → m < n
    -------------
  → m + p < n + p
+-monoˡ-< m n p m<n rewrite +-comm m p | +-comm n p  = +-monoʳ-< p m n m<n

+-mono-< : ∀ (m n p q : ℕ)
  → m < n
  → p < q
    -------------
  → m + p < n + q
+-mono-< m n p q m<n p<q  =  <-trans (+-monoˡ-< m n p m<n) (+-monoʳ-< n p q p<q)

-- Exercise ≤-iff-< (recommended)

-- Show that suc m ≤ n implies m < n, and conversely.
≤-iff-< : ∀ {m n : ℕ}
  → suc m ≤ n
    ---------
  → m < n
≤-iff-< {zero}  (s≤s m≤n) = z<s
≤-iff-< {suc m} (s≤s m≤n) = s<s (≤-iff-< m≤n)


-- Exercise <-trans-revisited (practice)

-- Give an alternative proof that strict inequality is transitive, using the relation between strict inequality and inequality and the fact that inequality is transitive.



-- Even and Odd

data even : ℕ → Set
data odd  : ℕ → Set

data even where

  zero :
      ---------
      even zero

  suc  : ∀ {n : ℕ}
    → odd n
      ------------
    → even (suc n)

data odd where

  suc  : ∀ {n : ℕ}
    → even n
      -----------
    → odd (suc n)

e+e≡e : ∀ {m n : ℕ}
  → even m
  → even n
    ------------
  → even (m + n)

o+e≡o : ∀ {m n : ℕ}
  → odd m
  → even n
    -----------
  → odd (m + n)

e+e≡e zero     en  =  en
e+e≡e (suc om) en  =  suc (o+e≡o om en)
o+e≡o (suc em) en  =  suc (e+e≡e em en)

-- Exercise o+o≡e (stretch)
-- Show that the sum of two odd numbers is even.
e+o≡o : ∀ { m n : ℕ}
  → even m
  → odd n
  ------------
  → odd (m + n) 
o+o≡e : ∀ {m n : ℕ}
  → odd m
  → odd n
    -----------
  → even (m + n)

e+o≡o zero     on = on
e+o≡o (suc om) on = suc (o+o≡e om on)
o+o≡e (suc em) on = suc (e+o≡o em on) 


-- Execrsie Bin-Prediactes  (stretch)

data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

inc : Bin → Bin
inc ⟨⟩ = ⟨⟩ I
inc (m I) = (inc m) O
inc (m O) = m I

-- Define a predicate 

data Can : Bin → Set

-- over all bitstrings that holds if the bitstring is canonical, meaning it has no leading zeros; the first representation of eleven above is canonical, and the second is not. To define it, you will need an auxiliary predicate

data One : Bin → Set where 
  one :
  -----------
    One (⟨⟩ I)

  suc-zero : ∀ { b : Bin } 
  -----------
    → One b
  ------------
    → One (b O)

  suc-one : ∀ {b : Bin } 
  -----------
    → One b
  ------------
    → One (b I)

data Can where
   zero : 
   ----------
     Can (⟨⟩)

   with-one : ∀ { b : Bin }
   -----------
     → One b
  ------------
    → Can b

inc-one-can : ∀ { b : Bin } 
  → One b
  ----------
  → One (inc b)
inc-one-can one = suc-zero one
inc-one-can (suc-zero b) = suc-one b 
inc-one-can (suc-one b) = suc-zero (inc-one-can b) 

inc-can : ∀ { b : Bin }
  → Can b
  ------------
  → Can (inc b)
  
inc-can zero = with-one one 
inc-can (with-one b) = with-one (inc-one-can b)


-- to and from Bin

toBin : ℕ → Bin
toBin zero = ⟨⟩
toBin (suc m) = inc (toBin m)

dbl : ℕ → ℕ
dbl zero = zero
dbl (suc m) = suc (suc (dbl m))

fromBin : Bin → ℕ
fromBin ⟨⟩ = zero
fromBin (m O) = dbl (fromBin m)
fromBin (m I) = suc (dbl (fromBin m)) 

inc-suc : (b : Bin) →  fromBin (inc b) ≡ suc (fromBin b)
inc-suc ⟨⟩ = refl
inc-suc (b O) = refl
inc-suc (b I) rewrite inc-suc b = refl

to-from : (n : ℕ) → fromBin (toBin n) ≡ n
to-from zero = refl
to-from (suc n) rewrite inc-suc (toBin n) | to-from n = refl

dblb : Bin → Bin
dblb ⟨⟩ = ⟨⟩
dblb b  = b O

dblb-inc : ∀ (m : Bin) → dblb (inc m) ≡ inc (inc (dblb m))
dblb-inc ⟨⟩ = refl
dblb-inc (x O) = refl
dblb-inc (x I) = refl

dbl-equivalence : ∀ { n : ℕ } → toBin (dbl n) ≡ dblb (toBin n) 
dbl-equivalence {zero} = refl
dbl-equivalence {suc n} rewrite
  dbl-equivalence {n} | sym (dblb-inc (toBin n)) = refl 

tofrom-id-Can : ∀ { b : Bin } 
  → Can b 
  ---------------
  → toBin (fromBin b) ≡ b
tofrom-id-Can zero = refl
tofrom-id-Can (with-one one) = refl
tofrom-id-Can (with-one (suc-zero x)) = {!!} 
tofrom-id-Can (with-one (suc-one x)) = {!!}  
