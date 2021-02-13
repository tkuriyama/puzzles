-- https://fivethirtyeight.com/features/can-you-cross-like-a-boss/
-- Find a matrix that satisfies the following row and col products
-- 			210
--                      144
--                       54
--                      135
--                        4
--                       49
-- 6,615  15,552  420

type Matrix = [[Int]]
digits = [1..9]

--------------------------------------------------------------------------------
-- Simple brute force with thoughtful ordering; ~0.3 secs on GHCI

matrices :: [Matrix]
matrices= [ [ [a1, a2, a3]
            , [b1, b2, b3]
            , [c1, c2, c3]
            , [d1, d2, d3]
            , [e1, e2, e3]
            , [f1, f2, f3]]
          | f1 <- digits, f2 <- digits, f3 <- digits
          , f1 * f2 * f3 == 49
          , e1 <- digits, e2 <- digits, e3 <- digits
          , e1 * e2 * e3 == 4
          , c1 <- digits, c2 <- digits, c3 <- digits
          , c1 * c2 * c3 == 54
          , a1 <- digits
          , b1 <- digits
          , d1 <- digits
          , a1 * b1 * c1 * d1 * e1 * f1 == 6615
          ,  a2 <- digits, a3 <- digits
          , a1 * a2 * a3 == 210
          , b2 <- digits, b3 <- digits
          , b1 * b2 * b3 == 144
          , d2 <- digits, d3 <- digits
          , d1 * d2 * d3 == 135
          , a3 * b3 * c3 * d3 * e3 * f3 == 420
          , a2 * b2 * c2 * d2 * e2 * f2 == 15552
          ]
