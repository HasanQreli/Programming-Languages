{-# LANGUAGE FlexibleInstances #-}

module PE3 where

import Data.List (sort, sortBy)
import Text.Printf (printf)

data Term = Const Integer | Pw Integer Power | Trig Integer Power Trigonometric | Exp Integer Power Exponential

data Power = Power Integer
data Polynomial = Polynomial [(Integer, Power)]
data Exponential = Exponential Polynomial
data Trigonometric = Sin Polynomial | Cos Polynomial

class Evaluable a where
    function :: a -> (Integer -> Double)

class Differentiable a where
    derivative :: a -> [Term]

-- You can use this as is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- You don't have to follow the order the functions appear in the file
-- For example, you could first define all Show instances, then all Eq instances etc.
-- if that implementation order is more convenient for you.



-- INSTANCES FOR POWER


instance Show Power where
    show (Power a)
            |a == 0 = "1"
            |a == 1 = "x"
            |otherwise = printf "x^%d" a

instance Eq Power where
    (Power a) == (Power b)
        |a == b = True
        |otherwise = False

instance Ord Power where
    (Power a) > (Power b)
        |a>b =True
        |otherwise = False
    (Power a) >= (Power b)
        |a>=b =True
        |otherwise = False
    (Power a) < (Power b)
        |a<b =True
        |otherwise = False
    (Power a) <= (Power b)
        |a<=b =True
        |otherwise = False

instance Evaluable Power where
    function (Power a) = \x -> fromInteger x^a

instance Differentiable Power where
    derivative (Power a)
            |a==0 = [Const 0]
            |a==1 = [Const 1]
            |otherwise = [Pw a (Power (a-1))]

-- INSTANCES FOR POLYNOMIAL


instance Show Polynomial where
    show (Polynomial []) = ""
    show (Polynomial ((a , Power b):xs))
            |b==0 = printf "%d" a ++ showPolihelper (Polynomial xs)
            |b==1 && a==1 = "x" ++ showPolihelper (Polynomial xs)
            |b==1 && a== -1 = "-x" ++ showPolihelper (Polynomial xs)
            |b==1 = printf "%dx" a ++ showPolihelper (Polynomial xs)
            |a==1 = printf "x^%d" b ++ showPolihelper (Polynomial xs)
            |a== -1 = printf "-x^%d" b ++ showPolihelper (Polynomial xs)
            |otherwise = printf "%dx^%d" a b ++ showPolihelper (Polynomial xs)


showPolihelper (Polynomial []) = ""
showPolihelper (Polynomial ((a , Power b):xs))
            |b==0 = printf " + %d" a ++ showPolihelper (Polynomial xs)
            |b==1 && a==1 = " + x" ++ showPolihelper (Polynomial xs)
            |b==1 && a== -1 = " + -x" ++ showPolihelper (Polynomial xs)
            |b==1 = printf " + %dx" a ++ showPolihelper (Polynomial xs)
            |a==1 = printf " + x^%d" b ++ showPolihelper (Polynomial xs)
            |a== -1 = printf " + -x^%d" b ++ showPolihelper (Polynomial xs)
            |otherwise = printf " + %dx^%d" a b ++ showPolihelper (Polynomial xs)

instance Evaluable Polynomial where
    function (Polynomial [])  = \x -> 0.0
    function (Polynomial ((a , Power b):xs)) = \x -> fromInteger (a*(x^b) ) + function (Polynomial xs) x

instance Differentiable Polynomial where
    derivative (Polynomial []) = []
    derivative (Polynomial((a , Power b):xs))
            |b==0 = derivative (Polynomial xs)
            |b==1 = [Const a] ++ derivative (Polynomial xs)
            |otherwise = [Pw (a*b) (Power (b-1))] ++ derivative (Polynomial xs)


-- INSTANCES FOR TRIGONOMETRIC

instance Show Trigonometric where
    show (Sin poli@(Polynomial [(a, Power b)]))
            |b>1 || a<0 = "sin(" ++ show poli ++ ")"
            |otherwise = "sin" ++ show poli
    show (Cos poli@(Polynomial [(a, Power b)]))
            |b>1 || a<0= "cos(" ++ show poli ++ ")"
            |otherwise = "cos" ++ show poli
    show (Sin poli@(Polynomial ((a , Power b):xs))) = "sin(" ++ show poli ++ ")"
    show (Cos poli@(Polynomial ((a , Power b):xs))) = "cos(" ++ show poli ++ ")"

    show _ = []

instance Evaluable Trigonometric where
    function (Sin poli) = \x -> getRounded (sin (function poli x ))
    function (Cos poli) = \x -> getRounded (cos (function poli x ))

instance Differentiable Trigonometric where
    derivative (Sin poli) = myMerge (Cos poli) (derivative poli)
    derivative (Cos poli) = myMerge (Sin poli) (derivative poli)

myMerge :: Trigonometric -> [Term] -> [Term]
myMerge trig [] = []
myMerge trig@(Cos poli) (Pw a (Power b):xs) = [Trig a (Power b) trig] ++ myMerge trig xs
myMerge trig@(Sin poli) (Pw a (Power b):xs) = [Trig (-a) (Power b) trig] ++ myMerge trig xs



-- INSTANCES FOR EXPONENTIAL

instance Show Exponential where
    show (Exponential poli@(Polynomial [(a, Power b)]))
            |b>1 || a<0 = "e^(" ++ show poli ++ ")"
            |otherwise = "e^" ++ show poli
    show (Exponential poli@(Polynomial ((a, Power b):xs))) = "e^(" ++ show poli ++ ")"

instance Evaluable Exponential where
    function (Exponential poli) = \x -> getRounded (exp (function poli x))


instance Differentiable Exponential where
    derivative (Exponential poli) = myMerge2 (Exponential poli) (derivative poli)


myMerge2 :: Exponential -> [Term] -> [Term]
myMerge2 expo [] = []
myMerge2 expo (Pw a (Power b):xs) = [Exp a (Power b) expo] ++ myMerge2 expo xs




-- INSTANCES FOR TERM


instance Show Term where
    show (Const a) = printf "%d" a
    show (Pw a power)
            |a==1 = show power
            |a == -1 = "-" ++ show power
            |otherwise = printf "%d" a ++ show power
    show (Trig a power@(Power b) trig)
                |b==0 = if a==1 then show trig else
                        if a== -1 then "-" ++ show trig else
                        printf "%d" a ++ show trig
                |a==1 =  show power ++ show trig
                |a== -1 = "-" ++ show power ++ show trig
                |otherwise = printf "%d" a ++ show power ++ show trig
    show (Exp a power@(Power b) expo)
                |b==0 = if a==1 then show expo else
                        if a== -1 then "-" ++ show expo else
                        printf "%d" a ++ show expo 
                |a==1 =  show power ++ show expo
                |a== -1 = "-" ++ show power ++ show expo
                |otherwise = printf "%d" a ++ show power ++ show expo
    
instance Evaluable Term where
    function (Const a) x = fromInteger a
    function (Pw a power) x = getRounded ( fromInteger a * function power x)
    function (Trig a power trig) x = getRounded ( fromInteger a * function power x * function trig x)
    function (Exp a power expo) x =  getRounded ( fromInteger (a) * function power x * function expo x)


instance Differentiable Term where
    derivative (Const a) = []
    derivative (Pw a (Power b)) = derivative (Polynomial [(a, Power b)])
    derivative (Trig a (Power b) trig@(Sin poli))
                    = myMergewo trig (derivative poli2) ++ myMerge (Cos poli) (mergePoli poli2 (derivative poli))
                    where poli2 = Polynomial [(a, Power b)]
    derivative (Trig a (Power b) trig@(Cos poli))
                    = myMergewo trig (derivative poli2) ++ myMerge (Sin poli) (mergePoli poli2 (derivative poli))
                    where poli2 = Polynomial [(a, Power b)]
    derivative (Exp a (Power b) expo@(Exponential poli))
                    = myMerge2 expo (derivative poli2) ++ myMerge2 expo (mergePoli poli2 (derivative poli))
                    where poli2 = Polynomial [(a, Power b)]
    

mergePoli :: Polynomial -> [Term] -> [Term]
mergePoli f [] = []
mergePoli f@(Polynomial[(a, Power b)]) (Pw c (Power d):xs)
                    = [Pw (c*a) (Power (b+d))] ++ mergePoli f xs

myMergewo :: Trigonometric -> [Term] -> [Term]
myMergewo trig [] = []
myMergewo trig (Pw a (Power b):xs) = [Trig a (Power b) trig] ++ myMergewo trig xs






-- INSTANCES FOR [TERM]

instance Evaluable [Term] where
    function [] x = 0.0
    function (Const a:xs) x = getRounded(fromInteger a + function xs x)
    function (Pw a power:xs) x = getRounded ( fromInteger a * function power x + function xs x)
    function (Trig a power trig:xs) x = getRounded ( fromInteger a * function power x * function trig x + function xs x)
    function (Exp a power expo:xs) x =  getRounded ( fromInteger a * function power x * function expo x + function xs x)


instance Differentiable [Term] where
    derivative lst = removezeros(birlestir(concat (map (derivative) lst)))


--birlestir :: [Term] -> [Term] -> [Term]

removezeros [] = []
removezeros (trig2@(Trig b _ _):xs2) 
                |b==0 = removezeros xs2
                |otherwise = [trig2] ++ removezeros xs2
removezeros (expo2@(Exp b _ _):xs2) 
                |b==0 = removezeros xs2
                |otherwise = [expo2] ++ removezeros xs2
removezeros (pow2@(Pw c (Power d)):xs) 
                |c==0 = removezeros xs
                |otherwise = [pow2] ++ removezeros xs
removezeros (Const b:xs2) 
                |b==0 = removezeros xs2
                |otherwise = [Const b] ++ removezeros xs2


birlestir xs = foldr birlestirHelper [] xs

birlestirHelper x [] = [x]
birlestirHelper trig1@(Trig a c d) (trig2@(Trig b _ _):xs2)
                |trigeq trig1 trig2 = birlestirHelper (Trig (a+b) c d) xs2
                |otherwise = birlestirHelper trig1 xs2 ++ [trig2] 
birlestirHelper expo1@(Exp a c d) (expo2@(Exp b _ _):xs2)
                |expoeq expo1 expo2 = birlestirHelper (Exp (a+b) c d) xs2
                |otherwise = birlestirHelper expo1 xs2 ++ [expo2] 
birlestirHelper pow1@(Pw a (Power b)) (pow2@(Pw c (Power d)):xs)
                |b == d = birlestirHelper (Pw (a+c) (Power b)) xs
                |otherwise =  birlestirHelper pow1 xs ++ [pow2] 
birlestirHelper cnst1@(Const a) (cnst2@(Const b):xs) = birlestirHelper (Const (a+b)) xs
birlestirHelper x (y:xs) = birlestirHelper x xs ++ [y] 



trigeq (Trig a (Power b) trig1@(Sin poli1)) (Trig c (Power d) trig2@(Sin poli2)) 
            |b==d = polieq poli1 poli2
            |otherwise = False
trigeq (Trig a (Power b) trig1@(Cos poli1)) (Trig c (Power d) trig2@(Cos poli2))
            |b==d = polieq poli1 poli2
            |otherwise = False
trigeq _ _ = False

expoeq (Exp a (Power b) expo1@(Exponential poli1)) (Exp c (Power d) expo2@(Exponential poli2)) 
            |b==d = polieq poli1 poli2
            |otherwise = False
expoeq _ _ = False


polieq (Polynomial []) (Polynomial []) = True
polieq (Polynomial((a , Power b):xs1)) (Polynomial((c , Power d):xs2))
            |a==c && b==d = polieq (Polynomial xs1) (Polynomial xs2)
            |otherwise = False
polieq _ _ = False