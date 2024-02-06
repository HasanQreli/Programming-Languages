module PE1 where


import Text.Printf

-- PE1: Recipe Calculator
-- The premise of this homework if to write a recipe calculator that
-- calculates: how much a recipe costs to make, what can be made with the
-- ingredients already available, and how much extra ingredients need to
-- be bought in order to make a recipe.

-- Recipe = Recipe Name [(Ingredient, Quantity)]
data Recipe = Recipe String [(String, Double)] deriving Show

-- Price = Price Ingredient Quantity Price
data Price = Price String Double Double deriving Show

-- You can use this as-is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- Calculate how much the given amount of the given ingredient costs
getIngredientCost :: (String, Double) -> [Price] -> Double
getIngredientCost (a,b) (Price name q p:rest) 
                    |a == name = getRounded (p/q*b)
                    |a /= name = getIngredientCost (a,b) rest
getIngredientCost _ _ = 0

-- Calculate how much it costs to buy all the ingredients of a recipe
recipeCost :: Recipe -> [Price] -> Double
recipeCost recipe@(Recipe name lst@((ing, price):xs)) pricelst@(Price namep q p:rest)
                    |lst == [] = 0
                    |lst /= [] = getIngredientCost (ing, price) pricelst + recipeCost (Recipe name xs) pricelst
recipeCost _ _ = 0


helper1 :: String ->  [(String, Double)] -> Double
helper1 str [] = 0
helper1 str lst2@((a,b):xss)
        |lst2 == [] = 0
        |str == a = getRounded b
        |str /= a = helper1 str xss

-- Given a list of how much you already have of each ingredient,
-- calculate how much of which ingredients are missing for a recipe
missingIngredients :: Recipe -> [(String, Double)] -> [(String, Double)]
missingIngredients recipe@(Recipe name lst1@((ing, price):xs)) lst2@((a,b):xss)
                    |lst1 == [] = []
                    |lst1 /= [] = if price - helper1 ing lst2>0 
                                    then [(ing, getRounded(price - helper1 ing lst2))] ++ missingIngredients (Recipe name xs) lst2 
                                    else missingIngredients (Recipe name xs) lst2
missingIngredients _ _ = []


recurs lst2@((a,b):xss) recipe@(Recipe name lst1@((ing, price):xs)) 
            |lst2 == [] = []
            |lst2 /= [] = [(a, getRounded(b - helper1 a lst1))] ++ recurs xss recipe 
recurs _ _ = []

-- Given a list of ingredients in your kitchen, calculate what you would
-- have left after making the given recipe. If there isn't enough of an
-- ingredient, the recipe cannot be made! You shouldn't change the amount
-- of ingredient in that case.
makeRecipe :: [(String, Double)] -> Recipe -> [(String, Double)]
makeRecipe lst2@((a,b):xss) recipe@(Recipe name lst1@((ing, price):xs)) 
            |missingIngredients recipe lst2 /= [] = lst2
            |otherwise = recurs lst2 recipe
makeRecipe _ _ = []





myconc [] lst2 = lst2
myconc lst2 [] = lst2
myconc [(d,e,f)] ((a,b,c):xs) 
            |a==d = [(a,getRounded(b+e),getRounded(c+f))] ++ xs
            |otherwise = [(a,b,c)] ++ myconc [(d,e,f)] xs 
myconc ((a,b,c):xs) [(d,e,f)]
            |a==d = [(a,getRounded(b+e),getRounded(c+f))] ++ xs
            |otherwise = [(a,b,c)] ++ myconc xs [(d,e,f)]
myconc _ _ = []



helper2 :: String -> Double -> [(String, Double)] -> [(String, Double)]
helper2 str double [] = []
helper2 str double lst2@((a,b):xss)
        |str == a = if b-double>0
                        then [(str,  getRounded(b-double))] ++ xss
                        else xss
        |str /= a = [(a,getRounded b)] ++ helper2 str double xss




-- Given a list of ingredients you already have, and a list of recipes,
-- make a shopping list showing how much of each ingredient you need
-- to buy, and its cost. Each ingredient mush appear in the shopping list
-- at most once (no duplicates!).
makeShoppingList :: [(String, Double)] -> [Recipe] -> [Price] -> [(String, Double, Double)]

makeShoppingList stock (Recipe rname []:xr) price = makeShoppingList stock xr price 

makeShoppingList stock recipe@((Recipe rname inception@((c,d):xri)): xr) price
                |otherwise = if remain>0 
                                then myconc [(c, getRounded remain, getRounded (getIngredientCost (c, remain) price))]  (makeShoppingList (helper2 c d stock) ([Recipe rname xri]++xr) price) 
                                else makeShoppingList (helper2 c d stock) ([Recipe rname xri]++xr) price
                where remain = getRounded (d - helper1 c stock)
                              

makeShoppingList _ _ _ = []