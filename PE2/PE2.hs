module PE2 where

-- PE2: Dungeon Crawler
-- Dungeon map is :: Tree Chamber [Encounter]
-- Each encounter is either a fight or a treasure
-- Fights deal you damage (reduce HP) but enemies drop some gold (add
-- gold)
-- Tresures just give gold, or potions (which give hp)
-- Nodes hold encounters, when you visit a node you go through all of them in order
-- You start with a certain amount of HP and 0 gold.
-- You lose HP and accumulate gold as you descend the tree and go through encounters

-- Polymorphic tree structure
data Tree a b = EmptyTree | Leaf a b | Node a b [Tree a b] deriving (Show, Eq)

-- Every location in the tree is of some Chamber type.
data Chamber = Cavern |
               NarrowPassage |
               UndergroundRiver |
               SlipperyRocks deriving (Show, Eq)

-- An enemy has a name, an amount of damage that it deals
-- and an amount of gold that it drops (in that order).
data Enemy = Enemy String Integer Integer deriving (Show, Eq)

-- Gold n gives n amount of gold
-- Potion n heals n hp
data Loot = Gold Integer | Potion Integer deriving (Show, Eq)

-- An encounter is either a Fight with an Enemy, or a treasure where
-- you find Loot
data Encounter = Fight Enemy | Treasure Loot deriving (Show, Eq)

-- This is a type synonym for how we will represents our dungeons
type Dungeon = Tree Chamber [Encounter]

-- First argument is starting HP
-- Second argument is the dungeon map
-- Third argument is the path (each integer in the list shows what child
-- you descend into)
-- Calculate how much HP you have left and how much gold you've
-- accumulated after traversing the given path
faceEncounter :: Integer -> Integer -> [Encounter] -> (Integer, Integer)
faceEncounter hp mygold [] = (hp, mygold)
faceEncounter hp mygold (Fight (Enemy name damage gold):rest) = faceEncounter (hp-damage) (mygold+gold) rest
faceEncounter hp mygold (Treasure (Gold gold):rest) = faceEncounter hp (mygold+gold) rest
faceEncounter hp mygold (Treasure (Potion heal):rest) = faceEncounter (hp+heal) mygold rest
                
traversePath :: Integer -> Dungeon -> [Int] -> (Integer, Integer)
traversePath hp dungeon index = traversePathhelper hp 0 dungeon index

traversePathhelper :: Integer -> Integer -> Dungeon -> [Int] -> (Integer, Integer)
traversePathhelper hp gold EmptyTree index = (hp,gold)
traversePathhelper hp gold (Leaf a b) [] = faceEncounter hp gold b
traversePathhelper hp gold (Node a b tree) (x:xs) = traversePathhelper (fst(faceEncounter hp gold b)) (snd(faceEncounter hp gold b)) (tree!!x) xs
traversePathhelper _ _ _ _ = (0, 0)

-- First argument is starting HP
-- Second argument is dungeon map
-- Find which path down the tree yields the most gold for you
-- You cannot turn back, i.e. you'll find a non-branching path
-- You do not need to reach the bottom of the tree
-- Return how much gold you've accumulated
findMaximumGain :: Integer -> Dungeon -> Integer
findMaximumGain hp dungeon = findMaximumGainhelper hp 0 dungeon 

findMaximumGainhelper:: Integer -> Integer -> Dungeon -> Integer 
findMaximumGainhelper hp gold EmptyTree = gold
findMaximumGainhelper hp gold (Leaf a b)
                        |fst result > 0 = snd result
                        |otherwise = gold
                        where result = faceEncounter hp gold b
findMaximumGainhelper hp gold (Node a b tree)
                        |fst result > 0 = maximum([findMaximumGainhelper (fst result) (snd result) x | x<-tree])
                        |otherwise = gold
                        where result = faceEncounter hp gold b 


-- First argument is starting HP
-- Second argument is the dungeon map
-- Remove paths that you cannot go thorugh with your starting HP. (By
-- removing nodes from tree).
-- Some internal nodes may become leafs during this process, make the
-- necessary changes in such a case.
findViablePaths :: Integer -> Dungeon -> Dungeon
findViablePaths hp dungeon = findViablePathshelper hp 0 dungeon

findViablePathshelper:: Integer -> Integer -> Dungeon -> Dungeon
findViablePathshelper hp gold EmptyTree = EmptyTree
findViablePathshelper hp gold (Leaf a b)
                        |fst result > 0 = Leaf a b
                        |otherwise = EmptyTree
                        where result = faceEncounter hp gold b 
findViablePathshelper hp gold (Node a b tree)
                        |fst result > 0 = if (listedonucu (fst result) (snd result) tree) == [] 
                                            then Leaf a b
                                            else (Node a b (listedonucu (fst result) (snd result) tree))
                        where result = faceEncounter hp gold b 

listedonucu :: Integer -> Integer -> [Tree Chamber [Encounter]] -> [Tree Chamber [Encounter]]
listedonucu _ _ [] = []
listedonucu hp gold ((Leaf a b):xs)
            |fst(result) <=0 = listedonucu hp gold xs
            |otherwise = [Leaf a b] ++ listedonucu hp gold xs
            where result = faceEncounter hp gold b
listedonucu hp gold ((Node a b tree):xs)
            |fst result <=0 = listedonucu hp gold xs
            |otherwise = if listedonucu (fst result) (snd result) tree == [] 
                                            then [Leaf a b] ++ listedonucu hp gold xs
                                            else [Node a b (listedonucu (fst result) (snd result) tree)] ++ listedonucu hp gold xs
            where result = faceEncounter hp gold b



-- First argument is starting HP
-- Second Argument is dungeon map
-- Find, among the viable paths in the tree (so the nodes you cannot
-- visit is already removed) the two most distant nodes, i.e. the two
-- nodes that are furthest awat from each other.
mostDistantPair :: Integer -> Dungeon -> (Integer, Dungeon)
mostDistantPair hp d = ikiliTupleMax (0,dungeon) (findLongestChilds dungeon)
                    where dungeon = findViablePaths hp d

ikiliTupleMax :: (Integer, Dungeon) -> [(Integer, Dungeon)] -> (Integer, Dungeon)
ikiliTupleMax curr [] = curr
ikiliTupleMax (c,d) ((a,b):xs)
                    |a>c = ikiliTupleMax (a,b) xs
                    |otherwise = ikiliTupleMax (c,d) xs



findLongestChilds :: Dungeon -> [(Integer, Dungeon)]
findLongestChilds EmptyTree = [(0,EmptyTree)]
findLongestChilds dungeon@(Leaf a b) = [(0, dungeon)]
findLongestChilds dungeon@(Node a b tree) = (findHeight dungeon + findSecondHeight dungeon, findTree dungeon) : concat [findLongestChilds x | x<-tree]

findHeight :: Dungeon -> Integer
findHeight (Leaf a b) = 0
findHeight (Node a b tree) = 1 + maximum [findHeight x | x<-tree]

findTree :: Dungeon -> Dungeon
findTree d@(Leaf a b) = d
findTree (Node a b tree)
            |length sorted == 1 = Node a b [findTreeforone $ snd (sorted!!0)]
            |i1<i2 = Node a b ([findTreeforone $ snd (sorted!!1)] ++ [findTreeforone $ snd (sorted!!0)])
            |otherwise = Node a b ([findTreeforone $ snd (sorted!!0)] ++ [findTreeforone $ snd (sorted!!1)])
            where sorted = mysort2 [(findHeight x, x) | x<-tree]
                  unsorted = [(findHeight x, x) | x<-tree]
                  sorted1 = snd (sorted!!1)
                  sorted2 = snd (sorted!!0)
                  i1 = getIndex 0 sorted1 unsorted
                  i2 = getIndex 0 sorted2 unsorted
--suraya basit bi siralama yaz (sortlanmadanki siralama ile )

getIndex :: Integer -> Dungeon -> [(Integer, Dungeon)] -> Integer
getIndex i a ((b,c):xs)
            |c==a = i
            |otherwise = getIndex (i+1) a xs
 
findTreeforone :: Tree Chamber [Encounter] -> Tree Chamber [Encounter]
findTreeforone d@(Leaf a b) = d
findTreeforone (Node a b tree)  = Node a b [findTreeforone $ snd (sorted!!0)]
                    where sorted = mysort2 [(findHeight x, x) | x<-tree]



heightlst (Leaf a b) = [0]
heightlst (Node a b tree) = [findHeight x | x<-tree]

findSecondHeight :: Dungeon -> Integer
findSecondHeight (Leaf a b) = 0
findSecondHeight (Node a b [c]) = 0
findSecondHeight (Node a b tree) = 1 + (mysort lst)!!1
                    where lst =  [findHeight x | x<-tree]




mysort [] = []
mysort (x:xs) = mysort (filter (>=x) xs) ++ [x] ++ mysort (filter (<x) xs)

mysort2 :: [(Integer, Dungeon)] -> [(Integer, Dungeon)]
mysort2 [] = []
mysort2 (x@(a,b):xs) = mysort2 (filter (myBigger x) xs) ++ [(a,b)] ++ mysort2 (filter (mySmaller (a,b)) xs)

myBigger :: (Integer, Dungeon) -> (Integer, Dungeon) -> Bool
myBigger (a,b) (c,d)
        |c>=a = True
        |otherwise = False

mySmaller (a,b) (c,d)
        |c<a = True
        |otherwise = False

-- Find the subtree that has the highest total gold/damage ratio
-- Simply divide the total gold in the subtree by the total damage
-- in the subtree. You only take whole subtrees (i.e you can take a new
-- node as the root of your subtree, but you cannot remove nodes
-- below it). Note that the answer may be the whole tree.
mostEfficientSubtree :: Dungeon -> Dungeon
mostEfficientSubtree EmptyTree = EmptyTree
mostEfficientSubtree dungeon = uc (maxfind (head(lst dungeon)) (lst dungeon))


maxfind :: (Integer, Integer, Tree Chamber [Encounter]) -> [(Integer, Integer, Tree Chamber [Encounter])] -> (Integer, Integer, Tree Chamber [Encounter])
maxfind curr [] = curr
maxfind curr ((a, b, tree):xs) 
            |bir(curr)*b < iki(curr)*a = maxfind (a,b,tree) xs
            |otherwise = maxfind curr xs

bir (a,b,c) = a
iki (a,b,c) = b
uc (a,b,c) = c

lst :: Tree Chamber [Encounter] -> [(Integer, Integer, Tree Chamber [Encounter])]
lst EmptyTree = []
lst (Leaf a b) = [(snd(encounter 0 0 b),fst(encounter 0 0 b), (Leaf a b))]
lst (Node a b tree) = [(fst (findEfficiency 0 0 (Node a b tree)), snd (findEfficiency 0 0 (Node a b tree)), (Node a b tree))] ++ lst2 tree

lst2 [] = []
lst2 (x:xs) = lst x ++ lst2 xs

findEfficiency :: Integer -> Integer -> Dungeon -> (Integer, Integer)
findEfficiency damage gold EmptyTree = (damage, gold)
findEfficiency damage gold (Leaf a b) =  (fst(encounter 0 0 b) + damage, snd(encounter 0 0 b) +gold)  
findEfficiency damage gold (Node a b tree) = addall (damage + fst(encounter 0 0 b), gold + snd(encounter 0 0 b)) [findEfficiency damage gold x| x<-tree]
                                    
addall (a,b) [] = (a,b)
addall (a,b) ((c,d):xs) = addall (a+c, b+d) xs



encounter :: Integer -> Integer -> [Encounter] -> (Integer, Integer)
encounter heal gold [] = (heal, gold)
encounter heal gold (Fight (Enemy name damage ngold):rest) = encounter (heal+damage) (gold+ngold) rest
encounter heal gold (Treasure (Gold ngold):rest) = encounter heal (gold+ngold) rest
encounter heal gold (Treasure (Potion nheal):rest) = encounter (heal-nheal) gold rest

dungeon = 
    Node Cavern [Fight (Enemy "Goblins" 5 2)] [
        Leaf UndergroundRiver [],
        Node NarrowPassage [Fight (Enemy "Skeleton" 10 15)] [
            Node NarrowPassage [Treasure (Gold 2)] [
                Node UndergroundRiver [Fight (Enemy "Necromancer" 30 100)] [
                    Leaf SlipperyRocks [Fight (Enemy "Skeleton" 10 15)]]]],
        Node SlipperyRocks [Fight (Enemy "Skeleton" 10 15), Treasure (Gold 10), Treasure (Potion 15)] [
            Node UndergroundRiver [Fight (Enemy "Goblins" 5 2), Fight (Enemy "Goblins" 5 2)] [
                Leaf NarrowPassage [Fight (Enemy "Lich" 25 50)],
                Leaf Cavern [Fight (Enemy "Goblins" 5 2)],
                Leaf NarrowPassage [Fight (Enemy "Zombie" 15 10)]]],
        Node NarrowPassage [Fight (Enemy "Bandit" 7 5)] [
            Node UndergroundRiver [Fight (Enemy "Rat" 2 1)] [
                Node Cavern [Fight (Enemy "Lich" 25 50)] [
                    Leaf SlipperyRocks [Fight (Enemy "Zombie" 15 10)],
                    Leaf Cavern [Fight (Enemy "Skeleton" 10 15)],
                    Leaf UndergroundRiver [Fight (Enemy "Necromancer" 30 100)]]]]]

pair6 = mostDistantPair 4 dungeon

ans6 = (0, EmptyTree :: Dungeon)

check (x, y) = if x == y then "Success!" else "Fail\nExpected:\n" ++ show y ++ "\n" ++ "Result:\n" ++ show x

pairs = [(pair6, ans6)]
results = map check pairs
points = sum (map (\x -> if x == "Success!" then 2 else 0) results)

main = do
    putStrLn (show points ++ "/2")
    putStrLn (check (pair6, ans6))
