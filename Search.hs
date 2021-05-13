{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node {
    state  :: s, 
    action :: Maybe a, 
    parent :: Maybe (Node s a),
    depth  :: Int,
    children :: [Node s a],
    cost :: Float
  } deriving (Show)
{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    node1 == node2 = nodeState node1 == nodeState node2 

instance Ord s => Ord (Node s a) where
    node1 <= node2 = nodeState node1 <= nodeState node2 

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState (Node st _ _ _ _ _) = st 

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ _ pa _ _ _) = pa 

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ _ de _ _) = de

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ _ _ _ chil _) = chil

nodeHeuristic :: Node s a -> Float
nodeHeuristic (Node _ _ _ _ _ cst) = cst

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ ac _ _ _ _) = ac
{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createStateSpaceHelper :: (ProblemState s a, Eq s) => s -> a -> Node s a -> Int -> Node s a
createStateSpaceHelper st ac pa dept = node
   where
    node = Node st (Just ac) (Just pa) dept his_children ((h st) + (fromIntegral dept :: Float))
    his_children =  map (\(act, sta) -> createStateSpaceHelper sta act node (dept + 1)) (successors st)

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = root
     where
        root = (Node initialState Nothing Nothing 0 children_list (h initialState))
        children_list = map (\(act, sta) -> createStateSpaceHelper sta act root 1) (successors initialState)

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = filter(\e -> (elem e (nodeChildren node)) && (S.member (nodeState e) visited) == False) (nodeChildren node) 

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node
   | PQ.lookup node frontier == Nothing = PQ.insert node (nodeHeuristic node) frontier
   | (fromJust (PQ.lookup node frontier)) > (nodeHeuristic node) = PQ.insert node (nodeHeuristic node) frontier
   | otherwise = frontier
  -- where
    -- key_list = keys frontier

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = foldl insertSucc frontier (suitableSuccs node visited)

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier
     | isGoal (nodeState node) = node
     | otherwise = astar' updated_visited updated_frontier
   where
    (node, new_frontier) = deleteFindMin frontier
    updated_visited = S.insert (nodeState node) visited
    updated_frontier = insertSuccs node new_frontier updated_visited 

{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' S.empty (insertSucc PQ.empty initialNode)

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPath :: Node s a -> [(a, s)]
extractPath goalNode = my_path
  where
    my_path = foldl (\acc e -> ((fromJust (nodeAction e)), (nodeState e)) : acc) [] real_parents
    real_parents = takeWhile (\e -> (nodeDepth e) > 0) (iterate (\e -> (fromJust (nodeParent e))) goalNode) 