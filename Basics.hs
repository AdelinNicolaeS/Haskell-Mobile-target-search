{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
--import Data.List
import Data.Maybe
--import Data.List.Split


{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

data Cell = Hunt | Targ | Obscta | Gate | Space
    deriving (Eq, Ord)

instance Show Cell
    where 
        show Hunt = "!"
        show Targ = "*"
        show Obscta = "@"
        show Gate = "#"
        show Space = " " 

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {
    targets :: [Target],
    hunters :: [Position],
    obstacles :: [Position],
    gateways :: [(Position, Position)],
    coloane :: Int,
    linii :: Int
} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

posToCell :: [Position] -> [Target] -> [Position] -> [(Position, Position)] -> Int -> Position -> String
posToCell hu t o g col pos@(_,b) 
    | elem pos hu && b == (col - 1) = "!\n"
    | elem pos hu = "!"
    | elem pos o && b == (col - 1) = "@\n"
    | elem pos o = "@"
    | elem pos (map (\(Target poz _) -> poz) t) && b == (col - 1) = "*\n"
    | elem pos (map (\(Target poz _) -> poz) t) = "*"
    | elem pos [a | (a, _) <- g] && b == (col - 1) = "#\n"
    | elem pos [a | (a, _) <- g] = "#"
    | b == (col - 1) = " \n"
    | otherwise = " "


gameAsString :: Game -> String
gameAsString (Game t hu o g col lin) = take (length final_string - 1) final_string
    where
        final_string = concatMap (posToCell hu t o g col) all_positions
        all_positions = [(a, b) | a <- [0..(lin - 1)], b <- [0..(col - 1)]]

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame x y = (Game t hu o g col lin)
      where
          t = []
          hu = [(1, 1)]
          o = [(0, a) | a <- [0..(y - 1)]] ++ [(a, (y - 1)) | a <- [1..(x - 1)]] ++ [((x - 1), a) | a <- [0..(y - 2)]] ++ [(a, 0) | a <- [1..(x - 2)]] 
          g = []
         -- s = [(1, a) | a <- [2..(y-2)]] ++ [(a, b) | a <- [2..(x-2)], b <- [1..(y-2)]]
          col = y
          lin = x

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter pos@(x, y) (Game t hu o g col lin) 
      | x < 0 || x > (lin - 1) || y < 0 || y > (col - 1) = (Game t hu o g col lin)
      | elem pos (map (\(Target poz _) -> poz) t) || elem pos hu || elem pos o || elem pos [a | (a, _) <- g] = (Game t hu o g col lin)
      | otherwise = (Game t [pos] o g col lin)
{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Target-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behav pos@(x, y) (Game t hu o g col lin)
       | x < 0 || x > (lin - 1) || y < 0 || y > (col - 1) = (Game t hu o g col lin)
       -- | elem pos s == False = (Game t hu o g s col lin)
       | otherwise = (Game (t ++ [(Target pos behav)]) hu o g col lin)

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway pereche@(pos1@(x1, y1), pos2@(x2, y2)) (Game t hu o g col lin)
      | x1 < 0 || x1 > (lin - 1) || y1 < 0 || y1 > (col - 1) = (Game t hu o g col lin)
      | x2 < 0 || x2 > (lin - 1) || y2 < 0 || y2 > (col - 1) = (Game t hu o g col lin)
      -- | elem pos1 s == False || elem pos2 s == False =  (Game t hu o g s col lin)
      | otherwise = (Game t hu o (g ++ [pereche] ++ [(pos2, pos1)]) col lin)

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos@(x, y) (Game t hu o g col lin) 
      | x < 0 || x > (lin - 1) || y < 0 || y > (col - 1) = (Game t hu o g col lin)
    --  | elem pos s == False = (Game t hu o g s col lin)
      | otherwise = (Game t hu (o ++ [pos]) g col lin)   

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos (Game t hu o g _ _)
    | elem pos (map (\(Target poz _) -> poz) t) == False && elem pos hu == False && elem pos o == False && elem pos [a | (a, _) <- g] == False = Just pos
    | elem pos [a | (a, _) <- g] = Just (head [b | (_, b) <- (filter (\(pos1, _) -> pos1 == pos) g)])
    | otherwise = Nothing

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}
goEast :: Behavior
goEast pos@(x, y) game@(Game t _ _ g _ _)
      | (attemptMove (x, y + 1) game) /= Nothing = (Target (fromJust (attemptMove (x, y + 1) game)) var2)
      | elem (x, y) [a | (a, _) <- g] = (Target (fromJust (attemptMove (x, y) game)) var2)
      -- | y == (col - 1) = my_initial_target
      -- | elem (x, y+1) o = my_initial_target
      | otherwise = my_initial_target
   where
      my_initial_target@(Target _ var2) = head (filter (\(Target posi _) -> pos == posi) t)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest pos@(x, y) game@(Game t _ _ g _ _)
      | (attemptMove (x, y - 1) game) /= Nothing = (Target (fromJust (attemptMove (x, y - 1) game)) var2)
      | elem (x, y) [a | (a, _) <- g] = (Target (fromJust (attemptMove (x, y) game)) var2)
      | otherwise = my_initial_target
   where
      my_initial_target@(Target _ var2) = head (filter (\(Target posi _) -> pos == posi) t)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth pos@(x, y) game@(Game t _ _ g _ _)
      | (attemptMove (x - 1, y) game) /= Nothing = (Target (fromJust (attemptMove (x - 1, y) game)) var2)
      | elem (x, y) [a | (a, _) <- g] = (Target (fromJust (attemptMove (x, y) game)) var2)
      | otherwise = my_initial_target
   where
      my_initial_target@(Target _ var2) = head (filter (\(Target posi _) -> pos == posi) t)

gogoNorth :: Behavior
gogoNorth pos@(x, y) game@(Game t _ _ g _ _)
      | (attemptMove (x - 1, y) game) /= Nothing = (Target (fromJust (attemptMove (x - 1, y) game)) var2)
      | elem (x, y) [a | (a, _) <- g] = (Target (fromJust (attemptMove (x, y) game)) var2)
      | otherwise = (Target (fromJust (attemptMove (x + 1, y) game)) gogoSouth)
   where
      (Target _ var2) = head (filter (\(Target posi _) -> pos == posi) t)
{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth pos@(x, y) game@(Game t _ _ g _ _)
      | (attemptMove (x + 1, y) game) /= Nothing = (Target (fromJust (attemptMove (x + 1, y) game)) var2)
      | elem (x, y) [a | (a, _) <- g] = (Target (fromJust (attemptMove (x, y) game)) var2)
      | otherwise = my_initial_target
   where
      my_initial_target@(Target _ var2) = head (filter (\(Target posi _) -> pos == posi) t)

gogoSouth :: Behavior
gogoSouth pos@(x, y) game@(Game t _ _ g _ _)
      | (attemptMove (x + 1, y) game) /= Nothing = (Target (fromJust (attemptMove (x + 1, y) game)) var2)
      | elem (x, y) [a | (a, _) <- g] = (Target (fromJust (attemptMove (x, y) game)) var2)
      | otherwise = (Target (fromJust (attemptMove (x - 1, y) game)) gogoNorth)
   where
      (Target _ var2) = head (filter (\(Target posi _) -> pos == posi) t)

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce directie pos@(x, y) game@(Game t _ _ _ _ _)
     | (attemptMove (x + directie, y) game) == Nothing = if (directie == 1) then (Target (fromJust (attemptMove (x - 1, y) game)) gogoNorth) else (Target (fromJust (attemptMove (x + 1, y) game)) gogoSouth)    
     | otherwise = (Target (fromJust (attemptMove (x + directie, y) game)) var2)
    where
        (Target _ var2) = head (filter (\(Target posi _) -> pos == posi) t)

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets game@(Game t hu o g col lin) = (Game new_t hu o g col lin)
      where
          new_t = map (\(Target pos behav) -> behav pos game) t

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (x1, y1) (Target (x2, y2) _)
   | x1 == x2 && abs (y1 - y2) == 1 = True
   | y1 == y2 && abs (x1 - x2) == 1 = True
   | otherwise = False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState directie boolean game
    | boolean = advanceGameState2 directie game
    | otherwise = advanceGameState1 directie game


advanceGameState1 :: Direction -> Game -> Game
advanceGameState1 directie game@(Game t hu o g col lin) = (Game t new_hu o g col lin)
   where
    (x, y) = head hu
    new_pos
        | directie == North = (x - 1, y)
        | directie == South = (x + 1, y)
        | directie == East = (x, y + 1)
        | otherwise = (x, y - 1) 
    new_hu
        | (attemptMove new_pos game) == Nothing = hu
        | otherwise = [fromJust (attemptMove new_pos game)]


advanceGameState2 :: Direction -> Game -> Game
advanceGameState2 directie game@(Game t hu o g col lin) = (Game t_final new_hu o g col lin)
   where
    (x, y) = head hu
    new_pos
        | directie == North = (x - 1, y)
        | directie == South = (x + 1, y)
        | directie == East = (x, y + 1)
        | otherwise = (x, y - 1) 
    new_hu
        | (attemptMove new_pos game) == Nothing = hu
        | otherwise = [fromJust (attemptMove new_pos game)]
    t_after_first_remove = filter (\e -> isTargetKilled (head new_hu) e == False) t
    t_after_move = map (\(Target pos behav) -> behav pos (Game t_after_first_remove new_hu o g col lin)) t_after_first_remove
    t_final = filter (\e -> isTargetKilled (head new_hu) e == False) t_after_move

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}

areTargetsLeft :: Game -> Bool
areTargetsLeft (Game t hu _ _ _ _) = length (filter(\e -> isTargetKilled (head hu) e == False) t) /= 0

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}

circle :: Position -> Int -> Behavior
circle centru@(a, b) raza pos@(x, y) (Game t _ o _ _ _)
  | x <= a && y >= b = (Target cadran1_pos var2)
  | x >= a && y >= b = (Target cadran4_pos var2)
  | x >= a && y <= b = (Target cadran3_pos var2)
  | otherwise = (Target cadran2_pos var2)
  where
    (Target _ var2) = head (filter (\(Target posi _) -> pos == posi) t)

    cadran1_posible_destinations = [(x + 1, y), (x, y + 1), (x + 1, y + 1)]
    cadran1_destinations = filter (\e -> elem e o == False) (cadran1_posible_destinations)
    intermediar1 = filter (\e -> hEuclidean centru e > (fromIntegral (raza - 1) :: Float)) cadran1_destinations
    cadran1_pos = head (filter (\e -> abs((hEuclidean centru e) - (fromIntegral raza :: Float)) == minim1) intermediar1)
    minim1 = foldr min 10000000000000 (map (\e -> (abs((hEuclidean centru e) - (fromIntegral raza :: Float)))) intermediar1)

    cadran2_posible_destinations = [(x - 1, y), (x - 1, y + 1), (x, y + 1)]
    cadran2_destinations = filter (\e -> elem e o == False) (cadran2_posible_destinations)
    intermediar2 = filter (\e -> hEuclidean centru e > (fromIntegral (raza - 1) :: Float)) cadran2_destinations
    cadran2_pos = head (filter (\e -> abs((hEuclidean centru e) - (fromIntegral raza :: Float)) == minim2) intermediar2)
    minim2 = foldr min 10000000000000 (map (\e -> (abs((hEuclidean centru e) - (fromIntegral raza :: Float)))) intermediar2) 

    cadran3_posible_destinations = [(x, y - 1), (x - 1, y - 1), (x - 1, y)]
    cadran3_destinations = filter (\e -> elem e o == False) (cadran3_posible_destinations)
    intermediar3 = filter (\e -> hEuclidean centru e > (fromIntegral (raza - 1) :: Float)) cadran3_destinations
    cadran3_pos = head (filter (\e -> abs((hEuclidean centru e) - (fromIntegral raza :: Float)) == minim3) intermediar3)
    minim3 = foldr min 10000000000000 (map (\e -> (abs((hEuclidean centru e) - (fromIntegral raza :: Float)))) intermediar3) 

    cadran4_posible_destinations = [(x, y - 1), (x + 1, y - 1), (x + 1, y)]
    cadran4_destinations = filter (\e -> elem e o == False) (cadran4_posible_destinations)
    intermediar4 = filter (\e -> hEuclidean centru e > (fromIntegral (raza - 1) :: Float)) cadran4_destinations
    cadran4_pos = head (filter (\e -> abs((hEuclidean centru e) - (fromIntegral raza :: Float)) == minim4) intermediar4)
    minim4 = foldr min 10000000000000 (map (\e -> abs((hEuclidean centru e) - (fromIntegral raza :: Float))) intermediar4) 


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = [(North, pair1), (South, pair2), (East, pair3), (West, pair4)]
       where
        pair1 = advanceGameState North False game
        pair2 = advanceGameState South False game
        pair3 = advanceGameState East False game
        pair4 = advanceGameState West False game

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal (Game t hu _ _ _ _) = any (\e -> isTargetKilled (head hu) e) t
    --h old_game@(Game t hu _ _ _ _) = hEuclidean (head hu) (findKillTarget (head hu) t)

    h game@(Game _ hu _ _ _ _) = if (isGoal game) then 1 else hEuclidean (head hu) (999, 999)


    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

hManhattan :: Position -> Position -> Float
hManhattan (x1, y1) (x2, y2) = fromIntegral (((abs(x1 - x2)) + (abs(y1 - y2))) * ((abs(x1 - x2)) + (abs(y1 - y2))))

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors (BonusGame game) = [(North, pair1), (South, pair2), (East, pair3), (West, pair4)]
       where
        pair1 = (BonusGame (advanceGameState North False game))
        pair2 = (BonusGame (advanceGameState South False game))
        pair3 = (BonusGame (advanceGameState East False game))
        pair4 = (BonusGame (advanceGameState West False game))

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal (BonusGame (Game t hu _ _ _ _)) = any (\e -> isTargetKilled (head hu) e) t

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h (bonus@(BonusGame (Game _ hu _ _ _ _))) = if (isGoal bonus) then 1 else hManhattan (head hu) (999999, 999999)
