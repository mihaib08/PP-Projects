{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List

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

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {
                    nL :: Int,
                    nC :: Int,
                    board :: [[Char]],
                    hunter :: Position,
                    targets :: [Target],
                    gateways :: [(Position, Position)]
                 }
                deriving (Eq, Ord)
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
gameAsString :: Game -> String
gameAsString = intercalate "\n" . board

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
emptyGame l c = (Game l
                     c
                     (bld 0 c)
                     (1, 1)
                     []
                     [])
                     where
                         nl = l

                         bld :: Int -> Int -> [[Char]]
                         bld 0 m = (getObs m) : (bld 1 m)
                         bld 1 m = ("@!" ++ (getEmpt (m - 3)) ++ "@") : (bld 2 m)
                         bld k m = if (k == nl - 1) then [getObs m]
                                                else ("@" ++ (getEmpt (m - 2)) ++ "@") : (bld (k + 1) m)
                         
                         getObs ct = replicate ct '@'
                         getEmpt ct = replicate ct ' '
                    

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}

validPos :: Int -> Int -> Position -> Bool
validPos n m (x, y) = (x >= 0 && x <= n) && (y >= 0 && y <= m)

getElem :: Position -> [[Char]] -> Char
getElem (x, y) b = head $ drop y $ take (y + 1) line
                            where
                                line = head $ drop x $ take (x + 1) b

posHunter :: Position -> Position -> Position
posHunter curr_pos pos = if (pos == curr_pos) then (-1, -1) -- se suprapune
                                              else curr_pos

placeElem :: [Char] -> Position -> [[Char]] -> [[Char]]
placeElem c (x, y) b = take x b ++
                           getPos c line ++
                           drop (x + 1) b
                                where
                                    line = head $ drop x $ take (x + 1) b
                                    getPos p curr_line = [take y curr_line ++
                                                          p ++
                                                          drop (y + 1) curr_line]

addHunterHelper :: Position -> Game -> Game
addHunterHelper pos g = if (pos_elem == ' ') then g {
                                                hunter = pos,
                                                board = if (curr_hunter == (-1, -1))
                                                        then placeElem "!"
                                                                  pos
                                                                  (board g)
                                                        else placeElem "!"
                                                                  pos
                                                                  (placeElem " " curr_hunter (board g))
                                              }
                                       else g
                                where
                                    pos_elem = getElem pos (board g)
                                    curr_hunter = hunter g

addHunter :: Position -> Game -> Game
addHunter pos g = if (validPos n m pos) then addHunterHelper pos g
                                        else g
                                where
                                    n = nL g
                                    m = nC g
                                        

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}

addTargetHelper :: Target -> Position -> Game -> Game
addTargetHelper tgt pos g = g {
                                hunter = posHunter h_pos pos,
                                targets = tgt : (targets g),
                                board = placeElem "*"
                                pos
                                (board g)
                              }
                                    where
                                        h_pos = hunter g


addTarget :: Behavior -> Position -> Game -> Game
addTarget bh pos g = if (validPos n m pos) then addTargetHelper tgt pos g
                                           else g
                                    where
                                        n = nL g
                                        m = nC g
                                        tgt = Target pos bh

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}

getHunterPos :: Position -> Position -> Position -> Position
getHunterPos curr_pos p1 p2 = if (p1 == curr_pos || p2 == curr_pos) then (-1, -1)
                                                                    else curr_pos

addGatewayHelper :: Position -> Position -> Game -> Game
addGatewayHelper g_in g_out g = g {
                                    hunter = getHunterPos h_pos g_in g_out,
                                    gateways = (g_out, g_in) : (g_in, g_out)
                                                             : (gateways g),
                                    board = placeElem "#"
                                                      g_out
                                                      placeIn
                                  }
                                            where
                                                b = board g

                                                h_pos = hunter g

                                                placeIn = placeElem "#" g_in b

addGateway :: (Position, Position) -> Game -> Game
addGateway (gate_in, gate_out) g = if ((validPos n m gate_in) &&
                                        (validPos n m gate_out)) then addGatewayHelper gate_in
                                                                                          gate_out
                                                                                          g
                                                                 else g
                                                    where
                                                        n = nL g
                                                        m = nC g

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}

addObstacleHelper :: Position -> Game -> Game
addObstacleHelper pos g = g {
                                hunter = posHunter h_pos pos,
                                board = placeElem "@"
                                                  pos
                                                  (board g)
                            }
                                    where
                                        h_pos = hunter g

addObstacle :: Position -> Game -> Game
addObstacle pos g = if (validPos n m pos) then addObstacleHelper pos g
                                          else g
                                    where
                                        n = nL g
                                        m = nC g

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

getOutGate :: Position -> [(Position, Position)] -> Maybe Position
getOutGate _ [] = Nothing
getOutGate pos (gate : gates) = if (g_in == pos) then (Just g_out)
                                                 else getOutGate pos gates
                                            where
                                                g_in = fst gate
                                                g_out = snd gate

attemptMove :: Position -> Game -> Maybe Position
attemptMove pos g = if (pos_elem == '@') then Nothing
                                         else
                                             if (pos_elem == '#') then getOutGate pos gates
                                                                  else (Just pos)
                                                    where
                                                        pos_elem = getElem pos b
                                                        b = board g
                                                        gates = gateways g

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

isGateway :: Position -> [(Position, Position)] -> Bool
isGateway _ [] = False
isGateway pos (gate : gates) = if (pos == (fst gate)) then True
                                                      else isGateway pos gates

isObstacle :: Position -> [[Char]] -> Bool
isObstacle pos b = (el == '@')
                            where el = getElem pos b

createTarget :: Position -> Behavior -> Target
createTarget pos bhv = Target pos bhv

checkGate :: Position -> Behavior -> Game -> Target
checkGate pos bhv g = case (getOutGate pos (gateways g)) of
        Nothing -> createTarget pos bhv
        Just dst -> createTarget dst bhv

checkMove :: Position -> Position -> Game -> Behavior -> Target
checkMove src
          dst
          g bhv = if ((validPos n m dst) && not(isObstacle dst b)) then checkGate dst bhv g
                                                                   else checkGate src bhv g
                                                            where
                                                                n = nL g
                                                                m = nC g
                                                                b = board g

goEast :: Behavior
goEast pos@(x, y) g = checkMove pos (x, y + 1) g goEast

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest pos@(x, y) g = checkMove pos (x, y - 1) g goWest

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth pos@(x, y) g = checkMove pos (x - 1, y) g goNorth

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth pos@(x, y) g = checkMove pos (x + 1, y) g goSouth

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
bounce d pos@(x, y) g = if ((validPos n m dst) && not(isObstacle dst b)) then checkGate dst
                                                                                        (bounce d)
                                                                                        g
                                                                         else
                                                                             if (not(isObstacle dst_aux b)) then checkGate dst_aux
                                                                                                                           (bounce rev_d)
                                                                                                                           g
                                                                                                            else checkGate pos
                                                                                                                           (bounce d)
                                                                                                                           g
                                                        where
                                                            n = nL g
                                                            m = nC g
                                                            b = board g

                                                            dst = (x + d, y)
                                                            dst_aux = (x - d, y)

                                                            rev_d = -d

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}

-- updateBoard :: [Target] -> Board -> new_Board
updateBoard :: [Target] -> [[Char]] -> [[Char]]
updateBoard [] b = b
updateBoard (t : ts) b = updateBoard ts new_b
                            where
                                new_b = placeElem "*"
                                                  (position t)
                                                   b

redoBoard :: [Target] -> [[Char]] -> [(Position, Position)] -> [[Char]]
redoBoard [] b _ = b
redoBoard (t : ts) b gates = redoBoard ts new_b gates
                            where
                                pos_t = position t
                                new_b = if (isGateway pos_t gates) then placeElem "#"
                                                                                  pos_t
                                                                                  b
                                                                   else placeElem " "
                                                                                  pos_t
                                                                                  b

moveTargets :: Game -> Game
moveTargets g = g { 
                    board = updateBoard new_targets (redoBoard curr_targets b gates),
                    targets = new_targets
                  }
                    where
                        b = board g
                        curr_targets = targets g
                        gates = gateways g

                        new_targets = map (\t -> (behavior t) (position t) g) curr_targets

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
isTargetKilled (hx, hy) tgt =  (hx - 1 == tx && hy == ty) ||
                               (hx == tx && hy + 1 == ty) ||
                               (hx + 1 == tx && hy == ty) ||
                               (hx == tx && hy - 1 == ty)
                            where 
                                (tx, ty) = position tgt


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

getDirection :: Position -> Direction -> Position
getDirection (x, y) North  = (x - 1, y)
getDirection (x, y) East   = (x, y + 1)
getDirection (x, y) South  = (x + 1, y)
getDirection (x, y) West   = (x, y - 1)

moveHunterHelper :: Position -> Position -> Game -> Game
moveHunterHelper src dst g = g {
                                 board = placeElem "!"
                                                    dst
                                                    new_b,
                                 hunter = dst
                               }
                            where
                                b = board g
                                new_b = if (isGateway src gates) then placeElem "#"
                                                                                src
                                                                                b
                                                                 else placeElem " "
                                                                                src
                                                                                b
                                gates = gateways g

moveHunter :: Position -> (Maybe Position) -> Game -> Game
moveHunter src dst g = case dst of
    Nothing -> g
    Just pos -> moveHunterHelper src pos g

imgMove :: Position -> Position -> Game -> Game
imgMove src dst g = if (validPos n m dst) then moveHunter src next_dst g
                                          else g
                                    where
                                        n = nL g
                                        m = nC g

                                        next_dst = attemptMove dst g

delTarget :: Position -> [Target] -> [Target]
delTarget _ [] = []
delTarget tpos (t : ts) = if (tpos == (position t)) then ts
                                                    else t : (delTarget tpos ts)

delAdjacent :: Position -> Game -> Game
delAdjacent pos g = g {
                        board = new_b,
                        targets = delTarget pos ts
                      }
                      where
                          n = nL g
                          m = nC g
                          ts = targets g
                          gates = gateways g
                          b = board g

                          new_b = if ((validPos n m pos) && not(isObstacle pos b)) then 
                                                    if (isGateway pos gates) then placeElem "#"
                                                                                  pos
                                                                                  b
                                                                             else placeElem " "
                                                                                  pos
                                                                                  b
                                          else b

elimTargets :: Game -> Game
elimTargets g = delAdjacent (hx, hy - 1) $ 
                delAdjacent (hx + 1, hy) $ 
                delAdjacent (hx, hy + 1) $ 
                delAdjacent (hx - 1, hy) g
                        where
                            (hx, hy) = hunter g

realMove :: Position -> Position -> Game -> Game
realMove src dst g = if (validPos n m dst) then elimTargets new_g
                                           else elimTargets g
                                    where
                                        n = nL g
                                        m = nC g

                                        next_dst = attemptMove dst g

                                        new_g = moveHunter src next_dst g

moveProc :: Position -> Position -> Game -> Game
moveProc src dst g = elimTargets (moveTargets new_g)
                        where
                            new_g = realMove src dst g -- ?? g / elimTargets g

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir ok g = if (ok == False) then imgMove hpos dst g
                                             else moveProc hpos dst g
                                        where
                                            dst = getDirection hpos dir

                                            hpos = hunter g

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft g = length (targets g) > 0

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}

-- Behavior = Position -> Game -> Target
circle :: Position -> Int -> Behavior
circle cpos r pos g = createTarget next_pos (circle cpos r)
                        where
                            next_pos = getNextPos cpos pos

                            getNextPos (cx, cy) (x, y) 
                                | y == cy  = if (cx > x) then (x + r, cy - r)
                                                         else (x - r, cy + r)
                                | x == cx  = if (cy > y) then (cx + r, y + r)
                                                         else (cx - r, y - r)


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors g = map (\d -> (d, adv d)) dirs
                        where 
                            dirs = [North, South, East, West]
                            adv d = advanceGameState d False g

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal g = foldr (\t acc -> (isTargetKilled hpos t) || acc) False ts
                        where
                            hpos = hunter g
                            ts = targets g

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h g = captureTarget hpos ts
            where
                hpos = hunter g
                ts = targets g

                captureTarget _ [] = 0.0
                captureTarget pos (x : xs) = if (isTargetKilled pos x) then hEuclidean pos (position x)
                                                                       else captureTarget pos xs

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

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
    successors (BonusGame g) = fmap (\(act, st) -> (act, BonusGame st)) succs
                                    where
                                        succs = successors g

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal (BonusGame g) = foldr (\t acc -> (isTargetKilled hpos t) || acc) False ts
                                where
                                    hpos = hunter g
                                    ts = targets g

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h (BonusGame g) = captureTarget hpos ts
            where
                hpos = hunter g
                ts = targets g

                -- captureTarget _ [] mini = mini
                -- captureTarget pos (x : xs) mini = if ((myH pos (position x)) < mini) then captureTarget pos xs (myH pos (position x))
                --                                                                      else captureTarget pos xs mini
                captureTarget pos xs = minimum $ map ((myH pos) . position) xs

                myH pos1 pos2 = throughGates pos1 pos2 (hEuclidean pos1 pos2) (gateways g)
                
                throughGates p1 p2 mini [] = mini
                throughGates p1 p2 mini (gt : gts)
                        | mini < getThrough = throughGates p1 p2 mini gts
                        | otherwise = throughGates p1 p2 getThrough gts
                            where
                                getThrough = ((hEuclidean p1 (fst gt)) + (hEuclidean p2 (snd gt)))
