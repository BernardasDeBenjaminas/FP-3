module Game 
( getGameMsg
) where 

import Parser
import Data.List
import Data.Char


myShips = 
    [   "A3", "B3", "C3", "B4", "B5",  
        "A8", "B8", "C8", "B9", "B10",
        "D5", "E5", "F5", "E6", "E7",
        "F1", "G1", "H1", "G2", "G3",
        "H8", "I8", "J8", "I9", "I10"   ]

-------------------------------------------------------
------------------------- MAIN ------------------------
-------------------------------------------------------

getGameMsg :: String -> Either String String
getGameMsg message = do
    case getShots message of
        Left e0 -> Left e0
        Right (xShots, yShots) ->
            let 
                yourShots = xShots
                myShots   = yShots
                -- Know which shots belong to which player
                yourCoords =  reverse $ filterCoords yourShots []
                yourResults = reverse $ filterResults yourShots []
                myCoords =    reverse $ filterCoords myShots []
                myResults =   reverse $ filterResults myShots []
                unknown =     fst $ head yourShots
            in
                -- " -1" means the player has lost
                if elem " -1" yourCoords 
                then Right "END" 
                else
                    case updateGame [] yourCoords yourResults myShips myCoords myResults of
                    Left e1 -> Left e1
                    Right ('W':'O':'N':_)       -> Right "I WON, EVEN WHEN YOU WERE CHEATING"
                    Right ('L':'O':'S':'T':_)   -> Right $ getPostMessage [] (didYouHit unknown) message
                    Right ('O':'K':_)           -> Right $ getPostMessage (whereToShoot myCoords) (didYouHit unknown) message


updateGame :: [String] -> [String] -> [String] -> [String] -> [String] -> [String] -> Either String String
updateGame yourShips yourShots yourRes myShips myShots myRes =
    case updateMyShips myShips yourShots yourRes of
    Left e0 -> Left e0
    Right myShips' ->
        if null myShips' 
        then Right "LOST"
        else case updateYourShips yourShips myShots myRes of
        Left e1 -> Left e1
        Right yourShips' ->
            if length yourShips' == 25
            then Right "WON"
            else Right "OK"

updateYourShips :: [String] -> [String] -> [String] -> Either String [String]
updateYourShips ships [] [] = Right ships
updateYourShips ships (hs:ts) (hr:tr)
    | elem hs ships == True  && hr == "HIT"   = Left "I've tried to hit the same ship twice."
    | elem hs ships == True  && hr == "MISS"  = Left "I've shot at the same ship, but this time I've missed."
    | elem hs ships == False && hr == "HIT"   = updateYourShips (hs:ships) ts tr
    | elem hs ships == False && hr == "MISS"  = updateYourShips ships ts tr
    | otherwise = Left $ "Contains? " ++ (show (elem hs ships)) ++ "  Result? " ++ hr

updateMyShips :: [String] -> [String] -> [String] -> Either String [String]
updateMyShips ships [] [] = Right ships
updateMyShips ships (hs:ts) (hr:tr)
    | elem hs ships == True  && hr == "HIT"   = updateMyShips (delete hs ships) ts tr
    | elem hs ships == True  && hr == "MISS"  = Left "Something's not right, you've actually hit me."
    | elem hs ships == False && hr == "HIT"   = Left "Liar, I don't have a ship there"
    | elem hs ships == False && hr == "MISS"  = updateMyShips ships ts tr
    | hr == "??" = if hs `elem` ships then Right $ delete hs ships else Right ships
    | otherwise = Left $ "Contains? " ++ (show (hs `elem` ships)) ++ "  Result? " ++ hr

-- -----------------------------------------------------
-- --------------------- HELPERS -----------------------
-- -----------------------------------------------------

didYouHit :: String -> String
didYouHit coords = if coords `elem` myShips then "HIT" else "MISS"

whereToShoot :: [String] -> String
whereToShoot myShots = head ([[col] ++ (show row) | col <- ['A'..'J'], row <- [1..10]] \\ myShots)

filterCoords :: [(String, String)] -> [String] -> [String]
filterCoords (h:t) acc = filterCoords t $ acc ++ [fst h]
filterCoords [] acc = acc

filterResults :: [(String, String)] -> [String] -> [String]
filterResults (h:t) acc = filterResults t $ acc ++ [snd h]
filterResults [] acc = acc

-- -----------------------------------------------------
-- ----------------------- HTTP ------------------------
-- -----------------------------------------------------

getPostMessage :: String -> String -> String -> String
getPostMessage coords result previous = coord ++ res ++ prev
    where coord = if null coords
                  then "[\"coord\",[]," -- we lost
                  else "[\"coord\",[\"" ++ [head coords] ++ "\"," ++ (show $ tail coords) ++ "],"
          res = "\"result\",\"" ++ result ++ "\","
          prev = "\"prev\"," ++ previous ++ "]"