module Parser 
(     Shots
    , Shot
    , getShots
    , getRealPrevious
) where 

  import Data.Char
  import Data.List


  --------------------------------
  ------------- MAIN -------------
  --------------------------------

  -- Žaidimo (galbūt nebaigto) rezultatas (kiek HIT'ų padarė kiekvienas žaidėjas)
  score :: String -> Either String (Int, Int)
  score game = countHits 0 (0, 0) game

  -- USE: countHits 0 (0, 0) "[\"coord\",[\"I\",\"1\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"A\",\"6\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"B\",\"4\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"E\",\"7\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]]]]]"
  -- RES: (2, 0)
  countHits :: Int -- ^ counter 
    -> (Int, Int) -- ^ initial players' HIT counts
    -> String -- game board
    -> Either String (Int, Int) -- calculated players' HIT counts
  countHits i (hits1, hits2) game =
    case parseArray game of
    Left e1 -> Left e1
    Right (letter, number, result, previous) ->
      if isPrefixOf "null" previous 
      then Right (hits1, hits2)
      else 
        if result /= "HIT" 
        then countHits (i+1) (hits1, hits2) previous
        else
          if isEven i 
          then countHits (i+1) (hits1 + 1, hits2) previous
          else countHits (i+1) (hits1, hits2 + 1) previous

  --------------------------------
  ----------- PARSING ------------
  --------------------------------

  type Shots = ([Shot], [Shot])
  type Shot = (String, String) -- "A4" && "MISS" 

  getShots :: String -> Either String Shots
  getShots game = 
    case areBracketsValid game of
    Left e0 -> Left e0
    Right message ->
      case getShots' 0 "??" message $ Right ([], []) of
      Left e -> Left e
      Right (shots1, shots2) -> Right (shots1, shots2)

  getShots' ::  Int -- ^ index (differentiate between players)
          -> String -- ^ result (did the last shot stick?)
          -> String -- ^ rest of the message (so we could continue parsing)
          -> Either String Shots -- ^ the accumulator
          -> Either String Shots
  getShots' _ _ _  (Left error) = Left error
  getShots' _ _ [] (Right (p1, p2)) = Right (p1, p2)
  getShots' i prevHit game (Right (p1, p2)) = 
    case parseArray game of
    Left ('E':'O':'F':r) -> Right (p1, p2)
    Left e -> Left e
    Right (col, row, result, rest) ->
      if isEven i 
      then getShots' (i + 1) result rest $ Right (p1 ++ [([col] ++ show row, prevHit)], p2)
      else getShots' (i + 1) result rest $ Right (p1, p2 ++ [([col] ++ show row, prevHit)])
  


  -- USE: parseArray "[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]"
  -- RES: Right ('C', 8, "null", "null]")
  parseArray :: String -> Either String (Char, Int, String, String)
  parseArray array = 
      case getCoordinates array of 
      Left e1 -> Left e1
      Right (letter, number, rest1) ->
        case getShotResult rest1 of 
          Left e2 -> Left e2
          Right (result, rest2) -> 
            case getPrevious rest2 of
            Left e3 -> Left e3
            Right previous -> Right (letter, number, result, previous)

  -- USE: getCoordinates "[\"coord\",[\"E\",\"7\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]]"
  -- RES: Right ('E',7,"\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]]")
  getCoordinates :: String -> Either String (Char, Int, String)
  getCoordinates ('n':'u':'l':'l':rest) = Left "EOF"
  getCoordinates ('[':'"':'c':'o':'o':'r':'d':'"':',':'[':'"':letter:'"':',':'"':digit1:digit2:'"':']':',':rest)
    | isProperLetter letter == False = Left "'coord' must contain a letter from A to J"
    | isDigit digit1  == False       = Left "'coord' must contain a valid digit"
    | digit2 /= '0'                  = Left "'coord' must contain a number lower than 11"
    | True                           = Right (letter, read [digit1, digit2], rest)
  getCoordinates ('[':'"':'c':'o':'o':'r':'d':'"':',':'[':'"':letter:'"':',':'"':digit:'"':']':',':rest)
    | isProperLetter letter == False = Left "'coord' must contain a letter from A to J"
    | isDigit digit  == False        = Left "'coord' must contain a valid digit"
    | True                           = Right (letter, digitToInt digit, rest)
  getCoordinates ('[':'"':'c':'o':'o':'r':'d':'"':',':'[':']':',':rest) = Right (' ', -1, rest)
  getCoordinates _ = Left "'coord' can only contain a letter and a number"

  -- USE: getShotResult "\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]]"
  -- RES: Right ("MISS","\"prev\",[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]]")
  getShotResult :: String -> Either String (String, String)
  getShotResult ('"':'r':'e':'s':'u':'l':'t':'"':',':rest)
      | isPrefixOf "null,"    rest = Right ("null", drop 5 rest)
      | isPrefixOf "\"HIT\""  rest = Right ("HIT",  drop 6 rest)
      | isPrefixOf "\"MISS\"" rest = Right ("MISS", drop 7 rest)
      | True = Left "'result' can only contain: HIT MISS null"
  getShotResult _ = Left "'result' key not found"

  -- USE: getPrevious "\"prev\",[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]]"
  -- RES: Right "[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]]"
  getPrevious :: String -> Either String String
  getPrevious ('"':'p':'r':'e':'v':'"':',':rest) = Right rest
  getPrevious _ = Left "'prev' key not found"

  getRealPrevious :: String -> String
  getRealPrevious message = init withLastBracket
    where count = (6+) $ length $ takeWhile (\n -> n /= 'p') message
          withLastBracket = drop count message

  --------------------------------
  ----------- HELPERS ------------
  --------------------------------

  -- USE: areBracketsValid "[\"coord\",[\"E\",\"7\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]]"
  -- RES: [\"coord\",[\"E\",\"7\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]]"
  -- ? Intruder is checked by seeing if the ending bracket sequence starts with the word 'null'
  areBracketsValid :: String -> Either String String
  areBracketsValid msg
    | numOpen /= numClose  = Left "Number of '[' must match the number of ']'"
    | startsNull == False  = Left "Ending bracket sequence has an intruder"
    | otherwise            = Right msg
      where numOpen  = length $ [x | x <- msg, x == '[']
            numClose = length $ [x | x <- msg, x == ']']
            reversed = reverse msg
            count = length $ takeWhile (\x -> x == ']') reversed
            startsNull = isPrefixOf "llun" $ drop count reversed

  -- USE: isProperLetter 'A'
  -- RES: True
  isProperLetter :: Char -> Bool
  isProperLetter l = l `elem` ['A'..'J']

  -- USE: isEven 3
  -- RES: False
  isEven :: Int -> Bool
  isEven number = 
    if number `mod` 2 == 0 
      then True
    else False

    
  --------------------------------
  ---------- TEST DATA -----------
  --------------------------------

-- 1 move
-- ["coord",["C","8"],"result",null,"prev",null]
-- "[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]"

-- 2 moves
-- ["coord",["E","7"],"result","MISS","prev",["coord",["C","8"],"result",null,"prev",null]]
-- "[\"coord\",[\"E\",\"7\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]]"

-- 5 moves
-- ["coord",["I","1"],"result","MISS","prev",["coord",["A","6"],"result","MISS","prev",["coord",["B","4"],"result","MISS","prev",["coord",["E","7"],"result","MISS","prev",["coord",["C","8"],"result",null,"prev",null]]]]]
-- "[\"coord\",[\"I\",\"1\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"A\",\"6\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"B\",\"4\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"E\",\"7\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]]]]]"
-- "[\"coord\",[\"I\",\"1\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"A\",\"6\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"B\",\"4\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"E\",\"7\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"8\"],\"result\",null,\"prev\",null]]]]]"

-- Whole message
-- ["coord",["D","8"],"result","HIT","prev",["coord",["F","3"],"result","MISS","prev",["coord",["G","8"],"result","MISS","prev",["coord",["A","2"],"result","MISS","prev",["coord",["J","8"],"result","MISS","prev",["coord",["C","6"],"result","MISS","prev",["coord",["D","2"],"result","MISS","prev",["coord",["E","9"],"result","HIT","prev",["coord",["I","3"],"result","MISS","prev",["coord",["J","4"],"result","MISS","prev",["coord",["A","7"],"result","MISS","prev",["coord",["A","5"],"result","MISS","prev",["coord",["E","3"],"result","MISS","prev",["coord",["B","2"],"result","MISS","prev",["coord",["J","1"],"result","MISS","prev",["coord",["G","4"],"result","HIT","prev",["coord",["F","9"],"result","MISS","prev",["coord",["E","4"],"result","HIT","prev",["coord",["A","10"],"result","HIT","prev",["coord",["I","9"],"result","MISS","prev",["coord",["I","1"],"result","MISS","prev",["coord",["A","6"],"result","MISS","prev",["coord",["B","4"],"result","MISS","prev",["coord",["E","7"],"result","MISS","prev",["coord",["C","8"],"result",null,"prev",null]]]]]]]]]]]]]]]]]]]]]]]]]



-- ["coord",["A","1"],"result","MISS","prev",["coord",["D","3"],"result","HIT","prev",["coord",["C","3"],"result","MISS","prev",["coord",["E","3"],"result","MISS","prev",["coord",["C","8"],"result","MISS","prev",["coord",["F","1"],"result","MISS","prev",["coord",["A","3"],"result","MISS","prev",["coord",["D","7"],"result","MISS","prev",["coord",["C","9"],"result","MISS","prev",["coord",["J","9"],"result","MISS","prev",["coord",["I","10"],"result","MISS","prev",["coord",["H","6"],"result","MISS","prev",["coord",["B","6"],"result",null,"prev",null]]]]]]]]]]]]]
-- "[\"coord\",[\"A\",\"1\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"D\",\"3\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"C\",\"3\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"E\",\"3\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"8\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"F\",\"1\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"A\",\"3\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"D\",\"7\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"9\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"J\",\"9\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"I\",\"10\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"H\",\"6\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"B\",\"6\"],\"result\",null,\"prev\",null]]]]]]]]]]]]]"