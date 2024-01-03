import IO
import List
import Control.Monad
import Char
import Data.Function

type Game = (Hand, Hand)
data Hand = Hand [Card]
instance Ord Hand = compare `on` handRank
data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq)
instance Ord Card where compare = compare `on` rank
type Rank = Int
data Suit = Diamonds | Clubs | Hearts | Spades deriving (Eq)
data HandRank =
      HighCard [Rank]
    | Pair Rank [Rank]
    | TwoPair Rank Rank [Rank]
    | ThreeOfAKind Rank
    | Straight Rank
    | Flush [Rank]
    | FullHouse Rank
    | FourOfAKind Rank
    | StraightFlush Rank
    deriving (Eq, Ord)

main :: IO ()
main = solve

solve = do
    contents <- readFile "poker.txt"
    putStrLn $ show (countWins (map parseGame (lines contents)))

countWins :: [Game] -> Int
countWins [] = 0
countWins (g:gs) = (if playerOneWins g then 1 else 0) + (countWins gs)

playerOneWins :: Game -> Bool
playerOneWins (h1, h2) = (handRank h1) >= (handRank h2)

handRank :: Hand -> HandRank
handRank h
    | straight && flush = StraightFlush
    | fourOfAKind = FourOfAKind
    | threeOfAKind && pair = FullHouse
    | flush = Flush
    | straight = Straight
    | threeOfAKind = ThreeOfAKind
    | length (filter (==2) rankBuckets) == 2 = TwoPair
    | pair = Pair
    | otherwise = HighCard
    where flush = isFlush h
          straight = isStraight sortedCards
          sortedCards = sortBy (compare `on` cardRank) h
          rankBuckets = map length (groupBy ((==) `on` cardRank) sortedCards)
          fourOfAKind = any (==4) rankBuckets
          threeOfAKind = any (==3) rankBuckets
          pair = any (==2) rankBuckets

isFlush :: Hand -> Bool
isFlush (c:cs) = all (((==) `on` cardSuit) c) cs

isStraight :: Hand -> Bool
isStraight [] = False
isStraight [c] = True
isStraight (c1:c2:cs) =
    if ((cardRank c1) + 1 == (cardRank c2))
       || (cardRank c1) == 14 && (cardRank c2) == 2
    then isStraight (c2:cs)
    else False

----------------------------------------------------------------------
-- PARSER
----------------------------------------------------------------------

parseGame :: String -> Game
parseGame str = (parseHand (take 15 str), parseHand (drop 15 str))

stringSplit :: Char -> String -> [String]
stringSplit c str = filter (\x->x/=" ") $ groupBy (\a b->(a == c)==(b==c)) str

parseHand :: String -> Hand
parseHand str = map parseCard (stringSplit ' ' str)

parseCard :: String -> Card
parseCard [r,s] = Card (parseRank r) (parseSuit s)

parseRank :: Char -> Rank
parseRank 'T' = 10
parseRank 'J' = 11
parseRank 'Q' = 12
parseRank 'K' = 13
parseRank 'A' = 14
parseRank c = digitToInt c

parseSuit :: Char -> Suit
parseSuit 'D' = Diamonds
parseSuit 'C' = Clubs
parseSuit 'H' = Hearts
parseSuit 'S' = Spades

