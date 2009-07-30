
-- Computer player for the game "Ghost"
-- (C) Paul Butler 2009

module Ghost where

import Data.List
import Char

-- shortestWord:
-- Number of characters for a word to be considered a word

shortestWord = 4

-- dictFile:
-- Location of dictionary file
dictFile = "/usr/share/dict/words"

-- We start by building a prefix tree (trie) of words in a dictionary file.

data Trie = Node [(Char, Trie)] | Word
  deriving Show

emptyTrie = Node []

-- applyAssoc: takes an associative list, a key, a null value, and
-- a transform function. If they key is found in the list, returns
-- the list with the transform function applied to the value found.
-- Otherwise, adds the null value as the element for the given key
-- and applies the transform function to that value.
-- > applyAssoc [] 'a' 4 (+ 3)
-- [('a',7)]

-- > applyAssoc [('a', []), ('b', []), ('c', [])] 'b' [] (1:)
-- [('a',[]),('b',[1]),('c',[])]

applyAssoc :: (Eq a) => [(a,b)] -> a -> b -> (b -> b) -> [(a,b)]

applyAssoc ((key, value) : rest) match null function
    | key == match      = (key, function value) : rest
    | otherwise         = (key, value) : (applyAssoc rest match null function)

applyAssoc [] match null function =
    [(match, function null)]

-- addWord: given a trie and a word suffix, add the word suffix to the trie.

-- Because of the rules of Ghost, each path down the trie must end
-- in exactly one word. If we encounter a word which contains another
-- word as a prefix, we ignore the longer word. We assume that words are
-- added in dictionary order, so we don't have to deal with the case of
-- the shorter (prefix) word being added *after* the longer word, only
-- the case of the shorter word being added before the longer one.

-- addWord :: Trie -> String -> Trie
addWord :: String -> Trie -> Trie

-- If we get to the last letter of the word, add a Word node to the tree
-- > addWord "a" emptyTrie
-- Node [('a',Word)]

addWord [char] (Node childs)
    = Node $ (char, Word) : childs

-- For any other letter, add a Node
-- > addWord "ab" emptyTrie
-- Node [('a',Node [('b',Word)])]

addWord (c:chars) (Node childs)
    = Node (applyAssoc childs c emptyTrie (addWord chars))

-- If we hit a Word end, stop trying to add the word to the tree
-- > addWord "abc" (Node [('a',Node [('b',Word)])])
-- Node [('a',Node [('b',Word)])]

addWord _ (Word)
    = Word

-- buildTrie: given a list of words, build a list of tries
-- that contains those words.
-- > buildTrie ["abc", "bad", "car", "aba"]
-- Node [('a',Node [('b',Node [('a',Word),('c',Word)])]),('b',Node [('a',Node [('d',Word)])]),('c',Node [('a',Node [('r',Word)])])]

buildTrie :: [String] -> Trie
buildTrie = foldl (flip addWord) emptyTrie

-- validWord: determine if a string is a valid word.
-- Words are at least `shortestWord` long and consist entirely of
-- lowercase ASCII characters.
-- > validWord "tests"
-- True

-- > validWord "a"
-- False

-- > validWord "Amazon"
-- False

-- > validWord "shrink<"
-- False

validWord :: String -> Bool
validWord w =
    (all isLower w) &&
    (all isAscii w) &&
    (shortestWord < length w)

data Move = Choose Char | Challenge | Forfeit
    deriving Show

-- makeMove:
-- Given a trie, determine the next move.
-- > makeMove Word
-- Challenge

-- > makeMove (buildTrie ["a", "aa"])
-- Forfeit

-- > makeMove (buildTrie ["a", "bb"])
-- Choose 'b'

-- > makeMove (buildTrie ["lter", "ter"])
-- Choose 'l'

-- > makeMove (buildTrie ["a", "b"])
-- Forfeit

-- > makeMove (buildTrie ["fink", "flunk"])
-- Forfeit

-- > makeMove (buildTrie ["ab", "fink", "flunk"])
-- Choose 'a'

makeMove :: Trie -> Move
makeMove Word = Challenge
makeMove (Node childs) =
    case (filter (not . canForceWin . snd) childs) of
        ((l,_):_)   -> Choose l
        _           -> Forfeit

-- canForceWin:
-- Given a trie, determine if the
-- player whose turn it is (ie. the player who must choose
-- a child from that trie) can force a win.

-- If the trie is Word, the last player has just completed
-- a word and so the current player wins.
-- > canForceWin Word
-- True

-- > canForceWin (buildTrie ["a", "aa"])
-- False

-- > canForceWin (buildTrie ["a", "bb"])
-- True

-- > canForceWin (buildTrie ["walter", "water"])
-- True

-- > canForceWin (buildTrie ["a", "b"])
-- False

-- > canForceWin (buildTrie ["aa", "bb"])
-- True

-- > canForceWin (buildTrie ["fink", "flunk"])
-- False

-- > canForceWin (buildTrie ["fink", "flunk", "ab"])
-- True

-- > canForceWin (buildTrie ["babble", "babbling"])
-- True

canForceWin :: Trie -> Bool

canForceWin Word
    = True

canForceWin (Node childs)
    = any (not . canForceWin . snd) childs

-- prefixDive
-- > prefixDive "ab" (buildTrie ["abc", "bad", "car", "aba"])
-- Just (Node [('a',Word),('c',Word)])

-- > prefixDive "foo" (buildTrie ["abc", "bad", "car", "aba"])
-- Nothing

prefixDive :: String -> Trie -> Maybe Trie
prefixDive "" trie = Just trie
prefixDive (c:chars) (Node childs) =
    (lookup c childs) >>= (prefixDive chars)

-- readDict: read a list of strings from a file.

readDict :: IO [String]
readDict = do
    words <- readFile dictFile
    return (lines words)

-- loadDict:
-- Load a dictionary into a trie data structure.

loadDict :: IO Trie
loadDict = do
    words <- readDict
    words <- return $ filter validWord words
    trie <- return $ buildTrie words
    return trie

-- anyWord:
-- return any word from a trie node.
-- > anyWord (buildTrie ["test", "twist"])
-- "test"

anyWord Word = ""
anyWord (Node ((c, n) : _)) = c : (anyWord n)

playGame :: Trie -> String -> IO ()
playGame (Node childs) prefix = do
    putStr $ prefix ++ "? "
    uc <- getChar
    putStrLn ""
    case (lookup uc childs) of
        Just (Word) -> putStrLn $ "You spelled the word " ++ prefix ++ [uc]
        Just (Node n) -> case (makeMove (Node n)) of
            Choose cc -> do
                putChar cc
                putStrLn ""
                case (lookup cc n) of
                    Just n -> playGame n (prefix ++ [uc, cc])
                    Nothing -> do
                        error "Shouldn't get here"
            Forfeit ->
                do
                putStrLn $ show $ n
                putStrLn "I give up!"
        Nothing -> putStrLn $
            "Challenge! No word starts with " ++ prefix ++ [uc] ++
            " (I was thinking of \"" ++ prefix  ++ (anyWord $ Node childs) ++ "\")"

initGame :: IO ()
initGame = do
    dict <- loadDict
    playGame dict ""

