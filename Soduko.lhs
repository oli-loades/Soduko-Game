
This is a starter file for U08074 Coursework

REMEMBER THAT MARKS WILL BE AWARDED FOR TESTING SO PLEASE INCLUDE TESTING OF EVERY
FUNCTION YOU SUBMIT.


Supplied in this file:

  * Various input/output functions
  * The main functions for the SuDoku puzzle

You don't need to understand the code supplied
at the top of this file in order to do the coursework,
although you might find some of them of Interest.

Don't alter any of the code supplied at the top of
the file; instead, add/alter your own functions at the
end of the file, where indicated.

------------------------------------
 Main SuDoku Input/Output Functions
------------------------------------

> module SuDoku where
> import Data.Char
> import Grids

------------------------------------

You can run the game by typing the command

   sudoku 35

or use some other Integer. The Integer simply provides a seed for
the random number generator, in order to pick a random grid to start with.

> sudoku seed =  do
>            let st = generateInitialState seed
>            putStrLn (displaySt st)
>            game doCommand st


The function "game" runs a game with:
the given state transition function f and
the initial state st


> game :: (State -> String -> (String, State)) -> State -> IO ()
> game f st = do
>    putStrLn prompt
>    line <-  getLine
>    if quitting line
>        then return ()
>    else do
>        let (response, st') = answer f st line
>        putStrLn (response ++ "\n\n")
>        game f st'

> prompt = "Insert a number (e.g. 4@h3) or undo >> \n(\"quit\" to quit)"


------------------------------------


This tests if the user typed in the quit string

> quitting :: String -> Bool
> quitting cmd = ((strToLowerCase cmd)=="quit")


This takes all lines of the output string up to and including the
first line that equals "Bye."  It is used in the code that
terminates the Interaction

> tilGoodbye :: String -> String
> tilGoodbye = unlines . (takeWhile (not . goodbye)) . lines

bye is a string used in terminating the program, whether from
the user typing "quit" or from the game finishing.

> bye = "Bye."

Testing a string to see whether it is the same string
as that above.

> goodbye :: String -> Bool
> goodbye str = (str == bye)

answer checks to see whether the player has quit or not, before performing
the (sort of) state transition function f on the player's input and previous state

> answer :: (State -> String -> (String, State)) -> State -> String -> (String, State)
> answer f st userString
>    | quitting userString = (bye ++ "\n\n",st)
>    | otherwise           = f st userString


--------------------------------------------------
 Useful Datatypes and Functions for SuDoku Grids
--------------------------------------------------

A SuDoku grid is square, divided Into boxes.
Each box has boxSize rows and boxSize columns
(boxSize is a constant defined in the Starters module).

The size of the whole grid is the boxSize squared.
So gridSize = boxSize*boxSize  and the whole grid has
gridSize rows and gridSize columns.

So for example, in the usual size of SuDoku grid,
the boxSize is 3, and size of the SuDoku grid is 9 by 9.

("Super" SuDoku has a boxSize of 4, and a grid size
of 16 by 16, but we won't be considering large grid sizes
in this program.)

In SuDoku, the contents of a single cell can be represented
by a single character, so a row of the grid can be represented
by a list of characters, and therefor a suitable datatype for
the whole grid is

> type SuDokuGrid = [[Char]]

In SuDoku grids with boxSize 3, the permitted characters
are '1'..'9', so we define the function

> validChar :: Char -> Bool
> validChar c = (c >= '1') && (c <= '9')

The position of a cell in the grid can be given by two Integers:
one represents the row, and the other represents the column, so we have:

> type Position = (Int,Int)


The current state of the game is recorded by two things:
1. the grid itself, and 2. the list of moves so far:

> type State = (SuDokuGrid,[Move])

The list of moves is needed for implementing the "undo"
feature. The puzzle will record a list of moves that
the puzzle solver has made, so that the puzzle solver can
undo as many times as they like. So a move can be
represented by using the type

> type Move = (Position,Char)

Because for undoing moves, we will need to know which
character to reinstate on the grid, the above type
represents a position on the grid and the character
that was *erased*, rather than the new character added.
(If we simply remembered what character was added, we
couldn't undo a move.)


----------------------------------
 Generating Initial SuDoku Grids
----------------------------------

The function "generateInitialState" generates a SuDoku grid
from the input number "seed". It uses the seed to create a
pseduorandom list of Integers seeds, and then creates a grid
in the following way: it chooses a random grid from the list
gridList (in the module Grids), then rotates/reflects it in
a random way, then relabels the numbers in the grid with a
random permutation of 1..9.  Thus a few initial grids can
generate a whole lot of SuDoku grids to solve.

As this is an initial state, no moves have yet been made,
so the list of moves is currently [].

> generateInitialState :: Int -> State
> generateInitialState seed = (grid, [])
>        where (s1:s2:seeds) = rands seed
>              initGrid      = gridList !! (s1 `mod` (length gridList))
>              rotflip       = s2 `mod` 8
>              grid          = pepper (randomPerm 9 seeds) (transf rotflip initGrid)


The function "pepper" renumbers the given grid with the
permutation "perm".

> pepper :: [Int] -> SuDokuGrid -> SuDokuGrid
> pepper perm = map (map (applyPerm perm))

The function "applyPerm" is used to relabel a particular
character, either leaving it alone if it is ' ', or
relabelling it according to the recipe given in the permutation p.

> applyPerm :: [Int] -> Char -> Char
> applyPerm p ' ' = ' '
> applyPerm p c = numToChar (p !! ((charToNum c)-1))



--------------------------------------------
 Functions Concerning Grid Rows and Colums
--------------------------------------------

The puzzle solver will need to refer to positions on the grid
in order to insert characters. The positions are things like
"a1" and "i9" so it is useful to check that a given row/column
character is valid. These functions assume the grid has
a gridSize of 9 by 9:


Whilst positions like "h7" make sense for the puzzle solver,
the Internal workings of the program use lists, which use
Integer indices starting from 0, so we will need some
conversion functions to convert the row and column identifiers
like "a1" and "h7" to and from the indices of the lists
representing the SuDoku grids.

The four functions "colToChr", "rowToChr", "chrToRow"
and "chrToCol" do the conversions.

For example, rowToChr will convert 0 to 'a'
                                   1 to 'b'
                               ... 8 to 'i'

> rowToChr :: Int -> Char
> rowToChr row = chr(ord 'a' + row)


For example, colToChr will convert 0 to '1'
                                   1 to '2'
                               ... 8 to '9'

> colToChr :: Int -> Char
> colToChr col = chr(ord '1' + col)


--------------------------
 Displaying SuDoku Grids
--------------------------

displaySt should take a state (grid,ps) and e.g. if the grid is
["  21 359 ",
 "1 6   7 3",
 "    9    ",
 " 45 1    ",
 "   7 4   ",
 "    5 96 ",
 "    7    ",
 "6 1   8 2",
 " 893 16  "]
it should produce a display like

   1 2 3  4 5 6  7 8 9
 +------+------+------+
a|     2| 1   3| 5 9  |
b| 1   6|      | 7   3|
c|      |   9  |      |
 +------+------+------+
d|   4 5|   1  |      |
e|      | 7   4|      |
f|      |   5  | 9 6  |
 +------+------+------+
g|      |   7  |      |
h| 6   1|      | 8   2|
i|   8 9| 3   1| 6    |
 +------+------+------+

> displaySt (grid,ps)
>   = columnslist ++ makeGrid grid ++ hline


columnslist produces the top line displaying the columns, e.g. like
   1 2 3  4 5 6  7 8 9

> columnslist :: String
> columnslist
>     = " " ++ concat (map makeCols (groupBy boxSize [0..gridSize-1])) ++ "\n"

> makeCols :: [Int] -> [Char]
> makeCols xs = " " ++ concat [[' ',colToChr x] | x <- xs]

hline draws a horizontal line for the grid, e.g. like
 +------+------+------+

> hline = " "
>    ++ concat (replicate boxSize ('+' : concat (replicate boxSize "--")))
>    ++ "+\n"

> makeGrid grid = concat (map makeSection (groupBy boxSize (zip [0..] grid)))

makeSection returns a whole section, e.g. the string representing
 +------+------+------+
d|   4 5|   1  |      |
e|      | 7   4|      |
f|      |   5  | 9 6  |

> makeSection lines = hline ++ concat (map makeLine lines)

e.g. makeLine (5,"4 678 9 1") returns "f| 4   6| 7 8  | 9   1|\n"

> makeLine (which,line)
>    = (rowToChr which) :
>      concat (map makeLineSection (groupBy boxSize line))
>      ++ "|\n"

e.g. makeLineSection "456" returns "| 4 5 6"

> makeLineSection cs = '|' : concat [[' ',c] | c <- cs]


----------------------------------------
 Useful Miscellaneous Utility Functions
----------------------------------------

The function "strToLowerCase" converts all letter characters
in the string to lower case. This is very useful when
the user accidentally has CAPS LOCK on, or for some reason
types in a mixture of upper/lower case letters.

> strToLowerCase :: String -> String
> strToLowerCase = map toLower

Note that the function toLower is a built-in library
function that changes an upper-case letter to lower-case, but
otherwise leaves it unchanged.


These two functions "charToNum" and "numToChar" convert
between the Integers 0..9 and the characters representing digits.

charToNum assumes that the input character is in the range '0'..'9'
e.g.  charToNum '5' would return 5

> charToNum :: Char -> Int
> charToNum c = ord c - ord '0'

numToChar assumes that the input Integer is in the range 0..9
e.g. numToChar 7  would return the character '7'

> numToChar :: Int -> Char
> numToChar n = chr (n + ord '0')

The function "remove" takes a character c and a list,
and removes all occurrences of c from the list.

> remove :: Eq a => a -> [a] -> [a]
> remove c xs = [x | x <- xs, x/=c]


The function "groupBy" partitions a list Into sections,
each of length n. For example,   groupBy 5 [1..22]
would return [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],
[16,17,18,19,20],[21,22]]

> groupBy :: Int -> [a] -> [[a]]
> groupBy n [] = []
> groupBy n xs = (take n xs) : groupBy n (drop n xs)


This function takes an initial seed Integer n, and generates
another large random number, based on n

> nextRandom :: Int -> Int
> nextRandom n = if test > 0 then test else test + 2147483647
>                    where test = 16807 * lo - 2836 * hi
>                          hi   = n `div` 127773
>                          lo   = n `rem` 127773

This gives a stream of pseudorandom numbers from an
initial seed, which is pretty useful when trying to generate
several random numbers (like for generating a random permutation).

> rands :: Int -> [Int]
> rands = iterate nextRandom


Given a list of Integers "seeds" (which is assumed to be a random
list), randomPerm n will give a random permutation of the numbers
1..n, for n an Integer >0

> randomPerm :: Int -> [Int] -> [Int]
> randomPerm 1 seeds = [1]
> randomPerm n (s:seeds) = (take pos xs) ++ [n] ++ (drop pos xs)
>         where pos = s `mod` n
>               xs  = randomPerm (n-1) seeds



---------------------------------------------------
 Parsing the Input and Calling the Correct Command
---------------------------------------------------

The function "doCommand" is similar to something called
a "state transition function".

It takes the current state (grid,ps) and an input string str,
and returns a string (to be displayed) and the new state.

If it can't parse (understand) the input successfully
then it will return a helpful message, otherwise it will
carry out the command (either to undo, or to add a
character to the grid).

> doCommand :: (State -> String -> (String,State))
> doCommand (grid,ps) str
>    |  undoCommand str && ps/=[]    = (displaySt undoneSt, undoneSt)
>    |  parsedOk && finished         = (finalMessage st', st')
>    |  parsedOk && not finished     = (displaySt st',st')
>    |  otherwise                    = (helpMessage str, (grid,ps))
>          where undoneSt         = undo (grid,ps)
>                (parsedOk,c,pos) = parseInput str
>                st'              = updateState  (pos,c) (grid,ps)
>                finished         = allCompleted st'

The function "undoCommand" is checking to see whether
the puzzle solver has typed in "undo" or an equivalent
string. It removes any space characters and converts
to lower case first, before testing:

> undoCommand :: String -> Bool
> undoCommand str = (remove ' ' (strToLowerCase str))=="undo"

The function "parseInput" is checking to see whether
the input string is of the form  "5 @ h3"  (or similar).
First it converts the string all to lower case and
removes and space characters, before handing it to another
function (parseInput2) to decipher which character and position.

> parseInput :: String -> (Bool,Char,Position)
> parseInput = parseInput2 . remove ' ' . strToLowerCase

Need to check that the character and position are in range.
The boolean returned indicates whether the input was
parsed successfully or not.

> parseInput2 (c:'@':y:x:[])
>    | validChar c
>      && validCol x
>      && validRow y = (True,c,(row,col))
>    | otherwise     = (False,c,(row,col))
>          where row = chrToRow y
>                col = chrToCol x
> parseInput2 str    = (False,' ',(0,0))


A message for when the player types an input string with an error in it:

> helpMessage :: String -> String
> helpMessage str = "I didn't understand \"" ++ str ++ "\"\n"
>                   ++ "Try again! (sample usage: 5 @ d2 )\n"


A message for when the SuDoku puzzle has successfully been solved:

> finalMessage :: State -> String
> finalMessage st
>    =   displaySt st
>       ++ "\n\n*** Well done! ***\n"
>       ++ "\nYour SuDoku puzzle is now completed correctly."
>       ++ "\n\n" ++ bye ++ "\n\n"



---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------- Your code goes below here --------------------------


Question 1

The definitions of validCol and validRow are wrong and need altering
(they have just been given these definitions temporarily so that
this file compiles as you're starting your coursework).

checks whether the input character is between 'a' and 'i'

> validRow :: Char -> Bool
> validRow c = c >= 'a' && c <= 'i'

checks whether the input character is between '1' and '9'

> validCol :: Char -> Bool
> validCol c = c >= '1' && c <= '9'

TESTING - include your tests for this question here:

validRow 'a' = True
validRow 'e' = True
validRow 'v' = False
validRow 'i' = True

validCol '1' = True
validCol '5' = True
validCol '9' = True
validCol '0' = False

2 @ n6 = "Try again!"
3 @ f0 = "Try again!"
2 @ a6 = no error
3 @ f4 = no error

Question 2

The definitions of chrToRow and chrToCol are wrong and need altering
(they have just been given these definitions temporarily so that
this file compiles as you're starting your coursework).

ord 'a' = 97 so 'a' - 'a' = 0

> chrToRow :: Char -> Int
> chrToRow y = ord y - 97

ord '1' = 49 so '1' - '1' = 0

> chrToCol :: Char -> Int
> chrToCol x = ord x - 49

TESTING - include your tests for this question here:

chrToRow 'a' = 0
chrToRow 'e' = 4
chrToRow 'v' = 21
chrToRow 'i' = 8

chrToCol '1' = 0
chrToCol '5' = 4
chrToCol '9' = 8
chrToCol '0' = -1

Question 3
pos = (row,col) and grid = [[Char] ]so gets the given row which is [Char] and the position in the list wich is col = Char

> getCharFromGrid :: Position -> SuDokuGrid -> Char
> getCharFromGrid pos grid = (grid!!fst pos)!!snd pos

TESTING - include your tests for this question here:

getCharFromGrid (0,0) sample1 = ' '
getCharFromGrid (4,5) sample1 = '8'
getCharFromGrid (3,5) ample3 = '3'

Question 4

take first n elements and takes n+1 last elements then appends first part, new element and the last part into new list

> substitute :: a -> Int -> [a] -> [a]
> substitute x n xs = (take n xs) ++ [x] ++ (drop (n+1) xs)

TESTING - include your tests for this question here:

substitute 2 3 [19,15,11,7,4,1] = [19,15,11,2,4,1]
substitute 0 1 [3,4,5,6] = [3,0,5,6]
substitute 0 0 [] = [0]

Question 5

uses substitue to add the new character to the row and then substitues the whole row in teh grid

> addCharToGrid :: Char -> Position -> SuDokuGrid -> SuDokuGrid
> addCharToGrid c (x,y) xs = substitute (substitute c y (xs!!x)) x xs

TESTING - include your tests for this question here:

addCharToGrid '7' (1,8) sample1 =
["  5 341 2"
,"8 2     7"
," 3 2  75 "
,"   3   1 "
,"2  6 8  3"
," 9   7   "
," 24  5 6 "
,"      3 8"
,"1 694 2  "]

addCharToGrid '2' (0,0) sample2 =
["2 3   5 8"
," 2 5 1 4 "
,"5       3"
,"  54 8   "
,"   372   "
,"   1 96  "
,"3       4"
," 6 2 5 3 "
,"8 1   2 9"]

Question 6


The definition of updateState is wrong, as it doesn't do anything
to change the state of the game at the moment. It needs altering
in Qn 6 (it has just been given a definition temporarily so that
this file compiles as you're starting your coursework).

adds character c tp the grid using addCharToGrid. appends the bew move onto the current moves which uses getCharFromGrid to store previous character that was replaced.

> updateState' :: Move -> State -> State
> updateState' ((row,col),c) (grid,moves) = (addCharToGrid c (row,col) grid, moves ++ [((row,col), (getCharFromGrid (row,col) grid))])

TESTING - include your tests for this question here:

updateState' ((7,2) ,  '5') (sample1,[]) =
(["  5 341 2"
,"8 2      "
," 3 2  75 "
,"   3   1 "
,"2  6 8  3"
," 9   7   "
," 24  5 6 "
,"  5   3 8"
,"1 694 2  "],[((7,2),' ')])

updateState' ((4,2),'3') (sample2,[]) =
(["9 3   5 8"
," 2 5 1 4 "
,"5       3"
,"  54 8   "
,"  3372   "
,"   1 96  "
,"3       4"
," 6 2 5 3 "
,"8 1   2 9"],[((4,2),' ')])

Question 7

The definition of undo is wrong and needs altering in Qn 7
(it has just been given a definition temporarily so that this
file compiles as you're starting your coursework).

removes the last two elements in the moves list as undo adds the previous move onto the moves list aswell as the unid move. list will never be empty

> removeUndoMove :: State -> State
> removeUndoMove (grid,moves) =  (grid,init (init moves))

uses update state to make the prevois move the current move then removes the last two elenebts from the moves list. if the moves list is empty it returns the current state

> undo :: State -> State
> undo (grid,[]) = (grid,[])
> undo (grid,moves) = removeUndoMove (updateState' (last moves) (grid,moves))

TESTING - include your tests for this question here:

undo (sample1,[((4,2),'3')]) =
(["  5 341 2"
,"8 2      "
," 3 2  75 "
,"   3   1 "
,"2 36 8  3"
," 9   7   "
," 24  5 6 "
,"      3 8"
,"1 694 2  "],[])

undo (sample3,[]) =
(["781635924"
,"965284137"
,"324791685"
,"618973542"
,"547162398"
,"239548716"
,"892356471"
,"476819253"
,"153427869"],[])

undo (sample1,[((4,2),'3'),((5,5),'1')]) =
(["  5 341 2"
,"8 2      "
," 3 2  75 "
,"   3   1 "
,"2  6 8  3"
," 9   1   "
," 24  5 6 "
,"      3 8"
,"1 694 2  "],[((4,2),'3')])

Question 8

sort both lists then check whether they are equal

> permOf :: Ord a => [a] -> [a] -> Bool
> permOf xs ys = (mySort xs) == (mySort ys)

quicksort.

> mySort :: (Ord a) => [a] -> [a]
> mySort []       = []
> mySort (x:xs)   = (mySort lesser) ++ [x] ++ (mySort greater)
>    where
>        lesser  = (filter (<x) xs)
>        greater = (filter (>x) xs)

TESTING - include your tests for this question here:

permOf [1,2,3] [3,1,2] = True
permOf [8,4,2] [9,9,4] = False
permOf [2,6] [3,2,6] = False

check of the length of both lists are equal if they are check whether all elements in xs are in ys. used and to take [Bool] to create Bool

> permOf' ::  Eq a => [a] -> [a] -> Bool
> permOf' xs ys | (length xs) /= (length ys) = False | otherwise =  and([elem a ys | a<-xs])

TESTING - include your tests for this question here:

permOf' [1,2,3] [3,1,2] = True
permOf' [8,4,2] [9,9,4] = False
permOf' [2,6] [3,2,6] = False

Question 9

The definition of allCompleted is wrong (it has just
been given a definition temporarily so that this
file compiles as you're starting your coursework).

checks whether each row,column and box are permutations of ['1'..'9']

> allCompleted :: State -> Bool
> allCompleted (grid,moves) = (and (map (permOf ['1'..'9']) (getRows grid)) ) && (and (map (permOf ['1'..'9']) (getColumns grid)) ) && (and (map (permOf ['1'..'9']) (getBoxes grid)) )

> getRows :: SuDokuGrid -> [[Char]]
> getRows = id

> getColumns :: SuDokuGrid -> [[Char]]
> getColumns = transpose

> getBoxes :: SuDokuGrid -> [[Char]]
> getBoxes = concat . map boxes . groupBy boxSize

> boxes :: [[a]] -> [[a]]
> boxes = map concat . transpose . map (groupBy boxSize)

TESTING

allCompleted (sample1,[]) = False

allCompleted (sample3,[]) = True

Questions 10 and 11

Type signatures to be provided by you for these.
Don't forget to include tests.

removes all character which are not spaces from eahc row. calculates the length of each of the rows with only spaces. sums each length.

> spaceCount :: SuDokuGrid -> Int
> spaceCount grid =  sum [length (filter isSpace a) | a <-getRows grid]

inverts each char in each row

> invertGrid :: SuDokuGrid->SuDokuGrid
> invertGrid grid =  map (map (invertChar)) (getRows grid)

if the character is between '1' and '9' then converts it into an Int and minuses 10 from it and converts it back to a character digit. if the character is a ' ' then it returns ' '

> invertChar :: Char -> Char
> invertChar c | c >= '1' && c <= '9' = intToDigit (10 - (digitToInt c)) | otherwise = c

performs unpdateState on (grid,moves) using newMoves

> updateStates :: State -> [Move] -> State
> updateStates (grid,moves) newMoves = foldr (updateState) (grid, moves) newMoves

checks whther the move is valid and then makes the move by updating the state. if the move is not valid the move is not made by returning teh current state.

> updateState :: Move -> State -> State
> updateState ((row,col),c) (grid,moves) | isValid ((row,col),c) grid = updateState' ((row,col),c) (grid,moves)  | otherwise = (grid,moves)

takes a position and calculates which box 0-8 that the position is in.

> calcBox :: Position -> Int
> calcBox (row,col) = 3 * (row `div` 3) + (col `div` 3)

checks whether the position is a character. if there is no characte3r at the position it cheks whether the character is not already within the gicen column, row and box

> isValid :: Move -> SuDokuGrid -> Bool
> isValid ((row,col),c) grid | isDigit (getCharFromGrid (row,col) grid) = False | otherwise = validMove c ((getColumns grid) !!col) && validMove c ((getRows grid) !! row) && validMove c ((getBoxes grid) !! (calcBox (row,col)))

checks if he char is in the given list  and then invers the result.

> validMove :: Char -> [Char] -> Bool
> validMove char list =  not (elem char list)

TESTING Q10 + 11

spaceCount sample1 = 51
spaceCount sample2 = 52
spaceCount sample3 = 0

invertGrid sample1 =
["  5 769 8"
,"2 8      "
," 7 8  35 "
,"   7   9 "
,"8  4 2  7"
," 1   3   "
," 86  5 4 "
,"      7 2"
,"9 416 8  "]

invertGrid sample2 =
["1 7   5 2"
," 8 5 9 6 "
,"5       7"
,"  56 2   "
,"   738   "
,"   9 14  "
,"7       6"
," 4 8 5 7 "
,"2 9   8 1"]

invertGrid sample3 =
["329475186"
,"145826973"
,"786319425"
,"492137568"
,"563948712"
,"871562394"
,"218754639"
,"634291857"
,"957683241"]

updateStates (sample1,[]) [((0,0),'1'),((0,1),'9')] =
(["195 341 2"
,"8 2      "
," 3 2  75 "
,"   3   1 "
,"2  6 8  3"
," 9   7   "
," 24  5 6 "
,"      3 8"
,"1 694 2  "],[((0,1),' '),((0,0),' ')])

valid move:

updateState ((0,0),'9') (sample1,[]) =
(["9 5 341 2"
,"8 2      "
," 3 2  75 "
,"   3   1 "
,"2  6 8  3"
," 9   7   "
," 24  5 6 "
,"      3 8"
,"1 694 2  "],[((0,0),' ')])

invalid move:

updateState ((0,0),'5') (sample1,[]) =
(["  5 341 2"
,"8 2      "
," 3 2  75 "
,"   3   1 "
,"2  6 8  3"
," 9   7   "
," 24  5 6 "
,"      3 8"
,"1 694 2  "],[])

character already exists in position:

updateState ((0,4),'1') (sample1,[]) =
(["  5 341 2"
,"8 2      "
," 3 2  75 "
,"   3   1 "
,"2  6 8  3"
," 9   7   "
," 24  5 6 "
,"      3 8"
,"1 694 2  "],[])
