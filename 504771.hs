-- MATHFUN - DISCRETE MATHEMATICS AND FUNCTIONAL PROGRAMMING
-- Functional Programming Assignment, 2012/13
--
-- 504771
--
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Ord (comparing)
-- Types

type Title   = String
type Actor   = String
type Year    = Int
type Fan     = String
data Film    = Film Title [Actor] Year [Fan]
              deriving(Eq,Ord,Show,Read)

testDatabase :: [Film]
testDatabase = [Film "Casino Royale" ["Daniel Craig", "Eva Green", "Judi Dench"] 2006 ["Garry", "Dave", "Zoe", "Kevin", "Emma"],
                Film "Cowboys & Aliens" ["Harrison Ford", "Daniel Craig", "Olivia Wilde"] 2011 ["Bill", "Jo", "Garry", "Kevin", "Olga", "Liz"],
                Film "Catch Me If You Can" ["Leonardo DiCaprio", "Tom Hanks"] 2002 ["Zoe", "Heidi", "Jo", "Emma", "Liz", "Sam", "Olga", "Kevin", "Tim"],
                Film "Mamma Mia!" ["Meryl Streep", "Pierce Brosnan"] 2008 ["Kevin", "Jo", "Liz", "Amy", "Sam", "Zoe"],
                Film "Saving Private Ryan" ["Tom Hanks", "Matt Damon"] 1998 ["Heidi", "Jo", "Megan", "Olga", "Zoe", "Wally"],
                Film "Life of Pi" ["Suraj Sharma"] 2012 ["Kevin", "Olga", "Liz", "Tim", "Zoe", "Paula", "Jo", "Emma"],
                Film "Titanic" ["Leonardo DiCaprio", "Kate Winslet"] 1997 ["Zoe", "Amy", "Heidi", "Jo", "Megan", "Olga"],
                Film "Quantum of Solace" ["Daniel Craig", "Judi Dench"] 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"],
                Film "You've Got Mail" ["Meg Ryan", "Tom Hanks"] 1998 ["Dave", "Amy"],
                Film "Collateral" ["Tom Cruise", "Jamie Foxx"] 2004 ["Dave", "Garry", "Megan", "Sam", "Wally"],
                Film "The Departed" ["Leonardo DiCaprio", "Matt Damon", "Jack Nicholson"] 2006 ["Zoe", "Emma", "Paula", "Olga", "Dave"],
                Film "Inception" ["Leonardo DiCaprio"] 2010 ["Chris", "Emma", "Jo", "Bill", "Dave", "Liz", "Wally", "Zoe", "Amy", "Sam", "Paula", "Kevin", "Olga"],
                Film "Up in the Air" ["George Clooney", "Vera Farmiga"] 2009 ["Wally", "Liz", "Kevin", "Tim", "Emma"],
                Film "The Shawshank Redemption" ["Tim Robbins", "Morgan Freeman"] 1994 ["Jo", "Wally", "Liz", "Tim", "Sam", "Zoe", "Emma", "Garry", "Olga", "Kevin"],
                Film "Gladiator" ["Russell Crowe", "Joaquin Phoenix"] 2000 ["Garry", "Ian", "Neal"],
                Film "The King's Speech" ["Colin Firth", "Geoffrey Rush"] 2010 ["Garry", "Megan", "Sam", "Ian", "Bill", "Emma", "Chris"],
                Film "The Descendants" ["George Clooney"] 2011 ["Wally", "Liz", "Kevin", "Tim", "Emma", "Chris", "Megan"],
                Film "Cloud Atlas" ["Tom Hanks", "Halle Berry"] 2012 ["Dave", "Amy", "Garry", "Ian", "Neal"],
                Film "The Reader" ["Kate Winslet", "Ralph Fiennes"] 2008 ["Emma", "Bill", "Dave", "Liz"],
                Film "Minority Report" ["Tom Cruise"] 2002 ["Dave", "Garry", "Megan", "Sam", "Wally"],
                Film "Revolutionary Road" ["Leonardo DiCaprio", "Kate Winslet"] 2008 ["Wally", "Sam", "Dave", "Jo"],
                Film "Forrest Gump" ["Tom Hanks"] 1994 ["Ian", "Garry", "Bill", "Olga", "Liz", "Sam", "Dave", "Jo", "Chris", "Wally", "Emma"],
                Film "Larry Crowne" ["Tom Hanks", "Julia Roberts"] 2011 ["Liz", "Wally"],
                Film "The Terminal" ["Tom Hanks", "Catherine Zeta Jones"] 2004 ["Olga", "Heidi", "Bill", "Sam", "Zoe"],
                Film "Django Unchained" ["Jamie Foxx", "Leonardo DiCaprio", "Christoph Waltz"] 2012 ["Kevin", "Tim", "Emma", "Olga"]]

                  
--  Your functional code goes here
--
-- General functions - these are 'helper' functions for tasks i - viii. The functions numbered i - viii are initiators
--
listToString [] = ""
listToString (element:[]) = show element
listToString (element:elements) = show element ++ ", " ++ listToString elements

displayFilm (Film title actors year fans) = "\nTitle: " ++ title ++ "\nCast: " ++ listToString actors ++ "\nYear: " ++ show year ++ "\nFans: " ++ show (length fans) ++ "\n"

-- The following functions have lambda expressions that match the pattern (Film title actors year fans) as (Film w x y z)
filmsByYear year = filter (\(Film _ _ y _) -> y == year)
    
isFanOf fanName = filter (\(Film _ _ _ z) -> elem fanName z)

actorFilmsPeriod actor startYear endYear = filter (\(Film _ x y _) -> elem actor x && y >= startYear && y <= endYear)

addFan fanName filmName = map (\(Film w x y z) -> if filmName == w && notElem fanName z then (Film w x y (z ++ [fanName])) else (Film w x y z))

actorFilms actor = filter (\(Film _ x _ _) -> elem actor x)

sortByDescendingFans = reverse . sortBy (comparing (\(Film _ _ _ z) -> length z))

filmInDatabase filmName films = foldr (||) False $ map (\(Film w _ _ _) -> w == filmName) films
--
-- Initiator functions
--
-- i - Adds a film
addFilm :: Title -> [Actor] -> Year -> [Film] -> [Film]
addFilm title actors year films = films ++ [Film title actors year []]

-- ii - Displays all given films in a formatted string (shows all films if testDatabase is passed directly)
displayAllFilms :: [Film] -> String
displayAllFilms [] = "No films to display!"
displayAllFilms films = foldr (++) "" $ map displayFilm films

-- iii - Displays films in a given year
displayFilmsByYear :: Int -> [Film] ->  String
displayFilmsByYear _ [] = "No films to display!"
displayFilmsByYear year films = displayAllFilms $ filmsByYear year films

-- iv - Shows films a person is a fan of
isFanOfFilms :: String -> [Film] -> String
isFanOfFilms _ [] = "No films to display!"
isFanOfFilms name films = displayAllFilms $ isFanOf name films

-- v - Shows which films a given actor is featured in within a time period in years
actorFilmPeriod :: String -> Int -> Int -> [Film] -> String
actorFilmPeriod _ _ _ [] = "No films to display!"
actorFilmPeriod name startYear endYear films = displayAllFilms $ actorFilmsPeriod name startYear endYear films

-- vi - User becomes a fan of a film, if not already
becomeFan :: String -> String -> [Film] -> [Film]
becomeFan _ _ [] = []
becomeFan fanName filmName films = addFan fanName filmName films

-- vii - Shows a given actor's best film
actorBestFilm :: String -> [Film] -> String
actorBestFilm _ [] = "No films to display!"
actorBestFilm name films = displayFilm $ head $ sortByDescendingFans $ actorFilms name films

-- viii - Shows the top 5 best films by number of fans
topFiveFilms :: [Film] -> String
topFiveFilms [] = "No films to display!"
topFiveFilms films = displayAllFilms $ take 5 $ sortByDescendingFans films -- 'take n' returns the first n items in a list

-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO ()
demo 1  = putStrLn $ displayAllFilms $ addFilm "The Great Gatsby" ["Leonardo DiCaprio", "Tobey Maquire"] 2013 testDatabase
demo 2  = putStrLn $ displayAllFilms testDatabase
demo 3  = putStrLn $ displayFilmsByYear 2012 testDatabase
demo 4  = putStrLn $ isFanOfFilms "Zoe" testDatabase
demo 5  = putStrLn $ actorFilmPeriod "Tom Hanks" 2000 2011 testDatabase
demo 6  = putStrLn $ displayAllFilms $ becomeFan "Zoe" "Forrest Gump" testDatabase
demo 61 = putStrLn $ displayAllFilms $ becomeFan "Zoe" "Inception" testDatabase
demo 7  = putStrLn $ actorBestFilm "Tom Hanks" testDatabase
demo 8  = putStrLn $ topFiveFilms testDatabase
--
--
-- Your user interface code goes here
--
-- Main IO functions
loadDatabase :: IO [Film]
loadDatabase = do database <- readFile "films-copy.txt"
                  length database `seq` return (read database :: [Film])
                  
main :: IO ()
main = do putStr "What is your name? "
          yourName <- getLine
          database <- loadDatabase
          putStr ("\nFound " ++ show (length database) ++ " films.\n\nType 'help' for commands. Leave blank to exit.\nEnter a command: ")
          interface yourName database
          
-- interface is a recursive IO function
interface :: String -> [Film] -> IO ()
interface yourName database = do command <- getLine
                                 if command == ""
                                     then do writeFile "films-copy.txt" (show database)
                                             putStrLn "Database has been saved. Goodbye!"
                                     else do if command == "1"
                                                 then do database <- addAFilm database
                                                         putStr ("Film added to database. There are now " ++ show (length database) ++ " films.\nEnter your next command: ")
                                                 else if command == "2"
                                                          then putStr (displayAllFilms database ++ "\nEnter your next command: ")
                                                          else if command == "3"
                                                                   then do putStr "Enter a year: "
                                                                           year <- getYear
                                                                           putStr (displayFilmsByYear year database ++ "\nEnter your next command: ")
                                                                   else if command == "4"
                                                                            then putStr (isFanOfFilms yourName database ++ "\nEnter your next command: ")
                                                                            else if command == "5"
                                                                                     then showActorFilmsPeriod database
                                                                                     else if command == "6"
                                                                                              then do database <- becomeFanOf yourName database
                                                                                                      putStr "\nEnter your next command: "
                                                                                              else if command == "7"
                                                                                                       then do actor <- getActor
                                                                                                               putStr (actorBestFilm actor database ++ "\nEnter your next command: ")
                                                                                                       else if command == "8"
                                                                                                                then putStr (topFiveFilms database ++ "\nEnter your next command: ")
                                                                                                                else if command == "help"
                                                                                                                         then callHelp
                                                                                                                         else putStr "Command not recognised. Enter your next command: "
                                             interface yourName database

-- Extra IO functions
addAFilm  :: [Film] -> IO [Film]
addAFilm films = do putStr "Enter the name of the film: \n"
                    title <- getLine
                    if title == ""
                        then do putStr "Input must comprise of characters. \n"
                                addAFilm films
                        else do actors <- getActors []
                                putStr "Enter the year of the film: "
                                year <- getYear
                                return (addFilm title actors year films)
                                
getActor :: IO String
getActor = do putStr "Enter the name of an actor: "
              str <- getLine
              if str == ""
                  then do putStr "Input must comprise of characters. \n"
                          getActor
                  else return str

showActorFilmsPeriod :: [Film] -> IO ()
showActorFilmsPeriod films = do actor <- getActor
                                putStr "Enter starting year: "
                                startYear <- getYear
                                putStr "Enter ending year: "
                                endYear <- getYear
                                putStr (actorFilmPeriod actor startYear endYear films ++ "\nEnter your next command: ")
                                
becomeFanOf :: String -> [Film] -> IO [Film]
becomeFanOf yourName films = do putStr "Enter the name of the film: "
                                filmName <- getLine
                                if filmInDatabase filmName films
                                    then do let newDB = becomeFan yourName filmName films
                                            if newDB == films
                                                then do putStr "You are already a fan of this film."
                                                        return films
                                                else do putStr ("You have become a fan of " ++ filmName)
                                                        return newDB
                                    else do putStr "Film is not in the database. "
                                            becomeFanOf yourName films
        
getActors :: [String] -> IO [Actor]
getActors lst = do putStr "Enter the name of an actor, or leave blank to move on: "
                   str <- getLine
                   if str == ""
                       then do if lst == []
                                   then do putStr "List is empty. "
                                           getActors []
                                   else return lst
                       else do let newlist = lst ++ [str]
                               putStrLn ("Actors in film: " ++ show newlist)
                               getActors newlist

getYear :: IO Int
getYear = do str <- getLine
             if foldr (&&) True (map isDigit str) && length str == 4
                 then return (read str :: Int)
                 else do putStr "Please input a 4-digit number: "
                         getYear
                        
callHelp :: IO ()
callHelp = putStrLn "Help stuff   \nEnter your next command: "