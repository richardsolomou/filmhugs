-- MATHFUN - DISCRETE MATHEMATICS AND FUNCTIONAL PROGRAMMING
-- Functional Programming Assignment, 2012/13
-- Student Number: 630745


{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#  DATA TYPES  #--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}

-- Film datatype consisting of Title, Cast, Year, and Fans.
data Film =  Film String [String] Int [String]
			 deriving (Eq, Ord, Show, Read)


{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#  FUNCTIONAL  CODE  #--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}

-- Adds a new film at the end of the list of films provided.
fnAddNewFilm :: Film -> [Film] -> [Film]
fnAddNewFilm filmToAdd listOfFilms = listOfFilms ++ [filmToAdd]

-- Checks the year for a single film and returns whether it was released during
-- that year.
fnCheckFilmByYear :: Int -> Film -> Bool
fnCheckFilmByYear yearRequested (Film _ _ year _)
	| year == yearRequested = True
	| otherwise				= False

-- Gets a list of films and filters out the ones that were not released during
-- that year.
fnDisplayFilmsByYear yearRequested = filter (fnCheckFilmByYear yearRequested)

-- Goes through all the films and gets the one that matches the title given
-- and returns the rest of the film's values.
fnGetFilmByTitle :: String -> [Film] -> Film
fnGetFilmByTitle _ [] = (Film "" [] 0 [])
fnGetFilmByTitle name ((Film title cast year fans):films)
	| name == title = (Film title cast year fans)
	| otherwise		= fnGetFilmByTitle name films

-- Checks the fans in a single film and returns whether the specified fan liked
-- that film.
fnCheckFilmByFan :: String -> Film -> Bool
fnCheckFilmByFan fanRequested (Film title cast year (fan:fans))
	| fan == fanRequested = True
	| fans == []		  = False
	| otherwise			  = fnCheckFilmByFan fanRequested (Film title cast year fans)

-- Gets a list of films and filters out the ones that were not liked by a
-- specified fan.
fnDisplayFilmsByFan fanRequested = filter (fnCheckFilmByFan fanRequested)

-- Checks the actors in a single film and returns whether the specified actor
-- starred in that film.
fnCheckFilmByActor :: String -> Film -> Bool
fnCheckFilmByActor actorRequested (Film title (actor:cast) year fans)
	| actor == actorRequested = True
	| cast == []			  = False
	| otherwise				  = fnCheckFilmByActor actorRequested (Film title cast year fans)

-- Gets an actor name and a database of films and checks if the actor starred
-- in any of those films.
fnActorExists :: String -> [Film] -> Bool
fnActorExists _ [] = False
fnActorExists actorRequested (film:films)
	| fnCheckFilmByActor actorRequested film = True
	| otherwise							   = fnActorExists actorRequested films

-- Gets a list of films and filters out the ones that the specified actor did
-- not star in.
fnDisplayFilmsByActor actorRequested = filter (fnCheckFilmByActor actorRequested)

-- Checks if the film was released within the period of the start and end year.
fnCheckFilmInPeriod :: Int -> Int -> Film -> Bool
fnCheckFilmInPeriod startYear endYear (Film _ _ year _)
	| year >= startYear && year <= endYear = True
	| otherwise							   = False

-- Gets a list of films and filters out the ones that were not released during
-- the specified time period and the ones where the specified actor did not
-- star in.
fnDisplayFilmsByActorInPeriod actorRequested startYear endYear = filter (fnCheckFilmByActor actorRequested) . filter (fnCheckFilmInPeriod startYear endYear)

-- Gets the user's name from IO and the film name that the user wants to be a
-- fan of and adds the user as a fan of that film.
fnAddNewFan :: String -> String -> [Film] -> [Film]
fnAddNewFan _ _ [] = []
fnAddNewFan fan film ((Film title cast year fans):films)
	| film == title = (Film title cast year (fan:fans)) : (fnAddNewFan fan film films)
	| otherwise		= (Film title cast year fans) : (fnAddNewFan fan film films)

-- Gets a film and returns the number of its fans.
fnNumberOfFans (Film _ _ _ fans) = length fans

-- Gets a database of films and the first film in the database and checks with
-- that to find the film with the most fans, and returns it.
fnFilmWithMostFans :: [Film] -> Film -> Film
fnFilmWithMostFans [] maxFans = maxFans
fnFilmWithMostFans (film:films) maxFans
	| (fnNumberOfFans film) > (fnNumberOfFans maxFans) = fnFilmWithMostFans films film
	| otherwise									   = fnFilmWithMostFans films maxFans

-- Gets an actor name and a database of films and checks if the actor requested
-- has starred in a film. Checks the films they starred in for the one with the
-- most fans, otherwise it gets the one film with the most fans.
fnBestFilmByActor :: String -> [Film] -> Film
fnBestFilmByActor actorRequested (film:films)
	| fnActorExists actorRequested films = fnFilmWithMostFans (fnDisplayFilmsByActor actorRequested films) film
	| otherwise						   = Film "" [] 0 []

-- Gets the database of films twice and creates a list of the returned films
-- in order to avoid duplicating films from the top films function.
fnRemoveRepeatingFilms :: [Film] -> [Film] -> [Film]
fnRemoveRepeatingFilms _ [] = []
fnRemoveRepeatingFilms (originalFilm:originalFilms) (returnedFilm:returnedFilms)
	| returnedFilm == fnFilmWithMostFans originalFilms originalFilm = fnRemoveRepeatingFilms (originalFilm:originalFilms) returnedFilms
	| otherwise													  = returnedFilm : fnRemoveRepeatingFilms (originalFilm:originalFilms) returnedFilms

-- Gets a number of top films to display and a database and creates a new list
-- in order to display those films in ascending order (to be reversed in IO).
fnTopFilms :: Int -> [Film] -> [Film]
fnTopFilms 0 _ = []
fnTopFilms numberOfFilms (film:films) = (fnFilmWithMostFans films film) : (fnTopFilms (numberOfFilms - 1) (fnRemoveRepeatingFilms (film:films) (film:films)))

-- Reverses a list of films.
fnReverseList films = reverse films


{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#   IO  CODE   #--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}

-- Opens the films.txt file to load the films database.
ioLoadFile :: IO [Film]
ioLoadFile = do
	allFilms <- readFile "films.txt"
	let films = read allFilms :: [Film]
	return films

-- Gets the database that was passed from ioloadFile and the user's name and
-- passes them up to the displayFilms function.
ioGetName :: [Film] -> IO ()
ioGetName films = do
	putStrLn ""
	putStrLn "--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--"
	putStrLn "--#"
	putStr "--#  User name: "
	name <- getLine
	putStrLn "--#"
	putStrLn "--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--"
	if name == ""
		then ioGetName films
		else ioDisplayMenu films name


{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#   DATABASE   #--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}

-- Database of films to be used for demonstration of functional code.
testDatabase :: [Film]
testDatabase = [(Film "Casino Royale" ["Daniel Craig", "Eva Green", "Judi Dench"] 2006 ["Garry", "Dave", "Zoe", "Kevin", "Emma"]),
				(Film "Cowboys & Aliens" ["Harrison Ford", "Daniel Craig", "Olivia Wilde"] 2011 ["Bill", "Jo", "Garry", "Kevin", "Olga", "Liz"]),
				(Film "Catch Me If You Can" ["Leonardo DiCaprio", "Tom Hanks"] 2002 ["Zoe", "Heidi", "Jo", "Emma", "Liz", "Sam", "Olga", "Kevin", "Tim"]),
				(Film "Mamma Mia!" ["Meryl Streep", "Pierce Brosnan"] 2008 ["Kevin", "Jo", "Liz", "Amy", "Sam", "Zoe"]),
				(Film "Saving Private Ryan" ["Tom Hanks", "Matt Damon"] 1998 ["Heidi", "Jo", "Megan", "Olga", "Zoe", "Wally"]),
				(Film "Life of Pi" ["Suraj Sharma"] 2012 ["Kevin", "Olga", "Liz", "Tim", "Zoe", "Paula", "Jo", "Emma"]),
				(Film "Titanic" ["Leonardo DiCaprio", "Kate Winslet"] 1997 ["Zoe", "Amy", "Heidi", "Jo", "Megan", "Olga"]),
				(Film "Quantum of Solace" ["Daniel Craig", "Judi Dench"] 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"]),
				(Film "You've Got Mail" ["Meg Ryan", "Tom Hanks"] 1998 ["Dave", "Amy"]),
				(Film "Collateral" ["Tom Cruise", "Jamie Foxx"] 2004 ["Dave", "Garry", "Megan", "Sam", "Wally"]),
				(Film "The Departed" ["Leonardo DiCaprio", "Matt Damon", "Jack Nicholson"] 2006 ["Zoe", "Emma", "Paula", "Olga", "Dave"]),
				(Film "Inception" ["Leonardo DiCaprio"] 2010 ["Chris", "Emma", "Jo", "Bill", "Dave", "Liz", "Wally", "Zoe", "Amy", "Sam", "Paula", "Kevin", "Olga"]),
				(Film "Up in the Air" ["George Clooney", "Vera Farmiga"] 2009 ["Wally", "Liz", "Kevin", "Tim", "Emma"]),
				(Film "The Shawshank Redemption" ["Tim Robbins", "Morgan Freeman"] 1994 ["Jo", "Wally", "Liz", "Tim", "Sam", "Zoe", "Emma", "Garry", "Olga", "Kevin"]),
				(Film "Gladiator" ["Russell Crowe", "Joaquin Phoenix"] 2000 ["Garry", "Ian", "Neal"]),
				(Film "The King's Speech" ["Colin Firth", "Geoffrey Rush"] 2010 ["Garry", "Megan", "Sam", "Ian", "Bill", "Emma", "Chris"]),
				(Film "The Descendants" ["George Clooney"] 2011 ["Wally", "Liz", "Kevin", "Tim", "Emma", "Chris", "Megan"]),
				(Film "Cloud Atlas" ["Tom Hanks", "Halle Berry"] 2012 ["Dave", "Amy", "Garry", "Ian", "Neal"]),
				(Film "The Reader" ["Kate Winslet", "Ralph Fiennes"] 2008 ["Emma", "Bill", "Dave", "Liz"]),
				(Film "Minority Report" ["Tom Cruise"] 2002 ["Dave", "Garry", "Megan", "Sam", "Wally"]),
				(Film "Revolutionary Road" ["Leonardo DiCaprio", "Kate Winslet"] 2008 ["Wally", "Sam", "Dave", "Jo"]),
				(Film "Forrest Gump" ["Tom Hanks"] 1994 ["Ian", "Garry", "Bill", "Olga", "Liz", "Sam", "Dave", "Jo", "Chris", "Wally", "Emma"]),
				(Film "Larry Crowne" ["Tom Hanks", "Julia Roberts"] 2011 ["Liz", "Wally"]),
				(Film "The Terminal" ["Tom Hanks", "Catherine Zeta Jones"] 2004 ["Olga", "Heidi", "Bill", "Sam", "Zoe"]),
				(Film "Django Unchained" ["Jamie Foxx", "Leonardo DiCaprio", "Christoph Waltz"] 2012 ["Kevin", "Tim", "Emma", "Olga"])]