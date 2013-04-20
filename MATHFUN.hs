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