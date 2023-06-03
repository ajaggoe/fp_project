module Jq.Filters where

data Filter = Identity
            | Parenthesis Filter
            | Indexing String
            | Iterator [String]
            | IndexingOpt String
            | IteratorOpt [String]
            | Pipe Filter Filter
            | Comma Filter Filter
            | Slice Int Int
            | ArrayIndexing Int
            | ArrayIterator [Int]
            | ArraySlicer Int Int
            | ArrayIndexingOpt Int
            | ArrayIteratorOpt [Int]
            | ArraySlicerOpt Int Int
            | RecDescent
            | CVNull                 
            | CVNum          Int      
            | CVFloat        Float    
            | CVBool         Bool     
            | CVString       String   
            | CVArray        [Filter] 
            | CVObject       [(Filter, Filter)]


instance Show Filter where
  show (Identity) = "."
  show (Parenthesis a) = "("++ show a++")"
  show (ArrayIndexing i) = ".["++show i++"]"
  show (Indexing a) = "."++a
  show (Pipe a b) = ". | ."
  show (Comma a b) = show a++", "++ show b 


instance Eq Filter where
  -- NIET KIJKEN NAAR REFLECTION
  Identity == Identity = True
  (Parenthesis a) == (Parenthesis b) = a == b
  (Indexing a)== (Indexing b) = a == b
  (Pipe a b) == (Pipe c d) = a == c && b == d
  (Comma a b) == (Comma c d) = a == c && b == d
  
  _ == _ = False

data Config = ConfigC {filters :: Filter}

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to Filter
-- For the tests to succeed fill them in with functions that return correct filters
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterParenthesisSC :: Filter -> Filter
filterParenthesisSC = Parenthesis 

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC = Indexing

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = Pipe

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = Comma 

