module OrphanA where

  data Book =
    Book 
      { author :: String
      , publisher  :: String
      }
      
  instance Show Book where
    show (Book author publisher) = 
      "The author of the book is " ++ author ++ " published by " ++ publisher ++ "."  
