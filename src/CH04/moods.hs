module CH04.Moods where

data Mood = Woot | Blah
  deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood _  = Woot
