module Options
  ( Opts (..),
    defaultOpts,
  )
where

data Opts = Opts
  { repl :: Bool,
    showTree :: Bool
  }
  deriving (Show)

defaultOpts :: Opts
defaultOpts =
  Opts
    { repl = False,
      showTree = False
    }
