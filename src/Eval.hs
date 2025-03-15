module Eval where 

import Def 
import Monads 
import qualified Data.Map.Strict              as M
import Data.Maybe
import Prelude                                hiding    ( fst 
                                                        , snd 
                                                        )
import Data.Strict.Tuple
import Control.Monad                                    ( liftM
                                                        , ap
                                                        )

-- Entornos
type Env = M.Map Date Int 

-- Entorno nulo 
initEnv :: Env 
initEnv = M.empty 

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
    return x = State (\s -> (x :!: s)) 
    m >>= f = State (\s -> let (x :!: s') = (runState m s) in (runState f x) s') 

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap


instance MonadState State where 
    lookfor d = State (\s -> (lookfor' d s :!: s))
        where lookfor' v s = fromJust $ M.lookup v s
    update d i = State (\s -> (() :!: update' v i s)) where update' = M.insert


