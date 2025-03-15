module Eval where 

-- La idea es que el evaluador devuelva una lista de tuplas (Date , Int).
-- Cada tupla establece cuanto es el dinero actual en una fecha dada. 
-- Posteriormente, plotear eso para obtener un gráfico.


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

-- Valores 
type PlotList = [(Date, Int)]

-- Entornos
type EnvDI = M.Map Date Int 
type EnvVC = M.Map Var Contract 
type EnvVD = M.Map Var Date 

-- Cambiar por dos estados y el valor a devolver sea la lista de tuplas date-dinero.
-- Entorno nulo 
initEnvDI :: EnvDI 
initEnvDI = M.empty 

initEnvVC :: EnvVC
initEnvVC = M.empty

initEnvVD :: EnvVD 
initEnvVD = M.empty

-- Mónada estado
newtype State a = State { runState :: EnvDI -> EnvVC -> EnvVD -> Pair (Pair (Pair a EnvDI) EnvVC) EnvVD }

instance Monad State where
    return x = State (\sdi svc svd -> (((x :!: sdi) :!: svc) :!: svd)) 
    m >>= f = State (\sdi svc svd -> let (((x :!: sdi') :!: svc') :!: svd') = (runState m sdi svc svd) in (runState f x) sdi' svc' svd') 

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap


instance MonadState State where 
    lookfor d = State (\sdi svc svd  -> (((lookfor' d sdi svc svd :!: sdi) :!: svc) :!: svd))
        where lookfor' v sdi svc sdv = case v of 
                                            D _ _ _ -> fromJust $ M.lookup v sdi
                                            _ -> case M.lookup v svc of 
                                                      Just x -> x 
                                                      Nothing -> fromJust $ M.lookup v sdv
    update d i = State (\sdi svc svd -> case d of 
                                          D _ _ _ -> ((( () :!: M.insert v i sdi) :!: svc) :!: svd)
                                          _       -> case i of 
                                                        D _ _ _ -> ((( () :!: sdi) :!: svc) :!: M.insert v i svd)
                                                        _       -> ((( () :!: sdi) :!: M.insert v i svc) :!: svd))



