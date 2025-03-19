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
-- Al final traducir todas las monedas a USD. 
type PlotList = [(Date, Int, Currency)]

-- Entornos
--type EnvDI = M.Map Date Int 
type EnvVC = M.Map Var Contract 
type EnvVD = M.Map Var Date 

-- Cambiar por dos estados y el valor a devolver sea la lista de tuplas date-dinero.
-- Entorno nulo 
initEnvVC :: EnvVC
initEnvVC = M.empty

initEnvVD :: EnvVD 
initEnvVD = M.empty

-- Mónada estado
newtype State a = State { runState :: EnvVC -> EnvVD -> Pair (Pair a EnvVC) EnvVD }

instance Monad State where
    return x = State (\svc svd -> ((x :!: svc) :!: svd)) 
    m >>= f = State (\svc svd -> let ((x :!: svc') :!: svd') = (runState m svc svd) in (runState f x) svc' svd') 

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap


instance MonadState State where 
    lookfor v = State (\svc svd  -> ((lookfor' v svc svd :!: svc) :!: svd))
        where lookfor' v svc sdv = case M.lookup v svc of 
                                        Just x  -> x 
                                        Nothing -> fromJust $ M.lookup v sdv
    update v i = State (\svc svd -> case i of 
                                       D _ _ _ -> ( () :!: svc ) :!: M.insert v i svd
                                       _       -> ( () :!: M.insert v i svc ) :!: svd)

-- Agrega la primera fecha y el monto de dinero actual.
-- Siempre empezamos en 0.
addInitDate :: MonadState m => Comm -> m PlotList
addInitDate InitDate d m y = return [(Date d m y, 0)]

-- Ver
--addVal :: MonadState m => PlotList -> m PlotList -> m PlotList
--addVal v m = 

-- Evalúa un programa en el estado nulo. 
eval :: Comm -> PlotList        
eval c = fst (fst ( runState (stepCommStar c) initEnvVC initEnvVD))

-- Evalúa múltiples pasos de un comando. Hasta alcanzar un Skip.
-- No devuelve un valor en sí ya que sólo tiene efectos secundarios.
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c = stepComm >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m PlotList
stepComm Skip = return [] -- Nunca va a ser Skip
stepComm (LetCont v1 ctr ) =  do eva <- evalCtr ctr
                                 update v1 eva 
                                 return [] 
stepComm (LetDate v1 date) =  do update v1 date 
                                 return [] 
stepComm (Seq id@(InitDate d m y) c2) = addInitDate id -- Ver bien 
stepComm (Seq Skip c2)  = return [] 
stepComm (Seq c1 c2)    = do  sc1 <- stepComm c1 
                              return []

evalCtr :: MonadState m => Contract -> m PlotList 
evalCtr Zero = return []
--evalCtr OneV v c = 

