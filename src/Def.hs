module Def where

-- Usado para definir la fecha 
type Day = Int
type Month = Int 
type Year = Int 

data Date = D Day Month Year

-- Monedas disponibles
data Currency = GBP | USD | AP | EU 

-- Las variables observables son aquellas cuyo valor 
-- puede ser determinado a partir de fuentes verificables,
-- como precios de mercado, tasas de interés, índices financieros, entre otros. 
data Obs a = CONST a | LIBOR a  | EUAP a -- | USDEUR a | GBPUSD a | USDAP a 

-- Lo hacemos instancia de Functor para así poder generalizar funciones como lift. 
instance Functor Obs where 
    fmap :: (a -> b) -> Obs a -> Obs b
    fmap f (CONST a) = CONST (f a)
    fmap f (LIBOR a) = LIBOR (f a)
    fmap f (EUAP a)  = EUAP (f a)

-- Lo hacemos instancia de Applicative para generalizar funciones como lift2. 
instance Applicative Obs where 
    pure :: a -> Obs a
    pure = CONST 

    (<*>) :: Obs (a -> b) -> Obs a -> Obs b
    (CONST f) <*> x         = fmap f x 
    (LIBOR f) <*> x         = fmap f x
    (EUAP f) <*>  x         = fmap f x


-- Representación de un contrato
data Contract = Zero
                | One Currency -- No es necesario que tenga fecha, es siempre para ahora.
                | Give Contract
                | And Contract Contract 
                | Or Contract Contract 
                | Truncate Date Contract 
                | Then Contract Contract
                | Scale (Obs Double) Contract -- Me permite crear contratos de mayor valor.
                | Get Contract
                | Anytime Contract

-- Valor del "proceso" (contrato).
-- TimeStep es su horizonte -> Su fecha límite de adquisisión. 
-- Slice es una lista de columnas una por paso de tiempo en orden al revés. 
-- Esto es el valor del proceso cambiando a lo largo del tiempo teniendo
-- como primer elemento su horizonte (cuanto da de por si).

-- Esto se entiende mejor pensando que un contrato que dice 
-- pagar 100usd el 28 de Diciembre del 2025 (su horizonte), puede 
-- pagar menos el tiempo antes de dicha fecha debido a como afectan
-- los intereses. La inflación siempre existe y pagar 100usd en Mayo
-- es totalmente diferente que hacerlo en Diciembre.
type ValProc = (TimeStep, [Slice])
type Slice = [Double]
type TimeStep = Date

-- ValProc puede ser una mónada que vaya llevando
-- la fecha. 
-- Puede ser mónada estado.
-- newtype ValProc a = VP {runVP :: (TimeStep, )}