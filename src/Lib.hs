module Lib where
import Text.Show.Functions

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

type Palo = Habilidad -> Tiro

cambiarVelocidad :: (Int -> Int) -> Tiro -> Tiro 
cambiarVelocidad f tiro = tiro {velocidad = f (velocidad tiro)}

cambiarAltura :: (Int -> Int) -> Tiro -> Tiro 
cambiarAltura f tiro = tiro {altura = f (altura tiro)}

cambiarPrecision :: (Int -> Int) -> Tiro -> Tiro 
cambiarPrecision f tiro = tiro {precision = f (precision tiro)}

putter :: Palo
putter habilidad = crearTiro 10 0 ((precisionJugador habilidad) *2)

madera :: Palo
madera habilidad = crearTiro 100 5 ((precisionJugador habilidad) `div`2)

hierros :: Int -> Palo
hierros n habilidad = crearTiro ((fuerzaJugador habilidad) *n) (min 0 (n-3)) ((precisionJugador habilidad) `div`n) 

crearTiro :: Int -> Int -> Int -> Tiro
crearTiro velocidad altura precision = UnTiro {velocidad = velocidad, precision = precision , altura = altura}

palos = [putter, madera,hierros 1, hierros 2, hierros 3, hierros 4 , hierros 5, hierros 6, hierros 7 ,hierros 8, hierros 9 ,hierros 10]

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = (palo.habilidad) jugador

---------------------3

--type Obstaculo = Tiro -> Tiro

data Obs = Hoyo | Tunel | Laguna {largoLaguna :: Int} deriving (Show, Eq)

pasaObstaculo :: Obs -> Tiro -> Bool
pasaObstaculo Tunel tiro = precision tiro > 90
pasaObstaculo (Laguna _) tiro = (velocidad tiro > 80) && alturaEntre tiro
pasaObstaculo Hoyo tiro = precision tiro > 95 && velocidadEntre tiro

modificarTiro :: Obs -> Tiro -> Tiro
modificarTiro obstaculo tiro
    |pasaObstaculo Tunel tiro = (cambiarVelocidad (*2) . cambiarPrecision (\_ -> 100). cambiarAltura (\_ -> 0)) tiro
    |pasaObstaculo (Laguna largoLaguna) tiro = cambiarAltura (div largoLaguna) tiro
    |otherwise = quedaEnCero tiro

--tunelConRampita :: Obstaculo
--tunelConRampita tiro 
--   |precision tiro > 90 = (cambiarVelocidad (*2) . cambiarPrecision (\_ -> 100). cambiarAltura (\_ -> 0)) tiro
--    | otherwise = quedaEnCero tiro

--laguna :: Int -> Obstaculo
--laguna largoLaguna tiro 
--    | (velocidad tiro > 80) && alturaEntre tiro =  cambiarAltura (div largoLaguna) tiro
--    | otherwise = quedaEnCero tiro

alturaEntre :: Tiro -> Bool
alturaEntre tiro = altura tiro >= 1 && altura tiro <= 5

--hoyo :: Obstaculo
--hoyo tiro 
--    |precision tiro > 95 && velocidadEntre tiro =quedaEnCero tiro
--    | otherwise = quedaEnCero tiro

velocidadEntre :: Tiro -> Bool
velocidadEntre tiro = velocidad tiro <=20 && velocidad tiro >= 5

quedaEnCero :: Tiro -> Tiro
quedaEnCero = cambiarVelocidad (\_ -> 0) . cambiarPrecision (\_ -> 0). cambiarAltura (\_ -> 0)

--------------------4

palosUtiles :: Jugador -> Obs -> [Palo]
palosUtiles jugador obstaculo = filter (condicionFiltrar jugador obstaculo) palos

condicionFiltrar :: Jugador -> Obs -> Palo -> Bool
condicionFiltrar jugador obstaculo palo = pasaObstaculo obstaculo (golpe jugador palo)

cuantosSupero :: [Obs] -> Tiro -> Int
cuantosSupero obstaculos tiro = length (supera obstaculos tiro)

supera :: [Obs] -> Tiro -> [Obs]
supera [] _ = []
supera (x:xs) tiro 
    |pasaObstaculo x tiro = x : supera xs (modificarTiro x tiro)
    |otherwise = []

paloMasUtil :: Jugador -> [Obs] -> Palo
paloMasUtil jugador obstaculos = foldl1 (maxObstaculos jugador obstaculos) palos 
    
maxSegun ()

buffMaximoAtaque = foldl1 (maxDePotenciar criterioDePersonaje personaje) buffs

maxSegun (criterioDePersonaje . potenciar personaje)


maxSegun f buff1 buff2
  | f buff1 > f buff2 = buff1
  | otherwise = buff2

--length (takeWhile (`pasaObstaculo` tiro) obstaculos)
------------------5

type Puntos = Int

padresQuePierden :: [(Jugador, Puntos)] -> [String]
padresQuePierden [] = []
padresQuePierden (x:xs) 