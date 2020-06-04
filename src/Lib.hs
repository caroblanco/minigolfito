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

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = crearTiro 10 0 ((precisionJugador habilidad) *2)

madera :: Palo
madera habilidad = crearTiro 100 5 ((precisionJugador habilidad) `div`2)

hierros :: Int -> Palo
hierros n habilidad = crearTiro ((fuerzaJugador habilidad) *n) (min 0 (n-3)) ((precisionJugador habilidad) `div`n) 

crearTiro :: Int -> Int -> Int -> Tiro
crearTiro velocidad altura precision = UnTiro {velocidad = velocidad, precision = precision , altura = altura}

--palos = [putter, madera,hierros 1, hierros 2, hierros 3, hierros 4 , hierros 5, hierros 6, hierros 7 ,hierros 8, hierros 9 ,hierros 10]
palos :: [Palo]
palos = [putter , madera] ++ map hierros [1..10]

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

---------------------3
{- OTRO
tiroDetenido = UnTiro 0 0 0

obstaculoSuperableSi :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo
obstaculoSuperableSi condicion efecto tiroOriginal
  | condicion tiroOriginal = efecto tiroOriginal
  | otherwise = tiroDetenido

type Obstaculo = Tiro -> Tiro

tunelConRampita :: Obstaculo
tunelConRampita = obstaculoSuperableSi superaTunelConRampita efectoTunelConRampita

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = UnTiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}

vaAlRasDelSuelo = (==0).altura

laguna :: Int -> Obstaculo
laguna largo = obstaculoSuperableSi superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5.altura) tiro
efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal `div` largo}

hoyo :: Obstaculo
hoyo = obstaculoSuperableSi superaHoyo efectoHoyo
superaHoyo tiro = (between 5 20.velocidad) tiro && vaAlRasDelSuelo tiro
efectoHoyo _ = tiroDetenido
-}

data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efectoLuegoDeSuperar :: Tiro -> Tiro
  }

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroOriginal
  | (puedeSuperar obstaculo) tiroOriginal = (efectoLuegoDeSuperar obstaculo) tiroOriginal
  | otherwise = tiroDetenido

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoTunelConRampita

tiroDetenido = UnTiro 0 0 0

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

vaAlRasDelSuelo = (== 0).altura

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiroOriginal = UnTiro {
  velocidad = velocidad tiroOriginal *2,
  precision = 100,
  altura = 0 }

laguna :: Int -> Obstaculo
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro
efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {
    altura = altura tiroOriginal `div` largo
  }

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && vaAlRasDelSuelo tiro && precision tiro > 95
efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroDetenido


{- MAL
cambiarVelocidad :: (Int -> Int) -> Tiro -> Tiro 
cambiarVelocidad f tiro = tiro {velocidad = f (velocidad tiro)}

cambiarAltura :: (Int -> Int) -> Tiro -> Tiro 
cambiarAltura f tiro = tiro {altura = f (altura tiro)}

cambiarPrecision :: (Int -> Int) -> Tiro -> Tiro 
cambiarPrecision f tiro = tiro {precision = f (precision tiro)}

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

alturaEntre :: Tiro -> Bool
alturaEntre tiro = altura tiro >= 1 && altura tiro <= 5

velocidadEntre :: Tiro -> Bool
velocidadEntre tiro = velocidad tiro <=20 && velocidad tiro >= 5

quedaEnCero :: Tiro -> Tiro
quedaEnCero = cambiarVelocidad (\_ -> 0) . cambiarPrecision (\_ -> 0). cambiarAltura (\_ -> 0)
-}

--------------------4

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = puedeSuperar obstaculo (golpe jugador palo)

-------B
cuantosSupero :: [Obstaculo] -> Tiro -> Int
cuantosSupero [] tiro = 0
cuantosSupero (x:xs) tiro 
  |puedeSuperar x tiro = 1 + cuantosSupero xs (efectoLuegoDeSuperar x tiro)
  |otherwise = 0

------------c
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (cuantosSupero obstaculos . golpe jugador) palos

maximoSegun :: Ord b=> (a->b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord x => (t->x)->(t->t->t)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

------------------5
jugadorDeTorneo = fst
puntosGanados = snd

padresQuePierden ::  [(Jugador, Puntos)] -> [String]
padresQuePierden lista = (map (padre.jugadorDeTorneo) . filter (not.gano lista)) lista
  
gano ::[(Jugador, Puntos)]-> (Jugador, Puntos) -> Bool
gano lista puntosDeUno = (all ((< puntosGanados puntosDeUno).puntosGanados) . filter (/= puntosDeUno)) lista

  
--map padre (map fst (filter (((foldl max 0 (map snd lista))/=).snd) lista))