module Parcial where
import Text.Show.Functions()

doble :: Int -> Int
doble = (*2)

data Perrito = UnPerrito{
    raza :: String,
    jugueteFavorito :: [String],
    tiempoPermanencia :: Int,
    energia :: Int
} deriving Show

type Ejercicio = Perrito -> Perrito --los ejercicios terminan siendo funciones que reciben unPerrito y tambien devuelven unPerrito
type Duracion = Int
type Rutina = [(Ejercicio, Duracion)]
data Guarderia = UnaGuarderia {
    nombre :: String,
    rutina :: Rutina
} deriving Show

cambioEnergia :: Int -> Perrito -> Perrito
cambioEnergia nueva unPerrito = unPerrito {energia = nueva}
cambioJuguetes :: ([String]-> [String]) -> Perrito -> Perrito
cambioJuguetes cambio unPerrito= unPerrito {jugueteFavorito = cambio (jugueteFavorito unPerrito)}

jugar :: Perrito -> Perrito --si es <0, que se quede en 0 -> max? -> si valor negativo entonces elige 0 (porque 0 va a ser mas grande)
jugar unPerrito = cambioEnergia (max 0 (energia unPerrito - 10)) unPerrito

ladrar :: Int-> Perrito -> Perrito
ladrar ladridos unPerrito = cambioEnergia (energia unPerrito + (div ladridos 2)) unPerrito

regalar :: String -> Perrito -> Perrito --agregar regalo a lista juguetes favoritos -> :
regalar juguete unPerrito = cambioJuguetes (juguete :) unPerrito

razaExtravagante :: Perrito -> Bool
razaExtravagante unPerrito = raza unPerrito == "dalmata" || raza unPerrito == "pomerania"
permanenciaMayor :: Perrito -> Bool
permanenciaMayor unPerrito = tiempoPermanencia unPerrito > 50
spaEvaluar :: Perrito -> Bool
spaEvaluar unPerrito = razaExtravagante unPerrito || permanenciaMayor unPerrito
diaDeSpa :: Perrito -> Perrito -- si x e y entonces hacer z -> guarda
diaDeSpa unPerrito
    | spaEvaluar unPerrito = (regalar "peine de goma" . cambioEnergia 100) unPerrito
    | otherwise = unPerrito

diaDeCampo :: Perrito -> Perrito
diaDeCampo unPerrito = cambioJuguetes (drop 1) unPerrito

zara :: Perrito
zara = UnPerrito "dalmata" ["pelota", "mantita"] 90 80
guarderia :: Guarderia
guarderia = UnaGuarderia "GuarderiaPdePerritos" 
            [(jugar, 30), 
            (ladrar 18, 20), 
            (regalar "pelota", 0), 
            (diaDeSpa, 120), 
            (diaDeCampo, 720)]

tiempoRutina:: Guarderia -> Int
tiempoRutina unaGuarderia = sum (map snd (rutina unaGuarderia))
perroAdmitido:: Perrito -> Guarderia -> Bool
perroAdmitido unPerrito unaGuarderia= tiempoPermanencia unPerrito > tiempoRutina unaGuarderia

perroResponsable:: Perrito -> Bool
perroResponsable unPerrito = length (jugueteFavorito (diaDeCampo unPerrito)) > 3