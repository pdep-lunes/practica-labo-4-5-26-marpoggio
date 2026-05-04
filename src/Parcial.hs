module Parcial where
import Text.Show.Functions()

doble :: Int -> Int
doble = (*2)

data Perrito = unPerrito{
    raza :: String
    jugueteFavorito :: [String]
    tiempoPermanencia :: Int
    energia :: Int
} deriving Show

data Guarderia = unaGuarderia {
    nombre :: String
    type rutina = [(nombre, duracion)]
} deriving Show

energiaNegativa :: Perrito -> Bool
energiaNegativa unPerrito = energia unPerrito > 0
jugar :: Bool -> Perrito -> Int
jugar energiaNegativa unPerrito = unPerrito {energia = (energia - 10)}

ladrar :: Perrito -> Int -> Int
ladrar unPerrito ladridos = unPerrito {energia = (energia + (ladridos/2))}

regalar :: Perrito -> String -> Perrito
regalar unPerrito juguete = juguete : jugueteFavorito unPerrito  --añadirle juguete especificado a lista ultimo

razaExtravagante :: Perrito -> Bool
razaExtravagante unPerrito = raza unPerrito == dalmata || unPerrito == pomerania
permanenciaMayor :: Perrito -> Bool
permanenciaMayor unPerrito = tiempoPermanencia unPerrito > 50
spaEvaluar :: Perrito -> Bool
spaEvaluar unPerrito = razaExtravagante unPerrito || permanenciaMayor unPerrito
spaSumarRegalar :: Perrito -> Int -> String
spaSumarRegalar unPerrito = spaSumarEnergia unPerrito{energia = 100, regalar "peine de goma"} --composicion con regalar

diaDeCampo :: Perrito -> Int -> [a] -> [a]
diaDeCampo unPerrito = jugueteFavorito = drop 1 jugueteFavorito unPerrito --perder 1er juguete en lista drop



zara :: Perrito
zara = un Perrito "dalamata", [pelota, mantita], 90, 80
guarderia :: Guarderia
guarderia = unaGuarderia "guarderiaPdePerritos" [(jugar, 30), (ladrar18, 20), (regalarPelota, 0), (diaDeSpa, 120), (diaDeCampo, 720)])

tiempoRutina::
tiempoRutina unaGuarderia = sum map 
perroAdmitido:: Perrito -> Bool
perroAdmitido unPerrito = 
