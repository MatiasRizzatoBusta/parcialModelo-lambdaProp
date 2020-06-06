module Lib where
import Text.Show.Functions
laVerdad = True
type Barrio = String
type Mail = String
data Depto = Depto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}deriving(Show)

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]
  
------------------------------------------- Punto 1 -------------------------------------------
mayor :: (Eq a , Ord b)=> (a->b)->a->a->Bool
mayor funcion valor  =  (< funcion valor).funcion  --se fija que el segundo sea menor que el primero. Al reves(< func valor) 
                                                   -- se fija que el seg sea mayor que el primero (menor a mayor)
menor :: (Eq a , Ord b)=>(a->b)->a->a->Bool
menor funcion valor  = not.mayor funcion valor

{-
ordenarSegun (mayor.length) lista
Entonces el criterio que ordenarSegun aplcia el lenght a los dos strings que va a evaluar y les aplica la funcion mayor para 
ver cual es mayor y ordenar de esta forma la lista.
Eq a es para los valores a comparar que pueden ser strings,Ints o floats y el Ord b es para lo que devuelve la funcion que le pase
para que sepa que son elementos con un orden y pueda comparalos.
ej: ordenarSegun (mayor length) ["gol","hola","martes","sopa do macaco"] devuelve ["sopa do macaco","martes","hola","gol"]
-}
------------------------------------------- Punto 2 -------------------------------------------
type Requisito = Depto -> Bool
type Busqueda = [Requisito]
ubicadoEn :: [Barrio]->Requisito
ubicadoEn listaBarrios depto = any (== barrio depto) listaBarrios
--este any esta para iterar la lista de depros y ver al primero q cumple

algunoCumple :: [String]->Requisito
algunoCumple listaBarrios depto = any (==(barrio depto)) listaBarrios

cumpleRango :: Ord a =>(Depto->a)->a->a->Requisito
cumpleRango funcion valorMaximo valorMinimo  = (between valorMaximo valorMinimo).funcion
------------------------------------------- Punto 3 ------------------------------------------- 
cumpleBusqueda :: Depto->Busqueda->Bool 
cumpleBusqueda depto busqueda  = all (cumpleRequisito depto) busqueda

cumpleRequisito :: Depto->Requisito->Bool
cumpleRequisito depto requisito  = requisito depto

buscar :: Busqueda->(Depto->Depto->Bool)->[Depto]->[Depto]
buscar requisitosBusqueda ordenamiento = ordenarSegun ordenamiento.filter (flip cumpleBusqueda requisitosBusqueda)
--flip hace que a cumple busqueda le lleguen los datos como Busqueda->Depto
-- a la lista filtrada la ordeno por el ordenamiento que quiero

ejemplo :: Busqueda
ejemplo = [ubicadoEn ["Recoleta", "Palermo"], cumpleRango ambientes 1 2,cumpleRango precio 0 6000]
------------------------------------------- Punto 4 -------------------------------------------
mailsDePersonasInteresadas :: Depto->[Persona]->[Mail]
mailsDePersonasInteresadas depto = map mail.filter (estaInteresada depto)

estaInteresada :: Depto->Persona->Bool
estaInteresada depto  = any (cumpleBusqueda  depto).busquedas