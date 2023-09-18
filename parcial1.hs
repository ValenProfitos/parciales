--Parcial 1 2020 09 14
--Tema A

------EJERCICIO 1------
--Programar la funcion estaEnDNI :: Int -> Bool que dado un numero, devuelve True si es una de las cifras de tu DNI.

estaEnDNI :: Int -> Bool
estaEnDNI x = (x == 1) || (x == 2) || (x == 3) || (x == 4) || (x == 8) || (x == 9) 

-- Numero de DNI: 43142839
-- estaEnDNI 1 = True
-- estaEnDNI 2 = True
-- estaEnDNI 3 = True
-- estaEnDNI 4 = True
-- estaEnDNI 8 = True
-- estaEnDNI 9 = True
-- estaEnDNI 7 = False

------EJERCICIO 2------
--Programar la funcion recursiva sumaDNI :: [Int] -> Int que dada una lista de enteros xs suma solo los elementos que son cifras d tu DNI

sumaDNI :: [Int] -> Int
sumaDNI [] = 0
sumaDNI (x:xs) = x + sumaDNI xs

--sumaDNI [4,3,1,4,2,8,3,9] = 34

------EJERCICIO 3------
--Programa mediate composicion sin recursion, usando sumatoria' definida en el apartado 4c del proyecto1, la funcion sumaDNI' :: [Int] -> Int que al igual que sumaDNI suma solo los elementos que son cifras de tu DNI. 
--Pueden definir una funion auxiliar con tipo Int -> Int que utilice a estaEnDNI

---SUMATORIA'---
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t

---FUNCION AUXILIAR---
--Creo una funcion auxiliar que dado un entero, si esta en mi DNI me devuelva el numero y sino que devuelva 0 (para que no interfiera con la suma de los numeros)
esCifraDNI :: Int -> Int
esCifraDNI x    | estaEnDNI x = x
                | otherwise = 0

sumaDNI' :: [Int] -> Int
sumaDNI' xs = sumatoria' xs esCifraDNI

--Da 34 proporcionando el mismo ejemplo anterior

------EJERCICIO 4------
--Programar mediante recursion la funcion reducir :: [a] -> (a -> a -> a) -> a que dada una lista xs y un operador op realiza dicha operacion entre todos los elementos de xs, para elo usar un caso base con listas de un solo elemento y un caso indctivo con listas de almenos dos elementos. la funcion reducir no funciona para las listas vacias

reducir :: [a] -> (a -> a -> a) -> a
reducir [x] _ = x
reducir (x:xs) op =  x `op` reducir xs op

--reducir [1,2,3,5] (+) = 11


--Tema B

------EJERCICIO 1------
--estaEnDNI

------EJERCICIO 2------
--Programar la funcion recursiva cuentaDNI :: [Int] -> Int que dada una lista de enteros xs cuenta la cantidad de elementos que son cifras de tu DNI

cuentaDNI :: [Int] -> Int
cuentaDNI [] = 0
cuentaDNI (x:xs)    | estaEnDNI x = 1 + cuentaDNI xs
                    | otherwise = cuentaDNI xs 

--cuentaDNI [4,3,1,4,2,8,3,9] = 8

------EJERCICIO 3------
--Programa mediante composicion sin recursion, uasndo sumatoria' definida en el apartado 4c del proyecto1 la funcion cuentaDNI' :: [Int] -> Int 
-- Pueden definir una funcion auxiliar tipo Int -> Int  que utilice a estaEnDNI

---FUNCION AUXILIAR---
--esCifraDNI 

cuentaDNI' :: [Int] -> Int
cuentaDNI' xs = sumatoria' xs esCifraDNI 

------EJERCICIO 4------
--Programar mediante recursion la funcion separar :: [a] -> (a -> Bool) -> ([a], [a]) que dada una lista xs y un predicado (a -> Bool) devuelve un par de lista en la primera los elementos que al aplicar el predicado dan True y en la segunda los elementos que al aplicar el predicado dan False

separar :: [a] -> (a -> Bool) -> ([a], [a])
separar xs p = (filter p xs, filter (not . p) xs)


--Tema C

------EJERCICIO 2------

cuentaNoDNI :: [Int] -> Int
cuentaNoDNI [] = 0
cuentaNoDNI (x:xs)  | estaEnDNI x = cuentaNoDNI xs
                    | otherwise = 1 + cuentaNoDNI xs 

------EJERCICIO 3------

---FUNCION AUXILIAR---
noEsCifra :: Int -> Int
noEsCifra x | estaEnDNI x = 0
            | otherwise = x

cuentaNoDNI' :: [Int] -> Int
cuentaNoDNI' xs = sumatoria' xs noEsCifra


------EJERCICIO 4------
aplicaSegun :: [Int] -> Int -> (Int -> a) -> (Int -> a) -> [a]
aplicaSegun [] _ _ _ = []
aplicaSegun (x:xs) n f g    | x >= n = f x : aplicaSegun xs n f g
                            | otherwise = g x : aplicaSegun xs n f g


--Tema D

------EJERCICIO 1------
--Programar la funcion letraEnApellido :: Char -> Bool que dado un caracter deuelve True si y solo si es una de las letras de tu apellido (en minuscula)

letraEnApellido :: Char -> Bool
letraEnApellido x = (x == 'p') || (x == 'r') || (x == 'o') || (x == 'f') || (x == 'i') || (x == 't') || (x == 's')

--El apellido usado es "PROFITOS"
-- letraEnApellido 'r' = True
-- letraEnApellido 'j' = False

------EJERCICIO 2------
--Programar la funcion recursiva cuentaEnApellido :: [Char] -> Int que dada una lista de caracteres xs devuelve la cantidad de letras (en minusculas) en xs que estan en tu apellido

cuentaEnApellido :: [Char] -> Int
cuentaEnApellido [] = 0
cuentaEnApellido (x:xs) | letraEnApellido x = 1 + cuentaEnApellido xs 
                        | otherwise = cuentaEnApellido xs 

--El apellido utilizado es "Profitos"
--cuentaEnApellido "suerte en el parcialito" = 9 (s,r,t,p,r,i,i,t,o)
--cuentaEnApellido "pROFITOS" = 1

------EJERCICIO 3------
--Programa mediante composicion sin recursion, usando sumatoria', la funcion cuentaEnApellido'
--Pueden definir una funcion auxiliar tipo Char -> Int que utilice a la funcion letraEnApellido

---FUNCION AUXILIAR---
esLetra :: Char -> Int
esLetra x   | letraEnApellido x = 1
            | otherwise = 0

cuentaEnApellido' :: [Char] -> Int
cuentaEnApellido' xs = sumatoria' xs esLetra 

--cuentaEnApellido' "dale porfi anda" = 5

------EJERCICIO 4------
--Programar mediante la recursion la funcion alterna :: [a] -> (a - > b) -> (a -> b) -> [b] que dada una lista xs y dos funciones f y g, devuelve una lista en la que a cada elemento de la lista de xs se le apica de manera alternada las funciones f y g 

alterna :: [a] -> (a -> b) -> (a -> b) -> [b]
alterna [] _ _ = []
alterna [x] f _ = [f x]
alterna (x:y:xs) f g = f x : g y : alterna xs f g

