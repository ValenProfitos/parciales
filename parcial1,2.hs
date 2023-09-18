-- Parcial 1 2022 04 25
--Tema A

------EJERCICIO 1------
--Se va a representar el juego piedra-papel-tijera

---A---
--definir el tipo Forma que consta de los constructores sin parametros Piedra, Papel y Tijera. El tipo Forma no debe estar en la clase Eq. Luego programar la funion le_gana ;: FOrma -> FOrma -> Bool que dadas dos valores f1 y f2 de tipo Forma devuelve True si y solo si la forma f1 le gana sobre la forma f2. La regla es : - La piedra le gana a la tijera. -La tijera al papel. -el papel a la piedra
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

data Forma = Piedra | Papel | Tijera

leGana :: Forma -> Forma -> Bool
leGana Piedra Tijera = True
leGana Piedra Papel = False
leGana Piedra Piedra = False
leGana Papel Tijera = False
leGana Papel Papel = False
leGana Papel Piedra = True
leGana Tijera Tijera = False
leGana Tijera Papel = True
leGana Tijera Piedra = False

---B---
--Definir el tipo Nombre como un sinonimo de String. Luego definir el tipo Jugador que consta de un unico constructor Mano que toma dos parametros, el primero de tipo NOmbre y el segundo de tipo Forma. Por ultimo programar la funcion ganador :: Jugador -> Jugador -> Maybe Nombre que dados dos valores j1 y j2 del tipo Jugador debe devolver el nombre del jugador ganador (el de j1 o j2 segun corresponda) usando el constructor Just, o Nothing en caso d que no haya ganador

type Nombre = String 

data Jugador = Mano Nombre Forma 

ganador :: Jugador -> Jugador -> Maybe Nombre
ganador (Mano n1 f1) (Mano n2 f2)   | leGana f1 f2 = Just n1
                                    | leGana f2 f1 = Just n2
                                    | otherwise = Nothing 

------EJERCICIO 2------
--Programar la funcion quienJugo :: FOrma -> [Jugador] -> [NOmbre] que dada una forma f y una lista de jugadores js devuelve los nombre de los js que usaron la forma f

---FUNCION AUXILIAR---
--Creo una funcion que me compare formas para poder buscar a los jugadores que tienen la forma que pido

mismaForma :: Forma -> Forma -> Bool
mismaForma Piedra Piedra = True
mismaForma Tijera Tijera = True
mismaForma Papel Papel = True 
mismaForma _ _ = False

quienJugo :: Forma -> [Jugador] -> [Nombre]
quienJugo _ [] = []
quienJugo f (Mano n f1:js)  | f `mismaForma` f1 = n : quienJugo f js 
                            | otherwise = quienJugo f js 

--quienJugo Piedra [Mano "Lauti" Piedra, Mano "Vale" Piedra, Mano "Agus" Papel] = ["Lauti","Vale"]

------EJERCICIO 3------
--Definir el tipo NotaMusical que con constructores Do,Re,Mi,Fa,Sol,La,Si. Los constructores no toman parametros. Lugo definir el tipo Figura de constructores Negra y Corchea. A partir de los tipos anteriores, definir el tipo recursivo Melodia cuyos constructor son -Entonar: Toma tres parametros. El primero de tipo Nota Musical(la nota que se agrega), el segundo de tipo Figura(que representa la duracion de la nota que s stra agregando) y el tercero de tipo Melodia(la melodia que se agrega la nueva nota y figura. -Vacia: no toma parametros y representa la melodia vacia)
--FInalmente programar la funcion contarTiempos :: Melodia -> Int que cuenta la cantidad de tiempos que tiene la melodia, sumando 2 por cada figura construida con Negra y sumando 1para las figuras construidas construidas con Corchea

data NotaMusical = Do | Re | Mi | Fa | Sol | La | Si 

data Figura = Negra | Corchea

data Melodia =  Entonar NotaMusical Figura Melodia
                | Vacia 

contarTiempos :: Melodia -> Int
contarTiempos Vacia = 0
contarTiempos (Entonar _ Negra fs) = 2 + contarTiempos fs
contarTiempos (Entonar _ Corchea fs) = 1 + contarTiempos fs 

------EJERCICIO 4------
--Programar la funcion arbol_sum :: Arbol Int -> Arbol Int -> Arbol Int que dado dos arboles as y bs devuelve un nuevo arbol cuyos valores son la suma d los elementos as y bs punto a punto

data Arbol a = Hoja | Rama (Arbol a) a (Arbol a) deriving Show

arbolSum :: Arbol Int -> Arbol Int -> Arbol Int 
arbolSum Hoja Hoja = Hoja
arbolSum (Rama a1 x1 a2) (Rama b1 x2 b2) = Rama (arbolSum a1 b1) (x1 + x2) (arbolSum a2 b2)
arbolSum a Hoja = a
arbolSum Hoja b = b 


--Tema B

------EJERCICIO 1------
--Se va a implementar algunos aspectos del juego escoba-del-quince

---A---
--Definir el tipo Palo que consta de los constructores Espada, Basto, Oro, Copa. Los constructores no toman parametros. EL tipo Palo no debe estar en la clase Eq. Luego programa la funcion usando patter matching: mismoPalo :: Palo -> Palo -> Bool que dados dos valores p1 y p2 del tipo Palo debe devolver True si y solo si p1 y p2 son del mismo palo

data Palo = Espada | Basto | Oro | Copa 

mismoPalo :: Palo -> Palo -> Bool
mismoPalo Espada Espada = True
mismoPalo Basto Basto = True
mismoPalo Oro Oro = True
mismoPalo Copa Copa = True 
mismoPalo _ _ = False  

---B---
--Como en este juego no se utilizan las cartas 9 y 9 definir el tipo FIgura que consta de constructores Uno, Dos, Tres, CUatro, CInco, Seis, Siete, Sota, Caballo Y Rey. El valor de una carta esta dado por su numero, slavo para la Sota, Caballo y Rey donde sus valores se pueden ver en la tabla. Programar la funcion valor_figura :: Figura -> INt que dada una fugura f devuelve su valor segun la tabla de mas arriba ahora definir el tipo Carta que tiene un unico constructor Naipe que toma dos parametros. EL primero de tipo FIgura y el segundo de tipo Palo. FInalmente programar la funion suma_cartas :: Carta -> Carta -> Maybe Int que dadas dos cartas c1 y c2, si tienen el mismo0 palo devuelve la suma de los valores de ambas cartas usando el constructor Just y si tienen distinto palo devuelve Nothing

data Fig = Uno | Dos | Tres | Cuatro | Cinco | Seis | Siete | Sota | Caballo | Rey deriving Show

valorCarta :: Fig -> Int
valorCarta Uno = 1
valorCarta Dos = 2
valorCarta Tres = 3
valorCarta Cuatro = 4
valorCarta Cinco = 5
valorCarta Seis = 6
valorCarta Siete = 7
valorCarta Sota = 8
valorCarta Caballo = 9
valorCarta Rey = 10

data Carta = Naipe Fig Palo 

sumarCartas :: Carta -> Carta -> Maybe Int
sumarCartas (Naipe f1 p1) (Naipe f2 p2) | mismoPalo p1 p2 = Just (valorCarta f1 + valorCarta f2 )
                                        | otherwise = Nothing


------Ejercicio 2------
--En esta version solo se pueden combinar si son del mismo palo programa la funcion compatibles :: Carta -> [Carta] -> [Figura] que dada una carta c y una lista de cartas cs devuelve las figuras de las carta de cs que son del mismo palo que c, y que al sumarles el valor de c no supera 15

---Funcion auxiliar---
--creo una funcion que me ddetermine que no supere 15

compatibles :: Carta -> [Carta] -> [Fig]
compatibles _ [] = []
compatibles (Naipe f p) cs = map (\(Naipe f _ )  -> f ) (filter (\(Naipe f2 p2 ) -> mismoPalo p p2 && noSupera15 f2) cs) 
        where 
            noSupera15 f2 = valorCarta f2 + valorCarta f <= 15


------EJERCICIO 3------

type Duracion = Int 
type Name = String

data Cancion = Tema Name Duracion

data Estado = Escuchado | NoEscuchado

data Playlist = EnLista Cancion Estado Playlist | VaciaP

tiempoReproduccion :: Playlist -> Int
tiempoReproduccion VaciaP = 0
tiempoReproduccion (EnLista (Tema _ d) Escuchado pl) = d + tiempoReproduccion pl
tiempoReproduccion (EnLista _ NoEscuchado pl) = tiempoReproduccion pl 


--cancion1 = Tema "Canción 1" 180
--cancion2 = Tema "Canción 2" 240
--cancion3 = Tema "Canción 3" 150

--playlistEjemplo = EnLista cancion1 Escuchado (EnLista cancion2 NoEscuchado (EnLista cancion3 Escuchado VaciaP))

--330