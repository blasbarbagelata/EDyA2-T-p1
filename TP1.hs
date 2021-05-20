-- Trabajo Practico 1
-- Estructura de Datos y Algoritmos II

-- Blas Barbagelata - Tomas Castro Rojas

data NdTree p = Node (NdTree p)  -- subárbol izquierdo
                      p          -- punto
                      (NdTree p) -- subárbol derecho
                      Int        -- eje
                | Empty deriving (Eq, Ord, Show)

class Punto p where
  dimension :: p -> Int       -- devuelve el número de coordenadas de un punto
  coord :: Int -> p -> Double -- devuelve la coordenada k-ésima de un punto (comenzando de 0)
  dist :: p -> p -> Double    -- calcula la distancia entre dos puntos
  dist p q = sum [(coord i p - coord i q)^2 | i <- [0..((dimension p) - 1)]] 

newtype Punto2d = P2d (Double, Double) deriving (Eq, Show)
newtype Punto3d = P3d (Double, Double, Double) deriving (Eq, Show)

-- Apartado 1)

instance Punto Punto2d  where
  dimension p = 2
  coord 0 (P2d (x,y)) = x
  coord 1 (P2d (x,y)) = y

instance Punto Punto3d where
  dimension p = 3
  coord 0 (P3d (x,y,z)) = x
  coord 1 (P3d (x,y,z)) = y
  coord 2 (P3d (x,y,z)) = z

-- Lista de puntos del ejemplo en el enunciado
listaP = [P2d(2,3), P2d(5,4), P2d(9,6),P2d(4,7), P2d(8,1), P2d(7,2){-, P2d(1,10), P2d(3,5), P2d(6,8)-}]

arbol = insertar (P2d(7,2)) Empty 
arbol1 = insertar (P2d(5,4)) arbol
arbol2 = insertar (P2d(9,6)) arbol1
arbol3 = insertar (P2d(8,1)) arbol2
arbol4 = insertar (P2d(4,7)) arbol3
arbol5 = insertar (P2d(2,3)) arbol4

-- Funciones para acceder a los campos de NdTree -------------------------------
treeDerecho :: NdTree p -> NdTree p
treeDerecho (Node  _ _ der _) = der

treeIzquierdo :: NdTree p -> NdTree p
treeIzquierdo (Node izq _ _ _) = izq

treePunto :: (Punto p) => NdTree p -> p  
treePunto (Node _ p _ _) = p

treeEje :: NdTree p -> Int
treeEje (Node _ _ _ eje) = eje

-- Implementacion de MergeSort para la clase Puntos -----------------------------
mergePunto :: Punto p => Int -> [p] -> [p] -> Bool -> [p]
mergePunto k [] ys _ = ys
mergePunto k xs [] _ = xs
mergePunto k (x:xs) (y:ys) False = if coord k x <= coord k y then (x:mergePunto k xs (y:ys) False)
                                                             else (y:mergePunto k (x:xs) ys False)
mergePunto k (x:xs) (y:ys) True = if coord k x >= coord k y then (x:mergePunto k xs (y:ys) True)
                                                            else (y:mergePunto k (x:xs) ys True)

split :: Punto p => [p] -> ([p], [p])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:zs) = let (xs, ys) = split zs in (x:xs, y:ys) 

msortPunto :: Punto p => [p] -> Int -> Bool -> [p]
msortPunto [] k _ = []
msortPunto [x] k _ = [x]
msortPunto xs k orden = let (ls, rs) = split xs
                            (ls1, rs1) = (msortPunto ls k orden, msortPunto rs k orden)
                        in mergePunto k ls1 rs1 orden

-------------------------------------------------------------------------------------

-- Apartado 2)

-- A partir de una lista de Puntos y un nivel, construye un NdTree
fromListLevel :: Punto p => [p] -> Int -> NdTree p
fromListLevel [] _ = Empty
fromListLevel [p] nivel = Node Empty p Empty (mod nivel (dimension p))
fromListLevel ps nivel = let eje = mod nivel (dimension (head ps))
                             listaOrd = msortPunto ps eje False                 -- Ordenadamos la lista de puntos
                             medianaInd = div (length listaOrd) 2               -- Indice de la mediana
                             medianaCoord = coord eje (listaOrd !! medianaInd)  -- Coordenada respecto al eje de la mediana
                             -- se podria usar drop
                             izqPuntos = init (filter (\p -> coord eje p <= medianaCoord) listaOrd) -- Puntos con la coordenda menor o igual a la mediana
                             trueMedianaInd = length izqPuntos                  -- Indice de la mediana que sera la raiz
                             derPuntos = drop (trueMedianaInd+1) listaOrd       -- Puntos estricamente mayores a la coordenada de la mediana
                             -- Si hay muchos puntos con la coordenada correspondiente iguales, tomamos el ultimo que aparece en la lista
                             puntoM = listaOrd !! trueMedianaInd                -- Punto mediana
                             izqNodo = fromListLevel izqPuntos (nivel+1)
                             derNode = fromListLevel derPuntos (nivel+1)
                             in Node izqNodo puntoM derNode eje                       

-- -- A partir de una lista de Puntos, construye un NdTree desde 0
fromList :: Punto p => [p] -> NdTree p
fromList ps = fromListLevel ps 0

-- Apartado 3)

-- Inserta un punto en un arbol segun el nivel
insertarlevel :: Punto p => p -> NdTree p -> Int -> NdTree p
insertarlevel p Empty nivel = Node Empty p Empty (mod nivel (dimension p))
insertarlevel p (Node izq q der eje) nivel = if coord eje p <= coord eje q
                                             then Node (insertarlevel p izq (nivel + 1)) q der eje
                                             else Node izq q (insertarlevel p der (nivel + 1)) eje

-- Inserta un punto en un NdTree
insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p arbol = insertarlevel p arbol 0

-- Apartado 4)

-- Transforma un NdTree en una lista de puntos
treeToList :: (Punto p) => NdTree p -> [p]
treeToList Empty = []
treeToList (Node izq punto der eje) = treeToList izq ++ [punto] ++ treeToList der

-- Devuelve el punto con que posee la coordenada minima del eje correspondiente
buscarMinimo :: (Punto p) => NdTree p -> Int -> p
buscarMinimo (Node Empty p Empty _) _ = p
buscarMinimo (Node izq p Empty eje) ejeBusqueda = if eje == ejeBusqueda then (buscarMinimo izq ejeBusqueda) 
                                                  else  min2 p (buscarMinimo izq ejeBusqueda) ejeBusqueda
buscarMinimo (Node Empty p der eje) ejeBusqueda = if eje == ejeBusqueda then p 
                                                  else min2 p (buscarMinimo der ejeBusqueda) ejeBusqueda
buscarMinimo (Node izq p der eje) ejeBusqueda = if eje == ejeBusqueda then min2 p (buscarMinimo izq ejeBusqueda) ejeBusqueda 
                                                else min3 p (buscarMinimo izq ejeBusqueda)  (buscarMinimo der ejeBusqueda) ejeBusqueda

min2 :: Punto p => p -> p -> Int -> p
min2 p q eje = if coord eje p < coord eje q then p else q

min3 :: Punto p => p -> p -> p -> Int -> p
min3 p q r eje = min2 p (min2 q r eje) eje


-- Devuelve el punto con que posee la coordenada maxima del eje correspondiente

buscarMaximo :: (Punto p) => NdTree p -> Int -> p
buscarMaximo (Node Empty p Empty _) _ = p
buscarMaximo (Node izq p Empty eje) ejeBusqueda = if eje == ejeBusqueda then p 
                                                  else max2 p (buscarMaximo izq ejeBusqueda) ejeBusqueda
buscarMaximo (Node Empty p der eje) ejeBusqueda = if eje == ejeBusqueda then (buscarMaximo der ejeBusqueda) 
                                                  else max2 p (buscarMaximo der ejeBusqueda) ejeBusqueda
buscarMaximo (Node izq p der eje) ejeBusqueda = if eje == ejeBusqueda then max2 p (buscarMaximo der ejeBusqueda) ejeBusqueda 
                                                else max3 p (buscarMaximo izq ejeBusqueda)  (buscarMaximo der ejeBusqueda) ejeBusqueda

max2 :: Punto p => p -> p -> Int -> p
max2 p q eje = if coord eje p < coord eje q then q else p

max3 :: Punto p => p -> p -> p -> Int -> p
max3 p q r eje = max2 p (max2 q r eje) eje


-- Reemplaza la raiz de un arbol
reemplazar :: (Eq p, Punto p) => NdTree p -> NdTree p 
reemplazar (Node izq _ der eje) = if der /= Empty 
                                  then let reemplazo = (buscarMinimo der eje) 
                                       in (Node izq reemplazo (eliminar reemplazo der) eje)
                                  else let reemplazo = (buscarMaximo izq eje) 
                                       in (Node (eliminar reemplazo izq) reemplazo der eje)

-- Elimina un punto de un conjunto de Puntos.
eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar p Empty = Empty
eliminar p (Node Empty q Empty eje) = if p == q then Empty else (Node Empty q Empty eje)
eliminar p (Node izq q der eje) | p == q = reemplazar (Node izq q der eje) -- Reemplazos la raiz con el punto correspondiente
                                | coord eje p <= coord eje q = (Node (eliminar p izq) q der eje)
                                | otherwise = (Node izq q (eliminar p der) eje)


-- Apartado 5)

type Rect = (Punto2d,Punto2d)

-- Determina si un punto se encuentra dentro de un rectangulo
inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d(x,y)) (P2d(x1,y1),P2d(x2,y2)) = x >= (min x1 x2) && x <= (max x1 x2) && y >= (min y1 y2) && y <= (max y1 y2)
                                          -- Region delimitada por las coordenadas x / Region delimitada por las coordenadas y

-- Compara coordenadas de dos puntos dado un eje, devuelve True si el primero es estrictamente mas grande que el segundo
greater :: Punto p => Int -> p -> p -> Bool
greater eje p q = coord eje p > coord eje q

-- Dado un rectangulo, pone los minimos valores en la primer componente y los mayores en la segunda
lowHigh :: Rect -> Rect
lowHigh (P2d(x1,y1),P2d(x2,y2)) = (P2d(min x1 x2, min y1 y2), P2d(max x1 x2, max y1 y2))

-- Dado un conjunto de Puntos y un rectangulo en forma (min,max), devuelve la lista de puntos que estan dentro del rectangulo.
ortogonalSearchR :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearchR Empty _ = []
ortogonalSearchR (Node Empty p Empty _) rect = if inRegion p rect then [p] else []
ortogonalSearchR (Node izq p der eje) (min,max) = let izqPuntos = if greater eje min p then [] else ortogonalSearchR izq (min,max)
                                                      derPuntos = if greater eje max p then ortogonalSearchR der (min,max) else []
                                                  in izqPuntos ++ (if inRegion p (min,max) then [p] else []) ++ derPuntos

-- Configura el rectangulo en forma (min, max), luego llama a ortogonalSearchR
ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch ndArbol rect = let rectangulo = lowHigh rect in ortogonalSearchR ndArbol rectangulo