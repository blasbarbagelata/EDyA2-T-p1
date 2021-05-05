
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
listaP = [P2d(2,3), P2d(5,4), P2d(9,6),P2d(4,7), P2d(8,1), P2d(7,2)]

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
mergePunto :: Punto p => Int -> [p] -> [p] -> [p]
mergePunto k [] ys = ys
mergePunto k xs [] = xs
mergePunto k (x:xs) (y:ys) = if coord k x <= coord k y then (x:mergePunto k xs (y:ys))
                                                       else (y:mergePunto k (x:xs) ys)

split :: Punto p => [p] -> ([p], [p])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:zs) = let (xs, ys) = split zs in (x:xs, y:ys) 

msortPunto :: Punto p => [p] -> Int -> [p]
msortPunto [] k = []
msortPunto [x] k = [x]
msortPunto xs k = let (ls, rs) = split xs
                      (ls1, rs1) = (msortPunto ls k, msortPunto rs k)
                  in mergePunto k ls1 rs1

-------------------------------------------------------------------------------------

-- Apartado 2)

-- A partir de una lista de Puntos y un nivel, construye un NdTree
fromListLevel :: Punto p => [p] -> Int -> NdTree p
fromListLevel [] _ = Empty
fromListLevel [p] level = Node Empty p Empty (mod level (dimension p))
fromListLevel ps level = let eje = mod level (dimension (head ps))
                             ordenada = msortPunto ps eje                       -- Ordenadamos la lista de puntos
                             medianIndex = div (length ordenada) 2              -- Indice de la mediana
                             medianCoord = coord eje (ordenada !! medianIndex)  -- Coordenada respecto al eje de la mediana
                             izqPuntos = init (filter (\p -> coord eje p <= medianCoord) ordenada) -- Puntos con la coordenda menor o igual a la mediana
                             trueMedian = length izqPuntos                      -- Indice de la mediana que sera la raiz
                             derPuntos = drop (trueMedian+1) ordenada           -- Puntos estricamente mayores a la coordenada de la mediana
                             -- Si hay muchos con la coordenada correspondiente iguales, tomamos el ultimo que aparece en la lista
                             puntoM = ordenada !! trueMedian                    -- Punto mediana
                             izqNode = fromListLevel izqPuntos (level+1)
                             derNode = fromListLevel derPuntos (level+1)
                             in Node izqNode puntoM derNode eje                       

-- -- A partir de una lista de Puntos, construye un NdTree desde 0
fromList :: Punto p => [p] -> NdTree p
fromList ps = fromListLevel ps 0

-- Apartado 3)

insertarlevel :: Punto p => p -> NdTree p -> Int -> NdTree p
insertarlevel p Empty level = Node Empty p Empty (mod level (dimension p))
insertarlevel p arbol level = let eje = treeEje arbol 
                              in if coord eje p <= coord eje (treePunto (arbol))
                              then insertarlevel p (treeIzquierdo arbol) (level + 1)
                              else insertarlevel p (treeDerecho arbol) (level + 1)

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p arbol = insertarlevel p arbol 0

-- Apartado 4)

-- Transforma un NdTree en una lista de puntos
treeToList :: (Punto p) => NdTree p -> [p]
treeToList Empty = []
treeToList (Node izq root der eje) = treeToList izq ++ [root] ++ treeToList der

-- Devuelve el punto con que posee la coordenada minima del eje correspondiente
buscarReemplazoMin :: (Punto p) => NdTree p -> Int -> p
buscarReemplazoMin arbol eje = head (msortPunto (treeToList arbol) eje) -- El primer elemento es el minimo

-- Devuelve el punto con que posee la coordenada maxima del eje correspondiente
buscarReemplazoMax :: (Punto p) => NdTree p -> Int -> p
buscarReemplazoMax arbol eje = last (msortPunto (treeToList arbol) eje) -- El ultimo elemento es el maximo

-- Reemplaza la raiz de un arbol
reemplazar :: (Eq p, Punto p) => NdTree p -> NdTree p 
reemplazar (Node izq q der eje) = if der /= Empty 
                                  then let reemplazo = (buscarReemplazoMin der eje) 
                                       in (Node izq reemplazo (eliminar reemplazo der) eje)
                                  else let reemplazo = (buscarReemplazoMax izq eje) 
                                       in (Node (eliminar reemplazo izq) reemplazo der eje)

eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar p Empty = Empty
eliminar p (Node Empty q Empty eje) = if p == q then Empty else (Node Empty q Empty eje)
eliminar p (Node izq q der eje) | p == q = reemplazar (Node izq q der eje) -- Reemplazos la raiz con el punto correspondiente
                                | coord eje p <= coord eje q = (Node (eliminar p izq) q der eje)
                                | otherwise = (Node izq q (eliminar p der) eje)


-- Apartado 5)

type Rect = (Punto2d,Punto2d)

inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d(x,y)) (P2d(x1,y1),P2d(x2,y2)) = x >= (min x1 x2) && x <= (max x1 x2) && y >= (min y1 y2) && y <= (max y1 y2)
                                          -- Region delimitada por las coordenadas x / Region delimitada por las coordenadas y

ortogonalSearchFacil :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearchFacil pTree rect = filter (\p -> inRegion p rect) (treeToList pTree)

{--- Compara coordenadas de dos puntos dado un eje, devuelve True si el primero es estrictamente mas grande que el segundo
greater :: Punto p => Int -> p -> p -> Bool
greater eje p q = coord eje p > coord eje q

ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty _ = []
ortogonalSearch (Node Empty p Empty _) rect = if inRegion p rect then [p] else []
ortogonalSearch (Node izq p der eje) (q1,q2) = 
  let (minEje, maxEje) = if greater eje q1 q2 then (q2, q1) else (q1, q2)
      leftP = if greater eje minEje p then [] else ortogonalSearch izq (q1,q2)
      rightP = if greater eje maxEje p then ortogonalSearch der (q1,q2) else []
  in leftP ++ (if inRegion p (q1,q2) then [p] else []) ++ rightP-}

-- Dado un conjunto de puntos no vacio devuelve el minimo rectangulo que contenga a todos los puntos
minimumBoundingBox :: NdTree Punto2d -> Rect
minimumBoundingBox ndTree = let listPoints = treeToList ndTree
                                sortPointsX = msortPunto (listPoints) 0
                                sortPointsY = msortPunto (listPoints) 0
                                (minX, maxX) = (coord 0 (head sortPointsX), coord 0 (last sortPointsX))
                                (minY, maxY) = (coord 1 (head sortPointsY), coord 1 (last sortPointsY))
                                in (P2d (minX, minY), P2d (maxX, maxY))

-- Dado un punto dentro de un rectangulo, devuelve el subrectangulo a la izquierdda del punto 
leftBelowBox:: Punto2d -> Rect -> Int -> Rect
leftBelowBox (P2d(x,y)) (P2d(lowX, lowY), P2d(highX, highY)) 0 = (P2d(lowX, lowY), P2d(highX, y))
leftBelowBox (P2d(x,y)) (P2d(lowX, lowY), P2d(highX, highY)) 1 = (P2d(lowX, lowY), P2d(x, highY))
-- Dado un punto dentro de un rectangulo, devuelve el subrectangulo a la izquierdda del punto 
rightAboveBox:: Punto2d -> Rect -> Int -> Rect
rightAboveBox (P2d(x,y)) (P2d(lowX, lowY), P2d(highX, highY)) 0 = (P2d(lowX, y), P2d(highX, highY))
rightAboveBox (P2d(x,y)) (P2d(lowX, lowY), P2d(highX, highY)) 1 = (P2d(x, lowY), P2d(highX, highY))

-- Determina si el primer rectangulo contiene al segundo
boxContain :: Rect -> Rect -> Bool
boxContain rect (r1,r2) = inRegion r1 rect && inRegion r2 rect

-- Determina si los rectangulos se intersecan
boxIntersect :: Rect -> Rect -> Bool
boxIntersect rect (r1,r2) = inRegion r1 rect || inRegion r2 rect

-- Dado un rectangulo, pone los minimos valores en la primer componente y los mayores en la segunda
lowHigh :: Rect -> Rect
lowHigh (P2d(x1,y1),P2d(x2,y2)) = (P2d(min x1 x2, min y1 y2), P2d(max x1 x2, max y1 y2))

ortogonalSearchBox :: NdTree Punto2d -> Rect -> Rect -> [Punto2d]
ortogonalSearchBox Empty _ _ = []
ortogonalSearchBox (Node Empty p Empty _) rect _ = [p | inRegion p rect]
ortogonalSearchBox (Node izq p der eje) rect box = 
  let leftBox = leftBelowBox p box eje
      rightBox = rightAboveBox p box eje
      left = if boxContain rect leftBox then treeToList izq 
      else if boxIntersect rect leftBox then ortogonalSearchBox izq rect leftBox else []
      right = if boxContain rect rightBox then treeToList der 
      else if boxIntersect rect rightBox then ortogonalSearchBox der rect rightBox else []
  in left ++ [p | inRegion p rect] ++ right

ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty _ = []
ortogonalSearch (Node Empty p Empty _) rect = [p | inRegion p rect]
ortogonalSearch (Node izq p der eje) (r1,r2) = 
  let rectangulo = lowHigh (r1,r2)
      box = minimumBoundingBox (Node izq p der eje)
  in if boxContain box rectangulo then treeToList (Node izq p der eje) else ortogonalSearchBox (Node izq p der eje) rectangulo box