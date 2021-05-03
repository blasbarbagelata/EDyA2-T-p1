
data NdTree p = Node (NdTree p)  -- subárbol izquierdo
                      p          -- punto
                      (NdTree p) -- subárbol derecho
                      Int        -- eje
                | Empty deriving (Eq, Ord, Show)

class Punto p where
  dimension :: p -> Int       -- devuelve el número de coordenadas de un punto
  coord :: Int -> p -> Double -- devuelve la coordenada k-ésima de un punto (comenzando de 0)
  dist :: p -> p -> Double    -- calcula la distancia entre dos puntos
  dist p q = sum [(coord i p - coord i q)^2 | i <- [0..(dimension p) - 1]] 

newtype Punto2d = P2d (Double, Double) deriving (Eq, Show)
newtype Punto3d = P3d (Double, Double, Double) deriving (Eq, Show)

instance Punto Punto2d  where
  dimension p = 2
  coord 0 (P2d (x,y)) = x
  coord 1 (P2d (x,y)) = y

instance Punto Punto3d where
  dimension p = 3
  coord 0 (P3d (x,y,z)) = x
  coord 1 (P3d (x,y,z)) = y
  coord 2 (P3d (x,y,z)) = z

treeDerecho :: NdTree p -> NdTree p
treeDerecho (Node  _ _ der _) = der

treeIzquierdo :: NdTree p -> NdTree p
treeIzquierdo (Node izq _ _ _) = izq

treePunto :: (Punto p) => NdTree p -> p  
treePunto (Node _ p _ _) = p

treeEje :: NdTree p -> Int
treeEje (Node _ _ _ eje) = eje

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

fromList :: Punto p => [p] -> NdTree p
fromList ps = fromListLevel ps 0

insertarlevel :: Punto p => p -> NdTree p -> Int -> NdTree p
insertarlevel p Empty level = Node Empty p Empty (mod level (dimension p))
insertarlevel p arbol level = let eje = treeEje arbol 
                              in if coord eje p <= coord eje (treePunto (arbol))
                              then insertarlevel p (treeIzquierdo arbol) (level + 1)
                              else insertarlevel p (treeDerecho arbol) (level + 1)

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p arbol = insertarlevel p arbol 0

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

listaP = [P2d(2,3), P2d(5,4), P2d(9,6),P2d(4,7), P2d(8,1), P2d(7,2)]

-- Transforma un NdTree en una lista de puntos
treeToList :: (Punto p) => NdTree p -> [p]
treeToList Empty = []
treeToList (Node izq root der eje) = treeToList izq ++ [root] ++ treeToList der

buscarReemplazoMin :: (Punto p) => NdTree p -> Int -> p
buscarReemplazoMin arbol eje = head (msortPunto (treeToList arbol) eje) -- El primer elemento es el minimo

buscarReemplazoMax :: (Punto p) => NdTree p -> Int -> p
buscarReemplazoMax arbol eje = last (msortPunto (treeToList arbol) eje) -- El ultimo elemento es el maximo

reemplazar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p 
reemplazar p (Node izq q der eje) = if der /= Empty 
                                           then let reemplazo = (buscarReemplazoMin der eje) 
                                                in (Node izq reemplazo (eliminar reemplazo der) eje)
                                           else let reemplazo = (buscarReemplazoMax izq eje) 
                                                in (Node (eliminar reemplazo izq) reemplazo der eje)

eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar p Empty = Empty
eliminar p (Node Empty q Empty eje) = if p == q then Empty else (Node Empty q Empty eje)
eliminar p (Node izq q der eje) | p == q = reemplazar p (Node izq q der eje) -- Reemplazos la raiz con el punto correspondiente
                                | coord eje p <= coord eje q = (Node (eliminar p izq) q der eje)
                                | otherwise = (Node izq q (eliminar p der) eje)
