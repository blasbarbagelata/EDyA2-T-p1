
data NdTree p = Node (NdTree p)  -- subárbol izquierdo
                      p          -- punto
                      (NdTree p) -- subárbol derecho
                      Int        -- eje
                | Empty deriving (Eq, Ord, Show)

class Punto p where
  dimension :: p -> Int -- devuelve el número de coordenadas de un punto
  coord :: Int -> p -> Double -- devuelve la coordenada k-ésima de un punto (comenzando de 0)
  dist :: p -> p -> Double -- calcula la distancia entre dos puntos
  dist p q = sum [(coord i p - coord i q)^2 | i <- [0..(dimension p) - 1]] 

newtype Punto2d = P2d (Double, Double) deriving Show
newtype Punto3d = P3d (Double, Double, Double) deriving Show

instance Punto Punto2d  where
  dimension p = 2
  coord 0 (P2d (x,y)) = x
  coord 1 (P2d (x,y)) = y

instance Punto Punto3d where
  dimension p = 3
  coord 0 (P3d (x,y,z)) = x
  coord 1 (P3d (x,y,z)) = y
  coord 2 (P3d (x,y,z)) = z

derecho :: NdTree p-> NdTree p
derecho (Node  _ _ der _) = der

izquierdo :: NdTree p -> NdTree p
izquierdo (Node izq _ _ _) = izq

punto :: (Punto p) => NdTree p -> p  
punto (Node _ p _ _) = p

eje :: NdTree p -> Int
eje (Node _ _ _ eje) = eje

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
insertarlevel p arbolito level = let hiperplano = eje arbolito in if coord hiperplano p <= coord hiperplano (punto (arbolito))
                                                       then insertarlevel p (izquierdo arbolito) (level + 1)
                                                       else insertarlevel p (derecho arbolito) (level + 1)

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p arbolito = insertarlevel p arbolito 0

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

listaP = [P2d (7, 3), P2d(2,3), P2d(5,4), P2d(9,6),P2d(4,7), P2d(8,1), P2d(7,2), P2d (7, 5)]

eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar 