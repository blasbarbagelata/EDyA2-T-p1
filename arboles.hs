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
  --coord _ p = NaN

instance Punto Punto3d where
  dimension p = 3
  coord 0 (P3d (x,y,z)) = x
  coord 1 (P3d (x,y,z)) = y
  coord 2 (P3d (x,y,z)) = z
  --coord _ p = NaN


{-fromList :: Punto p => [p] -> NdTree p
fromList [] = Empty
fromList (p:ps) = -- Eleccion eje = level%dimension p -}

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

mediana :: Punto p => [p] -> Int -> p
mediana ps k = let ordps = msortPunto ps k
                   largo = length ordps in if (mod largo 2) == 0 then ordps!!(div largo 2)
                                                                 else ordps!!(div (largo-1) 2)

--prueba = mediana [P2d(2,3), P2d(5,4), P2d(9,6), P2d(4,7), P2d(8,1), P2d(7,2)] 0