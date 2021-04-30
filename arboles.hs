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

newtype Punto2d = P2d (Double, Double)
newtype Punto3d = P3d (Double, Double, Double)

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

merge :: (Num a, Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then (x:merge xs (y:ys))
                                else (y:merge (x:xs) ys)

split :: (Num a, Ord a) => [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:zs) = let (xs, ys) = split zs in (x:xs, y:ys) 

msort :: (Num a, Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = let (ls, rs) = split xs
               (ls1, rs1) = (msort ls, msort rs)
           in merge ls1 rs1

mediana :: Punto p => [p] -> Int -> Double
mediana ps k = msort [coord k p | p<-ps]