{-data NdTree p = Node (NdTree p) -- subárbol izquierdo
                      p -- punto
                      (NdTree p) -- subárbol derecho
                      Int -- eje
                | Empty
deriving (Eq, Ord, Show)-}

class Punto p where
  dimension :: p -> Int -- devuelve el número de coordenadas de un punto
  coord :: Int -> p -> Double -- devuelve la coordenada k-ésima de un punto (comenzando de 0)
  dist :: p -> p -> Double -- calcula la distancia entre dos puntos
  dist p q = sum [(coord i p - coord i q)^2 | i <- [0..(dimension p) - 1]] 

newtype Punto2d = P2d (Double, Double)
newtype Punto3d = P3d (Double, Double, Double)

instance Punto Punto2d  where
  dimension p = 2
  coord i (P2d p) = if i == 0 then fst p else snd p

instance Punto Punto3d where
  dimension p = 3
  coord i (P3d (x,y,z)) = (if i == 0 then x else (if i == 1 then y else z )) 

