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

type Rect = (Punto2d,Punto2d)

-- Dado un conjunto de puntos no vacio devuelve el minimo rectangulo que contenga a todos los puntos
minimumBox :: NdTree Punto2d -> Rect
minimumBox ndTree = let listPoints = treeToList ndTree
                        sortPointsX = msortPunto (listPoints) 0
                        sortPointsY = msortPunto (listPoints) 1
                        (minX, maxX) = (coord 0 (head sortPointsX), coord 0 (last sortPointsX))
                        (minY, maxY) = (coord 1 (head sortPointsY), coord 1 (last sortPointsY))
                        in (P2d (minX, minY), P2d (maxX, maxY))

-- Dado un punto dentro de un rectangulo, devuelve el subrectangulo a la izquierdda del punto 
leftBelowBox:: Punto2d -> Rect -> Int -> Rect
leftBelowBox (P2d(x,y)) (P2d(lowX, lowY), P2d(highX, highY)) 0 = (P2d(lowX, lowY), P2d(x, highY))
leftBelowBox (P2d(x,y)) (P2d(lowX, lowY), P2d(highX, highY)) 1 = (P2d(lowX, lowY), P2d(highX, y))

-- Dado un punto dentro de un rectangulo, devuelve el subrectangulo a la izquierdda del punto 
rightAboveBox:: Punto2d -> Rect -> Int -> Rect
rightAboveBox (P2d(x,y)) (P2d(lowX, lowY), P2d(highX, highY)) 0 = (P2d(x, lowY), P2d(highX, highY))
rightAboveBox (P2d(x,y)) (P2d(lowX, lowY), P2d(highX, highY)) 1 = (P2d(lowX, y), P2d(highX, highY))

-- Determina si el primer rectangulo contiene al segundo
boxContain :: Rect -> Rect -> Bool
boxContain rect (r1,r2) = inRegion r1 rect && inRegion r2 rect

-- Determina si los rectangulos se intersecan
boxIntersect :: Rect -> Rect -> Bool
boxIntersect rect (P2d(xlow, ylow),P2d(xhigh,yhigh)) = 
  (inRegion (P2d(xlow, ylow)) rect) || (inRegion (P2d(xhigh,yhigh)) rect) || (inRegion (P2d(xlow, yhigh)) rect) || (inRegion (P2d(xhigh, ylow)) rect)

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
ortogonalSearch ndTree (r1,r2) = 
  let rectangulo = lowHigh (r1,r2)
      box = minimumBox ndTree
  in if boxContain rectangulo box then treeToList ndTree 
     else if boxIntersect box rectangulo then ortogonalSearchBox ndTree rectangulo box else []