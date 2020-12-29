module Week01.TowerOfHanoi where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numDiscs src dest temp
  | numDiscs == 1 = [(src, dest)]
  | otherwise     =
      let movesToTemp  = hanoi (numDiscs - 1) src temp dest
          moveTop      = hanoi 1 src dest temp
          moveFromTemp = hanoi (numDiscs - 1) temp dest src
       in movesToTemp ++ moveTop ++ moveFromTemp
