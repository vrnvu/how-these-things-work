module Wire where

data Wire = On | Off deriving (Show, Eq)

not :: Wire -> Wire
not On = Off
not Off = On

and :: Wire -> Wire -> Wire
and On On = On
and _ _ = Off

nand :: Wire -> Wire -> Wire
nand a b = Wire.not $ Wire.and a b

or :: Wire -> Wire -> Wire
or Off Off = Off
or _ _ = On

nor :: Wire -> Wire -> Wire
nor a b = Wire.not $ Wire.or a b

xor :: Wire -> Wire -> Wire
xor a b = Wire.or (Wire.and (Wire.not a) b) (Wire.and a (Wire.not b))

xnor :: Wire -> Wire -> Wire
xnor a b = Wire.not $ Wire.xor a b

sum :: Wire -> Wire -> Wire -> Wire
sum a b cin = Wire.xor (Wire.xor a b) cin


cout :: Wire -> Wire -> Wire -> Wire
cout a b cin = Wire.or 
    (Wire.or 
        (Wire.and a b) (Wire.and a cin))
    (Wire.and b cin)


split :: Wire -> (Wire, Wire)
split a = (a, Wire.not a)

demux2 :: [Wire] -> Wire -> ([Wire], [Wire])
demux2 multiwire d = (map (Wire.and positive) multiwire, map (Wire.and negative) multiwire)
    where (positive, negative) = Wire.split d


mux2 :: [Wire] -> [Wire] -> Wire -> [Wire]
mux2 a b w = zipWith (Wire.or) mapa mapb
    where
        mapa = (map (Wire.and positive) a)
        mapb = (map (Wire.and negative) b)
        (positive, negative) = Wire.split w
    
