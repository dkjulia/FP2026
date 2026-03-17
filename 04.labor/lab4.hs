--I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

    -- az első n páros szám négyzetét,
    parosNegyzet n = [x**2 | x <-[2,4, ... n*2]]
    
    -- az első [ 1 , 2 , 2 , 3 , 3 , 3 , 4 , 4 , 4 , 4 , … ] ,

    fel2 n = take n (ls 1)
        where
            aux i = ls i ++ aux (i+1)
            ls i = replicate i i : ls (i+1)

    -- az első [ 2 , 4 , 4 , 6 , 6 , 6 , 8 , 8 , 8 , 8 … ] ,



    -- az első [ n , n − 1 , … , 2 , 1 , 1 , 2 , … , n − 1 , n ] ,
    -- váltakozva tartalmazzon True és False értékeket,
    -- váltakozva tartalmazza a 0 ,   1 ,   − 1 értékeket.

--II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

    -- meghatározza egy adott szám osztóinak számát,
    -- meghatározza egy adott szám legnagyobb páratlan osztóját,
    -- meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
    -- meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
    -- meghatározza az a és b közötti Fibonacci számokat, a > 50 .

--III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

    -- meghatározza egy lista pozitív elemeinek átlagát,
    -- meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,
    -- tükrözi egy lista elemeit,
    -- két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
    -- meghatározza egy lista leggyakrabban előforduló elemét.
