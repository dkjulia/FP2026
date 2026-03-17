--I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

    -- az első n páros szám négyzetét,
    parosNegyzet n = [x**2 | x <-[2,4 .. n*2 ]]
    
    -- az első [ 1 , 2 , 2 , 3 , 3 , 3 , 4 , 4 , 4 , 4 , … ] ,

    fel2 n = take n (ls 1)
        where
            aux i = ls i ++ aux (i+1)
            ls i = replicate i i : ls (i+1)

    fel2_2 n = aux n
        where
            aux 1 = replicate 1 1
            aux i = aux(i-1) ++ replicate i i


    -- az első [ 2 , 4 , 4 , 6 , 6 , 6 , 8 , 8 , 8 , 8 … ] ,

    fel3 n = aux n
        where 
            aux 1 = replicate 1 1
            aux i = aux (i-1) ++ replicate i (i*2)

    --fel3_2 n id
     --   |i /=n = replicate i (i*2)

    -- az első [ n , n − 1 , … , 2 , 1 , 1 , 2 , … , n − 1 , n ] ,

    fel4 n =[n, n-1 .. 1] ++ [1 .. n]

    -- váltakozva tartalmazzon True és False értékeket,

    --fel5 n = take n ls
      --  where true

    -- váltakozva tartalmazza a 0 ,   1 ,   − 1 értékeket.

    fel6 n = take n ls
        where 
            ls=[0,1,-1] ++ ls

--II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

    -- meghatározza egy adott szám osztóinak számát,
    
    osztok n = foldl(\res x -> if mod n x == 0 then res + 1 else res) 0 [1 .. n]

    osztok2 n = length [i | i <- [1 .. n], mod n i ==0]

    osztok3 n = foldl (\res x -> if mod n x == 0 then res + 1 else res) 1 [1 .. div n 2]


    -- meghatározza egy adott szám legnagyobb páratlan osztóját,
    
    -- MaxParatlanOsztok n = maximum []
    -- MaxParatlanOsztok2 n = last []

    -- meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
    -- meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
    -- meghatározza az a és b közötti Fibonacci számokat, a > 50 .

--III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

    -- meghatározza egy lista pozitív elemeinek átlagát,
    -- meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,
    -- tükrözi egy lista elemeit,
    -- két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
    -- meghatározza egy lista leggyakrabban előforduló elemét.
