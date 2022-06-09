import Data.Char
{------------------------------------------
Prova 02 Programação Funcional 18/08/2021
-------------------------------------------
Nome completo 01: Arthur Barros Klimas
Matricula 01: 2020.1.08.004

Nome completo 02: André Neves Medeiros
Matrícula 02: 2020.1.08.028

-------------------------------------------
Instruções para o preenchimento do script
a) Implemente o código na sequência em que aparece na prova
b) separe cada questão com as linhas pontilhadas (abaixo)
   {--Questão X -----------------------------------------------}
c) Questões com mais de um item, separe-os da seguinte forma:

   {--Questão X ----------------------------------------------}

--Item X.a
 código da questão X item a

--Item X.b
  código da questão X item b

IMPORTANTE: Todo o script deve ter as citações de todas as questões e itens.

Então, as questões ou itens não solucionados por você deverão receber, impreterivelmente, o texto --EM BRANCO --
Caso você não organize a prova como indicado, poderá ser penalizado na nota final

d) Não use nomes de funções diferentes dos indicados nas questões.
De forma alternativa, e caso o nome não seja sugerido, dê preferência por começar como: funcQx, em que x refere-se à questão.
Ex: funcQ3a pode ser a implementação da questão 3, item a

e) não é necessário (mas não é um problema) incluir o enunciado das questões no script
f) Você pode inserir comentários pessoais explicando o código implementado antes do cabeçalho da função

g) QUESTÕES COM TÉCNICAS NÃO APRESENTADAS EM AULA NÃO SERÃO CONSIDERADAS


-------------------------------------------}

-- +++++++++++++++++ comece, aqui, sua prova. Boa prova +++++++++++++++++++++++ --

{----Questão 1 ------------------------------------------------------}

--Questão 1.a
type Brinde = (String, Int, Int)

mapa::Int->Brinde
mapa 1=("Natal", 21, 34)
mapa 2=("Bertioga", 17, 65)
mapa 3=("Rio de Janeiro", 9, 10)
mapa 4=("Curitiba", 3, 54)
mapa 5=("Petrolina", 2, 09)
mapa 6=("Salvador", 0, 01)
mapa 7=("Teresina", 21, 56)


--Questão 1.b
cidade::Brinde->String
cidade (x, y, z) = x

nPassagens::Brinde->Int
nPassagens (x, y, z) = y

nHospedagens::Brinde->Int
nHospedagens (x, y, z) = z


--Questão 1.c
funcQ1c::Int->Int
funcQ1c 0 = 0
funcQ1c x = nPassagens (mapa (x)) + funcQ1c(x-1)


--Questão 1.d
funcQ1d::Int->Int
funcQ1d 0 = 0
funcQ1d x = nHospedagens (mapa (x)) + funcQ1d(x-1)


--Questão 1.e
check1::String->Int->(Int, Int, Int)
check1 _ 0 = (0, 0, 0)
check1 x y
	|x == cidade(mapa(y)) = (y, nPassagens(mapa y), nHospedagens(mapa y))
	|otherwise = check1 x (y-1)

funcQ1e::String->(Int,Int,Int)
funcQ1e x = check1 x 7


--Questão 1.f
gerDoSistema::Brinde->Int->(Int,Int)
gerDoSistema _ 0 = (-1,-1)
gerDoSistema (a,b,c) z
	|a == cidade(mapa z) = (nPassagens(mapa z), nHospedagens(mapa z))
	|otherwise = gerDoSistema (a,b,c) (z-1)

funcQ1f::Brinde->Bool
funcQ1f (x,y,z)
	|gerDoSistema (x,y,z) 7 == (-1,-1) = False
	|y <= fst(gerDoSistema (x,y,z) 7) && z <= snd(gerDoSistema (x,y,z) 7) = True
	|otherwise = False


{----Questão 2 ------------------------------------------------------}
proximo::[(Char,Int)]->(Char,Int)
proximo [t] = t
proximo ((a,b):(c,d):x)
	|b<d = proximo ((a,b):x)
	|otherwise = proximo ((c,d):x)

realizado::(Char,Int)->[(Char,Int)]->[(Char,Int)]
realizado _ [] = []
realizado (k,m) ((a,b):x)
	|m == b = x
	|otherwise = (a,b):(realizado (k,m) x)

f01::[(Char,Int)]->String
f01 [] = []
f01 x = (fst (proximo x)):(f01 (realizado (proximo x) x))


--Questão 2.a
--"proximo" passa por uma lista de duplas [(Char, Int)] e retorna a dupla com o menor número da lista. Mas retorna o próximo se um número for igual ao próximo.
--Por exemplo, "proximo [('x',9), ('y', 4), ('z', 8)]" retorna "('y',4)". Mas se a entrada for "proximo [('x',20), ('y', 20), ('z', 30)]", a saída será "('y',20)".


--Questão 2.b
--"realizado" retira da lista de tuplas [(Char, Int)] o primeiro elemento em que o Int é igual ao Int da tupla (Char, Int) caso haja tal igualdade, senão retorna a lista normal.
--Por exemplo, "realizado ('w', 2) [('x',1), ('y', 2), ('z', 3)]" retorna "[('x',1),('z',3)]".


--Questão 2.c
--"f01" retorna uma String com todos os Char da lista de tuplas fornecida. A ordem é do menor Int para o maior Int. Com dois Int iguais, retorna o Char da tupla seguinte duas vezes.
--Por exemplo, "f01 [('t',3), ('h',4),('r',2),('r',6),('u',5),('a',1)]" retorna "arthur".


--Questão 2.d
--"proximo" retornará erro caso apenas "[]" seja dado, mas isso não ocorrerá na chamada por f01, pois f01 já possúi uma checkagem para este erro, não podendo mandar "[]" como parâmetro. "proximo" também não retornará erro por recursividade, afinal tem a checkagem de apenas um parâmetro.


--Questão 2.e
--t=(Char,Int)
--a=Char
--b=Int
--c=Char
--x=[(Char, Int)]


{----Questão 3 ------------------------------------------------------}
converterI :: String -> [Int]
converterI [] = []
converterI (a:b) = (ord(a) - 97) : converterI b

compararL2 :: [[Int]] -> [Int] -> [[Int]]
compararL2 [] _ = []
compararL2 (a:b) x
	|compararL a x = a : compararL2 b x
	|otherwise = compararL2 b x

compararL :: [Int] -> [Int] -> Bool
compararL [] _ = False
compararL (a:b) (c:d)
	|comparar a (c:d) = True
	|otherwise = compararL b (c:d)

comparar :: Int -> [Int] -> Bool
comparar _ [] = False
comparar x (a:b)
	|x == a = True
	|otherwise = comparar x b

converterS :: [[Int]] -> [String]
converterS [] = []
converterS (a:b) = converterL a : converterS b

converterL :: [Int] -> String
converterL [] = []
converterL (x:y) = chr(x + 97) : converterL y

funcQ3::[[Int]]->String->[String]
funcQ3 x y = converterS (compararL2 x (converterI y))

{----Questão 4 ------------------------------------------------------}
conta4 :: String -> Int -> Int
conta4 [] x = x
conta4 (a:b) x = conta4 b (x+1)

funcQ4 :: [String] -> [(Int,String)]
funcQ4 l = [((conta4 a 0), a)| a<-l]

{----Questão 5 ------------------------------------------------------}
--Não soube utilizar o "map", mas a solução resulta o pedido no exercício
elimina5::[Int]->[Int]
elimina5 ([]) = []
elimina5 (x:[]) = [x]
elimina5 (x:y:b) = [x]++elimina5 b

funcQ5::[([Int],Bool)]->[[Int]]
funcQ5 [] = []
funcQ5 (a:b)
	|(snd a)==True= (fst a):funcQ5 b
	|otherwise = elimina5 (fst a) : funcQ5 b


{----Questão 6 ------------------------------------------------------}

funcQ6 :: [Int] -> [Int]
funcQ6 [] = []
funcQ6 [t] = [t]
funcQ6 (x:y:z)
	|x `mod` 2 /= 0 && y `mod` 2 == 0 = x : funcQ6 z
	|otherwise = x : funcQ6 (y:z)

{----Questão 7 ------------------------------------------------------}
pos::[Int]->[Int]->Int->[Int]
pos [] [] _ = []
pos (a:b) (c:d) z
	|a == c = pos b d (z+1)
	|otherwise = [z] ++ (pos b (c:d) (z+1))

funcQ7 :: [Int] -> [Int] -> ([Int], [Int])
funcQ7 x y = (y, pos x y 0)


{----Questão 8 ------------------------------------------------------}

--EM BRANCO--

{----Questão 9 ------------------------------------------------------}
count9 :: String -> Int -> Int
count9 [] x = x
count9 (a:b) x
	|ord a >= 48 && ord a <= 57 = count9 b (x+1)
	|otherwise = count9 b x

funcQ9 :: String -> Int
funcQ9 y = count9 y 0


{----Questão 10 ------------------------------------------------------}
comparar10::[Int]->Int->Int->Bool
comparar10 [] _ _ = False
comparar10 (a:b) x i
	|x < 0 = False
	|i /= x = comparar10 b x (i+1)
	|a == x = True
	|otherwise = False


funcQ10 :: [Int] -> Int -> Bool
funcQ10 (a:b) x = comparar10 (a:b) x 0


{----Questão 11 ------------------------------------------------------}
funcQ11 :: [(Int,Int)] -> Int -> [Bool]
funcQ11 [] _ = []
funcQ11 (y:z) x
	|(fst y + snd y) > x = True : funcQ11 z x
	|otherwise = False : funcQ11 z x


{----Questão 12 ------------------------------------------------------}
div12 :: Int -> Int -> Int -> [Int]
div12 x y z
	|(x * z) <= y = [x*z] ++ div12 x y (z+1)
	|otherwise = []

nDiv :: Int -> Int -> [Int] -> [Int]
nDiv x y []
	|x <= y = [x] ++ nDiv (x+1) y []
	|otherwise = []
nDiv x y (a:b)
	|x < a = [x] ++ nDiv (x+1) y (a:b)
	|otherwise = nDiv (x+1) y b


funcQ12 :: Int -> Int -> ([Int],[Int])
funcQ12 x y = (div12 x y 1, nDiv x y (div12 x y 1))

{----Questão 13 ------------------------------------------------------}
count13 :: String -> Int -> Int
count13 [] x = x
count13 (a:b) x = count13 b (x+1)

cat13 :: String -> Int -> String
cat13 _ 0 = []
cat13 x y = x ++ cat13 x (y-1)

funcQ13 :: String -> String
funcQ13 s = cat13 s (count13 s 0)


{----Questão 14------------------------------------------------------}
--Questão 14.a
--São funções que recebem funções como argumentos.

--Questão 14.b
--Têm a vantagem da possibilidade de funções genéricas, que economizam espaço, tempo e trabalho ao excluír a necessidade de criar diversas funções para trabalhos parecidos.

--Questão 14.c
--Avaliação preguiçosa é um recurso que avalia expressões apenas quando pedidas.
--Permitindo, por exemplo, a manipulação de lsitas infinitas, impossíveis em C.

{----Questão 15 ------------------------------------------------------}

--EM BRANCO--

{----Questão 16 ------------------------------------------------------}
pos16 :: Int -> String -> Int -> Char
pos16 _ [] _ = '\0'
pos16 x (a:b) i
	|x == i = a
	|otherwise = pos16 x b (i+1)

funcQ16 :: [(Int,String)] -> String
funcQ16 [] = []
funcQ16 (a:b) = pos16 (fst a) (snd a) 0 : funcQ16 b


{----Questão 17 ------------------------------------------------------}
funcQ17::[Int]
funcQ17 = [0..]


{----Questão 18 ------------------------------------------------------}
filtraElimina::(Int->Bool)->[Int]->[Int]
filtraElimina _ [] = []
filtraElimina f (x:a)
	|f x == True = filtraElimina f a
	|otherwise = x:filtraElimina f a


{----Questão 19 ------------------------------------------------------}
--A função pedida utiliza três outras funções que, cada uma, resolve 1/3 do problema, sendo estas as funções 'check', que recebem a lista original e retornam, cada uma delas, uma das listas da tupla pedida.

check2::[Int]->[Int]
check2 [] = []
check2 (x:a)
	|x `mod` 2 ==0 = (x:check2 a)
	|otherwise = (check2 a)

check3::[Int]->[Int]
check3 [] = []
check3 (x:a)
	|x `mod` 3 ==0 = (x:check3 a)
	|otherwise = (check3 a)

checknot23::[Int]->[Int]
checknot23 [] = []
checknot23 (x:a)
	|x `mod` 2  /=0 && x `mod` 3 /=0 = (x:checknot23 a)
	|otherwise = (checknot23 a)

funcQ19 :: [Int]->([Int],[Int],[Int])
funcQ19 [] = ([],[],[])
funcQ19 x = (check2 x, check3 x , checknot23 x)


{----Questão 20 ------------------------------------------------------}
maior20 :: [Int] -> Int
maior20 (a:b:c)
	| a >= b && c == [] = a
	| a < b && c == [] = b
	| a >= b = maior20 (a:c)
	| b > a = maior20(b:c)

funcQ20::[[Int]]->[Int]
funcQ20 [] = []
funcQ20 (a:b) = maior20 (a) : funcQ20(b)


{----Questão 21 ------------------------------------------------------}
funcQ21::Char->[Char]->[Bool]
funcQ21 _ [] = []
funcQ21 a (b:c)
	|a==b && c==[] = True : []
	|a==b = True : funcQ21 a (c)
	|otherwise = False : funcQ21 a (c)


{----Questão 22 ------------------------------------------------------}
funcQ22::[Int]->Int->[(Int,Int)]
funcQ22 x y = [(z,y) | z<-x, z>y]


{----Questão 23 ------------------------------------------------------}

--Função pedida usa "checkRepetidos", que recebe o Int a ser analizado e o retorna apenas se houver outro no resto da lista. Posteriormente, utiliza "eliminaRepetidos", que exclui todos os repetidos de uma determinada lista.

checkRepetidos::Int->[Int]->[Int]
checkRepetidos _ []= []
checkRepetidos x (a:b)
	|x==a && [x]/=checkRepetidos x b = [x]
	|otherwise = checkRepetidos x b

eliminaRepetidos::[Int]->[Int]
eliminaRepetidos []=[]
eliminaRepetidos (x:a)
	|checkRepetidos x a == [x] = eliminaRepetidos a
	|otherwise = [x] ++ eliminaRepetidos a

repetidos::[Int]->[Int]
repetidos [] = []
repetidos (x:a) = eliminaRepetidos (checkRepetidos x a ++ repetidos a)


{----Questão 24 ------------------------------------------------------}
termina_em::Int->[Int]
termina_em x = [a | a <- [0, 1 .. 100] , a `mod` 10 == x]

{----Questão 25 ------------------------------------------------------}
--Questão 25.a

infixl 7 &&&
(&&&)::Int->(Int,Int)->Int
a &&& (b,c)
	|abs (b-a) < abs (c-a) = b
	|otherwise = c

--Questão 25.b
funcQ25b :: (Int->(Int,Int)->Int) -> Int -> [(Int,Int)] -> [Int]
funcQ25b _ _ [] = []
funcQ25b f x (a:b) = (f x a):(funcQ25b f x b)


--Questão 25.c
--digitando "funcQ25b (&&&) 5 [(7,2),(3,7),(5,6)]", temos como resultado "[7,7,5]", os números mais próximos de 5 em cada uma das duplas da lista.


{----Questão 26 ------------------------------------------------------}
--Nesta questão, "mescla" cria a nova lista excluindo os repetidos de trás para frente, utilizando "elimine26", "reverse26" e "check26", e então chama "mescla2". "mescla2" tem uma chamada recursiva e, a cada chamada, é adicionada uma dupla na lista final. A dupla é feita pela char atual e "charCount".


elimine26::[Char]->[Char]
elimine26 [] = []
elimine26 (x:a)
	|check26 x a ==[x] =elimine26 a
	|otherwise = [x] ++ elimine26 a

reverse26::[Char]->[Char]
reverse26 [] = []
reverse26 (x:a) = reverse26 a ++ [x]

check26::Char->[Char]->[Char]
check26 _ []= []
check26 x (a:b)
	|x==a && [x]/=check26 x b = [x]
	|otherwise = check26 x b

charCount::Char->[Char]->Int
charCount _ [] = 0
charCount y (x:a) = fromEnum (y == x) + charCount y a

mescla::[Char]->[Char]->[(Char,Int)]
mescla x a = mescla2 (reverse26 (elimine26( reverse26(x)))) a

mescla2::[Char]->[Char]->[(Char,Int)]
mescla2 [] _ = []
mescla2 (x:a) [] = [(x,0)] ++ mescla a []
mescla2 (x:a) b = [(x, charCount x b)] ++ mescla a b


{----Questão 27 ------------------------------------------------------}

infix 3 -*-

(-*-) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(-*-) (a,b) (c,d) = (maior27 (a,b) (c,d), menor27 (a,b) (c,d))

maior27 :: (Int, Int) -> (Int, Int) -> Int
maior27 (a,b) (c,d) = maior (maior a b) (maior c d)

menor27 :: (Int, Int) -> (Int, Int) -> Int
menor27 (a,b) (c,d) = menor (menor a b) (menor c d)

maior :: Int -> Int -> Int
maior x y
  | x >= y = x
  | otherwise = y

menor :: Int -> Int -> Int
menor x y
  | x <= y = x
  | otherwise = y


{----Questão 28 ------------------------------------------------------}

--Questão 28.a
ocorrencia::Int->[Int]->(Int,Int)
ocorrencia x [] = (x, 0)
ocorrencia x a = (x, conta28 x a)

conta28::Int->[Int]->Int
conta28 _ [] = 0
conta28 x (y:a) = fromEnum (x == y) + conta28 x a

--Questão 28.b
aplica::(Int->[Int]->(Int,Int))->[Int]->[(Int,Int)]
aplica f (x:a) = [f y (x:a) | y<-(x:a)]

--Questão 28.c
--Basta digitar "aplica ocorrencia " e a lista de inteiros.
--Por exemplo, "aplica ocorrencia [1,2,3,4,5,6,7,8,9,12,1,2,3,1,2]" retorna "[(1,3),(2,3),(3,2),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(12,1),(1,3),(2,3),(3,2),(1,3),(2,3)]", a lista de duplas que conta a ocorrência de cada número na lista.


{----Questão 29 ------------------------------------------------------}
funny x y z = (x > z) || (y < x) && True


{----Questão 30 ------------------------------------------------------}

infixl 7 ###

(###)::Int -> [Int] -> [Int]
(###) _ [] = []
x ### (a:b)
	|x == a = b
	|otherwise = a : (###) x b
