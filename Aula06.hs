{-3. Defina uma função chamada ultimo que seleciona o último elemento de uma lista não
vazia, usando as funções do prelúdio (exceto a função last). Defina mais duas formas
de fazer esta função.

4. Seja uma lista de alunos e suas notas.
type Aluno = (Integer, [Char], Double) -- Nro Aluno, Nome, Nota
type Curso = [Aluno]
Construa as seguintes funções para a manipulação desses dados:

a) Função recursiva maiorNota :: Curso -> [Char] que recebe um Curso e devolve
o nome do aluno com a maior nota.

b) Função insereAluno :: Aluno -> Curso -> Curso que recebe um Aluno e um
Curso e insere esse aluno no curso (cabeça da lista). Caso o aluno já tenha sido
adicionado anteriormente, retorne a lista sem alteração. -}

ultimo1 :: [Int]->Int
ultimo1 x = head(reverse x)

ultimo2 :: [Int]->Int
ultimo2 (h:[]) = h
ultimo2 (h:t) = ultimo2 t

ultimo3 :: [Int]->Int
ultimo3 x = x !! ((length x)- 1)

type Aluno = (Integer, [Char], Double) -- Nro Aluno, Nome, Nota
type Curso = [Aluno]

maiorNota :: Curso -> String
maiorNota [] = error "Curso vazio"
maiorNota [(nro, nome, nota)] = nome
maiorNota ((nro1, nome1, nota1):(nro2, nome2, nota2):alunos)
    | nota1 >= nota2 = maiorNota ((nro1, nome1, nota1):alunos)
    | otherwise      = maiorNota ((nro2, nome2, nota2):alunos)


insereAluno :: Aluno -> Curso -> Curso
insereAluno a [] = [a]
insereAluno a (h:t) 
    | nroAluno a == nroAluno h = (h:t) 
    | otherwise = h : insereAluno a t

nroAluno :: Aluno -> Integer
nroAluno (nro, _, _) = nro
