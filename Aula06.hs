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
