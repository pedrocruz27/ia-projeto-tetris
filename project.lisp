;;; GRUPO: 21 || ALUNOS: Henrique Lourenco - 77459 / Jose Touret - 78215 / Pedro Cruz - 78579 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;-------------------------------- Accao ----------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;CRIA-ACCAO
;;;construtor recebe um inteiro <c> correspondente a posicao da coluna mais a 
;;;esquerda a partir da qual a peca vai ser colocada, e um array <peca> com a 
;;;configuracao da peca a colocar, devolvendo uma nova accao
(defun cria-accao (c peca)
  (cons c peca)
  )

;;;ACCAO-COLUNA
;;;selector devolve um inteiro correspondente a coluna mais a esquerda a partir
;;;da qual a peca vai ser colocada
(defun accao-coluna (accao)
  (car accao)
  )

;;;ACCAO-PECA
;;;seletor devolve o array com a configuracao geometrica exacta com que vai 
;;;ser colocada
(defun accao-peca (accao)
  (cdr accao)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;----------------------------- Tabuleiro ---------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;CRIA-TABULEIRO
;;;construtor que nao recebe qualquer argumento e devolve um novo tabuleiro vazio 
(defun cria-tabuleiro ()
  (make-array '(18 10))
  )

;;;COPIA-TABULEIRO
;;;construtor que recebe um <tabuleiro> e devolve um novo tabuleiro com o mesmo
;;;conteudo do tabuleiro recebido
(defun copia-tabuleiro (tabuleiro)
  (let ((new-tabuleiro (cria-tabuleiro))
        (c (cadr (array-dimensions tabuleiro)))
        (l (car (array-dimensions tabuleiro)))
        )
    (dotimes (ic c) 
      (dotimes (il l)
        (setf (aref new-tabuleiro il ic) (aref tabuleiro il ic))
        )
      )
    new-tabuleiro
    )
  )

;;;TABULEIRO-PREENCHIDO-P
;;;seletor que recebe um <tabuleiro>, um inteiro <l> que equivale ao numero da linha
;;;e um inteiro <c> que equivale ao numero da coluna e devolve o valor logico verdade
;;;se essa posicao estiver preenchida, falso caso contrario 
(defun tabuleiro-preenchido-p (tabuleiro l c)
  (aref tabuleiro l c)
  )

;;;TABULEIRO-ALTURA-COLUNA
;;;seletor recebe um <tabuleiro>, um inteiro <c> correspondete ao numero de uma coluna
;;;e devolve a altura da coluna, ou seja, a posicao mais alta preenchida dessa coluna
(defun tabuleiro-altura-coluna (tabuleiro c)
  (let ((l (car (array-dimensions tabuleiro))))
    (dotimes (i l) 
      (if (tabuleiro-preenchido-p tabuleiro  (1- (- l  i)) c)
          (return-from tabuleiro-altura-coluna  (- l  i))
        )
      )
    0
    )
  )

;;;TABULEIRO-LINHA-COMPLETA-P
;;;reconhecedor recebe um <tabuleiro>, um inteiro que equivale ao numero de uma
;;;linha <l> e devolve o valor logico verdade se todas as posicoes da linha 
;;;recebida estiverem preenchidas, e falso caso contrario 
(defun tabuleiro-linha-completa-p (tabuleiro l)
  (let ((c (cadr (array-dimensions tabuleiro))))
    (dotimes (i c) 
      (cond ((not (tabuleiro-preenchido-p tabuleiro l  i)) (return-from tabuleiro-linha-completa-p nil)))
      )
    )
  t
  )

;;;TABULEIRO-PREENCHE!
;;;modificador recebe um <tabuleiro>, um inteiro <l> que equivale ao numero
;;;da linha e um inteiro <c> que equivale ao numero da coluna e altera o
;;;tabuleiro recebido para a posicao correspondente a linha e coluna passar a 
;;;estar preenchido
(defun tabuleiro-preenche! (tabuleiro l c)
  (if (and (< l (car (array-dimensions tabuleiro))) (>= l 0))
      (if (and (< c (cadr (array-dimensions tabuleiro))) (>= c 0))
          (setf (aref tabuleiro l c) T)
        )
    )
  )

;;;TABULEIRO-REMOVE-LINHA!
;;;modificador que recebe um <tabuleiro>, um inteiro <l> correspondente ao
;;;numero da linha, e altera o tabuleiro recebido removendo essa linha do
;;;tabuleiro, e fazendo com que as linhas por cima da linha removida descam
;;;uma linha
(defun tabuleiro-remove-linha! (tabuleiro l)
  (if (and (<= l (1- (car (array-dimensions tabuleiro)))) (>= l 0))
      (let ((c (cadr (array-dimensions tabuleiro))))
        (dotimes (i c)
          (dotimes (j (1- (- (car (array-dimensions tabuleiro)) l)))
            (setf (aref tabuleiro (+ j l) i) (aref tabuleiro (1+ (+ j l)) i))
            )
          (setf (aref tabuleiro (1- (car (array-dimensions tabuleiro))) i) NIL)
          )
        )
    )
  )

;;;TABULEIRO-TOPO-PREENCHIDO-P
;;;reconhecedor recebe um <tabuleiro>, e devolve o valor logico verdade se
;;;existir alguma posicao na linha do topo do tabuleiro que esteja preenchida,
;;;e falso caso contrario
(defun tabuleiro-topo-preenchido-p (tabuleiro)
  (let ((c (cadr (array-dimensions tabuleiro))))
    (dotimes (i c) 
      (cond ((tabuleiro-preenchido-p tabuleiro 17  i) (return-from tabuleiro-topo-preenchido-p t)))
      )
    )
  nil
  )

;;;TABULEIROS-IGUAIS-P
;;;teste recebe dois tabuleiros <t1> e <t2>, e devolve o valor logico verdade se
;;;os dois tabuleiros forem iguais, e falso caso contrario
(defun tabuleiros-iguais-p (t1 t2)
  (let ((l (car (array-dimensions t1)))
        (c (cadr (array-dimensions t1)))
        )
    (if (or (not (equalp l (car (array-dimensions t2)))) (not (equalp c (cadr (array-dimensions t1)))))
        (return-from tabuleiros-iguais-p nil) 
      )
    (dotimes (i l)
      (dotimes (j c)
        (if (not (equalp (aref t1 i j) (aref t2 i j)))
            (return-from tabuleiros-iguais-p nil)
          )
        )
      )
    t
    )
  )

;;;TABULEIRO->ARRAY
;;;transformador de saida recebe um <tabuleiro> e devolve um novo array, que para
;;;cada linha e coluna devera conter o valor logico correspondente a cada posicao
;;;do tabuleiro
(defun tabuleiro->array (tabuleiro)
  tabuleiro
  )

;;;ARRAY->TABULEIRO
;;;transformador de entrada recebe um <array> cujas posicoes logicas tem o valor
;;;logico T ou Nil, e constroi um novo tabuleiro com o conteudo do array recebido
(defun array->tabuleiro (array)
  array
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;-------------------------------- Estado ---------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;ESTADO - representa o estado de um jogo de tetris
;;;<pontos> - numero de pontos conseguidos ate ao momento
;;;<pecas-por-colocar> - lista contendo as pecas por colocar, por ordem de colocacao
;;;<pecas-colocadas> - lista com as pecas ja colocadas no tabuleiro
;;;<tabuleiro> - tabuleiro com as posicoes atualmente preenchidas do jogo
(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)

;;;COPIA-ESTADO
;;;construtor que recebe um <estado> e devolve um novo cujo conteudo deve ser
;;;copidado a partir do estado original
(defun copia-estado (estado)
  (make-estado :pontos (estado-pontos estado)
               :pecas-por-colocar (copy-list (estado-pecas-por-colocar estado))
               :pecas-colocadas (copy-list (estado-pecas-colocadas estado))
               :tabuleiro (copia-tabuleiro (estado-tabuleiro estado))
               )
  )

;;;ESTADOS-IGUAIS-P
;;;teste que recebe dois estados <estado1> e <estado2> , devolvendo o valor logico
;;;verdade se os dois estados forem iguais e falso caso contrario
(defun estados-iguais-p (estado1 estado2)
  (equalp estado1 estado2)
  )

;;;ESTADO-FINAL-P
;;;reconhecedor recebe um <estado> e devolve o valor logico verdade se corresponder a 
;;;um estado final onde o jogador ja nao possa fazer mais jogadas e falso caso
;;;contrario
(defun estado-final-p (estado)
  (if (or (not (car (estado-pecas-por-colocar estado))) (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)))
      T
    NIL
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;------------------------------ Problema ---------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;PROBLEMA - representa um problema generico de procura
;;;<estado-inicial> - contem o estado inicial do problema de procura
;;;<solucao> - funcao que verifica se um estado e solucao de um problema de procura
;;;<accoes> - funcao que devolve uma lista com todas as accoes possiveis para um
;;;         estado recebido
;;;<resultado> - funcao que devolve o estado sucessor que resulta de executar a accao
;;;         recebida no estado recebido
;;;<custo-caminho> - funcao que devolve o custo do caminho desde o estado incicial
;;;        ate um estado recebido      
(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

;;;SOLUCAO
;;;funcao recebe um <estado> e devolve o valor logico verdade se o estado recebido
;;;corresponder a uma solucao, e falso caso contrario
(defun solucao (estado)
  (if (and (not (car (estado-pecas-por-colocar estado))) (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))))
      t
    )
  )

;;;ACCOES
;;;funcao recebe um <estado> e devolve uma lista de accoes correspondendo a todas as
;;;accoes validas que podem ser feitas com a proxima peca a ser colocada
(defun accoes (estado)
  (if (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))
      nil
    (let ((c (cadr (array-dimensions (estado-tabuleiro estado))))
          (validos nil)
          (resposta ()))
      (cond ((equalp 'i (car (estado-pecas-por-colocar estado))) (setf validos (list peca-i0 peca-i1)))
            ((equalp 'j (car (estado-pecas-por-colocar estado))) (setf validos (list peca-j0 peca-j1 peca-j2 peca-j3)))
            ((equalp 'l (car (estado-pecas-por-colocar estado))) (setf validos (list peca-l0 peca-l1 peca-l2 peca-l3)))
            ((equalp 'o (car (estado-pecas-por-colocar estado))) (setf validos (list peca-o0)))
            ((equalp 's (car (estado-pecas-por-colocar estado))) (setf validos (list peca-s0 peca-s1)))
            ((equalp 'z (car (estado-pecas-por-colocar estado))) (setf validos (list peca-z0 peca-z1)))
            ((equalp 't (car (estado-pecas-por-colocar estado))) (setf validos (list peca-t0 peca-t1 peca-t2 peca-t3)))
            )
      (dolist(peca validos) 
        (dotimes (i (- c (1- (cadr (array-dimensions peca)))))
          (setf resposta (append resposta (list (cria-accao i peca))))
          )
        )
      resposta
      )
    )
  )

;;;RESULTADO
;;;funcao recebe um <estado> e uma <accao>, e devolve um novo estado que resulta de
;;;aplicar a accao recebida ao estado original
(defun resultado (estado accao)
  (let ((estado2 (copia-estado estado))
        (altura 0)
        (altura-peca 0)
        (l (car (array-dimensions (cdr accao))))
        (c (cadr (array-dimensions (cdr accao))))
        (linhas-removidas 0))
    (setf (estado-pecas-colocadas estado2) (append (list (car (estado-pecas-por-colocar estado2))) (estado-pecas-colocadas estado2)))
    (setf (estado-pecas-por-colocar estado2) (cdr (estado-pecas-por-colocar estado2)))

    (dotimes (i c)
      (setf altura-peca 0)
      (dotimes (j l)
        (if (aref (accao-peca accao)  j i)
            (return)
          (decf altura-peca)
          )
        )
        (if (< altura (+ (tabuleiro-altura-coluna (estado-tabuleiro estado2) (+ (car accao) i)) altura-peca))
          (setf altura (+ (tabuleiro-altura-coluna (estado-tabuleiro estado2) (+ (car accao) i)) altura-peca))
        )
      )

    (dotimes (i l)
      (dotimes (j c)
        (if (aref (cdr accao)  i j)
            (tabuleiro-preenche! (estado-tabuleiro estado2) (+ i altura) (+ j (car accao)))
          )
        )
      )
    (if (tabuleiro-topo-preenchido-p (estado-tabuleiro estado2))
        (return-from resultado estado2)
      (dotimes (i (1- (car (array-dimensions (estado-tabuleiro estado2)))))
        (if (tabuleiro-linha-completa-p (estado-tabuleiro estado2) i)
            (progn
              (tabuleiro-remove-linha! (estado-tabuleiro estado2) i)
              (setf linhas-removidas (1+ linhas-removidas))
              (setf i (1- i))
              )
          )
        )
      )
    (cond ((equalp linhas-removidas 1) (setf (estado-pontos estado2) (+ (estado-pontos estado2) 100)))
          ((equalp linhas-removidas 2) (setf (estado-pontos estado2) (+ (estado-pontos estado2) 300)))
          ((equalp linhas-removidas 3) (setf (estado-pontos estado2) (+ (estado-pontos estado2) 500)))
          ((equalp linhas-removidas 4) (setf (estado-pontos estado2) (+ (estado-pontos estado2) 800)))
          )
    estado2
    )
  )

;;;QUALIDADE
;;;funcao que recebe um <estado> e retorna um valor de qualidade que corresponde ao
;;;valor negativo dos pontos ganhos ate ao momento
(defun qualidade (estado)
  (* (estado-pontos estado) -1)
  )

;;;CUSTO-OPORTUNIDADE
;;;funcao que recebe um <estado> e devolve o custo de oportunidade de todas as accoes 
;;;realizadas ate ao momento
(defun custo-oportunidade (estado)
  (let ((resposta 0))
    (dolist(peca (estado-pecas-colocadas estado)) 
      (cond ((equalp 'i (car (estado-pecas-colocadas estado))) (setf resposta (+ resposta 800)))
            ((equalp 'j (car (estado-pecas-colocadas estado))) (setf resposta (+ resposta 500)))
            ((equalp 'l (car (estado-pecas-colocadas estado))) (setf resposta (+ resposta 500)))
            ((equalp 'o (car (estado-pecas-colocadas estado))) (setf resposta (+ resposta 300)))
            ((equalp 's (car (estado-pecas-colocadas estado))) (setf resposta (+ resposta 300)))
            ((equalp 'z (car (estado-pecas-colocadas estado))) (setf resposta (+ resposta 300)))
            ((equalp 't (car (estado-pecas-colocadas estado))) (setf resposta (+ resposta 300)))
            )
      )
    (- resposta (estado-pontos estado))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;------------------------------ Procuras ---------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;PROCURA-PP
;;;funcao que recebe um <problema> e devolve uma lista de accoes que corresponde  
;;;a um caminho de um estado inicial para um estado final. Para tal, esta funcao
;;;faz recurso ao algoritmo de procura em profundidade primeiro em arvore para
;;;obter uma solucao para resolver o problema.
(defun procura-pp (problema)
  (let ((solution nil)
        (nextnode nil))
    (if (funcall (problema-solucao problema) (problema-estado-inicial problema))
        (return-from procura-pp t)
      (dolist (accao (nreverse (funcall (problema-accoes problema) (problema-estado-inicial problema))))
        (setf nextnode (make-problema :estado-inicial (funcall (problema-resultado problema) (problema-estado-inicial problema) accao)
                                      :solucao (problema-solucao problema)
                                      :accoes (problema-accoes problema)
                                      :resultado (problema-resultado problema)
                                      :custo-caminho (problema-custo-caminho problema)))
        (setf solution (procura-pp nextnode))                                      
        (if (not(null solution))
            (if (equal t solution) 
                (return-from procura-pp (list accao))   
              (return-from procura-pp (append (list accao) solution)))
          )
        )
      )
    )
  )

;;;PROCURA-A*
;;;funcao que recebe um <problema> e uma funcao <heuristica> e devolve uma lista
;;;de accoes que corresponde a um caminho otimizado de um estado inicial para um 
;;;estado final. Utiliza o algoritmo A* em arvore para obter uma solucao para 
;;;resolver o problema. 
(defun procura-A* (problema heuristica)
  (let ((fronteira (list (list (problema-estado-inicial problema) (list nil) (+ (funcall (problema-custo-caminho problema) (problema-estado-inicial problema)) (funcall heuristica (problema-estado-inicial problema)))))))
    (let ((noduloActual (car fronteira))
          (proximoEstado nil))
      (loop while (not (funcall (problema-solucao problema) (car noduloActual))) do
            (dolist (accao (nreverse (funcall (problema-accoes problema) (car noduloActual))))
              (setf proximoEstado (funcall (problema-resultado problema) (car noduloActual) accao))
              (setf fronteira (append fronteira (list (list proximoEstado (append (cadr noduloActual) (list accao)) (+ (funcall (problema-custo-caminho problema) proximoEstado) (funcall heuristica proximoEstado))))))
              )
            (setf fronteira (cdr fronteira))
            (sort fronteira #'sort-nodulo)
            (setf noduloActual (car fronteira))
            (if (null fronteira)
                (return-from procura-A* nil)
              )
            )
      (return-from procura-A* (cdadr noduloActual))
      )
    )
  )

;;;SORT-NODULO
;;; Funcao auxiliar do sort.
(defun sort-nodulo (nodulo1 nodulo2)
  (if (< (caddr nodulo1)  (caddr nodulo2))
      t
    nil
    )
  )

;;;PROCURA-BEST
;;;funcao que recebe um <array> correspondente a um tabuleiro e uma <lista de pecas>
;;;por colocar e que vai usar a melhor procura e a melhor heuristica e a melhor
;;;funcao custo/qualidade de modo a conseguir colocar todas as pecas no tabuleiro
;;;com o maximo de pontuacao
(defun procura-best (array pecas)
  (let ((problema (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro array :pecas-colocadas () :pecas-por-colocar pecas) 
                                :solucao #'solucao 
                                :accoes #'accoes 
                                :resultado #'resultado 
                                :custo-caminho #'custo-oportunidade)))
    (procura-A* problema #'custo-oportunidade)
    )
  )

(load "utils.fas")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;------------------------------ FIM --------------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
