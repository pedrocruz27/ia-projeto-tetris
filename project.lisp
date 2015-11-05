;;; GRUPO: 21 || ALUNOS: Henrique Lourenco - 77459 / Jose Touret - 78215 / Pedro Cruz - 78579 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;-------------------------------- Accao ----------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;CRIA-ACCAO
(defun cria-accao (c peca)
 (cons c peca)
)

;;;ACCAO-COLUNA
(defun accao-coluna (accao)
  (car accao)
)

;;;ACCAO-PECA
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
(defun copia-tabuleiro (array)
  (let ((tabuleiro array)) tabuleiro)
)

;;;TABULEIRO-PREENCHIDO-P
;;;seletor que recebe um <tabuleiro>, um inteiro <l> que equivale ao numero da linha
;;;e um inteiro <c> que equivale ao numero da coluna e devolve o valor logico verdade
;;;se essa posicao estiver preenchida, falso caso contrario 
(defun tabuleiro-preenchido-p (tabuleiro l c)
  (aref tabuleiro (1- l) (1- c))
)

;;;TABULEIRO-ALTURA-COLUNA
;;;seletor recebe um <tabuleiro>, um inteiro <c> ao numero de uma coluna e devolve
;;;a altura da coluna de uma coluna.
(defun tabuleiro-altura-coluna (tabuleiro c)
  (let ((l (1- (first (array-dimensions tabuleiro))))
        (resultado 0))
    (dotimes (i l resultado) 
      (if (tabuleiro-preenchido-p tabuleiro (1+ i) c)
          (incf RESULTADO)
        )
      )
    )
)
  

;;;TABULEIRO-LINHA-COMPLETA-P
;;;reconhecedor recebe um <tabuleiro>, um inteiro que equivale ao numero de uma
;;;linha <l> e devolve o valor logico verdade se todas as posicoes da linha 
;;;recebida estiverem preenchidas, e falso caso contrario 
(defun tabuleiro-linha-completa-p (tabuleiro l)
  (let ((c (1- (cadr (array-dimensions tabuleiro)))))
    (dotimes (i c) 
      (cond ((not(tabuleiro-preenchido-p tabuleiro l c)) (return-from tabuleiro-linha-completa-p nil)))
      )
    )
  t
)

(defun tabuleiro-preenche! (tabuleiro l c)
  (setf (aref tabuleiro (1- l) (1- c)) T)
)

(defun tabuleiro-remove-linha! (tabuleiro l)
  (let ((c (1- (cadr (array-dimensions tabuleiro)))))
    (dotimes (i c) 
      (setf (aref tabuleiro (1- l) (i)) NIL)
      )
    )
)

(defun tabuleiro-todo-preenchido-p (tabuleiro)
  
)

(defun tabuleiros-iguais-p (t1 t2)
  
)

(defun tabuleiro->array (tabuleiro)
  
)

(defun array->tabuleiro (array)
  
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

)

;;;ESTADOS-IGUAIS-P
;teste que recebe dois estados <estado1> e <estado2> , devolvendo o valor logico
;verdade se os dois estados forem iguais e falso caso contrario
(defun estados-iguais-p (estado1 estado2)

)

;;;ESTADO-FINAL-P
;;;reconhecedor recebe um <estado> e devolve o valor logico verdade se corresponder a 
;;;um estado final onde o jogador ja nao possa fazer mais jogadas e falso caso
;;;contrario
(defun estado-final-p (estado)

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;----------------------------- Problema ---------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;PROBLEMA - representa um problema generico de procura
;;;<estado-inicial> - contem o estado inicial do problema de procura
;;;<solucao> - funcao que verifica se um estado e solucao de um problema de procura
;;;<accoes> - funcao que devolve uma lista com todas as accoes possiveis para um
;;;         estado recebido
;;;<resultado> - funcao que devolve o estado sucessor que resulta de executar a accao
;;;         recebida no estado recebido
;;;<custo> - caminho - funcao que devolve o custo do caminho desde o estado incicial
;;;        ate um estado recebido      
(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

;;;SOLUCAO
(defun solucao (estado)

)

;;;ACCOES
(defun accoes (estado)

)

;;;RESULTADO
(defun resultado (estado accao)

)

;;;QUALIDADE
(defun qualidade (estado)

)

;;;CUSTO-OPURTUNIDADE
(defun custo-opurtunidade (estado)

)
