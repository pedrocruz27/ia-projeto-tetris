;;; GRUPO: 21 ||  ALUNOS: Henrique Lourenco - 77459 / Jose Touret - 78215 / Pedro Cruz - 78579 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;-------------------------------- Accao ----------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cria-accao (c peca)
 (cons c peca)
)

(defun accao-coluna (accao)
  (car accao)
)

(defun accao-peca (accao)
  (cdr accao)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;----------------------------- Tabuleiro ---------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cria-tabuleiro ()
  (make-array '(18 10))
)

(defun copia-tabuleiro (array)
  (setf tabuleiro array) tabuleiro
)

(defun tabuleiro-preenchido-p (tabuleiro l c)
  (aref tabuleiro (1- l) (1- c))
)

(defun tabuleiro-altura-coluna (tabuleiro c)
  (let ((l (1- (first (array-dimensions tabuleiro))))
        (resultado 0))
    (dotimes (i l resultado) 
      (if (tabuleiro-preenchido-p tabuleiro l c)
          (incf RESULTADO)
        )
      )
    )
)
  

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


(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)

(defun copia-estado (estado)
  
)

(defun estados-iguais-p (estado1 estado2)
  
)

(defun estado-final-p (estado)
  
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;----------------------------- Problema ---------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

(defun solucao (estado)
  
)

(defun accoes (estado)
  
)

(defun resultado (estado accao)
  
)

(defun qualidade (estado)
  
)

(defun custo-opurtunidade (estado)
  
)
