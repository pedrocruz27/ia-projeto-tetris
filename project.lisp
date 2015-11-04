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
  (aref tabuleiro l-1 c-1)
  )

(defun tabuleiro-altura-coluna (tabuleiro c)
  (setf resultado 0)
  (setf l (1- (first (array-dimensions tabuleiro))))
  (dotimes (i l resultado) 
    (if (tabuleiro-preenchido-p tabuleiro l c)
        (+1 l)
      )
    )
  )

(defun tabuleiro-linha-completa-p (tabuleiro l)
  (if (< l-1 0) 
      (cond ((tabuleiro-preenchido-p (tabuleiro l c)))
            
            
            
      )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;-------------------------------- Estado ---------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)

(defun copia-estado (estado))

(defun estados-iguais-p (estado1 estado2))

(defun estado-final-p (estado))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;----------------------------- Problema ---------------------------------------
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

(defun solucao (estado))

(defun accoes (estado))

(defun resultado (estado accao))

(defun qualidade (estado))

(defun custo-opurtunidade (estado))