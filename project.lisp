;;;;////////////////////////////////////////// funcoes da lista////////////////////////////////////////

;;;ACCOES

(defun cria-accao (c peca)
  list(c peca)
)

(defun accao-coluna (accao)
  car(accao)
)

(defun accao-peca (accao)
  cdr(accao)
)

;;;TABULEIRO

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

;;;;////////////////////////////////////////////////////////////// estado ///////////////////////////////////

(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)