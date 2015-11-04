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

;;;;////////////////////////////////////////////////////////////// estado ///////////////////////////////////

(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)