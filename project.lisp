

;;;;////////////////////////////////////////// funcoes da lista////////////////////////////////////////
(defun cria-tabuleiro ()
  (make-array '(18 10))
  )

(defun copia-tabuleiro (array)
  (setf tabuleiro array) tabuleiro
  )

(defun tabuleiro-preenchido-p (tabuleiro l c)
  (aref tabuleiro l c)
  )

;////////////////////////////////////////////////////////////// estado ///////////////////////////////////

(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)