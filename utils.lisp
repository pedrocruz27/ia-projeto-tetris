;;; definicao das configuracoes possiveis para cada peca
;;peca i 
(defconstant peca-i0 (make-array (list 4 1) :initial-element T))
(defconstant peca-i1 (make-array (list 1 4) :initial-element T))
;;peca l
(defconstant peca-l0 (make-array (list 3 2) :initial-contents '((T T)(T nil)(T nil))))
(defconstant peca-l1 (make-array (list 2 3) :initial-contents '((T nil nil)(T T T))))
(defconstant peca-l2 (make-array (list 3 2) :initial-contents '((nil T)(nil T)(T T))))
(defconstant peca-l3 (make-array (list 2 3) :initial-contents '((T T T)(nil nil T))))
;;peca j
(defconstant peca-j0 (make-array (list 3 2) :initial-contents '((T T)(nil T)(nil T))))
(defconstant peca-j1 (make-array (list 2 3) :initial-contents '((T T T)(T nil nil))))
(defconstant peca-j2 (make-array (list 3 2) :initial-contents '((T nil)(T nil)(T T))))
(defconstant peca-j3 (make-array (list 2 3) :initial-contents '((nil nil T)(T T T))))
;;peca o
(defconstant peca-o0 (make-array (list 2 2) :initial-element T))
;;peca s
(defconstant peca-s0 (make-array (list 2 3) :initial-contents '((T T nil)(nil T T))))
(defconstant peca-s1 (make-array (list 3 2) :initial-contents '((nil T)(T T)(T nil))))
;;peca z
(defconstant peca-z0 (make-array (list 2 3) :initial-contents '((nil T T)(T T nil))))
(defconstant peca-z1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(nil T))))
;;peca t
(defconstant peca-t0 (make-array (list 2 3) :initial-contents '((T T T)(nil T nil))))
(defconstant peca-t1 (make-array (list 3 2) :initial-contents '((T nil)(T T)(T nil))))
(defconstant peca-t2 (make-array (list 2 3) :initial-contents '((nil T nil)(T T T))))
(defconstant peca-t3 (make-array (list 3 2) :initial-contents '((nil T)(T T)(nil T))))

;; acrescentei algumas funcoes auxiliares que vao dar jeito para testar automaticamente o codigo dos alunos
(defun ignore-value (x)
	(declare (ignore x))
	'ignore)

;;; random-element: list --> universal
;;; funcao que dada uma lista, devolve um elemento aleatorio dessa lista
;;; se a lista recebida for vazia, e devolvido nil
(defun random-element (list)
  (nth (random (length list)) list))

;;; random-pecas: inteiro --> lista
;;; funcao que recebe um inteiro que especifica o numero de pecas pretendidas, e devolve uma lista com
;;; pecas (representadas atraves de um simbolo) escolhidas aleatoriamente. O tamanho da lista devolvida corresponde
;;; ao inteiro recebido.
(defun random-pecas (n)
	(let ((lista-pecas nil))
		(dotimes (i n)
			(push (random-element (list 'i 'l 'j 'o 's 'z 't)) lista-pecas))
		lista-pecas))
		
;;; cria-tabuleiro-aleatorio: real (opcional) x real (opcional) --> tabuleiro
;;; funcao que recebe um valor real (entre 0 e 1) para a probabilidade a ser usada na primeira linha e outro real
;;; que representa o decrescimento de probabilidade de uma linha para a seguinte. Estes argumentos sao opcionais,
;;; e se nao forem especificados tem o valor por omissao de 1.0 (100%) e 0.05 (5%) respectivamente
;;; A funcao retorna um tabuleiro em que cada posicao foi preenchida de acordo com as probabilidades especificadas
;;; para cada linha. A linha inicial tera uma maior probabilidade, mas as linhas seguintes terao uma menor probabilidade
;;; de preenchimento, resultado em media mais posicoes preenchidas no fundo do tabuleiro do que no topo. 
(defun cria-tabuleiro-aleatorio (&optional (prob-inicial 1.0) (decaimento 0.05))
	(let ((tabuleiro (cria-tabuleiro))
		  (prob prob-inicial)
		  (coluna-a-evitar 0))
		(dotimes (linha 18)
			;;;precisamos de escolher sempre uma coluna para nao preencher, se nao podemos correr o risco de criarmos uma linha
			;;;completamente preenchida
			(setf coluna-a-evitar (random 10)) 
			(dotimes (coluna 10)
				(when (and (not (= coluna-a-evitar coluna)) (<= (random 1.0) prob)) (tabuleiro-preenche! tabuleiro linha coluna)))
			;;;nao podemos permitir valores negativos de probabilidade
			(setf prob (max 0 (- prob decaimento))))
		tabuleiro))
		
;;; executa-jogadas: estado x lista --> inteiro
;;; funcao que recebe um estado e uma lista de accoes e executa as accoes (pela ordem recebida) sobre o tabuleiro do estado inicial,
;;; desenhando no ecra os varios estados do tabuleiro. Para avancar entre ecras, o utilizador deve premir a tecla "Enter".
;;;	retorna o total de pontos obtidos pela sequencia de accoes no tabuleiro
(defun executa-jogadas (estado-inicial lista-accoes)
	(let ((estado estado-inicial))
		(do () ((or (estado-final-p estado) (null lista-accoes)))
			(desenha-estado estado)
			(read-char)
			(desenha-estado estado (first lista-accoes))
			(read-char)
			(setf estado (resultado estado (first lista-accoes)))
			(setf lista-accoes (rest lista-accoes)))
		(desenha-estado estado)
		(estado-pontos estado)))

;;; desenha-estado: estado x accao (opcional) --> {}
;;; funcao que recebe um estado (e pode receber opcionalmente uma accao) e desenha o estado do jogo de tetris no ecra
;;; se for recebida uma accao, entao essa accao contem a proxima jogada a ser feita, e deve ser desenhada na posicao correcta por cima 
;;; do tabuleiro de tetris. Esta funcao nao devolve nada.		
(defun desenha-estado (estado &optional (accao nil))
	(let ((tabuleiro (estado-tabuleiro estado)))
		(desenha-linha-exterior) (format T "  Proxima peca:~A~%" (first (estado-pecas-por-colocar estado))) 
		(do ((linha 3 (- linha 1))) ((< linha 0))
			(desenha-linha-accao accao linha) (format T "~%"))
		(desenha-linha-exterior) (format T "  Pontuacao:~A~%" (estado-pontos estado))
		(do ((linha 16 (- linha 1))) ((< linha 0))
			(desenha-linha tabuleiro linha) (format T "~%"))
		(desenha-linha-exterior)))

;;; desenha-linha-accao: accao x inteiro --> {}
;;; dada uma accao e um inteiro correspondente a uma linha que esta por cima do tabuleiro (linhas 18,19,20,21) desenha
;;; a linha tendo em conta que podera estar la a peca correspondente a proxima accao. Nao devolve nada.
(defun desenha-linha-accao (accao linha)
	(format T "| ")
	(dotimes (coluna 10)
		(format T "~A " (cond ((null accao) " ")
							  ((and (array-in-bounds-p (accao-peca accao) linha (- coluna (accao-coluna accao)))
									(aref (accao-peca accao) linha (- coluna (accao-coluna accao)))) "#")
							  (T " "))))
	(format T "|"))
	
;;; desenha-linha-exterior: {} --> {}
;;; funcao sem argumentos que desenha uma linha exterior do tabuleiro, i.e. a linha mais acima ou a linha mais abaixo
;;; estas linhas sao desenhadas de maneira diferente, pois utilizam um marcador diferente para melhor perceber 
;;; os limites verticais do tabuleiro de jogo
(defun desenha-linha-exterior ()
	(format T "+-")
	(dotimes (coluna 10)
		(format T "--"))
	(format T "+"))
	
;;; desenha-linha-vazia: {} --> {}
;;; funcao sem argumentos que desenha uma linha vazia. Nao devolve nada.
(defun desenha-linha-vazia ()
	(format T "| ")
	(dotimes (coluna 10)
		(format T "~A "))
	(format T "|"))
	
;;; desenha-linha: tabuleiro,inteiro --> {}
;;; esta funcao recebe um tabuleiro, e um inteiro especificando a linha a desenhar
;;; e desenha a linha no ecra, colocando o simbolo "#" por cada posicao preenchida, 
;;; e um espaco em branco por cada posicao nao preenchida. Nao devolve nada.
(defun desenha-linha (tabuleiro linha)
	(format T "| ")
	(dotimes (coluna 10)
		(format T "~A " (if (tabuleiro-preenchido-p tabuleiro linha coluna) "#" " ")))
	(format T "|"))			

			
;;exemplo muito simples de um tabuleiro com a primeira e segunda linha quase todas preenchidas
(defvar t1 (cria-tabuleiro))
(dotimes (coluna 9)
	(tabuleiro-preenche! t1 0 coluna))
(dotimes (coluna 9)
	(tabuleiro-preenche! t1 1 coluna))
(defvar e1 (make-estado :tabuleiro t1 :pecas-por-colocar '(i o j l t i)))

(defvar p1
	(make-problema :estado-inicial (make-estado :tabuleiro t1 :pecas-por-colocar '(i o j l t i))
				   :solucao #'solucao
				   :accoes #'accoes
				   :resultado #'resultado
				   :custo-caminho #'custo-oportunidade))