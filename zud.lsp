(defun c:zud(/ NR FILE_NAME MARK FILE FONT_SIZE LINE old_cmdecho)
	(setq old_cmdecho (getvar "CMDECHO"))
	(setvar "CMDECHO" 0)	
	(initget (+ 8 8 4 4))
		(setq NR (getint "Podaj numer pierwszego punktu <1>: "))
		(setq FONT_SIZE (getreal "Podaj rozmiar tekstu <10>:"))
		(setq LINE (getstring "Podaj oznaczenie lacza <t>:"))
		(setq MARK (getstring "Czy pytac o linie pomocnicze do punktu <N> [T/N]:"))
	(setq FILE_NAME (getfiled "Wpisz nazwe pliku" "" "txt;csv;xls" 1))
	(if (= NR nil) (setq NR 1))
	(if (= FONT_SIZE nil) (setq FONT_SIZE 10))
	(if (= LINE "") (setq LINE "t"))	
	(setq FILE (open FILE_NAME "W"))
	(setq I 0)
	(write-line (strcat "Nr  Y 		X" ) FILE)
	(while
		(setq MARK_POINT (getpoint (strcat "Wskaz punkt " (rtos NR)) ))
		(setq COORD_Y (RTOS (cadr MARK_POINT) 2 2))
		(setq COORD_X (RTOS (car MARK_POINT) 2 2))
		(setq COORDS
			(if (< NR 10) 
			(strcat (rtos NR)".  " COORD_Y ",  " COORD_X " 	" LINE )
			(strcat (rtos NR) ". " COORD_Y ",  " COORD_X " 	" LINE )
			);if 
		)
		(write-line COORDS FILE)
		(if (or (= MARK "t")(= MARK "T") )
			(progn
			(setq GUIDE "n")
			(setq GUIDE (getstring "Czy do tego punktu dorysowac linie <N> [T/N]:"))
			(if (or (= GUIDE "t")(= GUIDE "T") )
				(DRAW_GUIDE) 
			)
			(DRAW_TEXT)
			)
			(DRAW_TEXT)	
		);if
		(setq NR (1+ NR))
		(setq I (1+ I))
	);while
	(close FILE)
	(setvar "CMDECHO" old_cmdecho)
(EXIT)

);defun

(defun DRAW_TEXT (/ mark_text)
	(setq mark_text (strcat (rtos NR) LINE))
	(command "_layer" "_m" "zud-nr" "_c" "7" "" "")
	(command "_text" MARK_POINT FONT_SIZE "0" mark_text)
);defun

(defun EXIT (/)
(princ (strcat "\nKoniec, wskazano punktow: " (rtos I)) )
(princ)
);defun

;(defun DRAW_POINT (/)d ang_90 ang_270 p1 p2 p3 p4)
;	(command "_layer" "_m" "zud-pk" "_c" "7" "" "")
;	(setq d (/ FONT_SIZE 3))
;	(setq ang_90 (/ pi 2))
;	(setq ang_270 (* 3 ang_90))
;	(setq p1 (polar MARK_POINT ang_90 d))
;	(setq p2 (polar MARK_POINT ang_270 d))
;	(setq p3 (polar MARK_POINT 0 d))
;	(setq p4 (polar MARK_POINT pi d))
;	(command "_osmode" "0" "")
;	(command "_line" p1 p2 "")
;	(command "_line" p3 p4 "")
;);defun
;;; ---------------------------------------------------------------------------------- ;;;
(defun DRAW_GUIDE (/ d ang_90 ang_270 p1 p2 p3 p4)
;	(command "_layer" "_m" "zud-pk" "_c" "7" "" "")
;	(command "_osmode" "0" "")
(princ (strcat "rysuje linie") )
;	(command "_line" p1 p2 "")
);defun
(princ (strcat "\nPolecenie: ZUD") ) 
;;; ---------------------------------------------------------------------------------- ;;;