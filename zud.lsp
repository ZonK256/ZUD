;;;TEST;;;

(defun c:zud(/ NR FILE_NAME FILE FONT_SIZE LINE old_cmdecho)
	(setq old_cmdecho (getvar "CMDECHO"))
	(setvar "CMDECHO" 0)	
	(setq 	NR 1
		 	FONT_SIZE 1.5 
			LINE "t" 
			GUIDE_CREATE "T"
	)

	(princ (strcat "Domyslne wartosci: pierwszy punkt <" (rtos NR)  ">,
					rozmiar tekstu <" (rtos FONT_SIZE) ">, 
					oznaczenie lacza <" LINE ">, 
					tryb Tworzenia Znanczikow (T/N) <" GUIDE_CREATE "> "
			)
	)

	(setq DEFAULT (getstring "Zachowac domyslne wartosci? <T> [T/N]"))
		(if (or (= DEFAULT "N") (= DEFAULT "n")) 
			(progn
				(initget (+ 8 8 4))
				(setq NR (getint "Podaj numer pierwszego punktu: "))
				(setq LINE (getstring "Podaj oznaczenie lacza:"))
				(setq GUIDE_CREATE (getstring "Tworzyc znaczniki? [T/N]"))
				(if (or (= GUIDE_CREATE "T") (= GUIDE_CREATE "t"))
					(setq FONT_SIZE (getreal "Podaj rozmiar tekstu:"))
				)			
			)
		)
	(setq FILE_NAME (getfiled "Wpisz nazwe pliku" "" "txt;csv;xls" 33))
	(setq I 0)
	(READ_FILE)
	(TEST_FILE_BLANK)

		(if (= FILE_BLANK 0)
			(MODIFY_WRITE_FILE)
			(BLANK_WRITE_FILE)
		)
	(setvar "CMDECHO" old_cmdecho)
(EXIT)
)


(defun DRAW_TEXT (/ mark_text d1 d2)
	(setq mark_text (strcat (rtos NR 2 0) LINE))
	(command "_layer" "_m" "zud-nr" "_c" "7" "" "")
	(command "_text" MARK_POINT FONT_SIZE "0" mark_text)
	(ADD_TO_GROUP)
	(setq d1 
		(list 	
			(+ 
				(car MARK_POINT)
				(/ FONT_SIZE 2)
			) 
			(+ 
				(cadr MARK_POINT)
				(/ FONT_SIZE 2)
			) 
		)
	)
	(setq d2 
		(list 
			(+ 
				(car d1)
				(/ FONT_SIZE 3)
			) 
			(+ 
				(cadr d1)
				(/ FONT_SIZE 3)
			) 
		)
	)
	(command "_textmask" d1 d1"")
)	

(defun DRAW_GUIDE (/ d p1 p2 p3)
	(command "_layer" "_m" "zud-linie" "_c" "7" "" "")
	(setq d  (* 2 FONT_SIZE))
	(setq p1 MARK_POINT)
	(setq p2 (getpoint p1 "Wskaz koniec linii pomocniczej:"))
	(if (> (car p1) (car p2) )
		(progn
				(setq p3 (polar p2 pi d))
				(setq MARK_POINT 
							(list 
								(+ 
									(car p3)
									(/ d 5)
								) 
								(cadr p3)  
							)
				)
		)
		(progn
				(setq p3 (polar p2 0 d))
				(setq MARK_POINT 
							(list 
								(- 
									(car p3)
									(/ d 1.1)
								) 
								(cadr p3)  
							)
				)
		)
	)
	(setq grp (ssadd)) 
	(command "_line" p1 p2 "")
	(ADD_TO_GROUP)
	(command "_line" p2 p3 "")
	(ADD_TO_GROUP)
	(DRAW_TEXT)
	(ADD_TO_GROUP)
	(command "_-group" "" "*" "" grp "")
)

(defun READ_FILE ( / a)
;	(princ "\n>>pobieram plik")
	  (setq labelList(list))
		(setq FILE (open FILE_NAME "R"))
	          (while (setq a (read-line FILE))
	            (setq labelList 
					(append labelList (list a)) 
				)
	          )
	          (close FILE)
	   			(setq formList (list))
	   			(foreach a labelList 
			   		(setq formList 
					   (append formList(list a))
					)
	   			)
)

(defun DO_MARKS ()
;	(princ "\n>>pobieram punkt")
	(initget 128)
	(setq MARK_POINT 
		(getpoint 
			(strcat "Wskaz punkt " (rtos NR 2 0)) 
		)
	)

	(if (/= MARK_POINT nil)
		(progn
			(setq COORD_Y (rtos (cadr MARK_POINT) 2 2))
			(setq COORD_X (rtos (car MARK_POINT) 2 2))
			(setq COORDS
				(if (< NR 10) 
					(strcat (rtos NR 2 0)".  " COORD_Y ",  " COORD_X " 	" LINE )
					(strcat (rtos NR 2 0) ". " COORD_Y ",  " COORD_X " 	" LINE )
				)
			) 
		)
		
	)

)


(defun APPEND_TO_TEMP ( / found)
(while
	(DO_MARKS)
;	(princ "\n>>koncze pobieranie punktu, przechodze dalej")
	(if (/= MARK_POINT nil)
		(progn
;			(princ "\n>>dodaje do listy")
			(setq NEW_LINE COORDS)
			(setq test (strcat (rtos NR 2 0)"`.*"  ))
			(setq 	found 0
					iter 0
			)


			(setq elements (length labelList))
			(repeat elements
				(setq element (nth iter labelList))
				(if (= (wcmatch element test) t)
					(progn
					(princ "ZNALEZIONO LINIE")
					(setq labelList (REPLACE labelList iter NEW_LINE))
					(foreach n labelList (print n))
					(setq found 1)
					)	
				)
				(setq iter (+ iter 1))		
			)
			(AFTER_APPEND)
			(if (= found 0)
				(progn
;					(princ "NIE ZNALEZIONO LINI, DOPISUJE NA KONIEC")
					(setq labelList
						(append labelList (list NEW_LINE))
					)
				)
			)
		)
	)
)
)

(defun REPLACE (l n w)
  (cond
    ( (null l) '())
    ( (eq n 0) (cons w (cdr l)))
    ( (cons (car l) (Replace (cdr l) (- n 1) w)))))

(defun MODIFY_WRITE_FILE ()
;	(princ "\n>>plik NIE jest pusty")
		(APPEND_TO_TEMP)
;	(princ "\n>>wychodze z petli")
	(setq FILE (open FILE_NAME "W"))
	(foreach l labelList 
		(write-line (strcat l) FILE)
;		(princ  (strcat "\nzapisuje linie "l" do pliku "))
	)
;	(princ "\n zamykam plik")
	(close FILE)
)

(defun AFTER_APPEND ()
	(DRAW_GUIDE_OR_NOT)
	(setq NR (1+ NR))
	(setq I (1+ I))
)

(defun BLANK_WRITE_FILE ()
;	(princ "\n>>plik jest pusty")
	(setq FILE (open FILE_NAME "W"))
	(write-line (strcat "Nr  Y 		X" ) FILE)
	(while
		(DO_MARKS)
		(write-line COORDS FILE)
		(DRAW_GUIDE_OR_NOT)
		(setq NR (1+ NR))
		(setq I (1+ I))
	)
	(close FILE)
)

(defun TEST_FILE_BLANK ()
;	(princ "\n>>sprawdzam zawartosc pliku")
 	(if (or 
	 		(= (length labelList) 0) 
			(= (length labelList) 1)
		)
 		(setq FILE_BLANK 1)
		(setq FILE_BLANK 0)
	)
)		

(defun DRAW_GUIDE_OR_NOT ()
;		(princ "\n>>tworze wskaznik albo nie")
		(if (or (= GUIDE_CREATE "T") (= GUIDE_CREATE "t"))
			(DRAW_GUIDE)
		)
)

(defun ADD_TO_GROUP()
	(setq grp (ssadd (entlast) grp))
)

(defun EXIT ()
	(princ 
		(strcat "\nKoniec, wskazano punktow: " (rtos I 2 0))
	)
	(princ)
)

;|
TODO
>>> file modify

(defun DRAW_POINT (/)d ang_90 ang_270 p1 p2 p3 p4)
	(command "_layer" "_m" "zud-pk" "_c" "7" "" "")
	(setq d (/ FONT_SIZE 3))
	(setq ang_90 (/ pi 2))
	(setq ang_270 (* 3 ang_90))
	(setq p1 (polar MARK_POINT ang_90 d))
	(setq p2 (polar MARK_POINT ang_270 d))
	(setq p3 (polar MARK_POINT 0 d))
	(setq p4 (polar MARK_POINT pi d))
	(command "_osmode" "0" "")
	(command "_line" p1 p2 "")
	(command "_line" p3 p4 "")
)
|;

(princ (strcat "ZUD")) 
