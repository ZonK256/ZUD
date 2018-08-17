(defun c:zud(/ old_cmdecho)
	(setq old_cmdecho (getvar "CMDECHO"))
	(setvar "CMDECHO" 0)	
	(INIT)
	(READ_FILE)
	(TEST_FILE_BLANK)
	(setvar "CMDECHO" old_cmdecho)
	(EXIT)
)

(defun INIT ( / *error*)
	(defun *error* (msg) (ERROR_INIT msg))
	(setq 	NR 1
		 	FONT_SIZE 1.5 
			LINE "t" 
			GUIDE_CREATE "T"
			I 0
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
				(setq GUIDE_CREATE (getstring "Tworzyc znaczniki? <T> [T/N]"))
				(if (or (= GUIDE_CREATE "T") (= GUIDE_CREATE "t"))
					(setq FONT_SIZE (getreal "Podaj rozmiar tekstu:"))
				)			
			)
		)
	(setq FILE_NAME (getfiled "Wpisz nazwe pliku" "" "txt;csv;xls" 1))

	(if (= ERROR_OCCURED 1)
		(READ_FILE)
	)
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

(defun DRAW_GUIDE (/ d p1 p2 p3 *error*)
	(defun *error* (msg) (ERROR_DRAW_GUIDE msg))
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
	(princ "\n>>pobieram plik")

	  (setq labelList(list (strcat "Nr  Y 		X" ) ))
		(setq FILE (open FILE_NAME "R"))
			(if (/= (read-line FILE) nil)
				(progn
	          		(while (setq a (read-line FILE))
	           			(setq labelList 
							(append labelList (list a)) 
						)
						(print a)
	          		)
	          		(close FILE)
	   				(setq formList (list))
	   				(foreach a labelList 
			   			(setq formList 
					   	(append formList(list a))
						)
	   				)				
				)
			
			)

	(if (= ERROR_OCCURED 1)
		(TEST_FILE_BLANK)
	)

)

(defun DO_MARKS ( / *error* )
	(defun *error* (msg) (ERROR_DO_MARKS msg))

	(princ "\n>>pobieram punkt")
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
		(princ "\n>>koncze pobieranie punktu, przechodze dalej")
		(if (/= MARK_POINT nil)
			(progn
				(princ "\n>>dodaje do listy")
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
						(princ "NIE ZNALEZIONO LINI, DOPISUJE NA KONIEC")
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
   	 	( (cons (car l) (Replace (cdr l) (- n 1) w)))
	)
)

(defun MODIFY_WRITE_FILE ()
	(princ "\n>>plik NIE jest pusty")
		(APPEND_TO_TEMP)
	(princ "\n>>wychodze z petli")
	(setq FILE (open FILE_NAME "W"))
	(foreach l labelList 
		(write-line (strcat l) FILE)
		(princ  (strcat "\nzapisuje linie "l" do pliku "))
	)
	(princ "\n zamykam plik")
	(close FILE)
)

(defun AFTER_APPEND ()
	(DRAW_GUIDE_OR_NOT)
	(setq NR (1+ NR))
	(setq I (1+ I))
)

(defun BLANK_WRITE_FILE ()
	(princ "\n>>plik jest pusty")
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
	(princ "\n>>sprawdzam zawartosc pliku")
 	(if (or 
	 		(= (length labelList) 0) 
			(= (length labelList) 1)
		)
 		(BLANK_WRITE_FILE)		;file was blank
		(MODIFY_WRITE_FILE)		;file was not blank
	)
)		

(defun DRAW_GUIDE_OR_NOT ()
		(princ "\n>>tworze wskaznik albo nie")
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

;==[ERROR HANDLING]==;
(defun ERROR_INIT ( msg / DO_SMTH)
	(print msg)
	(setq DO_SMTH (getstring "\n Czy chcesz zakonczyc dzialanie programu? <N> [T/N]"))
		(if (or (= DO_SMTH "T") (= DO_SMTH "t"))
			(EXIT)
			(progn
				(setq ERROR_OCCURED 1)
				(INIT)
			)
	)
)
(defun ERROR_DO_MARKS ( msg / DO_SMTH)
	(print msg)
	(setq DO_SMTH (getstring "\n Czy chcesz zakonczyc dzialanie programu? <N> [T/N]"))
		(if (or (= DO_SMTH "T") (= DO_SMTH "t"))
			(EXIT)
			(progn
				(setq ERROR_OCCURED 1)
				(DO_MARKS)
			)
	)
)
(defun ERROR_DRAW_GUIDE ( msg / DO_SMTH)
	(print msg)
	(setq DO_SMTH (getstring "\n Czy chcesz zakonczyc dzialanie programu? <N> [T/N]"))
		(if (or (= DO_SMTH "T") (= DO_SMTH "t"))
			(EXIT)
			(progn
				(setq ERROR_OCCURED 1)
				(DRAW_GUIDE)
			)
	)
)	
;==[ERROR HANDLING]==;
(print (strcat "ZUD"))


;===============================;

(defun c:miarka ()
	(initget "Plik Projekt")
	(setq MEASURE (getstring "\n Chcesz zmierzyc odleglosc punktow z pliku, czy projektu (Plik/Projekt): "))
	(if (= MEASURE "Plik") 
			(MEASURE_FILE) 
			(MEASURE_PROJECT)
	)
	(princ)
)

(defun MEASURE_FILE ( / p1 p2)
		(setq FILE_NAME (getfiled "Wpisz nazwe pliku" "" "txt;csv;xls" 1))
		(READ_FILE)
		(setq 	
				TOTAL_DISTANCE 0
				lenghtList (- (length labelList) 1)
				n 1
		)
		(princ "\n>>rozpoczynam liczenie")
		(repeat (- lenghtList 1)
			(setq 
				p1  (list 
						(atof																			;string to real
							(vl-string-subst "" "," (sym2str											;symbol to string & delete comma
														(cadr											
															(read 
																(strcat "(" (nth n labelList) ")")		
															)
														)
													)
							)
						) 
						(caddr
							(read 
							(strcat "(" (nth n labelList) ")")
							)
						)
					) 
 				p2  (list 
				 		(atof
							(vl-string-subst "" "," (sym2str
														(cadr
															(read 
																(strcat "(" (nth (+ n 1) labelList) ")")
															)
														)
													)
							)
						) 
						(caddr 
							(read 
								(strcat "(" (nth (+ n 1) labelList) ")")
							)
						)
					) 
				TOTAL_DISTANCE	(+ TOTAL_DISTANCE (distance p1 p2))
				n (+ n 1)
			)
		)
		(princ (strcat "\nLaczny dystans = " (rtos TOTAL_DISTANCE)))
)

(defun MEASURE_PROJECT ( / p1 p2)
	(setq sumdist 0)
	(setq p1 (getpoint "\nWybierz pierwszy punkt: "))
	(while (setq p2 (getpoint p1 "\nWybierz nastepny punkt: "))
		(prompt
			(strcat 
				"\nDystans od ostatniego punktu = " (rtos (distance p1 p2))
				",\nLaczny dystans = " 	(rtos 	(setq sumdist 
													(+ 	sumdist 
														(distance p1 p2)
													)
												)
										)
			)
		)
		(setq p1 p2)
	)
	(princ)
)

(print (strcat "MIARKA"))

(defun sym2str (symbol / f n r)
	(cond	
		((setq f (open (setq n (strcat (getvar "tempprefix") "$.$")) "w"))
			(princ symbol f)
			(setq f (close f))
			(cond	
				( (setq f (open n "r"))
						(setq r (read-line f))
						(setq f (close f)) r
				)
			)
		)
	)
)