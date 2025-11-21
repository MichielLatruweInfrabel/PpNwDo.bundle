; This routine will take the numeric part of en entity
; decrement it by 1 en make the next selected text this string
; until the user break's
; Van de Kerckhove Wim
; 27.01.2013
; versie 2 : aangepast voor tailpart

(defun c:numaft (/ ent str cntr strpart numpart chkstr str flag tailpart num cnt ch numlen numstr
                 modtxt txtormtxt dimormtxt seldimt strdimedit el mt selatt)
  (setq ent (nentselp "Select source Text: " ))
  (if ent 
    (progn                                                               ;1
      (setq str (cdr(assoc 1 (entget (car ent )))))  
      (setq cntr 0)
      (while (< cntr 1)
        (setq strPart "" numPart "" chkStr str flag T                    ;var-init
              tailpart "" num nil cnt 0)                                 ;var-init
        (while(/= chkStr "")                                             ;zolang chkstr niet leeg is doe
          (setq ch(substr chkStr (strlen chkStr)))                       ;Get the last character in the string
          (if(= flag T)
            (progn                                                       ;2
              (if(member ch (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
                (setq numPart(strcat ch numPart)                         ;als ch=cijfer voeg samen met numgedeelte
                      num T)                                             ;set vlag numgedeelte gevonden
                (progn                                                   ;3
                  (if(= num T)                                           ;als numgedeelte gevonden
                    (setq flag nil strPart chkStr)                       ;then flag false want rest is letters
                    (setq tailpart (strcat ch tailpart)                  ;else staartstuk onthouden
                          cnt (+ 1 cnt))                                 ;staartstuk meten
                  )                                                      ;end if num T
                  (if(= cnt (strlen str))                                
                    (setq flag nil strpart tailpart                      
                          tailpart "")                                   ;geen numpart aanwezig
                  )                                                      ;end if cnt
                )                                                        ;progn3
              )                                                          ;end if member
            )                                                            ;progn2
          )                                                              ;if flag
          (setq chkStr(substr chkStr 1 (- (strlen chkStr) 1)))           ;laatste letter er af en next while loop
        )                                                                ;end while chkstr
        (setq numLen(strlen numPart))                                    ;bepaal lengte van numgedeelte
        (setq numStr (itoa(+ (atoi numPart) -1)))                        ;str to int -1 en int to str
          (while(< (strlen numStr) numLen)                               ;aanvullen met 0-en tot lengte orig
            (setq numStr(strcat "0" numStr))
          )                                                              ;end while numlen
        (setq str(strcat strPart numStr tailpart))                       ;voeg alles weer samen
        (setq modtxt (nentselp "\n Select Text to modify: "))
        (if modtxt
          (progn                                                         ;4
            (setq txtormtxt (cdr(assoc 0 (entget (car modtxt )))))  
            (cond                                                        ;afvraging
              ((or (= txtormtxt "TEXT")(= txtormtxt "MTEXT"))            ;optie 1 is het text of is het mtext
                (progn                                                   ;5
                  (setq dimormtxt (cdr(assoc 42 (entget (car modtxt )))))  
                  (if (/= dimormtxt nil)                                 ;if
                    (progn                                               ;then 6
                      (setq seldimt (cdr (assoc -1 (entget (car modtxt )))))
                      (setq strdimedit (substr str 5 (strlen str)))
                      (command "dimedit" "n" strdimedit seldimt "")
                    )                                                    ;progn6
                    (progn                                               ;else 7
                      (setq el (entget (car modtxt )))         
                      (setq mt (subst (cons 1 str) (assoc 1 EL) EL))
                      (entmod mt)
                    )                                                    ;progn7
                  )                                                      ;if dimortxt
                )                                                        ;progn5
              )                                                          ;cond or optie1
              ((= txtormtxt "ATTRIB")                                    ;optie 2 is het attrib
                (progn                                                   ;8
                  (setq selatt (cdr (assoc -1 (entget (car modtxt )))))  ;naam van de geselecteerde entity 
                  (command "-attedit" "" "" "" "" selatt "v" "r" str "") ;de waarde in voegen in deze entity
                )                                                        ;progn8
              )                                                          ;cond or optie 2
            (T (princ "\n Invalid Selection... "))
            )                                                            ;end cond
            (setq cntr 0)
          )                                                              ;progn4
          (setq cntr (+ cntr 1))	  
        )                                                                ;if modtxt
      )                                                                  ;while <1
    )                                                                    ;progn1
  )                                                                      ;if
 (princ)
)                                                                        ;defun


; This routine will take the numeric part of en entity
; increment it by 1 en make the next selected text this string
; until the user break's
; Van de Kerckhove Wim
; 27.01.2013
; versie 2 : aangepast voor tailpart
;09.02.21 wvdk: v 1062	toevoeging numopt2 optie




(defun c:numopt2 ( / )
  (setq hoeveel 2)
  (c:numopt)
)


(defun c:numopt (/ ent str cntr strpart numpart chkstr str flag tailpart num cnt ch numlen numstr
                 modtxt txtormtxt dimormtxt seldimt strdimedit el mt selatt)
  (setq ent (nentselp "Select source Text: " ))
  (if ent 
    (progn                                                    ;1
      (setq str (cdr(assoc 1 (entget (car ent )))))  
      (setq cntr 0)
      (while (< cntr 1)
        (setq strPart "" numPart "" chkStr str flag T         ;variabelen initialiseren
              tailpart "" num nil cnt 0)                      ;extra voor tailprt
        (while(/= chkStr "")                                  ;zolang chkstr niet leeg is doe
          (setq ch(substr chkStr (strlen chkStr)))            ;Get the last character in the string
          (if(= flag T)
            (progn                                            ;2
              (if(member ch (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
                (setq numPart(strcat ch numPart)              ;als ch=cijfer voeg samen met numgedeelte
                      num T)                                  ;set vlag numgedeelte gevonden
                (progn                                        ;3
                  (if(= num T)                                ;als numgedeelte gevonden
                    (setq flag nil strPart chkStr)            ;then flag false want rest is letters
                    (setq tailpart (strcat ch tailpart)       ;else staartstuk onthouden
                          cnt (+ 1 cnt))                      ;staartstuk meten
                  )                                           ;end if num T
                  (if(= cnt (strlen str))
                    (setq flag nil strpart tailpart
                          tailpart "")                        ;geen numpart aanwezig
                  )                                           ;end if cnt
                )                                             ;progn3
              )                                               ;end if member
            )                                                 ;progn2
          )                                                   ;if flag
          (setq chkStr(substr chkStr 1 (- (strlen chkStr) 1)));laatste letter er af en next while loop
        )                                                     ;end while chkstr
	(if (= hoeveel nil) (setq hoeveel 1))
        (setq numLen(strlen numPart))                         ;bepaal lengte van numgedeelte
        (setq numStr (itoa(+ (atoi numPart) hoeveel)))              ;str to int +1 en int to str
          (while(< (strlen numStr) numLen)                    ;aanvullen met 0-en tot lengte orig
            (setq numStr(strcat "0" numStr))
          )                                                   ;end while numlen
        (setq str(strcat strPart numStr tailpart))            ;voeg alles weer samen
        (setq modtxt (nentselp "\n Select Text to modify: "))
        (if modtxt
          (progn                                              ;4
            (setq txtormtxt (cdr(assoc 0 (entget (car modtxt )))))  
            (cond                                             ;afvraging
              ((or (= txtormtxt "TEXT")(= txtormtxt "MTEXT")) ;optie 1 is het text of is het mtext
                (progn                                        ;5
                  (setq dimormtxt (cdr(assoc 42 (entget (car modtxt )))))  
                  (if (/= dimormtxt nil)                      ;if
                    (progn                                    ;then 6
                      (setq seldimt (cdr (assoc -1 (entget (car modtxt )))))
                      (setq strdimedit (substr str 5 (strlen str)))
                      (command "dimedit" "n" strdimedit seldimt "")
                    )                                         ;progn6
                    (progn                                    ;else 7
                      (setq el (entget (car modtxt )))         
                      (setq mt (subst (cons 1 str) (assoc 1 EL) EL))
                      (entmod mt)
                    )                                         ;progn7
                  )                                           ;if dimortxt
                )                                             ;progn5
              )                                               ;cond or optie1
              ((= txtormtxt "ATTRIB")                         ;optie 2 is het attrib
                (progn                                        ;8
                  (setq selatt (cdr (assoc -1 (entget (car modtxt ))))) ;naam van de geselecteerde entity 
                  (command "-attedit" "" "" "" "" selatt "v" "r" str "");de waarde in voegen in deze entity
                )                                             ;progn8
              )                                               ;cond or optie 2
            (T (princ "\n Invalid Selection... "))
            )                                                 ;end cond
            (setq cntr 0)
          )                                                   ;progn4
          (setq cntr (+ cntr 1))	  
        )                                                     ;if modtxt
      )                                                       ;while <1
    )                                                         ;progn1
  )                                                           ;if
 (princ)
)                                                             ;defun
