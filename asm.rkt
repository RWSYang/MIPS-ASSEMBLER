#lang racket

;; scan is the main function provided, which uses the data definitions
;; and helper functions that follow. Sample tests are at the bottom of the file.
(define (scan str)
  (scan-func str asmtrlst 'start asmfinal))

;; scan-func: (listof char) trans-table symbol (listof symbol) -> (listof token)

(define (scan-func str trans start final)
  (scan-acc (string->list str) trans start final empty empty))

;; Next we specify the data definitions for tokens and the various components
;; of an FSM.

(define-struct token (kind lexeme) #:transparent)

;; A token is a (make-token k l), where k is a symbol
;;  and l is (union (list char) int).

(define-struct transition (state charset next) #:transparent)

;; A transition table is a list of transitions.
;; A transition is a (make-transition s cs ns), where s and ns are symbols,
;;  and cs is a function char->boolean that tests whether the transition applies.

;; The sample FSM provided is defined by (asmtrlst, 'start, asmfinal).
;; Definitions of asmtrlst and asmfinal follow.

;; functions used in defining sample transition table

(define (one-to-nine? ch)
  (and (char<=? #\1 ch) (char<=? ch #\9)))

(define (hex-digit? ch)
  (or
   (char-numeric? ch)
   (and (char<=? #\a ch) (char<=? ch #\f))
   (and (char<=? #\A ch) (char<=? ch #\F))))

(define (chartest ch)
  (lambda (x) (char=? x ch)))

;; sample transition table

(define asmtrlst
  (list
   (make-transition 'start char-whitespace? 'whitespace)
   (make-transition 'start char-alphabetic? 'id)
   (make-transition 'id char-alphabetic? 'id)
   (make-transition 'id char-numeric? 'id)
   (make-transition 'start one-to-nine? 'int)
   (make-transition 'int char-numeric? 'int)
   (make-transition 'start (chartest #\-) 'minus)
   (make-transition 'minus char-numeric? 'int)
   (make-transition 'start (chartest #\,) 'comma)
   (make-transition 'start (chartest #\() 'lparen)
   (make-transition 'start (chartest #\)) 'rparen)
   (make-transition 'start (chartest #\$) 'dollar)
   (make-transition 'dollar char-numeric? 'register)
   (make-transition 'register char-numeric? 'register)
   (make-transition 'start (chartest #\0) 'zero)
   (make-transition 'zero (chartest #\x) 'zerox)
   (make-transition 'zero char-numeric? 'int)
   (make-transition 'zerox hex-digit? 'hexint)
   (make-transition 'hexint hex-digit? 'hexint)
   (make-transition 'id (chartest #\:) 'label)
   (make-transition 'start (chartest #\;) 'comment)
   (make-transition 'comment (lambda (x) true) 'comment)
   (make-transition 'start (chartest #\.) 'dot)
   (make-transition 'dot (chartest #\w) 'dotw)
   (make-transition 'dotw (chartest #\o) 'dotwo)
   (make-transition 'dotwo (chartest #\r) 'dotwor)
   (make-transition 'dotwor (chartest #\d) 'dotword)
   ))

;; sample list of final states

(define asmfinal
  (list
    'register
    'int
    'id
    'label
    'comma
    'lparen
    'rparen
    'zero
    'hexint
    'comment
    'dotword
    'whitespace
    ))

;; scan-acc is the main workhorse of the lexer. It uses accumulative recursion
;; to run the FSM specified by (trans, state, final) on the list of characters cl.
;; acc accumulates the characters of the current token in reverse order, and
;; tacc accumulates the token list in reverse order.

;; scan-acc: (listof char) trans-table symbol (listof symbol) (listof char) (listof token) -> (listof token)

(define (scan-acc cl trans state final acc tacc)
  (cond
    [(empty? cl)
       (if (member state final)
           (if (or (symbol=? state 'whitespace) (symbol=? state 'comment))
               (reverse tacc)
               (reverse (cons (finalize-token state (reverse acc)) tacc)))
           (error 'ERROR "unexpected end of string\n"))]
    [else
      (let ([trl (memf (lambda (x) (found-trans? state (first cl) x)) trans)])
        (cond
          [(and (boolean? trl) (member state final))
             (if (symbol=? state 'whitespace)
                 (scan-acc cl trans 'start final empty tacc)
                 (scan-acc cl trans 'start final empty (cons (finalize-token state (reverse acc)) tacc)))]
          [(boolean? trl)
             (error 'ERROR "left to parse:~a ~a\n" state (list->string cl))]
          [(symbol=? state 'comment)
             (reverse tacc)]
          [else
             (scan-acc (rest cl) trans (transition-next (first trl)) final (cons (first cl) acc) tacc)]))]))

;; helper functions for scan-acc

(define (found-trans? state ch tr)
  (and (symbol=? state (transition-state tr))
       ((transition-charset tr) ch)))

;; finalize-token symbol (listof char) -> token
(define (finalize-token state l)
  (cond
    [(symbol=? state 'int) (make-token 'int (check-int-range (list->number l)))]
    [(symbol=? state 'zero) (make-token 'int 0)]
    [(symbol=? state 'hexint) (make-token 'hexint (check-hexint-range (list->hexint (rest (rest l)))))]
    [(symbol=? state 'register) (make-token 'register (check-reg-range (list->number (rest l))))]
    [else (make-token state l)]))

;; helper functions for finalize-token

(define (list->number lst) (string->number (list->string lst)))

(define (list->hexint lst) (string->number (list->string lst) 16))

;; Scheme supports unbounded integers but MIPS doesn't
(define (check-int-range n)
  (cond
    [(<= -2147483648 n 4294967295) n]
    [else (error 'ERROR "integer out of range: ~a" n)]))

(define (check-hexint-range n)
  (cond
    [(<= 0 n 4294967295) n]
    [else (error 'ERROR "integer out of range: ~a" n)]))

(define (check-reg-range n)
  (cond
    [(<= 0 n 31) n]
    [else (error 'ERROR "register out of range: ~a" n)]))

;; Some very basic tests
;(scan "01")
;(scan "0xabcd ; should be ignored")
;(scan ".word 01234")
;(scan "0add")
;(scan "foo:     add $1, $2, $3   ; A comment.")



(define htcounter 0)
(define integ 0)
(define jr 0)
(define jalr 0)
(define label (make-hash))
(define sub 0)
(define add 0)
(define slt 0)
(define sltu 0)

(define beq 0)
(define bne 0)
(define mflo 0)
(define mfhi 0)
(define lis 0)
(define mult 0)
(define multu 0)
(define div 0)
(define divu 0)
(define sw 0)
(define lw 0)
;prints a hash table
(define (print-hash ht)
  (define htlist (hash-keys ht))
  (for-each (lambda (x)
              (fprintf (current-error-port) "~a " x)
              (fprintf (current-error-port) "~a~n" (hash-ref ht x)))
              htlist))
(define valid #f)
(define invInput #f)
(define exsLabel #f)
(define lblInst #f)
(define incArgs #f)
(define noLabel #f)
(define singleInst #f)



(define sw_reg 0)
(define lw_reg 0)
(define add_reg 0)
(define sub_reg 0)
(define slt_reg 0)
(define sltu_reg 0)
(define bne_reg 0)
(define beq_reg 0)
(define mflo_reg 0)
(define mfhi_reg 0)
(define lis_reg 0)
(define mult_reg 0)
(define multu_reg 0)
(define div_reg 0)
(define divu_reg 0)

(define assembly-code (list))
;Scan the first time for labels
(define (make-label)
  (define line (read-line))
  (define instructioncarry #f)
  (define comma 0)
  (define reg 0)
  (define bracket 0)
  (cond
    [(eof-object? line) (void)]
    [(> (string-length line) 0) ; Ignore blank lines
        ; Ignore comment-only lines as well
        ; When a comment-only line is scanned, an empty struct is returned
        (define scanned (scan line))
        (define num-label 0)
        (cond
          [(empty? scanned) (make-label)]
          [else
           (cond
             [(empty? assembly-code)
              (set! assembly-code (list scanned))]
             [else 
               (set! assembly-code (append assembly-code (list scanned)))])
           (for-each (lambda (x) 
                     (cond
                       [(> bracket 0)
                        (cond
                          [(equal? bracket 1)
                           (cond
                             [(equal? (symbol->string (token-kind x)) "lparen")
                              (set! bracket 0)
                              (set! reg 1)
                              ]
                             [else
                              (fprintf (current-error-port) "ERROR:Invalid bracket~n")])]
                          [(equal? bracket 2)
                           (cond
                             [(equal? (symbol->string (token-kind x)) "rparen")
                              (set! bracket 0)
                              (set! reg 0) ]
                             [else
                              (fprintf (current-error-port) "ERROR:Invalid bracket~n")])]
                          [else
                           (fprintf (current-error-port) "ERROR:Invalid input~n")])]
                             
                       [(equal? (symbol->string (token-kind x)) "comma")
                        (cond
                          [(equal? comma 1)
                           (set! comma 0)
                           (set! reg 1)
                           ]
                          [else
                          (fprintf (current-error-port) "ERRROR:Invalid comma~n")])]
                                         
                       [(equal? (symbol->string (token-kind x)) "label")
                        (define len (length (token-lexeme x)))
                        (cond
                          [(equal? instructioncarry #t)
                          ; (error 'ERROR "label following instruction~n")
                           (fprintf (current-error-port) "ERROR:Label following instruction.~n")
                           ;(set! lblInst #t)
                           ]
                          [(hash-has-key? label (substring (list->string (token-lexeme x)) 0 (- len 1)))
                           (error 'ERROR "label already exists~n")
                           ;(fprintf (current-error-port) "ERROR:Label already exists.~n")
                           ;(set! exsLabel #t)
                           ]
                          [else
                           (set! num-label (+ 1 num-label))
                           (define temp (substring (list->string(token-lexeme x)) 0 (- len 1)))
                           (hash-set! label temp htcounter)
                          ])]

                       ;Check if you need to take a register
                         [(equal? reg 1)
                          (cond
                            ;Check for mult instruction
                            [(> mult 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! reg 0)
                                (set! comma 1)
                                (cond
                                  [(equal? mult 1)
                                   (set! mult (+ mult 1))
                                   ]
                                  [(equal? mult 2)
                                   (set! mult 0)
                                   (set! comma 0)
                                   (set! htcounter (+ htcounter 4))])]
                                   
                               [else
                                (error 'ERROR "not a valid register")])]

                            ;Check for multu instruction
                            [(> multu 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! reg 0)
                                (set! comma 1)
                                (cond
                                  [(equal? multu 1)
                                   (set! multu (+ multu 1))
                                   ]
                                  [(equal? multu 2)
                                   (set! multu 0)
                                   (set! comma 0)
                                   (set! htcounter (+ htcounter 4))])]
                                   
                               [else
                                (error 'ERROR "not a valid register")])]

                            ;Check for multu instruction
                            [(> div 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! reg 0)
                                (set! comma 1)
                                (cond
                                  [(equal? div 1)
                                   (set! div (+ div 1))
                                   ]
                                  [(equal? div 2)
                                   (set! div 0)
                                   (set! comma 0)
                                   (set! htcounter (+ htcounter 4))])]

                               
                                   
                               [else
                                (error 'ERROR "not a valid register")])]
                            ;Check for divu instruction
                            [(> divu 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! reg 0)
                                (set! comma 1)
                                (cond
                                  [(equal? divu 1)
                                   (set! divu (+ divu 1))
                                   ]
                                  [(equal? divu 2)
                                   (set! divu 0)
                                   (set! comma 0)
                                   (set! htcounter (+ htcounter 4))])]
                               [else
                                (error 'ERROR "not a valid register")])]
                            
                            ;Check for mflo instruction
                            [(equal? mflo 1)
                             (set! reg 0)
                             (set! mflo 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                
                                (set! htcounter (+ htcounter 4))]
                               [else
                                (fprintf (current-error-port) "ERROR:Invalid register")])]
                            ;Check for mfhi instruction
                            [(equal? mfhi 1)
                             (set! reg 0)
                             (set! mfhi 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! htcounter (+ htcounter 4))]
                               [else
                                (fprintf (current-error-port) "ERROR:Invalid register")])]
                            ;Check for lis instruction
                            [(equal? lis 1)
                             (set! reg 0)
                             (set! lis 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! htcounter (+ htcounter 4))]
                               [else
                                (fprintf (current-error-port) "ERROR:Invalid register")])]
                            
                            ;Check for beq instruction
                            [(> beq 0)
                             (cond
                               [(equal? beq 1)
                                (cond
                                  [(equal? (symbol->string (token-kind x)) "register")
                                   
                                   (set! reg 0)
                                   (set! comma 1)
                                   (set! beq (+ 1 beq))]
                                  [else
                                   (fprintf (current-error-port) "ERROR:invalid register~n")
                                   (set! reg 0)
                                   (set! comma 0)
                                   (set! beq 0)])]
                               [(equal? beq 2)
                                (cond
                                  [(equal? (symbol->string (token-kind x)) "register")
                                   (set! reg 0)
                                   (set! comma 1)
                                   (set! beq (+ 1 beq))]
                                  [else
                                   (fprintf (current-error-port) "ERROR:invalid register~n")
                                   (set! reg 0)
                                   (set! comma 0)
                                   (set! beq 0)])]
                               [(equal? beq 3)
                                (set! comma 0)
                                (set! reg 0)
                                (set! reg 0)
                                (cond
                                  [(equal? (symbol->string (token-kind x)) "hexint")
                                   (cond
                                     [(and (<= (token-lexeme x) 65535) (>= (token-lexeme x) 0))
                                      (set! reg 0)
                                      (set! comma 0)
                                      (set! reg 0)
                                      (set! htcounter (+ htcounter 4))
                                      ]
                                     [else
                                      (fprintf (current-error-port) "ERROR:value not in range~n")
                                      
                                      ])]
                                  [(equal? (symbol->string (token-kind x)) "int")
                                    (cond
                                      [(and (<= (token-lexeme x) 32767) (>= (token-lexeme x) -32768))
                                       (set! reg 0)
                                       (set! comma 0)
                                       (set! beq 0)
                                       (set! htcounter (+ htcounter 4))]
                                      [else
                                       (fprintf (current-error-port) "ERROR:value not in range~n")])]
                                   
                                   
                                  [(equal? (symbol->string (token-kind x)) "id")
                                   (set! reg 0)
                                   (set! comma 0)
                                   (set! beq 0)
                                   (set! htcounter (+ htcounter 4))
                                   ]
                                  [else
                                   (fprintf (current-error-port) "ERROR:invalid input~n")])]
                               )]
                                
                             
                             ;Check for bne instruction
                            [(> bne 0)
                             (cond
                               [(equal? bne 1)
                                (cond
                                  [(equal? (symbol->string (token-kind x)) "register")
                                   (set! reg 0)
                                   (set! comma 1)
                                   (set! bne (+ 1 bne))]
                                  [else
                                   (fprintf (current-error-port) "ERROR:invalid register~n")
                                   (set! comma 0)
                                   (set! reg 0)
                                   (set! bne 0)])]
                               [(equal? bne 2)
                                (cond
                                  [(equal? (symbol->string (token-kind x)) "register")
                                   (set! reg 0)
                                   (set! comma 1)
                                   (set! bne (+ 1 bne))]
                                  [else
                                   (fprintf (current-error-port) "ERROR:invalid register~n")
                                   (set! comma 0)
                                   (set! reg 0)
                                   (set! bne 0)
                                   ])]
                               [(equal? bne 3)
                                (set! comma 0)
                                (set! reg 0)
                                (set! bne 0)
                                (cond
                                  [(equal? (symbol->string (token-kind x)) "hexint")
                                   (cond
                                     [(and (<= (token-lexeme x) 65535) (>= (token-lexeme x) 0))
                                      (set! reg 0)
                                      (set! comma 0)
                                      (set! bne 0)
                                      (set! htcounter (+ htcounter 4))
                                      ]
                                     [else
                                      (fprintf (current-error-port) "ERROR:value not in range~n")])]
                                  [(equal? (symbol->string (token-kind x)) "int")
                                   (cond
                                     [(and (<= (token-lexeme x) 32767) (>= (token-lexeme x) -32768))
                                      (set! reg 0)
                                      (set! comma 0)
                                      (set! bne 0)
                                      (set! htcounter (+ htcounter 4))
                                      ]
                                     [else
                                      (fprintf (current-error-port) "ERROR:value not in range~n")])]
                                  [(equal? (symbol->string (token-kind x)) "id")
                                   (set! reg 0)
                                   (set! comma 0)
                                   (set! bne 0)
                                   (set! htcounter (+ htcounter 4))
                                   ]
                                  [else
                                   (fprintf (current-error-port) "ERROR:invalid input~n")])]
                               )]
                            ;Check for Add insruction
                            [(> add 0)
                             (cond
                               ;bitwise and shift accordingly depending on which register it is
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! reg 0)
                                (set! comma 1)
                                (cond
                                  [(equal? add 1)
                                   (set! add (+ add 1))
                                   ]
                                  [(equal? add 2)
                                   (set! add (+ add 1))
                                   ]
                                  [(equal? add 3)
                                   (set! htcounter (+ htcounter 4))
                                   (set! comma 0)
                                   (set! add 0)])]
                               [else
                                (error 'ERROR "not a valid register")])]
                            ;Check for Sub instruction
                            [(> sub 0)
                             (cond
                               ;bitwise and shift accordingly depending on which register it is
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! reg 0)
                                (set! comma 1)
                                (cond
                                  [(equal? sub 1)
                                   (set! sub (+ sub 1))
                                   ]
                                  [(equal? sub 2)
                                   (set! sub (+ sub 1))
                                   ]
                                  [(equal? sub 3)
                                   (set! htcounter (+ htcounter 4))
                                   (set! comma 0)
                                   (set! sub 0)])]
                               [else
                                (error 'ERROR "not a valid register")])]
                            ;Check for Slt instruction
                            [(> slt 0)
                             (cond
                               ;bitwise and shift accordingly depending on which register it is
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! reg 0)
                                (set! comma 1)
                                (cond
                                  [(equal? slt 1)
                                   (set! slt (+ slt 1))
                                   ]
                                  [(equal? slt 2)
                                   (set! slt (+ slt 1))
                                   ]
                                  [(equal? slt 3)
                                   (set! htcounter (+ htcounter 4))
                                   (set! comma 0)
                                   (set! slt 0)])]
                               [else
                                (error 'ERROR "not a valid register")])]
                            ;Check for Sltu instruction
                            [(> sltu 0)
                             (cond
                               ;bitwise and shift accordingly depending on which register it is
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! reg 0)
                                (set! comma 1)
                                (cond
                                  [(equal? sltu 1)
                                   (set! sltu (+ sltu 1))
                                   ]
                                  [(equal? sltu 2)
                                   (set! sltu (+ sltu 1))
                                   ]
                                  [(equal? sltu 3)
                                   (set! htcounter (+ htcounter 4))
                                   (set! sltu 0)
                                   (set! comma 0)
                                   ])]
                               [else
                                (error 'ERROR "not a valid register")])]

                            ;lw instruction
                            [(> lw 0)
                             (cond
                                  [(equal? lw 1)
                                   (set! reg 0)
                                   (cond
                                     [(equal? (symbol->string (token-kind x)) "register")
                                      (set! lw (+ lw 1))
                                      (set! comma 1)]
                                     [else
                                      (printf (current-error-port) "ERROR:invalid register~n")])]
                                   
                                  [(equal? lw 2)
                                   (set! reg 0)
                                   (cond
                                     [(equal? (symbol->string (token-kind x)) "int")
                                      (cond
                                        [(and (<= -32768 (token-lexeme x)) (>= 32767 (token-lexeme x)))
                                         (set! lw (+ lw 1))
                                         (set! bracket 1)]
                                        [else
                                         (fprintf (current-error-port) "ERROR:value not in range~n")])]
                                      [(equal? (symbol->string (token-kind x)) "hexint")
                                       (cond
                                         [(and (<= 0 (token-lexeme x)) (>= 65535 (token-lexeme x)))
                                          (set! lw(+ lw 1))
                                          (set! bracket 1)]
                                         [else
                                          (fprintf (current-error-port) "ERROR:value not in range~n")])]
                                         
                                        [else
                                      (printf (current-error-port) "ERROR:inalid value~n")])]
                                  
                                  [(equal? lw 3)
                                   (set! reg 0)
                                   (cond
                                     [(equal? (symbol->string (token-kind x)) "register")
                                      (set! lw 0)
                                      (set! bracket 2)
                                      (set! htcounter (+ htcounter 4))]
                                      
                                     [else
                                      (printf (current-error-port) "ERROR:invalid value~n")])]
                                   
                               [else
                                (fprintf (current-error-port) "ERROR:invalid register~n")])]
                             ;sw instruction
                            [(> sw 0)
                             (cond
                                  [(equal? sw 1)
                                   (set! reg 0)
                                   (cond
                                     [(equal? (symbol->string (token-kind x)) "register")
                                      (set! sw (+ sw 1))
                                      (set! comma 1)]
                                     [else
                                      (printf (current-error-port) "ERROR:invalid register~n")])]
                                   
                                  [(equal? sw 2)
                                   (set! reg 0)
                                   (cond
                                     [(equal? (symbol->string (token-kind x)) "int")
                                      (cond
                                        [(and (<= -32768 (token-lexeme x)) (>= 32767 (token-lexeme x)))
                                         (set! sw (+ sw 1))
                                         (set! bracket 1)]
                                        [else
                                         (fprintf (current-error-port) "ERROR:value not in range~n")])]
                                      [(equal? (symbol->string (token-kind x)) "hexint")
                                       (cond
                                         [(and (<= 0 (token-lexeme x)) (>= 65535 (token-lexeme x)))
                                          (set! sw(+ sw 1))
                                          (set! bracket 1)]
                                         [else
                                          (fprintf (current-error-port) "ERROR:value not in range~n")])]
                                         
                                        [else
                                      (printf (current-error-port) "ERROR:inalid value~n")])]
                                  
                                  [(equal? sw 3)
                                   (set! reg 0)
                                   (cond
                                     [(equal? (symbol->string (token-kind x)) "register")
                                      (set! sw 0)
                                      (set! bracket 2)
                                      (set! htcounter (+ htcounter 4))]
                                      
                                     [else
                                      (printf (current-error-port) "ERROR:invalid value~n")])]
                                   
                               [else
                                (fprintf (current-error-port) "ERROR:invalid register~n")])]
                               
                            )]


                          
                       ;Check for Jump instruction
                       [(equal? jr 1)
                          (cond
                            [(equal? (symbol->string (token-kind x)) "register")
                             (set! jr 0)
                             (set! htcounter (+ htcounter 4))]
                            [else
                             (error 'ERROR "not a valid register")])]
                       ;Check for Jalr instruction
                       [(equal? jalr 1)
                        (cond
                          [(equal? (symbol->string (token-kind x)) "register")
                           (set! jalr 0)
                           (set! htcounter (+ htcounter 4))]
                          [else
                           (error 'ERROR "not a valid register")])]
                           
                          
                       ;Check if it's an instruction
                       [(equal? (symbol->string (token-kind x)) "id")
                        (cond
                          
                          ;Jump Instruction
                          [(equal? (list->string (token-lexeme x)) "jr")
                           (cond
                             [(not (equal? (- (length scanned) num-label) 2))
                              (fprintf (current-error-port) "ERRROR:Incorrect number of arguments~n")
                            ;  (error 'Error "Incorrect number of arguments~n")
                              ]
                             [else
                              (set! jr 1)]
                             )]
                          ;Jalr instruction
                          [(equal? (list->string (token-lexeme x)) "jalr")
                           (cond
                             [(not (equal? (- (length scanned) num-label) 2))
                              (fprintf (current-error-port) "ERRROR:Incorrect number of arguments~n")
                              ;(error 'Error "Incorrect number of arguments~n")
                              ]
                             [else
                              (set! reg 0)
                              (set! jalr 1)]
                             )]
                          ;Add instruction
                          [(equal? (list->string (token-lexeme x)) "add")
                           (cond
                             [(not (equal? (- (length scanned) num-label) 6))
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")
                              ]
                             [else
                              (set! reg 1)
                              (set! add 1)])]
                          ;Sub instruction
                          [(equal? (list->string (token-lexeme x)) "sub")
                           (cond
                             [(not (equal? (- (length scanned) num-label) 6))
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")]
                             [else
                              (set! reg 1)
                              (set! sub 1)])]
                          ;Slt instruction
                          [(equal? (list->string (token-lexeme x)) "slt")
                           (cond
                             [(not (equal? (- (length scanned) num-label) 6))
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")]
                             [else
                              (set! reg 1)
                              (set! slt 1)])]
                          ;Sltu instruction
                          [(equal? (list->string (token-lexeme x)) "sltu")
                           (cond
                             [(not (equal? (- (length scanned) num-label) 6))
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")]
                             [else
                              (set! reg 1)
                              (set! sltu 1)])]
                          ;Beq instruction
                          [(equal? (list->string (token-lexeme x)) "beq")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 6)
                              (set! reg 1)
                              (set! beq 1)]
                             [else
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")])]
                          ;Bne instruction
                          [(equal? (list->string (token-lexeme x)) "bne")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 6)
                              (set! reg 1)
                              (set! bne 1)]
                             [else
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")])]
                          ;Mult instruction
                          [(equal? (list->string (token-lexeme x)) "mult")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 4)
                              (set! reg 1)
                              (set! mult 1)]
                             [else
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")])]
                          ;Multu instruction
                          [(equal? (list->string (token-lexeme x)) "multu")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 4)
                              (set! reg 1)
                              (set! multu 1)]
                             [else
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")])]
                          ;Div instruction
                          [(equal? (list->string (token-lexeme x)) "div")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 4)
                              (set! reg 1)
                              (set! div 1)]
                             [else
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")])]
                          ;Divu instruction
                          [(equal? (list->string (token-lexeme x)) "divu")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 4)
                              (set! reg 1)
                              (set! divu 1)]
                             [else
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")])]
                          ;mflo instruction
                          [(equal? (list->string (token-lexeme x)) "mflo")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 2)
                              (set! reg 1)
                              (set! mflo 1)]
                             [else
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")])]
                          ;lw instruction
                          [(equal? (list->string (token-lexeme x)) "lw")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 7)
                              (set! reg 1)
                              (set! lw 1)
                              ]
                             [else
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")])]
                          ;sw instruction
                          [(equal? (list->string (token-lexeme x)) "sw")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 7)
                              (set! reg 1)
                              (set! sw 1)]
                             [else
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")])]
                          ;mfhi instruction
                          [(equal? (list->string (token-lexeme x)) "mfhi")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 2)
                              (set! reg 1)
                              (set! mfhi 1)]
                             [else
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")])]
                          ;lis instruction
                          [(equal? (list->string (token-lexeme x)) "lis")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 2)
                              (set! reg 1)
                              (set! lis 1)]
                             [else
                              (fprintf (current-error-port) "ERROR:Incorrect number of arguments~n")]
                          )])]
                       
                       ;Check if it's a .word 
                       [(equal? (symbol->string (token-kind x)) "dotword")
                        (set! instructioncarry #t)
                        (cond
                          [(not (equal? (- (length scanned) num-label) 2))
                           (error 'ERROR "Incorrect number of arguments~n")
                           ]
                          [else
                           (set! integ 1)])]
                       ;Check if the instruction was a .word and if the next is a number or label
                      [(equal? integ 1)
                       (set! integ 0)
                       (cond
                         [(equal? (symbol->string (token-kind x)) "hexint")
                          (set! instructioncarry #t)
                          (set! htcounter (+ htcounter 4))
                          ]
                         [(equal? (symbol->string (token-kind x)) "int")
                          (set! instructioncarry #t)
                          (set! htcounter (+ htcounter 4))
                         ]
                         [(equal? (symbol->string (token-kind x)) "id")
                             (set! instructioncarry #t)
                             (set! htcounter (+ htcounter 4))]
                         [else
                          (error 'ERROR "Invalid Input~n")
                          ]
                      )]
                      [else
                       (error 'ERROR "Invalid Input")
                       ])) scanned )
           (make-label)])]
    ;         (print-hash label)])])]
    [else (make-label)]))
(define pc_counter 0)


; This file just uses scan to tokenize each line of the input and write byte everything
(define (scan-input assembly)
  (define instructioncarry #f)
  (define comma 0)
  (define reg 0)
  (define bracket 0)
  (cond
    [(empty? assembly) (void)]
    [(> (length assembly) 0) ; Ignore blank lines
        ; Ignore comment-only lines as well
        ; When a comment-only line is scanned, an empty struct is returned
        (define scanned (first assembly))
        (define num-label 0)
        (cond
          [(empty? scanned) (scan-input (rest assembly))]
          [else
           (for-each (lambda (x)
                       (cond
                         [(> bracket 0)
                        (cond
                          [(equal? bracket 1)
                           (cond
                             [(equal? (symbol->string (token-kind x)) "lparen")
                              (set! bracket 0)
                              (set! reg 1)
                              ])]
                          [(equal? bracket 2)
                           (cond
                             [(equal? (symbol->string (token-kind x)) "rparen")
                              (set! bracket 0)
                              (set! reg 0) ])]
                          [else
                           (fprintf (current-error-port) "ERROR:Invalid input~n")])]
                         ;Check for comma
                         [(equal? (symbol->string (token-kind x)) "comma")
                          (cond
                            [(equal? comma 1)
                             (set! comma 0)
                             (set! reg 1)
                             ]
                            )]
                         ;If there is a label
                         [(equal? (symbol->string (token-kind x)) "label")
                          (define len (length (token-lexeme x)))
                          (cond
                            ;Checks if there was an instruction before the label declaration
                            [(equal? instructioncarry #t)
                             (set! lblInst #t)]
                            ;Checks if the label is already in the hash table
                            ;[(hash-has-key? label (substring (list->string (token-lexeme x)) 0 (- len 1)))
                            ; (set! exsLabel #t)]
                            ;Adds 1 to the number of labels and adds the label to the hash table
                            [else
                             (set! num-label (+ 1 num-label))
                             ])]
                         ;If the instruction is .word
                         [(equal? (symbol->string (token-kind x)) "dotword")
                          ;Flag so that a label cannot follow a .word instruction
                          (set! instructioncarry #f)
                          (cond
                            ;Checks if there is only one argument after a .word instruction
                            [(equal? (- (length scanned) num-label) 2)
                             (set! integ 1)])]
                         ;Check for Jump instruction
                         [(equal? jr 1)
                          (cond
                            [(equal? (symbol->string (token-kind x)) "register")
                             ;(printf "jr~n")
                             (set! jr 0)
                             (set! pc_counter (+ pc_counter 4))
                             (define jump_register (bitwise-ior (arithmetic-shift (token-lexeme x) 21) 8))
                             (write-byte (bitwise-and (arithmetic-shift jump_register -24) 255)) 
                             (write-byte (bitwise-and (arithmetic-shift jump_register -16) 255))
                             (write-byte (bitwise-and (arithmetic-shift jump_register -8) 255))
                             (write-byte (bitwise-and jump_register 255))]
                            [else
                             (error 'ERROR "not a valid register")])]

                         [(equal? reg 1)
                           (cond
                             ;Check for mflo instruction
                            [(equal? mflo 1)
                             (set! reg 0)
                             (set! mflo 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! mflo_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 11) 18))
                                (write-byte (bitwise-and (arithmetic-shift mflo_reg -24) 255)) 
                                (write-byte (bitwise-and (arithmetic-shift mflo_reg -16) 255))
                                (write-byte (bitwise-and (arithmetic-shift mflo_reg -8) 255))
                                (write-byte (bitwise-and mflo_reg 255))
                                (set! pc_counter (+ pc_counter 4))
                                ]
                               )]
                            ;Check for mfhi instruction
                            [(equal? mfhi 1)
                             (set! reg 0)
                             (set! mfhi 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! mfhi_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 11) 16))
                                (write-byte (bitwise-and (arithmetic-shift mfhi_reg -24) 255)) 
                                (write-byte (bitwise-and (arithmetic-shift mfhi_reg -16) 255))
                                (write-byte (bitwise-and (arithmetic-shift mfhi_reg -8) 255))
                                (write-byte (bitwise-and mfhi_reg 255))
                                (set! pc_counter (+ pc_counter 4))])]
                            ;Check for lis instruction
                            [(equal? lis 1)
                             (set! reg 0)
                             (set! lis 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! lis_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 11) 20))
                                (write-byte (bitwise-and (arithmetic-shift lis_reg -24) 255)) 
                                (write-byte (bitwise-and (arithmetic-shift lis_reg -16) 255))
                                (write-byte (bitwise-and (arithmetic-shift lis_reg -8) 255))
                                (write-byte (bitwise-and lis_reg 255))
                                (set! pc_counter (+ pc_counter 4))])]
                            
                             ;Check for beq instruction
                             [(> beq 0)
                              (cond
                                [(equal? beq 1)
                                 (cond
                                   [(equal? (symbol->string (token-kind x)) "register")
                                    (set! reg 0)
                                    (set! comma 1)
                                    (set! beq (+ 1 beq))
                                    (set! beq_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 21) (arithmetic-shift 4 26)))
                                    ])]
                                [(equal? beq 2)
                                 (cond
                                   [(equal? (symbol->string (token-kind x)) "register")
                                    (set! reg 0)
                                    (set! comma 1)
                                    (set! beq (+ 1 beq))
                                    (set! beq_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 16) beq_reg))
                                    ])]
                                [(equal? beq 3)
                                 (cond
                                   [(equal? (symbol->string (token-kind x)) "hexint")
                                    (cond
                                      [(and (<= (token-lexeme x) 65535) (>= (token-lexeme x) 0))
                                       (set! reg 0)
                                       (set! comma 0)
                                       (set! beq 0)
                                       (set! pc_counter (+ pc_counter 4))
                                       (set! beq_reg (bitwise-ior  (token-lexeme x) beq_reg))
                                       (write-byte (bitwise-and (arithmetic-shift beq_reg -24) 255)) 
                                       (write-byte (bitwise-and (arithmetic-shift beq_reg -16) 255))
                                       (write-byte (bitwise-and (arithmetic-shift beq_reg -8) 255))
                                       (write-byte (bitwise-and beq_reg 255))])
                                    ]
                                   [(equal? (symbol->string (token-kind x)) "int")
                                    (cond
                                      [(and (<= (token-lexeme x) 32767) (>= (token-lexeme x) -32768))
                                       (cond
                                         [(> 0 (token-lexeme x))
                                          (set! beq_reg (bitwise-ior (bitwise-and (token-lexeme x) 65535) beq_reg))
                                          ]
                                         [else
                                          (set! beq_reg (bitwise-ior (token-lexeme x) beq_reg))])
                                       (set! reg 0)
                                       (set! comma 0)
                                       (set! beq 0)
                                       (set! pc_counter (+ pc_counter 4))
                                       (write-byte (bitwise-and (arithmetic-shift beq_reg -24) 255)) 
                                       (write-byte (bitwise-and (arithmetic-shift beq_reg -16) 255))
                                       (write-byte (bitwise-and (arithmetic-shift beq_reg -8) 255))
                                       (write-byte (bitwise-and beq_reg 255))
                                       ]
                                      )]
                                     
                                   [(equal? (symbol->string (token-kind x)) "id")
                                    (cond
                                      [(hash-has-key? label (list->string (token-lexeme x)))
                                       (set! reg 0)
                                       (set! comma 0)
                                       (set! beq 0)
                                       (define label_val (hash-ref label (list->string (token-lexeme x))))
                                       (define offset (- label_val pc_counter))
                                       (define temp-num (/ (- offset 4) 4))
                                      
                                       
                                       (cond
                                         [(and (>= temp-num -32768) (>= 32767 temp-num))
                                          ;(printf "temp-num:~a~n" temp-num)
                                          (cond
                                            [(< temp-num 0)
                                             (set! beq_reg (bitwise-ior (bitwise-and temp-num 65535) beq_reg))]
                                            [else
                                             (set! beq_reg (bitwise-ior temp-num  beq_reg))]
                                            )
                                          (set! pc_counter (+ pc_counter 4))
                                          (write-byte (bitwise-and (arithmetic-shift beq_reg -24) 255)) 
                                          (write-byte (bitwise-and (arithmetic-shift beq_reg -16) 255))
                                          (write-byte (bitwise-and (arithmetic-shift beq_reg -8) 255))
                                          (write-byte (bitwise-and beq_reg 255))
                                          ]
                                         [else
                                          (printf(current-error-port) "ERROR:value not in range")])]
                                       
                                      [else
                                       (fprintf (current-error-port) "ERROR:label doesn't exist~n")])]
                                   )]
                                   )]
                                
                                
                             
                             ;Check for bne instruction
                              [(> bne 0)
                              (cond
                                [(equal? bne 1)
                                 (cond
                                   [(equal? (symbol->string (token-kind x)) "register")
                                    (set! reg 0)
                                    (set! comma 1)
                                    (set! bne (+ 1 bne))
                                    (set! bne_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 21) (arithmetic-shift 5 26)))
                                    ])]
                                [(equal? bne 2)
                                 (cond
                                   [(equal? (symbol->string (token-kind x)) "register")
                                    (set! reg 0)
                                    (set! comma 1)
                                    (set! bne (+ 1 bne))
                                    (set! bne_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 16) bne_reg))
                                    ])]
                                [(equal? bne 3)
                                 (cond
                                   [(equal? (symbol->string (token-kind x)) "hexint")
                                    (cond
                                      [(and (<= (token-lexeme x) 65535) (>= (token-lexeme x) 0))
                                       (set! reg 0)
                                       (set! comma 0)
                                       (set! bne 0)
                                       (set! pc_counter (+ pc_counter 4))
                                       (set! bne_reg (bitwise-ior (token-lexeme x) bne_reg))
                                       (write-byte (bitwise-and (arithmetic-shift bne_reg -24) 255)) 
                                       (write-byte (bitwise-and (arithmetic-shift bne_reg -16) 255))
                                       (write-byte (bitwise-and (arithmetic-shift bne_reg -8) 255))
                                       (write-byte (bitwise-and bne_reg 255))
                                       ]
                                      )]
                                   [(equal? (symbol->string (token-kind x)) "int")
                                    (cond
                                      [(and (<= (token-lexeme x) 32767) (>= (token-lexeme x) -32768))
                                       (cond
                                         [(< (token-lexeme x) 0)
                                          (set! bne_reg (bitwise-ior (bitwise-and (token-lexeme x) 65535) bne_reg))
                                          ]
                                         [else
                                          (set! bne_reg (bitwise-ior (token-lexeme x) bne_reg))])
                                       (set! reg 0)
                                       (set! comma 0)
                                       (set! bne 0)
                                       (set! pc_counter (+ pc_counter 4))
                                       (write-byte (bitwise-and (arithmetic-shift bne_reg -24) 255)) 
                                       (write-byte (bitwise-and (arithmetic-shift bne_reg -16) 255))
                                       (write-byte (bitwise-and (arithmetic-shift bne_reg -8) 255))
                                       (write-byte (bitwise-and bne_reg 255))
                                    ])]
                                   [(equal? (symbol->string (token-kind x)) "id")
                                    (cond
                                      [(hash-has-key? label (list->string (token-lexeme x)))
                                       (set! reg 0)
                                       (set! comma 0)
                                       (set! bne 0)
                                       
                                       (define label_val (hash-ref label (list->string (token-lexeme x))))
                                       (define offset (- label_val pc_counter))
                                       (define temp-num (/ (- offset 4) 4))
                                       (cond
                                         [(and (>= temp-num -32768) (>= 32767 temp-num))
                                          
                                          (cond
                                            [(< temp-num 0)
                                             (set! bne_reg (bitwise-ior (bitwise-and temp-num 65535) bne_reg))]
                                            [else
                                             (set! bne_reg (bitwise-ior temp-num  bne_reg))]
                                            )
                                          (set! pc_counter (+ pc_counter 4))
                                          (write-byte (bitwise-and (arithmetic-shift bne_reg -24) 255)) 
                                          (write-byte (bitwise-and (arithmetic-shift bne_reg -16) 255))
                                          (write-byte (bitwise-and (arithmetic-shift bne_reg -8) 255))
                                          (write-byte (bitwise-and bne_reg 255))
                                          ]
                                         [else
                                          (fprintf (current-error-port) "ERROR:value not in range")]
                                         )
                                       ]
                                      [else
                                       (fprintf (current-error-port) "ERROR:label doesn't exist~n")])]
                                   )]
                                   )]



                              ;Check for mult instruction
                            [(> mult 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! reg 0)
                                (set! comma 1)
                                (cond
                                  [(equal? mult 1)
                                   (set! mult (+ mult 1))
                                   (set! mult_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 21) 24))
                                   ]
                                  [(equal? mult 2)
                                   (set! mult 0)
                                   (set! comma 0)
                                   (set! mult_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 16) mult_reg))
                                   (write-byte (bitwise-and (arithmetic-shift mult_reg -24) 255)) 
                                   (write-byte (bitwise-and (arithmetic-shift mult_reg -16) 255))
                                   (write-byte (bitwise-and (arithmetic-shift mult_reg -8) 255))
                                   (write-byte (bitwise-and mult_reg 255))
                                   (set! pc_counter (+ pc_counter 4))])]
                                   
                               [else
                                (error 'ERROR "not a valid register")])]

                            ;Check for multu instruction
                            [(> multu 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! reg 0)
                                (set! comma 1)
                                (cond
                                  [(equal? multu 1)
                                   (set! multu (+ multu 1))
                                   (set! multu_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 21) 25))
                                   ]
                                  [(equal? multu 2)
                                   (set! multu 0)
                                   (set! comma 0)
                                   (set! multu_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 16) multu_reg))
                                   (write-byte (bitwise-and (arithmetic-shift multu_reg -24) 255)) 
                                   (write-byte (bitwise-and (arithmetic-shift multu_reg -16) 255))
                                   (write-byte (bitwise-and (arithmetic-shift multu_reg -8) 255))
                                   (write-byte (bitwise-and multu_reg 255))
                                   (set! pc_counter (+ pc_counter 4))])]
                                   
                               [else
                                (error 'ERROR "not a valid register")])]

                            ;Check for div instruction
                            [(> div 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! reg 0)
                                (set! comma 1)
                                (cond
                                  [(equal? div 1)
                                   (set! div (+ div 1))
                                   (set! div_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 21) 26))
                                   ]
                                  [(equal? div 2)
                                   (set! div 0)
                                   (set! comma 0)
                                   (set! div_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 16) div_reg))
                                   (write-byte (bitwise-and (arithmetic-shift div_reg -24) 255)) 
                                   (write-byte (bitwise-and (arithmetic-shift div_reg -16) 255))
                                   (write-byte (bitwise-and (arithmetic-shift div_reg -8) 255))
                                   (write-byte (bitwise-and div_reg 255))
                                   (set! pc_counter (+ pc_counter 4))])] 
                               [else
                                (error 'ERROR "not a valid register")])]
                            ;Check for divu instruction
                            [(> divu 0)
                             (cond
                               [(equal? (symbol->string (token-kind x)) "register")
                                (set! reg 0)
                                (set! comma 1)
                                (cond
                                  [(equal? divu 1)
                                   (set! divu (+ divu 1))
                                   (set! divu_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 21) 27))
                                   ]
                                  [(equal? divu 2)
                                   (set! divu 0)
                                   (set! comma 0)
                                   (set! divu_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 16) divu_reg))
                                   (write-byte (bitwise-and (arithmetic-shift divu_reg -24) 255)) 
                                   (write-byte (bitwise-and (arithmetic-shift divu_reg -16) 255))
                                   (write-byte (bitwise-and (arithmetic-shift divu_reg -8) 255))
                                   (write-byte (bitwise-and divu_reg 255))
                                   (set! pc_counter (+ pc_counter 4))])]
                               [else
                                (error 'ERROR "not a valid register")])]


                            ;lw instruction
                            [(> lw 0)
                             (cond
                                  [(equal? lw 1)
                                   (set! reg 0)
                                   (cond
                                     [(equal? (symbol->string (token-kind x)) "register")
                                      (set! lw (+ lw 1))
                                      (set! comma 1)
                                      (set! lw_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 16) (arithmetic-shift 35 26)))])]
                                   
                                  [(equal? lw 2)
                                   (set! reg 0)
                                   (cond
                                     [(equal? (symbol->string (token-kind x)) "int")
                                      (cond
                                        [(and (<= -32768 (token-lexeme x)) (>= 32767 (token-lexeme x)))
                                         (set! lw (+ lw 1))
                                         (set! bracket 1)
                                         (cond
                                           [(< (token-lexeme x) 0)
                                            (set! lw_reg (bitwise-ior (bitwise-and (token-lexeme x) 65535) lw_reg))]
                                           [else
                                            (set! lw_reg (bitwise-ior (token-lexeme x) lw_reg))])])]
                                        
                                        
                                      [(equal? (symbol->string (token-kind x)) "hexint")
                                       (cond
                                         [(and (<= 0 (token-lexeme x)) (>= 65535 (token-lexeme x)))
                                          (set! lw(+ lw 1))
                                          (set! bracket 1)
                                          (set! lw_reg (bitwise-ior (token-lexeme x) lw_reg))])])]
                                  
                                  [(equal? lw 3)
                                   (set! reg 0)
                                   (cond
                                     [(equal? (symbol->string (token-kind x)) "register")
                                      (set! lw 0)
                                      (set! bracket 2)
                                      (set! lw_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 21) lw_reg))
                                      (write-byte (bitwise-and (arithmetic-shift lw_reg -24) 255)) 
                                      (write-byte (bitwise-and (arithmetic-shift lw_reg -16) 255))
                                      (write-byte (bitwise-and (arithmetic-shift lw_reg -8) 255))
                                      (write-byte (bitwise-and lw_reg 255))
                                      (set! pc_counter (+ pc_counter 4))
                                      ])])]
                            
                            ;sw instruction
                            [(> sw 0)
                             (cond
                                  [(equal? sw 1)
                                   (set! reg 0)
                                   (cond
                                     [(equal? (symbol->string (token-kind x)) "register")
                                      (set! sw (+ sw 1))
                                      (set! comma 1)
                                      (set! sw_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 16) (arithmetic-shift 43 26)))])]
                                   
                                  [(equal? sw 2)
                                   (set! reg 0)
                                   (cond
                                     [(equal? (symbol->string (token-kind x)) "int")
                                      (cond
                                        [(and (<= -32768 (token-lexeme x)) (>= 32767 (token-lexeme x)))
                                         (set! sw (+ sw 1))
                                         (set! bracket 1)
                                         (cond
                                           [(< (token-lexeme x) 0)
                                            (set! sw_reg (bitwise-ior (bitwise-and (token-lexeme x) 65535) sw_reg))]
                                           [else
                                            (set! sw_reg (bitwise-ior (token-lexeme x) sw_reg))])])]
                                        
                                        
                                      [(equal? (symbol->string (token-kind x)) "hexint")
                                       (cond
                                         [(and (<= 0 (token-lexeme x)) (>= 65535 (token-lexeme x)))
                                          (set! sw(+ sw 1))
                                          (set! bracket 1)
                                          (set! sw_reg (bitwise-ior (token-lexeme x) sw_reg))])])]
                                  
                                  [(equal? sw 3)
                                   (set! reg 0)
                                   (cond
                                     [(equal? (symbol->string (token-kind x)) "register")
                                      (set! sw 0)
                                      (set! bracket 2)
                                      (set! sw_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 21) sw_reg))
                                      (write-byte (bitwise-and (arithmetic-shift sw_reg -24) 255)) 
                                      (write-byte (bitwise-and (arithmetic-shift sw_reg -16) 255))
                                      (write-byte (bitwise-and (arithmetic-shift sw_reg -8) 255))
                                      (write-byte (bitwise-and sw_reg 255))
                                      (set! pc_counter (+ pc_counter 4))
                                      ])])]
                            
                             ;Check for Add insruction
                             [(> add 0)
                              (cond
                                ;bitwise and shift accordingly depending on which register it is
                                [(equal? (symbol->string (token-kind x)) "register")
                                 (set! reg 0)
                                 (set! comma 1)
                                 (cond
                                   [(equal? add 1)
                                    (set! add (+ add 1))
                                    (set! add_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 11) 32))
                                    ]
                                   [(equal? add 2)
                                    (set! add (+ add 1))
                                    (set! add_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 21) add_reg))
                                    ]
                                   [(equal? add 3)
                                    (set! add 0)
                                    (set! comma 0)
                                    (set! add_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 16) add_reg))
                                    (write-byte (bitwise-and (arithmetic-shift add_reg -24) 255)) 
                                    (write-byte (bitwise-and (arithmetic-shift add_reg -16) 255))
                                    (write-byte (bitwise-and (arithmetic-shift add_reg -8) 255))
                                    (write-byte (bitwise-and add_reg 255))])]
                                [else
                                 (error 'ERROR "not a valid register")])]
                             ;Check for Sub instruction
                             [(> sub 0)
                              (cond
                                ;bitwise and shift accordingly depending on which register it is
                                [(equal? (symbol->string (token-kind x)) "register")
                                 (set! reg 0)
                                 (set! comma 1)
                                 (cond
                                   [(equal? sub 1)
                                    (set! sub (+ sub 1))
                                    (set! sub_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 11) 34))
                                    ]
                                   [(equal? sub 2)
                                    (set! sub (+ sub 1))
                                    (set! sub_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 21) sub_reg))
                                    ]
                                   [(equal? sub 3)
                                    (set! sub 0)
                                    (set! comma 0)
                                    (set! sub_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 16) sub_reg))
                                    (write-byte (bitwise-and (arithmetic-shift sub_reg -24) 255)) 
                                    (write-byte (bitwise-and (arithmetic-shift sub_reg -16) 255))
                                    (write-byte (bitwise-and (arithmetic-shift sub_reg -8) 255))
                                    (write-byte (bitwise-and sub_reg 255))])]
                                [else
                                 (error 'ERROR "not a valid register")])]
                             ;Check for Slt instruction
                             [(> slt 0)
                              (cond
                                ;bitwise and shift accordingly depending on which register it is
                                [(equal? (symbol->string (token-kind x)) "register")
                                 (set! reg 0)
                                 (set! comma 1)
                                 (cond
                                   [(equal? slt 1)
                                    (set! slt (+ slt 1))
                                    (set! slt_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 11) 42))
                                    ]
                                   [(equal? slt 2)
                                    (set! slt (+ slt 1))
                                    (set! slt_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 21) slt_reg))
                                    ]
                                   [(equal? slt 3)
                                    (set! slt 0)
                                    (set! comma 0)
                                    (set! slt_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 16) slt_reg))
                                    (write-byte (bitwise-and (arithmetic-shift slt_reg -24) 255)) 
                                    (write-byte (bitwise-and (arithmetic-shift slt_reg -16) 255))
                                    (write-byte (bitwise-and (arithmetic-shift slt_reg -8) 255))
                                    (write-byte (bitwise-and slt_reg 255))])]
                                [else
                                 (error 'ERROR "not a valid register")])]
                             ;Check for Sltu instruction
                             [(> sltu 0)
                              (cond
                                ;bitwise and shift accordingly depending on which register it is
                                [(equal? (symbol->string (token-kind x)) "register")
                                 (set! reg 0)
                                 (set! comma 1)
                                 (cond
                                   [(equal? sltu 1)
                                    (set! sltu (+ sltu 1))
                                    (set! sltu_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 11) 43))
                                    ]
                                   [(equal? sltu 2)
                                    (set! sltu (+ sltu 1))
                                    (set! sltu_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 21) sltu_reg))
                                    ]
                                   [(equal? sltu 3)
                                    (set! sltu 0)
                                    (set! comma 0)
                                    (set! sltu_reg (bitwise-ior (arithmetic-shift (token-lexeme x) 16) sltu_reg))
                                    (write-byte (bitwise-and (arithmetic-shift sltu_reg -24) 255)) 
                                    (write-byte (bitwise-and (arithmetic-shift sltu_reg -16) 255))
                                    (write-byte (bitwise-and (arithmetic-shift sltu_reg -8) 255))
                                    (write-byte (bitwise-and sltu_reg 255))])]
                                [else
                                 (error 'ERROR "not a valid register")])]
                             )]
                         ;Check for jalr instruction
                         [(equal? jalr 1)
                          (cond
                            [(equal? (symbol->string (token-kind x)) "register")
                            ; (printf "jalr~n")
                             (set! jalr 0)
                             (define jalr_register (bitwise-ior (arithmetic-shift (token-lexeme x) 21) 9))
                             (write-byte (bitwise-and (arithmetic-shift jalr_register -24) 255)) 
                             (write-byte (bitwise-and (arithmetic-shift jalr_register -16) 255))
                             (write-byte (bitwise-and (arithmetic-shift jalr_register -8) 255))
                             (write-byte (bitwise-and jalr_register 255))]
                            [else
                             (error 'ERROR "not a valid register")])]
                          ;Now the next token has to be of type int, hexint, or id
                         [(equal? integ 1)
                          (set! integ 0)
                          (cond
                            ;write-byte and adds to address counter
                            [(equal? (symbol->string (token-kind x)) "hexint")
                             (set! instructioncarry #t)
                             (write-byte (bitwise-and (arithmetic-shift (token-lexeme x) -24) 255))
                             (write-byte (bitwise-and (arithmetic-shift (token-lexeme x) -16) 255))
                             (write-byte (bitwise-and (arithmetic-shift (token-lexeme x) -8) 255))
                             (write-byte (bitwise-and (token-lexeme x) 255))]
                             
                            [(equal? (symbol->string (token-kind x)) "int")
                             (set! instructioncarry #t)
                             (write-byte (bitwise-and (arithmetic-shift (token-lexeme x) -24) 255))
                             (write-byte (bitwise-and (arithmetic-shift (token-lexeme x) -16) 255))
                             (write-byte (bitwise-and (arithmetic-shift (token-lexeme x) -8) 255))
                             (write-byte (bitwise-and (token-lexeme x) 255))]
                             
                            [(equal? (symbol->string (token-kind x)) "id")
                             (cond
                               ;checks if the label is in the hash table
                               [(hash-has-key? label (list->string (token-lexeme x))) 
                                (set! instructioncarry #t)
                                (define temp-num (hash-ref label (list->string (token-lexeme x))))
                                (write-byte (bitwise-and (arithmetic-shift temp-num -24) 255))
                                (write-byte (bitwise-and (arithmetic-shift temp-num -16) 255))
                                (write-byte (bitwise-and (arithmetic-shift temp-num -8) 255))
                                (write-byte (bitwise-and temp-num 255))]
                               [else
                                (fprintf (current-error-port) "ERROR:Label doesn't exist~n")])]
                                
                            [else
                             (set! invInput #t)])]
                         ;Check if it's an instruction
                       [(equal? (symbol->string (token-kind x)) "id")
                        (cond
                          ;Jump Instruction
                          [(equal? (list->string (token-lexeme x)) "jr")
                           (cond
                             [(equal? (- (length scanned) num-label) 2)
                              (set! jr 1)]
                             )]
                          ;Jalr instruction
                          [(equal? (list->string (token-lexeme x)) "jalr")
                           (cond
                             [(equal? (- (length scanned) num-label) 2)
                              (set! jalr 1)]
                             )]
                          ;Add instruction
                        [(equal? (list->string (token-lexeme x)) "add")
                         (cond
                           [ (equal? (- (length scanned) num-label) 6)
                            (set! reg 1)
                            (set! add 1)])]
                        ;Sub instruction
                        [(equal? (list->string (token-lexeme x)) "sub")
                         (cond
                           [(equal? (- (length scanned) num-label) 6)
                            (set! reg 1)
                            (set! sub 1)])]
                        ;Slt instruction
                        [(equal? (list->string (token-lexeme x)) "slt")
                         (cond
                           [(equal? (- (length scanned) num-label) 6)
                            (set! reg 1)
                            (set! slt 1)])]
                        ;Sltu instruction
                        [(equal? (list->string (token-lexeme x)) "sltu")
                         (cond
                           [(equal? (- (length scanned) num-label) 6)
                            (set! reg 1)
                            (set! sltu 1)])]
                        ;beq instruction
                        [(equal? (list->string (token-lexeme x)) "beq")
                           (cond
                             [(equal? (- (length scanned) num-label) 6)
                              (set! reg 1)
                              (set! beq 1)]
                            )]
                        ;lw instruction
                          [(equal? (list->string (token-lexeme x)) "lw")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 7)
                              (set! reg 1)
                              (set! lw 1)
                              ])]
                          ;sw instruction
                          [(equal? (list->string (token-lexeme x)) "sw")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 7)
                              (set! reg 1)
                              (set! sw 1)])]
                         ;Mult instruction
                          [(equal? (list->string (token-lexeme x)) "mult")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 4)
                              (set! reg 1)
                              (set! mult 1)])]
                          ;Multu instruction
                          [(equal? (list->string (token-lexeme x)) "multu")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 4)
                              (set! reg 1)
                              (set! multu 1)])]
                          ;Div instruction
                          [(equal? (list->string (token-lexeme x)) "div")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 4)
                              (set! reg 1)
                              (set! div 1)])]
                          ;Divu instruction
                          [(equal? (list->string (token-lexeme x)) "divu")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 4)
                              (set! reg 1)
                              (set! divu 1)])]
                        ;Bne instruction
                          [(equal? (list->string (token-lexeme x)) "bne")
                           (cond
                             [(equal? (- (length scanned) num-label) 6)
                              (set! reg 1)
                              (set! bne 1)]
                             )]
                          ;mflo instruction
                          [(equal? (list->string (token-lexeme x)) "mflo")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 2)
                              (set! reg 1)
                              (set! mflo 1)])]
                          ;mfhi instruction
                          [(equal? (list->string (token-lexeme x)) "mfhi")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 2)
                              (set! reg 1)
                              (set! mfhi 1)])]
                          ;lis instruction
                          [(equal? (list->string (token-lexeme x)) "lis")
                           (set! instructioncarry #t)
                           (cond
                             [(equal? (- (length scanned) num-label) 2)
                              (set! reg 1)
                              (set! lis 1)]
                          )]
                          )]
                         [else
                          (set! invInput #t)])) scanned)
          ; (print-error-messages)
           (scan-input (rest assembly))])]
    
    [else (scan-input (rest assembly))]))


(make-label)
;(printf "Assembly Code: ~a~n" assembly-code)
;(printf "Rest: ~a~n" (first (rest assembly-code)))
(scan-input assembly-code) 
;(print-hash label)
