#lang racket

(require racklog)

(struct
  parse-context
  (result text position))


(define (string-context str)
        (parse-context '() str 0))

(define (with-result value ctxt)
  (struct-copy parse-context ctxt [result value]))


(define (get-char-from-context ctxt)
  (struct-copy
     parse-context
     ctxt
     [result (string-ref (parse-context-text ctxt) (parse-context-position ctxt))]
     [position (+ 1 (parse-context-position ctxt))]))

(define (has-more-chars? ctx)
   (<
      (parse-context-position ctx)
      (string-length (parse-context-text ctx))))

(define %a-char
  (%rel (a-char ctxt new-ctxt)
        [(a-char ctxt new-ctxt)
            (%is #t (has-more-chars? ctxt))
            (%is new-ctxt (get-char-from-context ctxt))
            (%is a-char (parse-context-result new-ctxt))
            ]))

(define %a-char-pred
  (λ (pred)
    (%rel (ctxt new-ctxt)
          [( ctxt new-ctxt)
           (%let (a-char)
                 (%and 
                  (%is #t (has-more-chars? ctxt))
                  (%is new-ctxt (get-char-from-context ctxt))
                  (%is a-char (parse-context-result new-ctxt))
                  (%is #t (pred a-char))
                  
                  ))])))


(define (list-element e)
  (if (list? e) e (list e)))

(define %p-simple-sequence
  (λ (pred)
    (%rel (ctxt new-ctxt)
          [(ctxt new-ctxt)
           (%let (tmp-ctxt tmp-result)
                 (%or
                  (%and
                   (pred ctxt tmp-ctxt)
                   (%is tmp-result (with-result                                     
                                     (cons (parse-context-result tmp-ctxt)
                                           (parse-context-result ctxt))
                                     tmp-ctxt))
                   ((%p-simple-sequence pred) tmp-result new-ctxt))
                  (%is new-ctxt ctxt)))])))

(define %p-sequence
  (λ (pred)
    (%rel ( ctxt new-ctxt)
          [(ctxt new-ctxt)
           (%let (tmp-ctxt)
               (%and
                 (%is tmp-ctxt (with-result '() ctxt))
                 ((%p-simple-sequence pred)
                  tmp-ctxt new-ctxt)
                 ))]
          )))


(define %whitespaces
   (%p-simple-sequence
     (λ (c1 c2)
       ((%a-char-pred char-whitespace?) c1 c2))))
    
(define %identifier
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (ct1 ct2)
               (%and
                ((%a-char-pred char-alphabetic?) ctxt ct1)
                ((%p-sequence
                  (λ (c1 c2)
                    ((%a-char-pred char-alphabetic?) c1 c2)))
                 ct1 ct2)
                (%is new-ctxt
                     (struct-copy
                      parse-context
                      ct2
                      [result
                         (list->string
                          (cons
                               (parse-context-result ct1)
                               (reverse (parse-context-result ct2))))]))
                ))            
         ]))

(define (char-sequence-with-predicates initial-char-pred content-char-pred)
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (ct1 ct2)
               (%and
                ((%a-char-pred initial-char-pred) ctxt ct1)
                ((%p-sequence
                  (λ (c1 c2)
                    ((%a-char-pred content-char-pred) c1 c2)))
                 ct1 ct2)
                (%is new-ctxt
                     (struct-copy
                      parse-context
                      ct2
                      [result
                         (list->string
                          (cons
                               (parse-context-result ct1)
                               (reverse (parse-context-result ct2))))]))
                ))            
            ]))


(define %a-number
  (char-sequence-with-predicates char-numeric? char-numeric?))

(define %an-identifier
  (char-sequence-with-predicates
   char-alphabetic?
   (λ (x)
     (or (char-numeric? x)
         (char-alphabetic? x)))))


(define %simple-expression
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (tmp-ctxt1 tmp-ctxt2)
               (%and
                (%whitespaces ctxt tmp-ctxt1)
                (%or
                 (%and
                  (%a-number tmp-ctxt1 tmp-ctxt2)
                  (%is new-ctxt
                       (with-result
                         `(-numeric-literal ,(parse-context-result tmp-ctxt2))
                         tmp-ctxt2)))
                 (%and
                  (%an-identifier tmp-ctxt1 tmp-ctxt2)
                  (%is new-ctxt
                       (with-result
                         `(-id ,(parse-context-result tmp-ctxt2))
                         tmp-ctxt2)))))
          )]))

(define (char->ast-symbol operator-char)
  (string->symbol (string #\- operator-char)))

(define (%binary-expression operator-predicate operands-parser)
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (left-ctxt1 tmp-ctxt2 op-sequence-ctxt)
               (%and
                 (operands-parser ctxt left-ctxt1)
                 ((%p-sequence
                   (%rel (c1 c2)
                         [(c1 c2)
                          (%let (tmp-ctxt3 tmp-ctxt4 op2-ctxt)
                                (%and
                                 (%whitespaces c1 op2-ctxt)
                                 ((%a-char-pred operator-predicate) op2-ctxt tmp-ctxt3)
                                 (operands-parser tmp-ctxt3 tmp-ctxt4)
                                 (%is c2  (with-result (list (parse-context-result tmp-ctxt3)
                                                              (parse-context-result tmp-ctxt4))
                                                       tmp-ctxt4 )) )) ])) 
                  left-ctxt1 op-sequence-ctxt)
                 
                 (%is new-ctxt
                      (with-result
                        (foldl
                         (λ ( acc current) `(,(char->ast-symbol (car acc)) ,current ,(cadr acc)))
                         (parse-context-result left-ctxt1)
                         (reverse (parse-context-result op-sequence-ctxt)))
                        op-sequence-ctxt))
                               
                 ))]))

(define %multiplicative-expression
  (%binary-expression (λ (op) (or (eq? op #\*) (eq? op #\/))) %simple-expression))
(define %additive-expression
  (%binary-expression (λ (op) (or (eq? op #\+) (eq? op #\-))) %multiplicative-expression))

(define %expression %additive-expression)

(define %a-sep-list1
  (%rel (c1 res)
        [(c1 res)
         (%let (c2 c3 c4)
               (%and
                (%expression c1 c2)
                (%whitespaces c2 c3)
                (%a-char #\, c3 c4)
                (%is res (struct-copy parse-context c4 [result  (parse-context-result c2)]))))
         ]))

(define %a-sep-list
  (%rel (c1 res)
      [(c1 res)  (%let
         (c2 c3)
         (%and 
          ((%p-sequence %a-sep-list1) c1 c2)
          (%expression c2 c3)
          (%is res (struct-copy parse-context c3 [result (cons (parse-context-result c3) (parse-context-result c2))]))))]))

(define %opt
  (λ (parser)
    (%rel (ctxt new-ctxt)
          [(ctxt new-ctxt)
           (%let (tmp-ctxt)
                 (%and
                  (%or (parser ctxt new-ctxt)
                       (%is new-ctxt (with-result '() ctxt)))))])))

(define (%literal-id id)
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (ws-ctxt)
               (%and
                (%whitespaces ctxt ws-ctxt)
                (%identifier ws-ctxt new-ctxt)
                (%is id (parse-context-result new-ctxt))))]))


(define %call
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (ws-ctxt1 id-ctxt
                lpar-ctxt exprs-ctxt rpar-ctxt)
                (%and
                 (%whitespaces ctxt ws-ctxt1)
                 (%identifier ws-ctxt1 id-ctxt)
                 ((%punctuator #\() id-ctxt lpar-ctxt) 
                 ((%opt %a-sep-list) lpar-ctxt exprs-ctxt)
                 ((%punctuator #\)) exprs-ctxt rpar-ctxt)
                 (%is new-ctxt
                      (with-result 
                        `(-call ,(parse-context-result id-ctxt)
                                ,(reverse (parse-context-result exprs-ctxt)))
                        rpar-ctxt))
                 ))]))

(define %stat
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%or (%call-stat ctxt new-ctxt)
              (%if-stat ctxt new-ctxt))]))
         

(define %call-stat
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (call-ctxt semicolon-ctxt)
               (%and
                (%call ctxt call-ctxt)
                (%a-char #\; call-ctxt semicolon-ctxt)
                (%is new-ctxt
                     (with-result
                       `(-call-stat ,(parse-context-result call-ctxt))
                        semicolon-ctxt))))]))


(define %else-block
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (ws1 else-kw-ctxt ws2 stats-ctxt)
               (%and
                ((%literal-id "else") ctxt else-kw-ctxt)
                ;   ((%p-sequence %stat) ws2 stats-ctxt)
                (%stat else-kw-ctxt stats-ctxt)
                (%is new-ctxt
                     (with-result `(-else ,(parse-context-result stats-ctxt))
                       stats-ctxt))
                )
               )]))

(define (%punctuator char)
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (ws-ctxt)
               (%and
                (%whitespaces ctxt ws-ctxt)
                (%a-char char ws-ctxt new-ctxt)))]))

(define %lpar (%punctuator #\())
(define %rpar (%punctuator #\)))


(define %if-stat
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (ifkw-ctxt lpar-ctxt rpar-ctxt
                cond-ctxt then-ctxt
                stats-ctxt else-ctxt end-ctxt)
               (%and
                ((%literal-id "if")   ctxt ifkw-ctxt) 
                (%lpar                ifkw-ctxt lpar-ctxt)
                (%expression          lpar-ctxt cond-ctxt)
                (%rpar                cond-ctxt rpar-ctxt)
                ((%literal-id "then") rpar-ctxt then-ctxt)
                (%stat                then-ctxt stats-ctxt)
                ((%opt %else-block)   stats-ctxt else-ctxt)
                (%is new-ctxt (with-result
                                `(-if ,(parse-context-result cond-ctxt)
                                      ,(parse-context-result stats-ctxt)
                                      ,(parse-context-result else-ctxt))
                                else-ctxt))))]))


(define (quick-test parser str)
  (parse-context-result (cdar (%which (c) (parser (string-context str) c)))))

; (parse-context-result (cdar (%which (c) (%a-plus (string-context "abbcd sds") c))))
; (parse-context-result (cdar (%which (c) (%identifier (string-context "abbcd ") c))))

(define (list->graphviz e)
  (string-append*
   (flatten
    (list
     "digraph G {\n"
     (converting-list->graphviz e)
     "\n}\n"))))

(define (converting-list->graphviz e)          
  (if (and (list? e) (not (null? e)) (symbol? (car e)))
      (let ((s (gensym))
            (children  (filter pair? (map converting-list->graphviz (cdr e)))))
        (match e
          [(list '-id the-id)
           (list* (string-append
                   (symbol->string s)
                   " [label=\"id: " the-id "\"];\n")
                  (symbol->string s))]
          [_ (list*
              (string-append
               (symbol->string s)
               " [label=\"" (symbol->string (car e)) "\"];\n"
               (string-append* (map car children))
               (string-append* (map (λ (e) (string-append (symbol->string s )  " -> " (cdr e) ";\n")) children)))
              (symbol->string s))]
        ))
      '()))

(define (save-as-dot file-name tree)
  (call-with-output-file
    file-name
    (λ (f) (display (list->graphviz tree) f)) ))
      


(parse-context-result
 (cdar
   (%which (result-ctxt)
           (%if-stat (string-context "if (c) then
                                        foo(c);
                                      else
                                         moo(d);")
                 result-ctxt))))  

(define sample-code
  "if (x) then if (y) then foo(); else goo();")

(parse-context-result
 (cdar
  (%which (result-ctxt)
          (%if-stat (string-context sample-code)
                    result-ctxt))))
