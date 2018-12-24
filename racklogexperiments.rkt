#lang racket

(require racklog)

(struct
  parse-context
  (result text position))

(define (string-context str)
        (parse-context '() str 0))

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

(define %two-outputs
  (%rel (x y z)
        [(x y z)
             (%is y (+ x 1))
             (%is z  (+ x 2))]))

(define %a-char
  (%rel (a-char ctxt new-ctxt)
        [(a-char ctxt new-ctxt)
            (%is #t (has-more-chars? ctxt))
            (%is new-ctxt (get-char-from-context ctxt))
            (%is a-char (parse-context-result new-ctxt))
            ]))

(define %a-char-pred
  (lambda (pred)
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

(define %p-char-sequence
  (lambda (pred)
    (%rel ( ctxt new-ctxt)
          [( ctxt new-ctxt)
           (%let (tmp-ctxt tmp-result)
                 (%or (%and (pred ctxt tmp-ctxt)
                            (%is tmp-result (struct-copy
                                             parse-context
                                             tmp-ctxt
                                             [result (cons (parse-context-result tmp-ctxt) (parse-context-result ctxt))]))
                            ((%p-char-sequence pred) tmp-result new-ctxt))
                      (%is new-ctxt ctxt)))])))

(define %p-chars-sequence
  (lambda (pred)
    (%rel ( ctxt new-ctxt)
          [(ctxt new-ctxt)
           (%let (tmp-ctxt)
               (%and
                 (%is tmp-ctxt (struct-copy
                                  parse-context
                                  ctxt
                                  [result '()]))
                 ((%p-char-sequence pred)
                  tmp-ctxt new-ctxt)
                 
                 ))]
          )))


(define %whitespaces
   (%p-char-sequence
     (lambda (c1 c2)
       ((%a-char-pred char-whitespace?) c1 c2))))
    
(define %identifier
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (ct1 ct2)
               (%and
                ((%a-char-pred char-alphabetic?) ctxt ct1)
                ((%p-chars-sequence
                  (lambda (c1 c2)
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
                ((%p-chars-sequence
                  (lambda (c1 c2)
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
   (lambda (x)
     (or (char-numeric? x)
         (char-alphabetic? x)))))


(define (with-result value ctxt)
  (struct-copy parse-context ctxt [result value]))


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


(define %additive-expression
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
          (%let (tmp-ctxt1 tmp-ctxt2)
                (%and 
                 (%simple-expression ctxt tmp-ctxt1)
                 ((%p-chars-sequence
                   (%rel (c1 c2)
                         [(c1 c2)
                          (%let (tmp-ctxt3 tmp-ctxt4)
                                (%and
                                 (%or (%a-char #\+ c1 tmp-ctxt3)
                                      (%a-char #\- c1 tmp-ctxt3))
                                 (%simple-expression tmp-ctxt3 tmp-ctxt4)
                                 (%is c2  (with-result (list* (parse-context-result tmp-ctxt3)
                                                              (parse-context-result tmp-ctxt4))
                                                       tmp-ctxt4 )) )) ])) 
                  tmp-ctxt1 new-ctxt)))]))

(define %a-plus
  (%rel (c1 res)
        [(c1 res)
         (%let (c2 c3 c4)
               (%and
                (%identifier c1 c2)
                (%whitespaces c2 c3)
                (%identifier c3 c4)
                (%is res (struct-copy parse-context c4 [result (list (parse-context-result c2)  (parse-context-result c4))]))))
         ]))

(define %a-sep-list1
  (%rel (c1 res)
        [(c1 res)
         (%let (c2 c3 c4)
               (%and
                (%identifier c1 c2)
                (%whitespaces c2 c3)
                (%a-char #\, c3 c4)
                (%is res (struct-copy parse-context c4 [result  (parse-context-result c2)]))))
         ]))
(define %a-sep-list
  (%rel (c1 res)
      [(c1 res)  (%let
         (c2 c3)
         (%and 
          ((%p-chars-sequence %a-sep-list1) c1 c2)
          (%identifier c2 c3)
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
         (%identifier ctxt new-ctxt)
         (%is id (parse-context-result new-ctxt))]))


(define %call
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (ws-ctxt1 id-ctxt
                lpar-ctxt exprs-ctxt rpar-ctxt)
                (%and
                 (%whitespaces ctxt ws-ctxt1)
                 (%identifier ws-ctxt1 id-ctxt)
                 (%a-char #\( id-ctxt lpar-ctxt)
                 (%a-sep-list lpar-ctxt exprs-ctxt)
                 (%a-char #\) exprs-ctxt rpar-ctxt)
                 (%is new-ctxt
                      (with-result 
                        `(-call ,(parse-context-result id-ctxt)
                                ,(parse-context-result exprs-ctxt))
                        rpar-ctxt))
                 ))]))

(define %stat
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%or (%call-stat ctxt new-ctxt))]))
         

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
                (%whitespaces ctxt ws1)
                ((%literal-id "else") ws1 else-kw-ctxt)
                (%whitespaces else-kw-ctxt ws2)
                ((%p-chars-sequence %stat) ws2 stats-ctxt)
                (%is new-ctxt
                     (with-result `(-else ,(parse-context-result stats-ctxt))
                       stats-ctxt))
                )
               )]))



(define %if-stat
  (%rel (ctxt new-ctxt)
        [(ctxt new-ctxt)
         (%let (ws1 ifkw lpar-ctxt rpar-ctxt
                    cond-ctxt ws-ctxt2)
               (%and
                (%whitespaces ctxt ws1)
                (%literal-id "if" ws1 ifkw)
                (%a-char "(" ifkw lpar-ctxt)
                (%simple-expression lpar-ctxt cond-ctxt)
                (%a-char ")" cond-ctxt rpar-ctxt)
                (%whitespaces rpar-ctxt ws-ctxt2)
                (%literal-id "then" ws1 ifkw)



(define (quick-test parser str)
  (parse-context-result (cdar (%which (c) (parser (string-context str) c)))))

; (parse-context-result (cdar (%which (c) (%a-plus (string-context "abbcd sds") c))))
; (parse-context-result (cdar (%which (c) (%identifier (string-context "abbcd ") c))))

