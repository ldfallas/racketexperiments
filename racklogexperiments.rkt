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

; (parse-context-result (cdar (%which (c) (%a-plus (string-context "abbcd sds") c))))
; (parse-context-result (cdar (%which (c) (%identifier (string-context "abbcd ") c))))
