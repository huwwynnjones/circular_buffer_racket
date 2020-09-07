#lang racket

(require rackunit
         "circular-buffer.rkt")

(test-case
 "Test simple reading and writing into buffer"
 (let ([buff (new-circular-buffer 5)]
       [item 5])
   (write! buff item)
   (check-equal? (read buff) item)))

(test-case
 "Test that the buffer overwrites when moving past the end of the buffer"
 (let ([buff (new-circular-buffer 6)]
       [input "Hello Ada"]
       [correct-output "Adalo "])
   (for ([c input])
     (write! buff c)
     (next-write! buff))
   (check-equal?
    (list->string
     (for/list ([i (in-range 0 6)])
       (let ([c (read buff)])
         (next-read! buff)
         c)))
    correct-output)))

(test-case
 "Test we can know when the reader catches up with the writer"
 (let ([buff (new-circular-buffer 6)]
       [input "Hello Ada"]
       [correct-output "Adalo Ada"])
   (for ([c input])
     (write! buff c)
     (next-write! buff))
   (check-equal?
    (list->string
     (for/list ([i (in-naturals)]
                #:break (reader-matches-writer? buff))
       (let ([c (read buff)])
         (next-read! buff)
         c)))
    correct-output)))

(test-case
 "Test pre-condition on next-read"
 (let ([buff (new-circular-buffer 6)]
       [input "Hello Ada"]
       [correct-output "Adalo Ada"])
   (for ([c input])
     (write! buff c)
     (next-write! buff))
   (for ([i input])
     (next-read! buff))
   (check-exn exn:fail:contract?
              (λ ()
                (next-read! buff)))))