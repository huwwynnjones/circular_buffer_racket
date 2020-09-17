#lang racket

(provide new-circular-buffer
         read
         write!
         (contract-out
          [next-read! (-> (flat-named-contract
                           "Buffer with reader <= writer"
                           (λ (cb)
                             (not (reader-matches-writer? cb)))) any)])
         next-write!
         reader-matches-writer?)

(struct circular-buffer (buffer read-idx write-idx) #:mutable)

(define (new-circular-buffer size)
  (circular-buffer (make-vector size) 1 1))

(define (read circular-buffer)
  (vector-ref (circular-buffer-buffer circular-buffer)
              (wrap-around
               (vector-length (circular-buffer-buffer circular-buffer))
               (circular-buffer-read-idx circular-buffer))))

(define (write! circular-buffer item)
  (vector-set! (circular-buffer-buffer circular-buffer)
               (wrap-around
                (vector-length (circular-buffer-buffer circular-buffer))
                (circular-buffer-write-idx circular-buffer))
               item))

(define (reader-matches-writer? circular-buffer)
  (equal? (circular-buffer-read-idx circular-buffer) (circular-buffer-write-idx circular-buffer)))

(define (next-read! circular-buffer)
  (increment-idx circular-buffer 'read))

(define (next-write! circular-buffer)
  (increment-idx circular-buffer 'write))

(define (increment-idx circular-buffer idx-type)
  (case idx-type
    ['read (set-circular-buffer-read-idx!
            circular-buffer
            (+ (circular-buffer-read-idx circular-buffer) 1))]
    ['write (set-circular-buffer-write-idx!
             circular-buffer
             (+ (circular-buffer-write-idx circular-buffer) 1))]))

(define (wrap-around buffer-length idx)
  (let ([remainder (remainder idx buffer-length)])
    (if (= remainder 0)
        (- buffer-length 1)
        (- remainder 1))))