#lang racket

;; Reads a histogram from a file and converts it into a list
(define (read-histogram filename)
  (with-input-from-file filename
    (lambda ()
      (map string->number (string-split (read-line) ",")))))

;; Compares two histograms and calculates their intersection
(define (histogram-intersection h1 h2)
  (apply + (map min h1 h2)))

;; Recursively processes files in a directory, calculating similarity
(define (process-files query-histogram files)
  (if (null? files)
      '()
      (let* ((file (car files))
             (histogram (read-histogram file))
             (similarity (histogram-intersection query-histogram histogram))
             (rest-similarity (process-files query-histogram (cdr files))))
        (cons (cons file similarity) rest-similarity))))

;; Returns the top 5 similar images based on histogram intersection
(define (top-similar-images similarities)
  (take (sort similarities (lambda (x y) (> (cdr x) (cdr y)))) 5))

;; Main function to start the similarity search
(define (similaritySearch queryHistogramFilename imageDatasetDirectory)
  (let* ((query-histogram (read-histogram queryHistogramFilename))
         (files (directory-list imageDatasetDirectory))
         (jpg-files (filter (lambda (f) (string-suffix? (path->string f) ".jpg")) files))
         (similarities (process-files query-histogram jpg-files)))
    (top-similar-images similarities)))

;; Example usage:
;; (similaritySearch "path/to/query/histogram.txt" "path/to/dataset/directory/")
