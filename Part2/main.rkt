#lang racket

(require racket/list
         racket/port
         racket/file)

; Function to read all lines from a file and return a list of lines
(define (read-all-lines filename)
  (printf "Reading file: ~a\n" filename) ; Indicator of file being read
  (if (file-exists? filename)
      (with-input-from-file filename
        (lambda ()
          (let loop ((line (read-line)))
            (if (eof-object? line)
                '()
                (cons line (loop (read-line)))))))
      (error "File does not exist")))

; Function for splitting strings into numbers
(define (string-split-into-numbers str)
  (map string->number (filter (lambda (s) (not (string=? s "")))
                              (string-split str " ")))) ; Use " " for splitting by space

; Helper function to group every 3 elements, adjusted to handle lists with fewer than 3 elements gracefully
(define (group-by-3 lst)
  (cond
    [(null? lst) '()] ; When the list is empty, return an empty list
    [(< (length lst) 3) (list lst)] ; When the list has fewer than 3 elements, return the list wrapped in a list
    [else (cons (take lst 3) (group-by-3 (drop lst 3)))])) ; Otherwise, group normally

; Function to read a PPM image and return its pixel data as a list of RGB tuples
(define (read-ppm filename)
  (let* ((lines (read-all-lines filename))
         (pixel-lines (drop lines 3))
         (pixels-flat-list (apply append (map string-split-into-numbers pixel-lines))))
    (group-by-3 pixels-flat-list)))

; Helper function for computing histogram index
(define (calculate-index r g b depth)
  (let* ([bin-size (expt 2 depth)]
         [r-index (/ r bin-size)]
         [g-index (/ g bin-size)]
         [b-index (/ b bin-size)])
    (+ (* r-index (expt bin-size 2)) (* g-index bin-size) b-index)))

; Function to compute the histogram from pixel data
(define (compute-histogram pixels depth)
  (let* ([num-bins (expt 2 (* 3 depth))]
         [histogram (make-vector num-bins 0)])
    (for-each (lambda (pixel)
                (let* ([r (first pixel)]
                       [g (list-ref pixel 1)]
                       [b (list-ref pixel 2)]
                       [index (calculate-index r g b depth)])
                  (vector-set! histogram index (+ 1 (vector-ref histogram index)))))
              pixels)
    histogram))

; Function to save a histogram to a file
(define (save-histogram histogram filename)
  (with-output-to-file filename
    (lambda () (for-each (lambda (val) (display val) (newline)) (vector->list histogram)))))

; Function to compare two histograms
(define (compare-histograms hist1 hist2)
  (let ([size (vector-length hist1)])
    (if (not (= size (vector-length hist2)))
        (error "Histograms must be of the same size")
        (let loop ([index 0]
                   [sum 0])
          (if (= index size)
              sum
              (loop (add1 index)
                    (+ sum (abs (- (vector-ref hist1 index)
                                   (vector-ref hist2 index))))))))))

; Custom structure to represent an image
(define-struct color-image (width height depth pixels))

; Function to load an image from a PPM file
(define (load-ppm-image filename)
  (let* ((lines (read-all-lines filename))
         (dimensions (string-split-into-numbers (list-ref lines 1)))
         (width (first dimensions))
         (height (list-ref dimensions 1))
         (max-color (string->number (list-ref lines 2)))
         (pixels (read-ppm filename)))
    (make-color-image width height 24 pixels)))

; Function to list all PPM files in a directory
(define (list-ppm-files dir)
  (let ((files (filter (lambda (file) (string-suffix? file ".ppm"))
                       (directory-list dir))))
    (printf "Files to be processed after directory scan: ~a\n" files) ; Indicator of files to be processed
    files))

; Main SimilaritySearch function with printing top 5 similarities
; Main SimilaritySearch function with printing top 5 similarities and additional diagnostics
(define (SimilaritySearch queryImageFilename imageDatasetDir)
  (printf "Starting SimilaritySearch with query image: ~a in directory: ~a\n" queryImageFilename imageDatasetDir)
  (let* ((queryImage (load-ppm-image queryImageFilename))
         (queryHistogram (compute-histogram (color-image-pixels queryImage) 8))
         (ppmFiles (list-ppm-files imageDatasetDir)))
    (printf "PPM files found: ~a\n" ppmFiles) ; Print found PPM files for debugging
    (let ((similarities (map (lambda (file)
                               (printf "Processing file: ~a\n" file) ; Print each file being processed
                               (let* ((datasetImage (load-ppm-image file))
                                      (datasetHistogram (compute-histogram (color-image-pixels datasetImage) 8))
                                      (similarity (compare-histograms queryHistogram datasetHistogram)))
                                 (cons file similarity)))
                             ppmFiles)))
      (let ((sorted-similarities (sort similarities (lambda (x y) (> (cdr x) (cdr y))))))
        (for-each (lambda (pair)
                    (printf "File: ~a - Similarity: ~a\n" (car pair) (cdr pair)))
                  (take sorted-similarities 5)))))
  (printf "SimilaritySearch completed.\n"))






(SimilaritySearch "./queryImages/q00.ppm" "./queryImages/")