#lang racket

(require racket/list
         racket/port
         racket/file)

; ColorHistogram Section
; Corrected function to read all lines from a file and return a list of lines
(define (read-all-lines filename)
  (if (file-exists? filename)
      (with-input-from-file filename
        (lambda ()
          (let loop ((line (read-line)))
            (if (eof-object? line)
                '()
                (cons line (loop (read-line)))))))
      (error "File does not exist")))

; Corrected renaming and function definition for splitting strings into numbers
(define (string-split-into-numbers str)
  (map string->number (filter (lambda (s) (not (string=? s "")))
                              (string-split str #\space)))) ; Use #\space for splitting by space

; New generic string-split function that accepts a delimiter
(define (string-split str delimiter)
  (string-split str delimiter))

; Helper function to group every 3 elements
(define (group-by-3 lst)
  (if (null? lst)
      '()
      (cons (take lst 3) (group-by-3 (drop lst 3)))))

; Function to read a PPM image and return its pixel data as a list of RGB tuples
(define (read-ppm filename)
  (let* ((lines (read-all-lines filename))
         (pixel-lines (drop lines 3))
         (pixels-flat-list (apply append (map string-split pixel-lines))))
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
                       [g (second pixel)]
                       [b (third pixel)]
                       [index (calculate-index r g b depth)])
                  (vector-set! histogram index (+ 1 (vector-ref histogram index)))))
              pixels)
    histogram))

; Function to save a histogram to a file
(define (save-histogram histogram filename)
  (with-output-to-file filename
    (lambda () (for-each (lambda (val) (display val) (newline)) (vector->list histogram)))))





; ColorImage Section
; Custom structure to represent an image
(define-struct color-image (width height depth pixels))

; Function to load an image from a PPM file
; Adjust the usage in load-ppm-image and similar places
(define (load-ppm-image filename)
  (let* ((lines (read-all-lines filename))
         (header (first lines))
         (dimensions (string-split-into-numbers (second lines))) ; Use the renamed function here
         (width (first dimensions))
         (height (second dimensions))
         (max-color (string->number (third lines)))
         (pixels (read-ppm filename)))
    (make-color-image width height 24 pixels))) ; Assuming 24-bit depth for simplicity

; Function to get a pixel's RGB value from an image
(define (get-pixel image x y)
  (let* ((width (color-image-width image))
         (index (+ (* y width) x))
         (pixels (color-image-pixels image)))
    (list-ref pixels index)))

; Function to reduce color depth of an image
(define (reduce-color image new-depth)
  (let* ((shift-amount (- 8 new-depth))
         (reduced-pixels (map (lambda (pixel)
                                (map (lambda (channel)
                                       (bitwise-and channel (- (expt 2 new-depth) 1)))
                                     pixel))
                              (color-image-pixels image))))
    (make-color-image (color-image-width image)
                      (color-image-height image)
                      new-depth
                      reduced-pixels)))





; SimilaritySearch Section
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
                    (+ sum (min (vector-ref hist1 index)
                                (vector-ref hist2 index)))))))))

; Function to list all PPM files in a directory
(define (list-ppm-files dir)
  (filter (lambda (file) (string-suffix? file ".ppm"))
          (directory-list dir)))

; Main SimilaritySearch function with printing top 5 similarities
(define (SimilaritySearch queryImageFilename imageDatasetDir)
  (let* ((queryImage (load-ppm-image queryImageFilename))
         (queryHistogram (compute-histogram (color-image-pixels queryImage) 8)) ; Assume 8-bit depth for histogram
         (ppmFiles (list-ppm-files imageDatasetDir))
         (similarities (map (lambda (file)
                              (let* ((datasetImage (load-ppm-image file))
                                     (datasetHistogram (compute-histogram (color-image-pixels datasetImage) 8))
                                     (similarity (compare-histograms queryHistogram datasetHistogram)))
                                (cons file similarity)))
                            ppmFiles))
         (sorted-similarities (sort similarities (lambda (x y) (> (cdr x) (cdr y))))))
    (for-each (lambda (pair)
                (printf "File: ~a - Similarity: ~a\n" (car pair) (cdr pair)))
              (take sorted-similarities 5)))) ; Print the top 5 similar images



(SimilaritySearch "./queryImages/q00.ppm" "./queryImages/")
