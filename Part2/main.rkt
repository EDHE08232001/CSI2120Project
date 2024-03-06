#lang racket

(require racket/list
         racket/port
         racket/file)

; ColorHistogram Section
; Custom function to read all lines from a file and return a list of lines, with basic error handling
(define (read-all-lines filename)
  (if (file-exists? filename)
      (with-input-from-file filename
        (lambda ()
          (let loop ((line (read-line 'eof))
                     (lines '()))
            (if (eq? line 'eof)
                (reverse lines)
                (loop (read-line 'eof) (cons line lines))))))
      (error "File does not exist")))

; Corrected function to split string into a list of numbers, assuming space-separated values
(define (string-split str)
  (map string->number (filter (lambda (s) (not (string=? s ""))) (string-split str " "))))

; Corrected string-split to avoid self-call and ensure it works as expected
(define (corrected-string-split str)
  (string-split str " "))

; Helper function to group every 3 elements (for RGB)
(define (group-by-3 lst)
  (if (null? lst)
      '()
      (cons (take lst 3) (group-by-3 (drop lst 3)))))

; Function to read a PPM image and return its pixel data as a list of RGB tuples
(define (read-ppm filename)
  (let* ((lines (read-all-lines filename))
         ; Assuming the first three lines are header, dimensions, and max color value
         (pixel-lines (drop lines 3))
         (pixels-flat-list (apply append (map corrected-string-split pixel-lines)))) ; Flatten the list of lists into a single list
    (group-by-3 pixels-flat-list))) ; Group every 3 elements (for RGB)

; Function to compute the histogram from pixel data
(define (compute-histogram pixels depth)
  (let* ([shift-amount (- 8 depth)]
         [num-bins (expt 2 (* 3 depth))]
         [histogram (make-vector num-bins 0)])
    (for-each (lambda (pixel-group)
                (for-each (lambda (pixel)
                            (let* ([r (first pixel)]
                                   [g (second pixel)]
                                   [b (third pixel)]
                                   [index (calculate-index r g b depth shift-amount)])
                              (vector-set! histogram index (+ 1 (vector-ref histogram index)))))
                          pixel-group))
              pixels)
    (normalize-histogram histogram)))

; Normalize the histogram
(define (normalize-histogram histogram)
  (let ([total (apply + (vector->list histogram))])
    (map (lambda (val) (/ val total)) (vector->list histogram))))

; Calculate histogram index
(define (calculate-index r g b depth shift-amount)
  (let* ([r-shifted (arithmetic-shift r (- shift-amount))]
         [g-shifted (arithmetic-shift g (- shift-amount))]
         [b-shifted (arithmetic-shift b (- shift-amount))])
    (+ (arithmetic-shift r-shifted (* 2 depth)) (arithmetic-shift g-shifted depth) b-shifted)))

; Compare two histograms
(define (compare-histograms hist1 hist2)
  (let ([min-pairs (map min hist1 hist2)])
    (apply + min-pairs)))

; Function to save a histogram to a file
(define (save-histogram histogram filename)
  (with-output-to-file filename
    (lambda () (for-each (lambda (val) (display val) (display ", ")) histogram))))




; ColorImage Section
; Custom structure to represent an image with its properties
(define-struct color-image (width height depth pixels))

; Function to load an image from a PPM file
(define (load-ppm-image filename)
  (with-input-from-file filename
    (lambda ()
      (let* ((header (read-line))
             (dimensions (map string->number (string-split (read-line) " ")))
             (max-color (read-line))
             (width (first dimensions))
             (height (second dimensions))
             (pixels (read-pixels width height)))
        (make-color-image width height 24 pixels)))))

; Helper function to read pixel data from PPM file
(define (read-pixels width height)
  (for/list ([i (in-range (* width height))])
    (let ((r (read))
          (g (read))
          (b (read)))
      (list r g b))))

; Function to get a pixel's RGB value from an image
(define (get-pixel image i j)
  (let ((width (color-image-width image)))
    (list-ref (color-image-pixels image) (+ i (* j width)))))

; Function to reduce color depth of an image
(define (reduce-color image new-depth)
  (let ((shift (- 24 new-depth))
        (pixels (color-image-pixels image)))
    (map (lambda (pixel)
           (map (lambda (channel) (bitwise-and channel (arithmetic-shift 255 (- new-depth))))
                pixel))
         pixels)))




; SimilaritySearch Section
; Function to list all files in a directory
(define (list-files dir)
  (filter file-exists? (directory-list dir)))

; Function to filter only PPM files from a list of files
(define (filter-ppm-files files)
  (filter (lambda (file) (string-suffix? file ".ppm")) files))

; Main SimilaritySearch function
(define (SimilaritySearch queryImageFilename imageDatasetDir)
  (let* ((queryImage (load-ppm-image queryImageFilename)) ; Load query image
         (queryHistogram (compute-histogram (color-image-pixels queryImage) 3)) ; Compute histogram for query image
         (imageFiles (list-files imageDatasetDir)) ; List all files in dataset directory
         (ppmFiles (filter-ppm-files imageFiles)) ; Filter for .ppm files only
         (similarities '())) ; List to store (filename, similarity score) pairs
    (for-each (lambda (file)
                (let* ((datasetImage (load-ppm-image file)) ; Load each dataset image
                       (datasetHistogram (compute-histogram (color-image-pixels datasetImage) 3)) ; Compute histogram
                       (similarity (compare-histograms queryHistogram datasetHistogram))) ; Compare histograms
                  (set! similarities (cons (cons file similarity) similarities)))) ; Store (file, similarity) pair
              ppmFiles)
    (let ((sorted-similarities (sort similarities (lambda (x y) (> (cdr x) (cdr y)))))) ; Sort by similarity score
      (for-each (lambda (pair)
                  (printf "~a - Similarity: ~a\n" (car pair) (cdr pair)))
                (take sorted-similarities 5))))) ; Print top 5 similar images