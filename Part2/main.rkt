; Project Part 2
; Edward He 300176553
; LianYi Chen 300248303

#lang racket

(require racket/list
         racket/vector
         racket/file)

; ColorHistogram
; Helper function to read a PPM file and return its contents as a list
(define (read-ppm filename)
  (with-input-from-file filename
    (lambda ()
      (let* ((header (read-line))
             (dimensions (map string->number (string-split (read-line) " ")))
             (max-color-value (read-line))
             (pixels '()))
        (do ((line (read-line) (read-line)))
            ((eof-object? line) (if (= (length dimensions) 2) ; Ensure dimensions are correct
                                    (list (first dimensions) ; width
                                          (second dimensions) ; height
                                          (apply append (reverse pixels)))
                                    '())) ; Return an empty list on error
          (set! pixels (cons (map string->number (string-split line " ")) pixels)))))))

; Image width from PPM data
(define (image-width image)
  (if (and (list? image) (> (length image) 0))
      (first image)
      0)) ; Return 0 as default if image data is incorrect

; Image height from PPM data
(define (image-height image)
  (if (and (list? image) (> (length image) 1))
      (second image)
      0)) ; Return 0 as default if image data is incorrect

; Get pixel from PPM data
(define (get-pixel image x y)
  (let* ((width (image-width image))
         (pixels (third image))
         (index (* 3 (+ x (* y width)))))
    (vector (list-ref pixels index)
            (list-ref pixels (+ index 1))
            (list-ref pixels (+ index 2)))))

; Constructor for ColorHistogram Based on Color Depth
(define (create-color-histogram depth)
  (let ((num-bins (expt 2 (* depth 3))))
    (make-vector num-bins 0)))

; Calculating Histogram Index (Helper) using arithmetic-shift
(define (calculate-histogram-index rgb depth)
  (let* ((r (arithmetic-shift (vector-ref rgb 0) (- depth 8)))
         (g (arithmetic-shift (vector-ref rgb 1) (- depth 8)))
         (b (arithmetic-shift (vector-ref rgb 2) (- depth 8))))
    (+ (arithmetic-shift r (* 2 depth)) (arithmetic-shift g depth) b)))

; Setting Image Histogram
(define (set-image-histogram histogram image depth)
  (let* ((width (image-width image))
         (height (image-height image))
         (total-pixels (* width height)))
    (vector-fill! histogram 0) ; Reset histogram
    (for ([x (in-range width)])
      (for ([y (in-range height)])
        (let* ((pixel (get-pixel image x y))
               (index (calculate-histogram-index pixel depth)))
          (vector-set! histogram index (+ (vector-ref histogram index) 1)))))
    (if (> total-pixels 0) ; Check to prevent division by zero
        (for ([i (in-range (vector-length histogram))])
          (vector-set! histogram i (/ (vector-ref histogram i) total-pixels)))
        (displayln "Warning: Image has zero pixels, cannot normalize histogram."))))

; Comparing Histograms
(define (compare-histograms hist1 hist2)
  (let ((similarity (apply + (map min (vector->list hist1) (vector->list hist2)))))
    similarity))

; Saving Histogram to a File
(define (save-histogram histogram filename)
  (with-output-to-file filename #:exists 'replace
    (lambda ()
      (for ([i (in-range (vector-length histogram))])
        (display (vector-ref histogram i))
        (when (< i (- (vector-length histogram) 1))
          (display ","))))))





; ColorImage
; Function to reduce color depth of an image
(define (reduce-color image depth)
  (let* ((width (image-width image))
         (height (image-height image))
         (pixels (third image))
         (shift (- 8 depth))
         (new-pixels '()))
    (for ([y (in-range height)])
      (for ([x (in-range width)])
        (let* ((pixel-index (* 3 (+ x (* y width))))
               (rgb (list (list-ref pixels pixel-index)
                          (list-ref pixels (+ pixel-index 1))
                          (list-ref pixels (+ pixel-index 2))))
               (reduced-rgb (map (lambda (color)
                                   (bitwise-and (arithmetic-shift color (- shift)) #xFF))
                                 rgb)))
          (set! new-pixels (append new-pixels reduced-rgb)))))
    (list width height new-pixels)))

; Function to access a specific pixel's RGB value after color reduction
(define (get-reduced-pixel image x y depth)
  (let* ((reduced-image (reduce-color image depth))
         (pixels (third reduced-image))
         (index (* 3 (+ x (* y (image-width image))))))
    (vector (list-ref pixels index)
            (list-ref pixels (+ index 1))
            (list-ref pixels (+ index 2)))))





; SimilaritySearch
; Helper function to list all .ppm files in a directory
(define (list-ppm-files directory)
  (filter (lambda (file-path)
            (string-suffix? (path->string file-path) ".ppm"))
          (directory-list directory)))

; SimilaritySearch function
(define (SimilaritySearch queryImageFilename imageDatasetDirectory depth)
  (let ((queryImage (read-ppm queryImageFilename))
        (imageFiles (list-ppm-files imageDatasetDirectory))
        (queryHistogram (create-color-histogram depth)))
    ; Set histogram for the query image
    (set-image-histogram queryHistogram queryImage depth)
    ; Process each image in the directory
    (let ((scores (map (lambda (imageFile)
                         (let ((image (read-ppm (build-path imageDatasetDirectory imageFile)))
                               (imageHistogram (create-color-histogram depth)))
                           ; Set histogram for the current image
                           (set-image-histogram imageHistogram image depth)
                           ; Compute similarity and associate with the file name
                           (cons imageFile (compare-histograms queryHistogram imageHistogram))))
                       imageFiles)))
      ; Sort by similarity score and select top 5
      (let ((sorted-scores (sort scores (lambda (x y) (> (cdr x) (cdr y))))))
        (map car (take sorted-scores 5))))))

; Example Usage:
(SimilaritySearch "./queryImages/q00.ppm" "./queryImages/" 3)
