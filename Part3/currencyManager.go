package main

import (
  "sync"
)

// computeHistograms concurrently computes histograms for a slice of images and sends them through a channel.
func computeHistograms(imagePaths []string, depth int, hChan chan<- Histo) {
  var wg sync.WaitGroup // Use a WaitGroup to wait for all goroutines to finish

  for _, path := range imagePaths {
    wg.Add(1) // Increment the WaitGroup counter

    go func(p string) {
      defer wg.Done() // Decrement the counter when the goroutine completes

      img, err := loadImage(p) // Load the image from the given path
      if err != nil {
        // Error handling: Log error or decide on error strategy
        // For simplicity, we'll skip the error handling here.
        return
      }

      histo, err := computeHistogram(img, depth) // Compute the histogram for the loaded image
      if err != nil {
        // Handle error in histogram computation
        return
      }

      histo.Name = p // Optionally, set the Name field of the Histo struct to the image path
      hChan <- histo // Send the histogram through the channel

    }(path) // Pass the current path to the goroutine
  }

  wg.Wait() // Wait for all goroutines to finish
  close(hChan) // Close the channel once all histograms have been computed and sent
}
