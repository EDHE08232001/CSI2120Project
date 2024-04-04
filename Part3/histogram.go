package main

import (
    "image"
    "math"
)

// Histo represents a color histogram of an image.
type Histo struct {
    Name string
    H    []float64 // Use float64 for normalized histogram values
}

// computeHistogram computes the histogram of the specified image, reduces it to the specified depth, and normalizes it.
func computeHistogram(img image.Image, depth int) (Histo, error) {
    // Number of bins is 2^(depth*3) because we have depth bits per channel and 3 channels (RGB)
    binsPerChannel := int(math.Pow(2, float64(depth)))
    totalBins := binsPerChannel * binsPerChannel * binsPerChannel
    histogram := make([]float64, totalBins)

    // Factor to scale color values down based on depth
    scaleFactor := uint32(math.Pow(2, float64(8-depth)))

    // Bounds for iterating over image pixels
    bounds := img.Bounds()
    totalPixels := float64(bounds.Dx() * bounds.Dy())

    for y := bounds.Min.Y; y < bounds.Max.Y; y++ {
        for x := bounds.Min.X; x < bounds.Max.X; x++ {
            // Get pixel at x, y
            r, g, b, _ := img.At(x, y).RGBA()

            // Convert to 8-bit depth and reduce depth based on scaleFactor
            r = (r >> 8) / scaleFactor
            g = (g >> 8) / scaleFactor
            b = (b >> 8) / scaleFactor

            // Calculate index for the histogram
            index := int(r)*binsPerChannel*binsPerChannel + int(g)*binsPerChannel + int(b)
            histogram[index]++
        }
    }

    // Normalize the histogram so that the sum of all bins equals 1
    for i := range histogram {
        histogram[i] /= totalPixels
    }

    // Return the histogram wrapped in a Histo struct. Name can be added/set later if needed.
    return Histo{H: histogram}, nil
}
