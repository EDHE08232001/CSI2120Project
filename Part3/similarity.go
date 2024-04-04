package main

// compareHistograms calculates the similarity score between two histograms using the intersection method.
// The function assumes both histograms are normalized and of the same length.
func compareHistograms(h1, h2 Histo) float64 {
    similarity := 0.0

    // Ensure histograms have the same number of bins.
    if len(h1.H) != len(h2.H) {
        return 0.0 // Consider proper error handling or logging here.
    }

    // Calculate the intersection (sum of minimums for corresponding bins).
    for i := range h1.H {
        similarity += min(h1.H[i], h2.H[i])
    }

    return similarity
}

// min returns the smaller of two float64 values.
func min(a, b float64) float64 {
    if a < b {
        return a
    }
    return b
}
