package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "os"
    "path/filepath"
    "sort"
    "sync"
    "time"
)

// Define a struct to hold key-value pairs from the map for sorting.
type imageScore struct {
    Name  string
    Score float64
}

func main() {
    startTime := time.Now() // Start timing.

    if len(os.Args) < 3 {
        log.Fatalf("Usage: %s <queryImageFilename> <imageDatasetDirectory>\n", os.Args[0])
    }

    queryImagePath := os.Args[1]
    imageDatasetDirectory := os.Args[2]

    hChan := make(chan Histo)
    depth := 3 // Example depth for color reduction.

    queryImage, err := loadImage(queryImagePath)
    if err != nil {
        log.Fatalf("Failed to load query image: %v", err)
    }

    queryHisto, err := computeHistogram(queryImage, depth)
    if err != nil {
        log.Fatalf("Failed to compute histogram for query image: %v", err)
    }

    files, err := ioutil.ReadDir(imageDatasetDirectory)
    if err != nil {
        log.Fatalf("Failed to read dataset directory: %v", err)
    }

    var imagePaths []string
    for _, file := range files {
        if !file.IsDir() {
            fullPath := filepath.Join(imageDatasetDirectory, file.Name())
            imagePaths = append(imagePaths, fullPath)
        }
    }

    // Number of slices/goroutines is set dynamically
    K := 1 // Placeholder for K. You will change this according to the experiment.
    slices := splitIntoSlices(imagePaths, K)

    var wg sync.WaitGroup
    for _, slice := range slices {
        wg.Add(1)
        go func(s []string) {
            defer wg.Done()
            computeHistograms(s, depth, hChan)
        }(slice)
    }

    go func() {
        wg.Wait()
        close(hChan)
    }()

    similarityScores := make(map[string]float64)
    for histo := range hChan {
        similarityScores[histo.Name] = compareHistograms(queryHisto, histo)
    }

    var scores []imageScore
    for name, score := range similarityScores {
        scores = append(scores, imageScore{Name: name, Score: score})
    }

    sort.Slice(scores, func(i, j int) bool {
        return scores[i].Score > scores[j].Score
    })

    fmt.Println("Top 5 similar images:")
    for i, img := range scores {
        if i >= 5 {
            break
        }
        fmt.Printf("%d: %s - Similarity: %.4f\n", i+1, img.Name, img.Score)
    }

    // Report execution time.
    fmt.Printf("Execution Time for K=%d: %v\n", K, time.Since(startTime))
}