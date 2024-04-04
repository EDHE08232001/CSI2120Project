
# Image Similarity Search

This project implements a concurrent image similarity search algorithm in Go. The program analyzes a dataset of images to find the ones most similar to a given query image based on color histogram comparisons. It utilizes Go's concurrency model to parallelize the computation of image histograms, aiming to optimize performance by experimenting with different levels of concurrency.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

- Go (version 1.15 or later recommended)
- A dataset of JPEG images

### Usage

1. Clone the repository to your local machine.
2. Navigate to the project directory.
3. Run the program using Go:

```sh
go run . <path/to/queryImage.jpg> <path/to/imageDatasetDirectory>
```

Replace `<path/to/queryImage.jpg>` with the path to your query image and `<path/to/imageDatasetDirectory>` with the path to the directory containing your dataset of images.

### Conducting Experiments

To evaluate the impact of concurrency on processing time, you can adjust the `K` value in `main.go`, which determines the number of concurrent goroutines used for computing histograms. Rerun the program with different `K` values to find the optimal configuration.

## Built With

- [Go](https://golang.org/) - The Go Programming Language

## Authors

- **Edward He** - *Initial work* - Student ID: 300176553
- **LianYi Chen** - *Initial work* - Student ID: 300248303

## Project Structure

- `main.go` - The entry point of the program.
- `imageLoader.go` - Functions for loading images from disk.
- `histogram.go` - Functions for computing and normalizing image histograms.
- `similarity.go` - Functions for calculating similarity scores between histograms.
- `concurrencyManager.go` - Manages concurrent computation of histograms.
- `utils.go` - Utility functions for the project.

## Acknowledgments

- Special thanks to the Go community for providing extensive documentation and resources.
