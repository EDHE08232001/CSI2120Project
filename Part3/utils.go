package main

// splitIntoSlices divides a slice of strings (image paths) into smaller slices for concurrent processing.
func splitIntoSlices(paths []string, numSlices int) [][]string {
    var slices [][]string
    sliceSize := len(paths) / numSlices

    for i := 0; i < len(paths); i += sliceSize {
        end := i + sliceSize
        if end > len(paths) {
            end = len(paths)
        }
        slices = append(slices, paths[i:end])
    }
    return slices
}
