package main

import (
    "image"
    "image/jpeg"
    "os"
)

// loadImage reads an image from a file and returns it as an image.Image.
func loadImage(filePath string) (image.Image, error) {
    file, err := os.Open(filePath)
    if err != nil {
        return nil, err
    }
    defer file.Close()

    img, err := jpeg.Decode(file)
    if err != nil {
        return nil, err
    }

    return img, nil
}
