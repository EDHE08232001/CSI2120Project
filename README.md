
# Image Similarity Search Project for CSI 2120

This project implements an image similarity search using color histograms to compare images. It includes three main Java classes: `ColorImage`, `ColorHistogram`, and `SimilaritySearch`. The program computes the color histogram of an image and compares it with the histograms of images in a dataset to find and list the top 5 most similar images.

## Authors

- Edward He (Student ID: 300176553)
- LianYi Chen (Student ID: 300248303)

## How to Compile and Run

Ensure you have Java installed on your system. Then, follow these steps to compile and run the project:

1. **Compile the Java Files**:
   Navigate to the project directory and run the following command to compile the Java files and output the class files to the `./output/` directory:

   ```bash
   javac -d ./output/ ColorImage.java ColorHistogram.java SimilaritySearch.java
   ```

2. **Run the Similarity Search**:
   After compiling, you can run the similarity search on a query image against a dataset of images. Use the following command, replacing `[queryImage]` with the path to your query image and `[imageDatasetPath]` with the path to the directory containing your dataset images:

   ```bash
   java -cp ./output/ SimilaritySearch [queryImage] [imageDatasetPath]
   ```

   For example:

   ```bash
   java -cp ./output/ SimilaritySearch ./queryImages/q00.jpg ./queryImages
   ```

## Test Results

Test results showing the top 5 similar images for two different query images:

1. **Query Image 1 (`q00.jpg`)**:

   ```
   Top 5 similar images:
   q00.jpg - Similarity: 1.0
   q15.jpg - Similarity: 0.5978703703703704
   q06.jpg - Similarity: 0.5635358796296296
   q03.jpg - Similarity: 0.46017361111111105
   q02.jpg - Similarity: 0.4597222222222225
   ```

2. **Query Image 2 (`25.jpg` in `imageDataset2`)**:

   ```
   Top 5 similar images:
   25.jpg - Similarity: 0.9999999999999997
   4728.jpg - Similarity: 0.7720543981481481
   3672.jpg - Similarity: 0.7575636574074072
   4854.jpg - Similarity: 0.7242534722222216
   4805.jpg - Similarity: 0.7082638888888887
   ```

## Dependencies

- Java JDK

This project demonstrates how to implement a simple yet effective image similarity search algorithm using color histograms.
