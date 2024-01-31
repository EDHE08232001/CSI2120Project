import java.io.File;
import java.util.*;

/**
 * Class to perform similarity search on images based on color histograms.
 */
public class SimilaritySearch {

  /**
   * The main method for executing the similarity search.
   * 
   * @param args Command line arguments - args[0] is the path to the query image,
   *             args[1] is the directory containing the dataset images.
   */
  public static void main(String[] args) {
    String queryImagePath = "./queryImages/q00.jpg";
    String datasetPath = "./queryImages/";

    // Load the query image and compute its histogram
    ColorImage queryImage = new ColorImage(queryImagePath);
    ColorHistogram queryHistogram = new ColorHistogram(3); // 3-bit depth for simplicity
    queryHistogram.setImage(queryImage);

    // Directory containing dataset images
    File datasetDir = new File(datasetPath);
    File[] imageFiles = datasetDir.listFiles();

    // Map to store similarity scores with corresponding image file names
    Map<String, Double> similarityScores = new HashMap<>();

    // Processing each image in the dataset
    for (File imageFile : imageFiles) {
      if (!imageFile.getName().endsWith(".jpg") && !imageFile.getName().endsWith(".jpeg")) {
        continue; // Skip non-JPEG files
      }
      ColorImage datasetImage = new ColorImage(imageFile.getPath());
      ColorHistogram datasetHistogram = new ColorHistogram(3);
      datasetHistogram.setImage(datasetImage);

      // Compute and store the similarity score
      double similarity = queryHistogram.compare(datasetHistogram);
      similarityScores.put(imageFile.getName(), similarity);
    }

    // Sort the entries by similarity scores in descending order
    List<Map.Entry<String, Double>> sortedEntries = new ArrayList<>(similarityScores.entrySet());
    sortedEntries.sort(Map.Entry.<String, Double>comparingByValue().reversed());

    // Output the top 5 similar images
    System.out.println("Top 5 similar images:");
    for (int i = 0; i < Math.min(5, sortedEntries.size()); i++) {
      System.out.println(sortedEntries.get(i).getKey() + " - Similarity: " + sortedEntries.get(i).getValue());
    }
  }
}