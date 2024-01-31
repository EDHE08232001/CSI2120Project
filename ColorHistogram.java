import java.io.*;

/**
 * Class representing a color histogram of an image.
 */
public class ColorHistogram {
  private double[] histogram; // Array to store the normalized histogram
  private int depth; // Bit depth of the color

  /**
   * Constructor to initialize a histogram based on color depth.
   * 
   * @param depth The bit depth for each color channel.
   */
  public ColorHistogram(int depth) {
    this.depth = depth;
    int numBins = (int) Math.pow(2, depth * 3);
    this.histogram = new double[numBins];
  }

  /**
   * Constructor to load a histogram from a file.
   * 
   * @param filename The file path to load the histogram from.
   * @throws IOException If there is an error reading the file.
   */
  public ColorHistogram(String filename) throws IOException {
    // Load histogram from file
    try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
      String line = br.readLine();
      String[] values = line.split(",");
      histogram = new double[values.length];
      for (int i = 0; i < values.length; i++) {
        histogram[i] = Double.parseDouble(values[i]);
      }
    }
  }

  /**
   * Computes and sets the histogram for a given ColorImage.
   * 
   * @param image The ColorImage to compute the histogram from.
   */
  public void setImage(ColorImage image) {
    // Reset histogram
    for (int i = 0; i < histogram.length; i++) {
      histogram[i] = 0;
    }

    // Compute histogram from the image
    for (int x = 0; x < image.getWidth(); x++) {
      for (int y = 0; y < image.getHeight(); y++) {
        int[] rgb = image.getPixel(x, y);
        int index = calculateHistogramIndex(rgb);
        histogram[index]++;
      }
    }

    // Normalize histogram
    int totalPixels = image.getWidth() * image.getHeight();
    for (int i = 0; i < histogram.length; i++) {
      histogram[i] /= totalPixels;
    }
  }

  /**
   * Helper method to calculate the histogram index for a given RGB value.
   * 
   * @param rgb The RGB values.
   * @return The index in the histogram.
   */
  private int calculateHistogramIndex(int[] rgb) {
    int r = rgb[0] >> (8 - depth);
    int g = rgb[1] >> (8 - depth);
    int b = rgb[2] >> (8 - depth);
    return (r << (2 * depth)) + (g << depth) + b;
  }

  /**
   * Compares this histogram with another histogram.
   * 
   * @param otherHistogram The other ColorHistogram to compare with.
   * @return The similarity score between the two histograms.
   */
  public double compare(ColorHistogram otherHistogram) {
    double similarity = 0.0;
    for (int i = 0; i < histogram.length; i++) {
      similarity += Math.min(this.histogram[i], otherHistogram.histogram[i]);
    }
    return similarity;
  }

  /**
   * Saves the histogram to a file.
   * 
   * @param filename The file path to save the histogram to.
   * @throws IOException If there is an error writing to the file.
   */
  public void save(String filename) throws IOException {
    try (BufferedWriter bw = new BufferedWriter(new FileWriter(filename))) {
      for (int i = 0; i < histogram.length; i++) {
        bw.write(histogram[i] + (i < histogram.length - 1 ? "," : ""));
      }
    }
  }

  /**
   * Getter for the histogram data.
   * 
   * @return The histogram array.
   */
  public double[] getHistogram() {
    return histogram;
  }
}