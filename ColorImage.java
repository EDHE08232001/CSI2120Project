import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

/**
 * Class representing a color image, facilitating operations like pixel access
 * and color depth reduction.
 */
public class ColorImage {
  private int width; // Image width in pixels
  private int height; // Image height in pixels
  private int depth = 24; // Default color depth (24-bit for RGB)
  private int[][] pixelData; // 2D array to store pixel data

  /**
   * Constructor that loads an image from a file and initializes its properties.
   * 
   * @param filename Path to the image file.
   */
  public ColorImage(String filename) {
    File file = new File(filename);
    if (!file.exists()) {
      System.err.println("File not found: " + filename);
      return;
    }

    try {
      BufferedImage image = ImageIO.read(file);
      if (image == null) {
        System.err.println("Failed to load image: " + filename);
        return;
      }

      width = image.getWidth();
      height = image.getHeight();
      pixelData = new int[width][height];

      // Extract RGB values for each pixel and store them
      for (int x = 0; x < width; x++) {
        for (int y = 0; y < height; y++) {
          pixelData[x][y] = image.getRGB(x, y);
        }
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * Retrieves the RGB value of a specific pixel.
   * 
   * @param x X-coordinate of the pixel.
   * @param y Y-coordinate of the pixel.
   * @return An array of three integers representing the RGB values of the pixel.
   */
  public int[] getPixel(int x, int y) {
    int rgb = pixelData[x][y];
    int red = (rgb >> 16) & 0xFF;
    int green = (rgb >> 8) & 0xFF;
    int blue = rgb & 0xFF;
    return new int[] { red, green, blue };
  }

  /**
   * Reduces the color depth of the image.
   * 
   * @param d The desired bit depth per color channel.
   */
  public void reduceColor(int d) {
    int shift = 8 - d; // Calculate the number of bits to shift
    for (int x = 0; x < width; x++) {
      for (int y = 0; y < height; y++) {
        // Extract each color component and reduce its bit depth
        int rgb = pixelData[x][y];
        int red = ((rgb >> 16) & 0xFF) >> shift;
        int green = ((rgb >> 8) & 0xFF) >> shift;
        int blue = (rgb & 0xFF) >> shift;

        // Reassemble the color with reduced depth
        red = red << shift;
        green = green << shift;
        blue = blue << shift;

        pixelData[x][y] = (red << 16) | (green << 8) | blue;
      }
    }
  }

  // Getter methods for image properties
  public int getWidth() {
    return width;
  }

  public int getHeight() {
    return height;
  }

  public int getDepth() {
    return depth;
  }
}