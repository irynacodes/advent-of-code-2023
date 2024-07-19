import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * @author Iryna Lazouskaya
 */
public class InputReader {

    private final Path inputFilePath;

    public InputReader(Path inputFilePath) {
        this.inputFilePath = inputFilePath;
    }

    public String readInput() throws IOException {
        String input = "";
        input = new String(
                Files.readAllBytes(inputFilePath));

        return input;
    }
}
