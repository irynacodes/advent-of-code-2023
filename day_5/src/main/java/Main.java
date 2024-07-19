import java.io.IOException;
import java.nio.file.Paths;
import com.google.common.collect.Range;
import com.google.common.collect.RangeMap;
import com.google.common.collect.TreeRangeMap;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author Iryna Lazouskaya
 */
public class Main {
    private static final Set<Long> SEEDS = new HashSet<>();
    private static final RangeMap<Long, Range<Long>> SEED_TO_SOIL = TreeRangeMap.create();
    private static final RangeMap<Long, Range<Long>> SOIL_TO_FERTILIZER = TreeRangeMap.create();
    private static final RangeMap<Long, Range<Long>> FERTILIZER_TO_WATER = TreeRangeMap.create();
    private static final RangeMap<Long, Range<Long>> WATER_TO_LIGHT = TreeRangeMap.create();
    private static final RangeMap<Long, Range<Long>> LIGHT_TO_TEMPERATURE = TreeRangeMap.create();
    private static final RangeMap<Long, Range<Long>> TEMPERATURE_TO_HUMIDITY = TreeRangeMap.create();
    private static final RangeMap<Long, Range<Long>> HUMIDITY_TO_LOCATION = TreeRangeMap.create();
    private static final List<RangeMap<Long, Range<Long>>> MAPS = Arrays.asList(SEED_TO_SOIL, SOIL_TO_FERTILIZER, FERTILIZER_TO_WATER, WATER_TO_LIGHT,
            LIGHT_TO_TEMPERATURE, TEMPERATURE_TO_HUMIDITY, HUMIDITY_TO_LOCATION);

    public static void main(String[] args) throws IOException {
        processInputFile();
        var locations = new HashSet<Long>();
        for (var seed : SEEDS) {
            locations.add(getLocationForSeed(seed));
        }
        System.out.println(Collections.min(locations));
    }

    private static Long getLocationForSeed(Long seed) {
        var source = seed;
        var dest = 0L;

        for (var map : MAPS) {
            dest = getDestValue(map, source);
            source = dest;
        }

        return dest;
    }

    private static Long getDestValue(RangeMap<Long, Range<Long>> map, Long source) {
        for (var entry : map.asMapOfRanges().entrySet()) {
            if (entry.getKey().contains(source)) {
                var diff = source - entry.getKey().lowerEndpoint();
                return entry.getValue().lowerEndpoint() + diff;
            }
        }

        return source;
    }

    private static void processInputFile() throws IOException {
        var inputReader = new InputReader(Paths.get(System.getProperty("user.dir"), "src", "main", "java", "input.txt"));
        var input = inputReader.readInput().split("\n\n");
        readInputSeeds(input[0]);
        readInputMaps(input);
    }

    private static void readInputMaps(String[] input) {
        var index = 1;
        for (var map : MAPS) {
            readMapping(map, input[index++]);
        }
    }

    private static void readInputSeeds(String input) {
        var seeds = Arrays.stream(input.split("\\s")).collect(Collectors.toList());
        seeds.remove(0);

        for (var seed : seeds) {
            SEEDS.add(Long.valueOf(seed));
        }
    }

    private static void readMapping(RangeMap<Long, Range<Long>> map, String input) {
        var mappings = Arrays.stream(input.split("\n")).collect(Collectors.toList());
        mappings.remove(0);

        for (var mapping : mappings) {
            var values = Arrays.stream(mapping.split("\\s")).map(Long::valueOf).toList();
            map.put(Range.open(values.get(1), values.get(1) + values.get(2)),
                    Range.open(values.get(0), values.get(0) + values.get(2)));
        }
    }
}