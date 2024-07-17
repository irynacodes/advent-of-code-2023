using day_2.Enums;
using System.Text;

namespace day_2;

public static class Program
{
    private static readonly Dictionary<Cubes, int> CubesAvailable = new()
    {
        { Cubes.Red, 12 },
        { Cubes.Green, 13 },
        { Cubes.Blue, 14 }
    };
    private static readonly string ProjectRoot = Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName;
    private static readonly string InputFilePath = Path.Combine(ProjectRoot, "input.txt");
    
    public static void Main()
    {
        var games = GetInput(InputFilePath);
        var gameIdSum = games
            .Where(game => !string.IsNullOrWhiteSpace(game))
            .Sum(game => IsGamePossible(game));
        Console.WriteLine(gameIdSum);
    }

    /**
     * Determines if the game could've been possible given the amount of cubes available.
     * Returns game's Id if it was possible, 0 if it was not.
     *
     * This function assumes game is of format "Game x: y red, z green"
     */
    private static int IsGamePossible(String game)
    {
        var gameId = Int32.Parse(game.Split(":")[0].Split(" ")[1]);
        var rounds = game.Split(":")[1].Trim().Split(";").ToList();

        foreach (var round in rounds)
        {
            var cubeColors = round.Split(",").ToList();
            foreach (var cubeColor in cubeColors)
            {
                var count = Int32.Parse(cubeColor.Trim().Split(" ")[0]);
                var colorS = cubeColor.Trim().Split(" ")[1];

                if (!Enum.TryParse(typeof(Cubes), colorS, true, out var color) ||
                    CubesAvailable[(Cubes)color] < count)
                {
                    return 0;
                }
            }
        }

        return gameId;
    }

    private static List<String> GetInput(String fileName)
    {
        string readContents;
        using (StreamReader streamReader = new StreamReader(fileName, Encoding.UTF8))
        {
            readContents = streamReader.ReadToEnd();
        }

        return readContents.Split(Environment.NewLine).ToList();
    }
}