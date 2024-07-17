using System.Text;
using day_3.Entities;

namespace day_3;

class Program
{
    private static readonly HashSet<Number> CountedNumbersIndeces = new();
    private static List<string> _data = new();
    
    private static readonly string ProjectRoot = Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName;
    private static readonly string InputFilePath = Path.Combine(ProjectRoot, "input.txt");
    
    public static void Main()
    {
        _data = GetInput(InputFilePath);
        var symbols = GetSymbols(_data);
        var res = symbols.Sum(symbol => CountAdjacentNumbers(symbol));
        Console.WriteLine(res);
    }

    private static int CountAdjacentNumbers(Symbol symbol)
    {
        var numberSum = 0;
        var startLine = int.Max(symbol.Line - 1, 0);
        var endLine = int.Min(symbol.Line + 1, _data.Count - 1);
        var startCol = int.Max(symbol.Col - 1, 0);
        var endCol = int.Min(symbol.Col + 1, _data[0].Length);

        for (int i = startLine; i <= endLine; i++)
        {
            var visitedDigitsIndexes = new HashSet<int>();
            for (int j = startCol; j <= endCol; j++)
            {
                if (!Char.IsDigit(_data[i][j]) || visitedDigitsIndexes.Contains(j - 1))
                {
                    continue;
                }

                visitedDigitsIndexes.Add(j);
                var number = GetNumberFromIndex((i, j));
                if (!CountedNumbersIndeces.Contains(number))
                {
                    numberSum += number.Value;
                    CountedNumbersIndeces.Add(number);
                }
            }
        }

        return numberSum;
    }

    private static Number GetNumberFromIndex((int, int) index)
    {
        var line = _data[index.Item1];
        var numberStart = index.Item2;
        var numberEnd = index.Item2;
        
        while (numberStart > 0 && Char.IsDigit(line[numberStart - 1]))
        {
            numberStart--;
        }
        
        while (numberEnd < line.Length - 1 && Char.IsDigit(line[numberEnd + 1]))
        {
            numberEnd++;
        }

        return new Number(Int32.Parse(line.Substring(numberStart, numberEnd - numberStart + 1)),
            (index.Item1, numberStart));
    }
 
    private static HashSet<Symbol> GetSymbols(List<string> data)
    {
        return new HashSet<Symbol>(data.SelectMany((line, index) => ParseOneLine(line, index)));
    }

    private static HashSet<Symbol> ParseOneLine(string line, int lindeIndex)
    {
        var symbols = new HashSet<Symbol>();
        for (int index = 0; index < line.Length; index++)
        {
            var ch = line[index];
            if (ch.Equals('.') || Char.IsDigit(ch))
            {
                continue;
            }

            symbols.Add(new Symbol(lindeIndex, index));
        }

        return symbols;
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