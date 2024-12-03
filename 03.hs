import Text.Regex

main = getContents >>= putStrLn . show . matchRegex (mkRegex "mul\\(\\d*,\\d*)")
