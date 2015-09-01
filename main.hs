import qualified List as List
main =
     print $ 1 `List.append`(2 `List.append` (3 `List.append` (
             4 `List.append` List.EmptyList)))
