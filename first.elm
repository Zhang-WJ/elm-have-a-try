main : Signal Element
main = map showmouse Mouse.position

showmouse : (Int, Int) -> Element
showmouse (x, y) = join "," [toString x, toString y] |> show