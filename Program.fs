//Дерево содержит строки. Найти суммарную длину этих строк. 

open System
open System.IO

type BinTreeNode<'T> =
    | Node of 'T * BinTreeNode<'T> * BinTreeNode<'T>
    | Empty

// добавление нового элемента в дерево
let rec tree_add (newKey, node) =
    match node with
    | Node(key, left, right) ->
        if newKey > key then
            Node(key, left, tree_add (newKey, right))
        else
            Node(key, tree_add (newKey, left), right)
    | Empty -> Node(newKey, Empty, Empty)


let rec tree_print node space =
    match node with
    | Node(key, left, right) ->
        tree_print right (space + "   ")
        printfn "%s%A" space key
        tree_print left (space + "   ")
    | Empty -> ()

// дерево из списка
let rec tree_init (keys, node) =
    match keys with
    | head :: tail -> 
        tree_init ( tail, (tree_add (head,node)) )
    | [] -> node


//let rec tree_getTotalLen node =
//    match node with
//    | Node(key, left, right) ->
//        (String.length key) + tree_getTotalLen left + tree_getTotalLen right
//    | Empty -> 0

//let rec myFold f acc node =
//    match node with
//    | Node(key, left, right) ->
//        myFold f (f key acc) left
//        myFold f (f key acc) right
//        (f key acc) 
//        //myFold func (string.con left + myFold func acc right
//    | Empty -> acc

//let rec tree_getTotalLen node =
//    let Sum a b = 
//        a + b

//    myFold Sum 0 node

let tree_getTotalLen node =
    let rec my_map func node  =
        match node with
        | Node(key, left, right) ->
            Node(func key, my_map func left, my_map func right)
        | Empty -> Empty

    let rec myFold f acc node =
        match node with
        | Node(key, left, right) ->
            f (f key (myFold f acc left)) (myFold f acc right)
        | Empty -> acc

    let LenTree = my_map (String.length) node
    myFold (+) 0 LenTree

// Парсинг с выводом ошибок -----------------------------
let parseFloat s =
    try
        Some (float s)
    with
    | :? System.FormatException ->
        printfn "'%s' не допустимый формат числа" s
        None
    | ex ->
        printfn "ошибка: %s" ex.Message
        None

let parseInt s =
    try
        Some (int s)
    with
    | :? System.FormatException ->
        printfn "'%s' не допустимый формат числа" s
        None
    | ex ->
        printfn "ошибка: %s" ex.Message
        None
// ------------------------------------------------------

// Получение списка float с клавиатуры
let сonsoleReadStrings mess =
    Console.Clear()
    printf "%s >> " mess
    let line = Console.ReadLine()

    if line = "" then
        Some []
    else
        let strings =
            // получение списка чисел из строки
            line.Split(' ')
            |> List.ofArray
            |> List.map string

        // отработка ошибки при парсинге
        // список без None станет содержать просто int
        Some (strings)

let readFromFile filePath =
    Console.Clear()
    try
        let lines = File.ReadAllLines(filePath)
        if lines = [||] then
            Some []
        else
            let strings = 
                List.ofArray lines
                |> List.map string

            Some strings
    with
    | :? FileNotFoundException -> 
        Console.Clear()
        printfn "Файл не найден" 
        None
    | ex -> 
        Console.Clear()
        printf "Ошибка при чтении файла: %s" ex.Message
        printfn ""
        None

let randomGen () =
    // Ввод числа int >= 0
    let rec readSize mess =
        Console.Clear()
        printf "%s >> " mess
        let s = Console.ReadLine()
        try
            let num = int s
            if num >= 0 then
                Some num
            else
                printfn "число '%s' не допустимый размер, возможные: 0 1 2 ..." s
                None
        with
            | :? System.FormatException ->
                printfn "'%s' не допустимый формат числа" s
                None
            | :? System.OverflowException ->
                printfn "число '%s' слишком большое" s
                None
            | ex ->
                printfn "ошибка: %s" ex.Message
                None

    let rec getRandStr (rand: Random) n =
        if n > 0 then
            string ( char (rand.Next(32, 126)) ) + (getRandStr rand (n-1))
        else
            ""

    let rec getRandStrings n maxLen =
        if n > 0 then
            let rand = Random()
            getRandStr rand (rand.Next(1, maxLen + 1)) :: (getRandStrings (n-1) maxLen)
        else
            []


    let num =  readSize "Введите кол-во случайных строк в списке"
    if num <> None then
        Some (getRandStrings (Option.get num) 20)
    else
        None

// ============================================================================

let rec getStrings mess =
    Console.Clear()
    printfn "%s" mess
    printfn "   [1] Ввести с клавиатуры"
    printfn "   [2] Сгенерировать случайно"
    printfn "   [3] Из файла"
    printfn "   [Esc] <- назад"

    match Console.ReadKey(true).Key with
    | ConsoleKey.D1 -> сonsoleReadStrings "введите строки через пробел"
    | ConsoleKey.D2 -> randomGen ()
    | ConsoleKey.D3 -> readFromFile "test.txt"
    | ConsoleKey.Escape -> None
    | _ -> getStrings (mess)

let rec readFloat mess =
    printf "%s >> " mess
    parseFloat (Console.ReadLine())

let rec tree_mapDiv (node, divider) =
    match node with
    | Node(key, left, right) ->
        Node((float key)/divider, tree_mapDiv (left, divider), tree_mapDiv (right, divider))
    | Empty -> Empty

let rec main () =
    let stringsOrNone = getStrings "Выберете метод получения значений для дерева"
    if stringsOrNone <> None then
        let strings = Option.get stringsOrNone

        Console.Clear()
        printfn "Получены строки: %A" strings

        let str_tree = tree_init (strings, Empty)
        printfn "Дерево:"
        tree_print str_tree ""

        printfn "Суммарная длина строк = %d" (tree_getTotalLen str_tree)

    // выход
    printf        "Любую клавишу для повторного ввода, для выхода Esc..."
    if (Console.ReadKey(true).Key <> ConsoleKey.Escape) then
        printfn "\r                                                            "
        main ()
    else
        printfn "\rПрограмма приостановлена                                    "


// =========== ТОЧКА ВХОДА ===========
main ()
