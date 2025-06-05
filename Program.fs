open System

// 1. Определяем тип дерева
type Tree<'a> =
    | Leaf
    | Node of 'a * Tree<'a> * Tree<'a>
    // Функция для преобразования дерева (добавление цифры в конец числа)
  let rec mapTree f tree =
        match tree with
        | Leaf -> Leaf
        | Node(value, left, right) ->
            Node(f value, mapTree f left, mapTree f right)
    // Функция для проверки, содержит ли число цифру
    let containsDigit digit number =
        let rec check num =
            if num = 0 then false
            else (abs num % 10 = digit) || check (abs num / 10)
        check number
    let rec countNodesWithDigit tree digit =
        match tree with
        | Leaf -> 0
        | Node(value, left, right) ->
            let current = if containsDigit digit value then 1 else 0
            current + countNodesWithDigit left digit + countNodesWithDigit right digit
  
    let appendDigit digit number =
        number * 10 + (abs digit % 10) 

// 2. Функция создания дерева (упрощённый вариант)
let rec createTree () =
    printf "Введите число (или 'L' для Leaf): "
    match Console.ReadLine() with
    | "L" -> 
        printfn "Создан Leaf"
        Leaf
    | input ->
        match Int32.TryParse(input) with
        | true, number ->
            printfn $"Создаём левое поддерево для {number}"
            let left = createTree()
            
            printfn $"Создаём правое поддерево для {number}"
            let right = createTree()
            
            Node(number, left, right)
        | _ ->
            printfn "Ошибка! Введите число или 'L'"
            createTree()

//Функция добавления цифры к числам
let rec inputDigit() =
    printf "Введите цифру (0-9), которую нужно добавить: "
    match Int32.TryParse(Console.ReadLine()) with
    | true, digit when digit >= 0 && digit <= 9 -> digit
    | _ -> 
        printfn "Ошибка: нужно ввести цифру от 0 до 9!"
        inputDigit()
// Ввод цифры для поиска (0-9)
  let rec inputDigit2() =
      printf "Введите цифру (0-9) для поиска в дереве: "
      match Int32.TryParse(Console.ReadLine()) with
      | true, digit when digit >= 0 && digit <= 9 -> digit
      | _ -> 
          printfn "Ошибка: нужно ввести цифру от 0 до 9!"
          inputDigit2()
// Функция печати дерева
let drawTree tree =
    let rec loop depth pos tree =
        match tree with
        | Leaf -> ()
        | Node(value, left, right) ->
          
            Console.SetCursorPosition(pos, depth * 2)
            printf "%d" value
            
        
            let offset = 5 / (pown 2 depth)
            let leftPos = pos - offset
            let rightPos = pos + offset
            
           
            if left <> Leaf then
                Console.SetCursorPosition(pos - offset/2, depth*2 + 1)
                printf "/"
                loop (depth + 1) leftPos left
            
          
            if right <> Leaf then
                Console.SetCursorPosition(pos + offset/2, depth*2 + 1)
                printf "\\"
                loop (depth + 1) rightPos right
    
    Console.Clear()
    loop 0 (Console.WindowWidth / 2) tree

[<EntryPoint>]
let main argv =
    printfn "Создаем дерево:"
    let tree = createTree()
    
    printfn "\nИсходное дерево:"
    drawTree tree
    Console.SetCursorPosition(0, Console.CursorTop + 5)
    
    let digit = inputDigit()
    let transformedTree = mapTree (fun x -> appendDigit digit x) tree
    
    printfn "\nПреобразованное дерево (к каждому числу добавлена цифра %d):" digit
    drawTree transformedTree
    
    Console.SetCursorPosition(0, Console.CursorTop + 5)
    
    let digit = inputDigit2()
    let count = countNodesWithDigit tree digit
    
    printfn $"Количество узлов, содержащих цифру {digit}: {count}"
    
    printfn "\nНажмите любую клавишу для выхода..."
    Console.ReadKey() |> ignore
    0












    

    
    
    

     
    
   
    
  
    

        

