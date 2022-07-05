(*A estratégia abordada, resumidamente, é guardar as restrições num array; o tabuleiro, inicialmente, tem todas as posições a 0 (=vazio), depois
posição-a-posição, vai-se meter tentar meter um valor minimo, se não for possivel a célula passa a 0, caso toda o tabuleiro for impossível de preencher
é considerado que é impossível resolver o jogo nas condições dadas.   
*)

(*--------------------------------------------------------------área de input--------------------------------------------------------------*)
(*-> size: tamanho do tabuleiro, além disso é o valor máximo possível numa célula do tabuleiro
  -> restrictions: número de restrições
  -> restrict_matrix: array onde são guardadas as restrições
  -> restrict_matrix_maker: permite modificar a matriz restrict_matrix com as restrições inseridas
  *)
let size = read_int()
let restrictions = read_int ()
   
let restrict_matrix = Array.make restrictions (0,0,0,0)

let restrict_matrix_maker =
  for i=0 to restrictions-1 do
    Scanf.sscanf (read_line ()) "%d %d %d %d" (fun a b c d -> restrict_matrix.(i) <- (a, b, c, d))
 done

  
(*-----------------------------------------------------------------------------------------------------------------------------------------*)
(*-> board: tabuleiro onde inicialmente está tudo a 0 (ou seja, vazio) e onde serão depois preenchidas as células até à solução (ou não)*)
let board = Array.make_matrix size size 0       


(*Função restr - val restr : int array array -> (int * int * int * int) array -> bool
   Esta função recebe uma matriz e um array com as restrições, se ambas as entradas da matriz foram não nulas,
   então é verificado se estas seguem as restrições, se não seguirem é devolvido falso, se pelo menos uma das entradas for nula
   então é considerado como "seguir as restrições", visto que ainda estão vazias *)    
    
let restr matrix matrix_restr =
    let return = [|true|] in
    ((try
    for i=0 to restrictions-1 do
      let (x1,y1,x2,y2) = matrix_restr.(i) in
        if matrix.(x1).(y1) <> 0 && matrix.(x2).(y2)<>0 then 
        if matrix.(x1).(y1) <= matrix.(x2).(y2) then raise Exit
    done
  with Exit -> return.(0)<- false)
  ; return.(0))    

(*Função checkline - val checkline : 'a array array -> int -> 'a -> bool
   Esta função recebe uma matriz, o número linha (x) e um valor que se for encontrado na nessa linha a função irá retornar falso, 
   caso contrário verdadeiro *)

let checkline matrix x value =
    let return = [|true|] in
    ((try
    for i=0 to size-1 do
      if matrix.(x).(i)= value then raise Exit
    done
  with Exit -> return.(0) <- false
    ); return.(0))

(*Função checkcoluna - val checkcoluna : 'a array array -> int -> 'a -> bool
   Esta função recebe uma matriz, o número coluna (y) e um valor que se for encontrado na nessa coluna a função irá retornar falso, 
   caso contrário verdadeiro *)

let checkcoluna matrix y value =   
    let return = [|true|] in
    ((try   
    for i=0 to size-1 do
      if matrix.(i).(y)= value then raise Exit
    done
  with Exit -> return.(0) <- false)
    ; return.(0))  

(*Função isOk - basicamente, é uma função que junta todas as anteriores, permite simplificar a escrita para uso posterior*)
let isOk matrix x y matrix_restr value = (checkline matrix x value) && (checkcoluna matrix y value) &&( restr matrix matrix_restr )&& ( matrix.(x).(y) )=0

(*Função findEmpty - val findEmpty : int array array -> int -> int -> int * int
   Esta função recebe uma matriz e dois valores (linha e coluna), devolve as coordenadas da primeira célula da matriz com valor 0,
   se não houverem mais células com valor 0, então é devolvido um valor impossível (-1,-1) que simboliza que não há mais
   células vazias*)

let rec findEmpty matrix linha coluna =
  match matrix.(linha).(coluna) with
  | 0 -> (linha,coluna)
  | _ -> 
    match linha, coluna with
    | linha, coluna when coluna < size-1 -> findEmpty matrix linha (coluna+1)
    | linha, coluna when linha < size-1 && coluna = size-1 -> findEmpty matrix (linha+1) 0
    | _ -> (-1,-1)

(*Função solve - val solve : int array array -> (int * int * int * int) array -> bool
   Esta função recebe uma matriz e um array de restrições. Primeiro, a função obtem o as coordenadas de uma
   célula vazia, (se essas coordenadas não forem (-1,-1), pois estas simbolizam a ausência de espaços vazios).
   Depois, no ciclo-for será tentado meter valores de 1 a size (tamanho do tabuleiro), se for possível meter algum
   destes valores, então é metido; depois é chamado novamente a função solve (recursivamente) e se for possível meter
   na próxima posição livre um valor entre 1 a size até ao fim, então a função devolve verdadeiro, se não for possível então 
   é devolvido falso*)

let rec solve matrix matrix_restr =
  let return = [|false|] in
  let (x,y) = findEmpty matrix 0 0 in (*coordenadas de posição livre*)
  match (x,y) with
  | x, y when x = -1 && y = -1 -> true (*se estiver completo, então é logo devolvido true; condição de paragem*)
  | _-> 
    (try
    for i=1 to size do
    if (isOk matrix x y matrix_restr i) = true (*se é possível meter o valor, então prossegue para o *then*,
                                                 caso contrário será feito loop até valor size*)
    then( (matrix.(x).(y)<-i;   (*o valor é inserido*)
      if (solve matrix matrix_restr) = true (*a função é chamada recursivamente até ao fim*)
         then raise Exit
      else matrix.(x).(y)<-0 )) (*um tabuleiro impossível terá as células a 0*)
    done
  with Exit -> return.(0) <- true); return.(0) (*retorno*)

(*Função print - val print : int array array -> unit
   Esta função recebe uma matriz e imprime-a se a solução for válida, caso contrário é impresso "IMPOSSIBLE"*)  

let print board =
  if (solve board restrict_matrix) = true then ((*se houver solução, então é impressa*)
  for i = 0 to size-1 do
    for j = 0 to size-1 do
      if j <> size-1 then Printf.printf "%d " board.(i).(j)
      else Printf.printf "%d" board.(i).(j)
    done; print_newline()
  done
  )
else ( print_string "IMPOSSIBLE"; print_newline() )

let _ = print board (*impressão de output*)

(*Exemplo de input:
4
3
0 0 1 0
0 1 0 0
2 1 3 1

Output:
2 3 1 4 
1 2 4 3 
3 4 2 1 
4 1 3 2 *)
