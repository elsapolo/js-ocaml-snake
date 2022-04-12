Caml1999N030����            ,lib/game.mli����  �  �    j�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����)snake_lib��.<command-line>A@A�A@J@@��A@@�A@K@@@@�@@����������������/ppx_optcomp.env�@�@@�������#env��&_none_A@ �A@@���(flambda2����'Defined�����%false@@@@@���/flambda_backend��������%false@@@@@���-ocaml_version���&�������!4@.@@����"13@3@@����!1@8@@@8@@8@@@8@@@�@@�������@�@@@�@@�@@@�@@�@@@@�@@@�@��������$Base��,lib/game.mliA@F�A@J@@��A@@�A@J@@��A@@�A@J@���A�    �!t��E s x�E s y@@@@A@���)ocaml.doc��@@ ��@@ �A�������	a A [t] represents the entire game state, including the current snake, apple,
    and game state. ��#CLL�$D \ r@@��&CLL�'D \ r@@@@��)CLL�*D \ r@@��,CLL�-D \ r@���(deriving��3E s }�4E s �@��������'sexp_of��?E s ��@E s �@��BE s ��CE s �@@@@��EE s ��FE s �@@��HE s z�IE s �@@��KE s s�LE s �@@��NE s s�OE s �@�����������-ocaml.warning��������#-32�@�@@@�@��A���Р)sexp_of_t[��@����ba@a@@������1Ppx_sexp_conv_lib$Sexp!t�jiA@@@@@@@11@@���)ocaml.docĐ������'@inline�@�@@@�@̠��+merlin.hideА@�@(���Р)to_string���H � ���H � �@��@����!t���H � ���H � �@@���H � ���H � �@@@����&string���H � ���H � �@@���H � ���H � �@@@���H � ���H � �@@@@�������@@ ���@@ �A�������	3 Used for pretty-printing game contents for tests. ���G � ���G � �@@���G � ���G � �@@@@���G � ���G � �@@���G � ���G � �@@���H � ���H � �@���H � ���H � �@���Р&create���K$(��K$.@���&height����#int���K$8��K$;@@���K$8��K$;@@@���%width����#int���K$E��K$H@@���K$E��K$H@@@���4initial_snake_length����#int��K$a�K$d@@��K$a�K$d@@@����!t��K$h�K$i@@��K$h�K$i@@@��K$L�K$i@@@��K$?�K$i@@@��K$1�K$i@@@@�����@@ ��@@ �A�������	8 [create] creates a new game with specified parameters. ��)J � ��*J �#@@��,J � ��-J �#@@@@��/J � ��0J �#@@��2J � ��3J �#@@��5K$$�6K$i@��8K$$�9K$i@���Р&snake1��AN���BN��@��@����!t��KN���LN��@@��NN���ON��@@@�����%Snake!t��XN���YN��@@��[N���\N��@@@��^N���_N��@@@@���M��b@@ ��c@@ �A�������	; [snake] returns the snake1 that is currently in the game. ��oMkk�pMk�@@��rMkk�sMk�@@@@��uMkk�vMk�@@��xMkk�yMk�@@��{N���|N��@��~N���N��@���Р&snake2���Q��Q@��@����!t���Q��Q@@���Q��Q@@@�����%Snake!t���Q��Q!@@���Q��Q!@@@���Q��Q!@@@@�������@@ ���@@ �A�������	; [snake] returns the snake2 that is currently in the game. ���P����P�@@���P����P�@@@@���P����P�@@���P����P�@@���Q��Q!@���Q��Q!@���Р&apples���Tdh��Tdn@��@����!t���Tdq��Tdr@@���Tdq��Tdr@@@��������%Apple!t���Tdv��Td}@@���Tdv��Td}@@@������%Apple!t���Td���Td�@@���Td���Td�@@@@���Tdv��Td�@@@���Tdq��Td�@@@@������@@ ��@@ �A�������	; [snake] returns the apples that is currently in the game. ��S##�S#c@@��S##�S#c@@@@��S##�S#c@@��S##�S#c@@��Tdd�Td�@��Tdd�Td�@���Р&score1��'W���(W��@��@����!t��1W���2W��@@��4W���5W��@@@����#int��<W���=W��@@��?W���@W��@@@��BW���CW��@@@@���1��F@@ ��G@@ �A�������	; [snake] returns the score1 that is currently in the game. ��SV���TV��@@��VV���WV��@@@@��YV���ZV��@@��\V���]V��@@��_W���`W��@��bW���cW��@���Р&score2��kZ"&�lZ",@��@����!t��uZ"/�vZ"0@@��xZ"/�yZ"0@@@����#int���Z"4��Z"7@@���Z"4��Z"7@@@���Z"/��Z"7@@@@���u���@@ ���@@ �A�������	; [snake] returns the score2 that is currently in the game. ���Y����Y�!@@���Y����Y�!@@@@���Y����Y�!@@���Y����Y�!@@���Z""��Z"7@���Z""��Z"7@���Р*handle_key���^����^��@��@����!t���^����^��@@���^����^��@@@��@����$char���^����^��@@���^����^��@@@����$unit���^����^��@@���^����^��@@@���^����^��@@@���^����^��@@@@���ɰ��@@ ���@@ �A�������	v [handle_key] will be called whenever the user presses a key.  It takes that key and
    updates the game accordingly ���\99��]��@@���\99��]��@@@@���\99��]��@@���\99��]��@@���^����^��@���^����^��@���Р*game_state��a�a"@��@����!t��a%�a&@@��a%�a&@@@�����*Game_state!t��a*�a6@@��a*�a6@@@�� a%�!a6@@@@�����$@@ ��%@@ �A�������	5 [game_state] returns the state of the current game. ��1`���2`�@@��4`���5`�@@@@��7`���8`�@@��:`���;`�@@��=a�>a6@��@a�Aa6@���Р$step��Id���Jd��@��@����!t��Sd���Td��@@��Vd���Wd��@@@����$unit��^d���_d��@@��ad���bd��@@@��dd���ed��@@@@���S��h@@ ��i@@ �A�������	J [step] is called in a loop, and the game is re-rendered after each call. ��uc88�vc8�@@��xc88�yc8�@@@@��{c88�|c8�@@��~c88�c8�@@���d����d��@���d����d��@@