Caml1999N030����            -lib/apple.mli����  4    �  T�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����)snake_lib��.<command-line>A@A�A@J@@��A@@�A@K@@@@�@@����������������/ppx_optcomp.env�@�@@�������#env��&_none_A@ �A@@���(flambda2����'Defined�����%false@@@@@���/flambda_backend��������%false@@@@@���-ocaml_version���&�������!4@.@@����"13@3@@����!1@8@@@8@@8@@@8@@@�@@�������@�@@@�@@�@@@�@@�@@@@�@@@�@��������$Base��-lib/apple.mliA@F�A@J@@��A@@�A@J@@��A@@�A@J@������%Color��CLS�CLX@�����A�    �!t��D_f�D_g@@@��Р%Green��%D_j�&D_o@�@@��)D_j�*D_o@@�Р$Blue��0D_r�1D_v@�@@��4D_p�5D_v@@@A@@��7D_a�8D_v@@��:D_a�;D_v@@��=CL[�>Ewz@@@��@CLL�AEwz@��CCLL�DEwz@���A�    �!t��MG| A�NG| B@@@@A@���(deriving��TG| F�UG| N@��������'sexp_of��`G| O�aG| V@��cG| O�dG| V@@@@��fG| O�gG| V@@��iG| C�jG| W@@��lG||�mG| W@@��oG||�pG| W@�����������-ocaml.warning������#-32�@�@@@�@ʰA���Р)sexp_of_t@��@����GF@F@@������1Ppx_sexp_conv_lib$Sexp!t�ONA@@@@@@@11@@���)ocaml.doc吠�����'@inline�@�@@@�@����+merlin.hide�@�@(���Р&create���N����N��@���%board�����%Board!t���N����N��@@���N����N��@@@���&snake1�����%Snake!t���N����N��@@���N����N��@@@���&snake2�����%Snake!t���N����N��@@���N����N��@@@����&option���N����N��@��������!t���N����N��@@���N����N��@@@�����!t��N���N��@@��
N���N��@@@@��N���N��@@@@��N���N��@@@��N���N��@@@��N���N��@@@��N���N��@@@@���)ocaml.doc��@@ ��@@ �A�������
  1 [create] takes in a [Board.t] representing the area in which an apple can be
    generated, as well as a [Snake.t], and creates an [Apple.t] such that it appears in
    the board's play area and does not overlap with the snake.

    [create] returns [None] if there are no valid positions for the apple. ��+I Y Y�,MB�@@��.I Y Y�/MB�@@@@��1I Y Y�2MB�@@��4I Y Y�5MB�@@��7N���8N��@��:N���;N��@���Р(location��CQ$(�DQ$0@��@����!t��MQ$3�NQ$4@@��PQ$3�QQ$4@@@�����(Position!t��ZQ$8�[Q$B@@��]Q$8�^Q$B@@@��`Q$3�aQ$B@@@@���G��d@@ ��e@@ �A�������	< [location] returns the location of the apple on the board. ��qP���rP�#@@��tP���uP�#@@@@��wP���xP�#@@��zP���{P�#@@��}Q$$�~Q$B@���Q$$��Q$B@���Р%color���Trv��Tr{@��@����!t���Tr~��Tr@@���Tr~��Tr@@@�����%Color!t���Tr���Tr�@@���Tr���Tr�@@@���Tr~��Tr�@@@@�������@@ ���@@ �A�������	( [color] returns the color of the apple ���SDD��SDq@@���SDD��SDq@@@@���SDD��SDq@@���SDD��SDq@@���Trr��Tr�@���Trr��Tr�@���Р.amount_to_grow���X����X�
@��@����!t���X���X�@@���X���X�@@@����#int���X���X�@@���X���X�@@@���X���X�@@@@���Ѱ��@@ ���@@ �A�������	f [amount_to_grow] returns the number of squares the snake should grow by due to eating
    this apple ���V����W��@@���V����W��@@@@��V���W��@@��V���W��@@��X���X�@��
X���X�@@