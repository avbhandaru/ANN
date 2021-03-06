Caml1999N022����            .lib/matrix.mli����  V  +  9�  7������1ocaml.ppx.context��&_none_A@ �A����������)tool_name���.migrate_driver@@����,include_dirs����"[]@@����)load_path!����
%@%@����,open_modules*����.@.@����+for_package3����$None8@8@����%debug=����%falseB@B@����+use_threadsG����
K@K@����-use_vmthreadsP����T@T@����'cookiesY����B]@]@@@]@@]@]���A�    �!t��.lib/matrix.mliCW\�CW]@@@@A@���)ocaml.doc��lA@ ��mA@ �A�������0 Type of matrix @��BAA�BAV@@@��BAA�BAV@@@��CWW�CW]@@��CWW�CW]@���Р%zeros��%F _ c�&F _ h@���!m����#int��1F _ m�2F _ p@@��4F _ m�5F _ p@@���!n����#int��@F _ v�AF _ y@@��CF _ v�DF _ y@@����!t��KF _ }�LF _ ~@@��NF _ }�OF _ ~@@��QF _ t�RF _ ~@@��TF _ k�UF _ ~@@@���S���A@ ���A@ �A�������	: [zeros ~m ~n] is a matrix full of zeros of size [m x n]. @��eE__�fE_ ^@@@��hE__�iE_ ^@@@��kF _ _�lF _ ~@��nF _ _�oF _ ~@���Р$ones��wI � ��xI � �@���!m����#int���I � ���I � �@@���I � ���I � �@@���!n����#int���I � ���I � �@@���I � ���I � �@@����!t���I � ���I � �@@���I � ���I � �@@���I � ���I � �@@���I � ���I � �@@@������A@ ��A@ �A�������	8 [ones ~m ~n] is a matrix full of ones of size [m x n]. @���H � ���H � �@@@���H � ���H � �@@@���I � ���I � �@���I � ���I � �@���Р&random���L'+��L'1@���!m����#int���L'6��L'9@@���L'6��L'9@@���!n����#int���L'?��L'B@@���L'?��L'B@@����!t���L'F��L'G@@���L'F��L'G@@���L'=��L'G@@���L'4��L'G@@@������bA@ ��cA@ �A�������	C [random ~m ~n] is a matrix full of random floats of size [m x n]. @��	K � ��
K �&@@@��K � ��K �&@@@��L''�L'G@��L''�L'G@���Р'of_list��O��O�@��@����$list��%O��&O�@�����$list��.O��/O�@�����%float��7O��8O�@@��:O��;O�@@@��=O��>O�@@@��@O��AO�@@����!t��HO��IO�@@��KO��LO�@@��NO��OO�@@@���M���A@ ���A@ �A�������	0 [of_list l] is a matrix representation of [l]. @��_NII�`NI~@@@��bNII�cNI~@@@��eO�fO�@��hO�iO�@���Р+to_row_list��qR���rR��@��@����!t��{R���|R��@@��~R���R��@@����$list���R����R��@�����!t���R����R��@@���R����R��@@@���R����R��@@���R����R��@@@������A@ ��A@ �A�������	6 [to_row_list m] is a list of each row in matrix [m]. @���Q����Q��@@@���Q����Q��@@@���R����R��@���R����R��@���Р*mat_values���UFJ��UFT@���!m����#int���UFY��UF\@@���UFY��UF\@@���!n����#int���UFb��UFe@@���UFb��UFe@@���!v����%float���UFk��UFp@@���UFk��UFp@@����!t���UFt��UFu@@���UFt��UFu@@���UFi��UFu@@���UF`��UFu@@���UFW��UFu@@@������fA@ ��gA@ �A�������	B [mat_values m n v] is a matrix full of float [v] of size [m x n] @��T���T�E@@@��T���T�E@@@��UFF�UFu@��UFF�UFu@���Р&equals��Y��� Y��@��@����!t��)Y���*Y��@@��,Y���-Y��@@��@����!t��6Y���7Y��@@��9Y���:Y��@@����$bool��AY���BY��@@��DY���EY��@@��GY���HY��@@��JY���KY��@@@���I���A@ ���A@ �A�������	X [equals m1 m2] is true if [m1] is the same (structurally and same values) as
    [m2]. @��[Www�\X��@@@��^Www�_X��@@@��aY���bY��@��dY���eY��@���Р#sep��m\6:�n\6=@��@����!t��w\6@�x\6A@@��z\6@�{\6A@@�������!t���\6F��\6G@@���\6F��\6G@@�����!t���\6J��\6K@@���\6J��\6K@@@���\6F��\6K@@���\6@��\6L@@@������A@ ��A@ �A�������	> [sep m] separates the augmented matrix [a | m] into [(a, m)] @���[����[�5@@@���[����[�5@@@���\66��\6L@���\66��\6L@���Р&return���`����`��@��@����&option���`����`��@�����!t���`����`��@@���`����`��@@@���`����`��@@����!t���`����`��@@���`����`��@@���`����`��@@@�����NA@ ��OA@ �A�������	Z [return to] takes [t] out of the option if possible, raising
    an exception otherwise. @���^NN��_��@@@���^NN��_��@@@���`����`��@���`����`��@���Р)fold_left��dcg�dcp@��@��@��!a��dct�dcv@@��@����%float��dcz�dc@@��dcz�dc@@��!a��$dc��%dc�@@��'dcz�(dc�@@��*dct�+dc�@@��@��!a��2dc��3dc�@@��@����!t��<dc��=dc�@@��?dc��@dc�@@��!a��Edc��Fdc�@@��Hdc��Idc�@@��Kdc��Ldc�@@��Ndcs�Odc�@@@���M���A@ ���A@ �A�������	� [fold_left fun acc t] returns the result of a fold_left function with 
    anonymous function fun and initial accumulator acc applied to matrix t @��_b���`cb@@@��bb���ccb@@@��edcc�fdc�@��hdcc�idc�@���Р#set��qi"�ri%@��@����!t��{i(�|i)@@��~i(�i)@@��@����%float���i-��i2@@���i-��i2@@���!i����#int���i8��i;@@���i8��i;@@���!j����#int���iA��iD@@���iA��iD@@����&option���iJ��iP@�����!t���iH��iI@@���iH��iI@@@���iH��iP@@���i?��iP@@���i6��iP@@���i-��iP@@���i(��iP@@@���˰�6A@ ��7A@ �A�������	D [set m v ~i ~j] replaces cell [i, j] with value [v] in matrix [m]. @���h����h�@@@���h����h�@@@���i��iP@���i��iP@���Р&height���l����l��@��@����!t���l����l��@@���l����l��@@����#int��l���l��@@��l���l��@@��
l���l��@@@���	��tA@ ��uA@ �A�������	2 [height a] where [a] is an [m x n] matrix is [m] @��kRR�kR�@@@��kRR�kR�@@@��!l���"l��@��$l���%l��@���Р%width��-o���.o��@��@����!t��7o���8o��@@��:o���;o��@@����#int��Bo���Co��@@��Eo���Fo��@@��Ho���Io��@@@���G���A@ ���A@ �A�������	1 [width a] where [a] is an [m x n] matrix is [n] @��Yn���Zn��@@@��\n���]n��@@@��_o���`o��@��bo���co��@���Р'entries��kr*.�lr*5@��@����!t��ur*8�vr*9@@��xr*8�yr*9@@����#int���r*=��r*@@@���r*=��r*@@@���r*8��r*@@@@�������A@ ���A@ �A�������	6 [entries t] returns the number of entries in matrix t@���q����q�)@@@���q����q�)@@@���r**��r*@@���r**��r*@@���Р+size_equals���u����u��@��@����!t���u����u��@@���u����u��@@��@����!t���u����u��@@���u����u��@@����$bool���u����u��@@���u����u��@@���u����u��@@���u����u��@@@���Ӱ�>A@ ��?A@ �A�������	@ [width m1 m2] is [true] if m1 has the same dimensions as [m2]. @���tCC��tC�@@@���tCC��tC�@@@���u����u��@���u����u��@���Р#map���y&*��y&-@��@��@����%float��y&1�y&6@@��y&1�y&6@@����%float��y&:�y&?@@��y&:�y&?@@��y&1�y&?@@��@����!t��y&D�y&E@@��!y&D�"y&E@@����!t��)y&I�*y&J@@��,y&I�-y&J@@��/y&D�0y&J@@��2y&0�3y&J@@@���1���A@ ���A@ �A�������	u[map f m] is the map for [f] on [m].
   Requires: [m] is an m x 1 matrix where m is the number of nodes in the layer @��Cw���Dx�%@@@��Fw���Gx�%@@@��Iy&&�Jy&J@��Ly&&�My&J@���Р$map2��U}���V}��@��@��@����%float��a}���b}��@@��d}���e}��@@��@����%float��n}���o}��@@��q}���r}��@@����%float��y}���z}��@@��|}���}}��@@��}����}��@@���}����}��@@��@����!t���}����}��@@���}����}��@@��@����!t���}����}��@@���}����}��@@����&option���}����}��@�����!t���}����}��@@���}����}��@@@���}����}��@@���}����}��@@���}����}��@@���}����}��@@@������&A@ ��'A@ �A�������	e [map2 f m1 m2] is a matrix [m] where every [i, j] in [m] is equal to
    [f m1_i m2_i, f m1_j m2_j] @���{LL��|��@@@���{LL��|��@@@���}����}��@���}����}��@���Р$iter��� @FJ�� @FN@��@��@����%float��� @FR�� @FW@@��� @FR�� @FW@@����$unit��� @F[�� @F_@@��� @F[�� @F_@@��� @FR�� @F_@@��@����!t�� @Fd� @Fe@@��	 @Fd�
 @Fe@@����$unit�� @Fi� @Fm@@�� @Fi� @Fm@@�� @Fd� @Fm@@�� @FQ� @Fm@@@������A@ ���A@ �A�������	M [iter f m] applies [f] to every element in [m]. [f] needs to return [unit]. @��+���,�E@@@��.���/�E@@@��1 @FF�2 @Fm@��4 @FF�5 @Fm@���Р#sum��= C���> C��@��@����!t��G C���H C��@@��J C���K C��@@����%float��R C���S C��@@��U C���V C��@@��X C���Y C��@@@���W���A@ ���A@ �A�������	. [sum m1] is the sum of every element in [m1] @��i Boo�j Bo�@@@��l Boo�m Bo�@@@��o C���p C��@��r C���s C��@���Р#add��{ G		!�| G		$@��@����!t��� G		'�� G		(@@��� G		'�� G		(@@��@����!t��� G		,�� G		-@@��� G		,�� G		-@@����&option��� G		3�� G		9@�����!t��� G		1�� G		2@@��� G		1�� G		2@@@��� G		1�� G		9@@��� G		,�� G		9@@��� G		'�� G		9@@@������A@ ��A@ �A�������	^ [add m1 m2] is the matrix addition of [m1] and [m2].
    [m1] must be the same size as [m2]. @��� E���� F�	@@@��� E���� F�	@@@��� G		�� G		9@��� G		�� G		9@���Р#sub��� J	z	~�� J	z	�@��@����!t��� J	z	��� J	z	�@@��� J	z	��� J	z	�@@��@����!t��� J	z	��� J	z	�@@��� J	z	��� J	z	�@@����&option��� J	z	��� J	z	�@�����!t��  J	z	�� J	z	�@@�� J	z	�� J	z	�@@@�� J	z	�� J	z	�@@��	 J	z	��
 J	z	�@@�� J	z	�� J	z	�@@@�����vA@ ��wA@ �A�������	9 [sub m1 m2] is the matrix subtraction of [m1] and [m2]. @�� I	;	;� I	;	y@@@��  I	;	;�! I	;	y@@@��# J	z	z�$ J	z	�@��& J	z	z�' J	z	�@���Р&square��/ O
@
D�0 O
@
J@��@����!t��9 O
@
M�: O
@
N@@��< O
@
M�= O
@
N@@��@����#int��F O
@
R�G O
@
U@@��I O
@
R�J O
@
U@@����!t��Q O
@
Y�R O
@
Z@@��T O
@
Y�U O
@
Z@@��W O
@
R�X O
@
Z@@��Z O
@
M�[ O
@
Z@@@���Y���A@ ���A@ �A�������	� [square m n] converts the matrix [m] into a square matrix with side length 
    [n], where [n] is greater than the original max_dimension of the matrix 
    [m] @��k L	�	��l N
5
?@@@��n L	�	��o N
5
?@@@��q O
@
@�r O
@
Z@��t O
@
@�u O
@
Z@���Р*quad_split��} R
�
��~ R
�
�@��@����!t��� R
�
��� R
�
�@@��� R
�
��� R
�
�@@�������!t��� R
�
��� R
�
�@@��� R
�
��� R
�
�@@�����!t��� R
�
��� R
�
�@@��� R
�
��� R
�
�@@�����!t��� R
�
��� R
�
�@@��� R
�
��� R
�
�@@�����!t��� R
�
��� R
�
�@@��� R
�
��� R
�
�@@@��� R
�
��� R
�
�@@��� R
�
��� R
�
�@@@������,A@ ��-A@ �A�������	; [quad_split m] splits the matrix into its four quadrants. @��� Q
\
\�� Q
\
�@@@��� Q
\
\�� Q
\
�@@@��� R
�
��� R
�
�@��� R
�
��� R
�
�@���Р)quad_conn��� U�� U"@��@�������!t��� U%�� U&@@��� U%�� U&@@�����!t��� U)�� U*@@�� U)� U*@@�����!t��
 U-� U.@@�� U-� U.@@�����!t�� U1� U2@@�� U1� U2@@@�� U%� U2@@����!t��$ U6�% U7@@��' U6�( U7@@��* U%�+ U7@@@���)���A@ ���A@ �A�������	M [quad_conn (q1, q2, q3, q4)] conjoins all quadrants into one larger matrix. @��; T
�
��< T
�@@@��> T
�
��? T
�@@@��A U�B U7@��D U�E U7@���Р$mult��M Y���N Y��@��@����!t��W Y���X Y��@@��Z Y���[ Y��@@��@����!t��d Y���e Y��@@��g Y���h Y��@@����!t��o Y���p Y��@@��r Y���s Y��@@��u Y���v Y��@@��x Y���y Y��@@@���w���A@ ���A@ �A�������	� [mult m1 m2] is the matrix multiplication of [m1] and [m2].
    Raises: Failure "Bad Dimensions" if the dimensions of [m1] and [m2] are not correct @��� W99�� Xy�@@@��� W99�� Xy�@@@��� Y���� Y��@��� Y���� Y��@���Р%mult_��� [���� [��@��@����!t��� [���� [��@@��� [���� [��@@��@����!t��� [���� [��@@��� [���� [��@@����!t��� [��� [�@@��� [��� [�@@��� [���� [�@@��� [���� [�@@@@��� [���� [�@��� [���� [�@���Р#dot��� ^NR�� ^NU@��@����!t��� ^NX�� ^NY@@��� ^NX�� ^NY@@��@����!t��� ^N]�� ^N^@@��� ^N]�� ^N^@@����%float��� ^Nb�� ^Ng@@��� ^Nb�� ^Ng@@��� ^N]�� ^Ng@@��	  ^NX�	 ^Ng@@@������	jA@ ��	kA@ �A�������	C [dot m1 m2] takes the dot product of matrix [m1] and matrix [m2]. @��	 ]�	 ]M@@@��	 ]�	 ]M@@@��	 ^NN�	 ^Ng@��	 ^NN�	 ^Ng@���Р&square��	# c,0�	$ c,6@��@����!t��	- c,9�	. c,:@@��	0 c,9�	1 c,:@@��@����#int��	: c,>�	; c,A@@��	= c,>�	> c,A@@����!t��	E c,E�	F c,F@@��	H c,E�	I c,F@@��	K c,>�	L c,F@@��	N c,9�	O c,F@@@���	M��	�A@ ��	�A@ �A�������	� [square m n] takes matrix m and extends its size until its dimensions are 
    n x n, therefore square. m must be greater than or equal to m. Otherwise the
    slice function can be used. @��	_ `ii�	` b	+@@@��	b `ii�	c b	+@@@��	e c,,�	f c,F@��	h c,,�	i c,F@���Р+scalar_mult��	q f���	r f��@��@����!t��	{ f���	| f��@@��	~ f���	 f��@@��@����%float��	� f���	� f��@@��	� f���	� f��@@����!t��	� f���	� f��@@��	� f���	� f��@@��	� f���	� f��@@��	� f���	� f��@@@���	���
A@ ��
A@ �A�������	@ [scalar_mult m x] is the scalar multiplication of [m] and [x]. @��	� eII�	� eI�@@@��	� eII�	� eI�@@@��	� f���	� f��@��	� f���	� f��@�����*ocaml.text��
#A@ ��
$A@ �A�������	u[map m f] is the map for [f] on [m].
   Requires: [m] is an m x 1 matrix where m is the number of nodes in the layer @��	� h���	� i�,@@@��	� h���	� i�,@@��	� h���	� i�,@���Р%slice��	� m���	� m��@��@�������#int��	� m���	� m��@@��	� m���	� m��@@�����#int��	� m���	� m��@@��	� m���	� m��@@@��	� m���	� m��@@��@�������#int��
 m���
 m��@@��
 m���
	 m��@@�����#int��
 m���
 m��@@��
 m���
 m��@@@��
 m���
 m��@@��@����!t��
! m���
" m��@@��
$ m���
% m��@@����!t��
, m���
- m��@@��
/ m���
0 m��@@��
2 m���
3 m��@@��
5 m���
6 m��@@��
8 m���
9 m��@@@���
7��
�A@ ��
�A@ �A�������	>[slice m p1 p2] is a sub-section of [m] between [p1] and [p2] @��
I lYY�
J lY�@@@��
L lYY�
M lY�@@@��
O m���
P m��@��
R m���
S m��@���Р#inv��
[ p�
\ p@��@����!t��
e p�
f p@@��
h p�
i p@@����&option��
p p"�
q p(@�����!t��
y p �
z p!@@��
| p �
} p!@@@��
 p �
� p(@@��
� p�
� p(@@@���
���
�A@ ��
�A@ �A�������	@ [inv m] is [Some i] if [m] is invertable and [None] otherwise. @��
� o���
� o�@@@��
� o���
� o�@@@��
� p�
� p(@��
� p�
� p(@���Р#det��
� sTX�
� sT[@��@����!t��
� sT^�
� sT_@@��
� sT^�
� sT_@@����%float��
� sTc�
� sTh@@��
� sTc�
� sTh@@��
� sT^�
� sTh@@@���
���*A@ ��+A@ �A�������	$ [det m] is the determinant of [m]. @��
� r**�
� r*S@@@��
� r**�
� r*S@@@��
� sTT�
� sTh@��
� sTT�
� sTh@���Р$norm��
� v���
� v��@��@����!t��
� v���
� v��@@��
� v���
� v��@@����%float��
� v���
� v��@@��
� v���
� v��@@��
� v���
� v��@@@���
���hA@ ��iA@ �A�������> [norm m] is the norm of [m]. @�� ujj� uj�@@@�� ujj� uj�@@@�� v��� v��@�� v��� v��@���Р)transpose��! y���" y��@��@����!t��+ y���, y��@@��. y���/ y��@@����!t��6 y���7 y��@@��9 y���: y��@@��< y���= y��@@@���;���A@ ���A@ �A�������	/ [transpose m] is the transpose of matrix [m]. @��M x���N x��@@@��P x���Q x��@@@��S y���T y��@��V y���W y��@���Р(drop_row��_ |&*�` |&2@��@����#int��i |&5�j |&8@@��l |&5�m |&8@@��@����!t��v |&<�w |&=@@��y |&<�z |&=@@����!t��� |&A�� |&B@@��� |&A�� |&B@@��� |&<�� |&B@@��� |&5�� |&B@@@�������A@ ���A@ �A�������	. [drop_row m i] removes the [i]th row of [m]. @��� {���� {�%@@@��� {���� {�%@@@��� |&&�� |&B@��� |&&�� |&B@���Р(drop_col��� {�� {�@��@����#int��� {��� {�@@��� {��� {�@@��@����!t��� {��� {�@@��� {��� {�@@����!t��� {��� {�@@��� {��� {�@@��� {��� {�@@��� {��� {�@@@���װ�BA@ ��CA@ �A�������	1 [drop_col m j] removes the [j]th column of [m]. @��� ~DD�� ~Dz@@@��� ~DD�� ~Dz@@@��� {{�� {�@��� {{�� {�@���Р)to_matrix��� �%)�� �%2@��@����%array�� �%D� �%I@�����%array�� �%=� �%B@�����&string�� �%6� �%<@@�� �%6� �%<@@@�� �%6� �%B@@@��  �%5�! �%I@@����!t��( �%M�) �%N@@��+ �%M�, �%N@@��. �%5�/ �%N@@@���-���A@ ���A@ �A�������	� [to_matrix b] takes in an array of arrays of strings and converts it to a 
    matris of the floats that are contained in each string@��? ����@ ��$@@@��B ����C ��$@@@��E �%%�F �%N@��H �%%�I �%N@���Р%print��Q �y}�R �y�@��@����!t��[ �y��\ �y�@@��^ �y��_ �y�@@����$unit��f �y��g �y�@@��i �y��j �y�@@��l �y��m �y�@@@���k���A@ ���A@ �A�������	# [format t] prints [t] to [stdout].@��} �PP�~ �Px@@@��� �PP�� �Px@@@��� �yy�� �y�@��� �yy�� �y�@���Р&format��� ����� ���@��@�����&Format)formatter��� ����� ���@@��� ����� ���@@��@����!t��� ����� ���@@��� ����� ���@@����$unit��� ����� ���@@��� ����� ���@@��� ����� ���@@��� ����� ���@@@������&A@ ��'A@ �A�������	) [format fmt t] prints out [t] to [fmt]. @��� ����� ���@@@��� ����� ���@@@��� ����� ���@��� ����� ���@���Р$size��� �.2�� �.6@��@����!t��� �.9�� �.:@@��� �.9�� �.:@@����#int��� �.>�� �.A@@��� �.>�� �.A@@��� �.9�� �.A@@@������dA@ ��eA@ �A�������	= [size m] returns the length of the outer array of matrix m. @�� ���� ��-@@@�� ���� ��-@@@�� �..� �.A@�� �..� �.A@���Р+get_first_n�� ���� ���@��@����!t��' ����( ���@@��* ����+ ���@@��@����#int��4 ����5 ���@@��7 ����8 ���@@�������!t��B ����C ���@@��E ����F ���@@�����!t��N ����O ���@@��Q ����R ���@@@��T ����U ���@@��W ����X ���@@��Z ����[ ���@@@���Y���A@ ���A@ �A�������	p [get_first_n m n] returns a touple with the first n elements of matrix m 
    along with the rest of matrix m  @��k �CC�l ���@@@��n �CC�o ���@@@��q ����r ���@��t ����u ���@���Р,to_flat_list��} �8<�~ �8H@��@����!t��� �8K�� �8L@@��� �8K�� �8L@@����$list��� �8V�� �8Z@�����%float��� �8P�� �8U@@��� �8P�� �8U@@@��� �8P�� �8Z@@��� �8K�� �8Z@@@������A@ ��A@ �A�������	T [to_flat_list t] returns a representation of matrix t as a flattened list of floats@��� ����� ��7@@@��� ����� ��7@@@��� �88�� �8Z@��� �88�� �8Z@@