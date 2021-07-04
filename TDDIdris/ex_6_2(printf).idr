import Data.Vect

data Format = ||| This represents %d, followed by the rest of the format specifier.
             Number Format
            | ||| This represents %s, followed by the rest of the format specifier.
             Str Format
            | ||| This represents %c, followed by the rest of the format specifier.
             Cdt Format
            | ||| This represents %f, followed by the rest of the format specifier.
             Ddt Format
            | ||| A literal string, followed by the rest of the format specifier.
             Lit String Format
            | |||An empty format specifier
             End
%name Format fmt

PrintfType: Format -> Type
PrintfType (Number fmt) = (i: Int) -> PrintfType fmt
PrintfType (Str fmt) = (str: String) -> PrintfType fmt
PrintfType (Cdt fmt) = (cdt: Char) -> PrintfType fmt
PrintfType (Ddt fmt) = (ddt : Double) -> PrintfType fmt
PrintfType (Lit Str fmt) = PrintfType fmt
PrintfType End = String

printfFmt: (fmt: Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Cdt fmt) acc = \cdt => printfFmt fmt (acc ++  show cdt)
printfFmt (Ddt fmt) acc = \ddt => printfFmt fmt (acc ++ show ddt)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

toFormat : (xs: List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Cdt (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Ddt (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt: String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

Matrix: Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Nat)

TupleVect : Nat -> Type -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = (ty, (TupleVect k ty))

test: TupleVect 4 Nat
test = (1, 2, 3, 4, ())

sample: TupleVect 2 String
sample = ("Hello", "World", ())
