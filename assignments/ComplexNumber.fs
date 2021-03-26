module ComplexNumber
type ComplexNumber =
    | C of float * float
    static member (+) (C(x1,y1), C(x2,y2)) = C(x1 + y1, x2 + y2)
    static member (-) (C(x1,y1), C(x2,y2)) = C(x1 + -y1, x2 + -y2)
    static member (*) (C(x1,y1), C(x2,y2)) = C((x1 * y1) - (x2 * y2), (y1 * x2) + (x1 * y2))
    static member (/) (C(x1,y1), C(x2,y2)) = C((x1 * x2 - y1 * y2) / (c ** 2.0 + d ** 2.0), (b * c - a * d) / (c ** 2.0 + d ** 2.0))
let make (x,y) = C(x,y)