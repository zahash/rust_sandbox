mod expr;
mod macros;
mod xform;

pub use expr::Expr;
pub use xform::XForm;

use crate::{fun, sym, var, xform};

fn run() {
    let differentiation = xform!(
        fun!(DIFF, fun!(var!(F), var!(x))),
        fun!(
            LIM,
            var!(h),
            sym!(ZERO),
            fun!(
                DIV,
                fun!(
                    SUB,
                    fun!(var!(F), fun!(ADD, var!(x), var!(h))),
                    fun!(var!(F), var!(x))
                ),
                var!(h)
            )
        )
    );

    let a_plus_b_square_expansion = {
        let a_plus_b = fun!(ADD, var!(a), var!(b));
        let a_plus_b_squared = fun!(SQUARE, a_plus_b);

        let a_square = fun!(SQUARE, var!(a));
        let b_square = fun!(SQUARE, var!(b));
        let two_a_b = fun!(MUL, sym!(TWO), fun!(MUL, var!(a), var!(b)));
        let a_square_plus_b_square = fun!(ADD, a_square, b_square);
        let a_square_plus_b_square_plus_2_a_b = fun!(ADD, a_square_plus_b_square, two_a_b);

        xform!(a_plus_b_squared, a_square_plus_b_square_plus_2_a_b)
    };

    let a_plus_b_minus_c_equals_a_minus_c_plus_b = xform!(
        fun!(SUB, fun!(ADD, var!(a), var!(b)), var!(c)),
        fun!(ADD, fun!(SUB, var!(a), var!(c)), var!(b))
    );

    let sub_same = xform!(fun!(SUB, var!(a), var!(a)), sym!(ZERO));

    let add_zero = xform!(fun!(ADD, sym!(ZERO), var!(a)), var!(a));

    let square_to_mul = xform!(fun!(SQUARE, var!(x)), fun!(MUL, var!(x), var!(x)));

    let comm_mul = xform!(fun!(MUL, var!(x), var!(y)), fun!(MUL, var!(y), var!(x)));

    let assoc_mul = xform!(
        fun!(MUL, fun!(MUL, var!(a), var!(b)), var!(c)),
        fun!(MUL, var!(b), fun!(MUL, var!(a), var!(c)))
    );

    let dist_mul = xform!(
        fun!(
            ADD,
            fun!(MUL, var!(a), var!(b)),
            fun!(MUL, var!(a), var!(c))
        ),
        fun!(MUL, var!(a), fun!(ADD, var!(b), var!(c)))
    );

    let div_same = xform!(fun!(DIV, fun!(MUL, var!(a), var!(b)), var!(a)), var!(b));

    let res = fun!(DIFF, fun!(SQUARE, var!(x)))
        .apply(&differentiation)
        .apply(&a_plus_b_square_expansion)
        .apply(&a_plus_b_minus_c_equals_a_minus_c_plus_b)
        .apply(&a_plus_b_minus_c_equals_a_minus_c_plus_b)
        .apply(&sub_same)
        .apply(&add_zero)
        .apply(&square_to_mul)
        .apply(&comm_mul)
        .apply(&assoc_mul)
        .apply(&dist_mul)
        .apply(&div_same);

    println!("{}", res);
}
