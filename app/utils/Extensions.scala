package utils

object Extensions {
    // simple function that seemed like it could live happily on our Iterable types and would help this exercise
    implicit class Crossable[X](xs: Iterable[X]) {
        def cross[Y](ys: Iterable[Y]): Iterable[(X, Y)] =
            for {
                x <- xs
                y <- ys
            } yield (x, y)
    }
}
