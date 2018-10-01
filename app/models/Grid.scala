package models

case class Grid(rows: Int, cols: Int, nbShipN: Int) {
    val emptyGrid: Array[Array[Element]] = Array.ofDim[Element](rows, cols)
    val alphabet: Array[String] = Array("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
    var grid: Array[Array[Element]] = initialize(emptyGrid, 0, 0)
    var nbShipNeeded: Int = nbShipN
    var nbShipPlaced: Int = 0
    var isGridReady: Boolean = false

    /*
    Return : grid where each slot contains an Element (isShipHere, isShooted)
     */
    def initialize (g: Array[Array[Element]], x: Int, y: Int): Array[Array[Element]] = {
        /* Array maximum size is 26 */
        if (x > 26 || y > 26) g
        /* We copy and initialize the given grid slot */
        var newGrid = g map(identity)
        newGrid(x)(y) = Element(x.toString, alphabet(y))

        if (x != g.size - 1) {
            if (y != g.size - 1) {
                /* We continue to init on the same row */
                initialize(g, x, y + 1)
            } else {
                /* We continue to init on a new row */
                initialize(g, x + 1, 0)
            }
        } else {
            if (y != g.size - 1) {
                /* We continue to init on the last row */
                initialize(g, x, y + 1)
            } else {
                /* We have initialized all grid slot */
                g
            }
        }
    }

    /*
    We check origin, orientation and size.
    Return : True if parameters are ok. False if they aren't.
     */
    private def checkShipParameters (grid: Array[Array[Element]], size: Int, origin: Array[Int], orientation: String): Boolean = {
        if (size.getClass != "int" || origin(0).getClass != "int" || origin(1).getClass != "int" || orientation.getClass != "string") {
            /* A parameter is wrong */
            false
        } else if (size < 1) {
            /* size null */
            false
        } else if (origin(0) < 0 || origin(0) > grid.size - 1) {
            /* x out of range */
            false
        } else if (origin(1) < 0 || origin(1) > grid(0).size - 1) {
            /* y out of range */
            false
        } else if (orientation == "right") {
            if (origin(0) + size > grid(0).size) {
                /* Ship out of range */
                false
            } else {
                true
            }
        } else if (orientation == "left") {
            if (origin(0) - size < -1) {
                /* Ship out of range */
                false
            } else {
                true
            }
        } else if (orientation == "bottom") {
            if (origin(1) + size > grid(1).size) {
                /* Ship out of range */
                false
            } else {
                true
            }
        } else if (orientation == "top") {
            if (origin(1) - size < -1) {
                /* Ship out of range */
                false
            } else {
                true
            }
        } else {
            /* Wrong parameters */
            false
        }
    }

    /*
    We place a ship on the grid if the maximum number of ship isn't reached.
    Return : True if the ship has been placed. False if the ship hasn't been placed.
     */
    def placeShip(grid: Array[Array[Element]], size: Int, origin: Array[Int], orientation: String, nbPiecePlaced: Int): Boolean = {
        if (!checkShipParameters(grid, size, origin, orientation)) {
            /* A parameters is missing/wrong */
            false
        } else if (nbPiecePlaced == size) {
            /* We have place all part of the ship*/
            true
        } else {
            orientation match {
                case "right" => grid(origin(0))(origin(1) + nbPiecePlaced).isShipHere = true
                case "left" => grid(origin(0))(origin(1) - nbPiecePlaced).isShipHere = true
                case "top" => grid(origin(0) - nbPiecePlaced)(origin(1)).isShipHere = true
                case "bottom" => grid(origin(0) + nbPiecePlaced)(origin(1)).isShipHere = true
                case _ => false
            }
            this.placeShip(grid, size, origin, orientation, nbPiecePlaced + 1)
        }
    }

}
