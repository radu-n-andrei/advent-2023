package day24

final case class Coordinate3d(x: Double, y: Double, z: Double) {

    def moveX(vx: Int): Coordinate3d = Coordinate3d(x + vx, y, z)
    def moveY(vy: Int): Coordinate3d = Coordinate3d(x, y + vy, z)
    def moveZ(vz: Int): Coordinate3d = Coordinate3d(x, y, z + vz)
}

