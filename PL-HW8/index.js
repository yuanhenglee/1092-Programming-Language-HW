class Point {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }
  toString() {
    return "(" + this.x + ", " + this.y + ")";
  }
  equals(other) {
    if ( this instanceof ColorPoint && other instanceof ColorPoint) {
      return (
        this.x === other.x && this.y === other.y && this.color === other.color
      );
    } else {
      return this.x === other.x && this.y === other.y;
    }
  }
}

class ColorPoint extends Point {
  constructor(x, y, color) {
    super(x, y);
    this.color = color;
  }
  toString() {
    return super.toString() + " in " + this.color;
  }
}

let pt1 = new Point(3, 5);
let cp = new ColorPoint(3, 5, "green");
let cp2 = new ColorPoint(3, 5, "red");
let pt2 = cp;

let b = cp.equals(pt2);
console.log(b);
